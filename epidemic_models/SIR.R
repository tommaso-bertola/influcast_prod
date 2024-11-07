library(magrittr)
library(dplyr)
library(tidyr)
library(deSolve)
library(compiler)

source("source/lambda_generator.R")

SIR_simple_ <- function(times, yini, params) {
    with(as.list(params), {
        mm_aa <- n_mm * n_aa
        s_i <- yini[1:mm_aa]
        x_i <- yini[(mm_aa + 1):(2 * mm_aa)]
        r_i <- yini[(2 * mm_aa + 1):(3 * mm_aa)]
        n_i <- s_i + x_i + r_i

        ds <- -phi * (lambda %*% (x_i / n_i) * s_i) + (alpha * r_i)
        dx <- +phi * (lambda %*% (x_i / n_i) * s_i) - mu * x_i
        dr <- (mu * x_i) - (alpha * r_i)

        return(list(c(as.vector(ds), as.vector(dx), as.vector(dr))))
    })
}
SIR_simple <- cmpfun(SIR_simple_)

generate_ts_ <- function(params) {
    with(as.list(params), {
        mm_aa <- n_mm * n_aa
        pop_patch <- prop_pop * pop_tot
        pop_inf <- pop_patch * prop_inf
        pop_sus <- pop_patch - pop_inf
        pop_rec <- (pop_patch - pop_inf) * prop_rec

        yini <- c(pop_sus, pop_inf, pop_rec)
        solution_out <- rk(y = yini, times = times, func = SIR_simple, parms = params, method = "rk2")

        return(solution_out)
    })
}

get_singular_ts_ <- function(solution_out, n_mm, n_aa) {
    mm_aa <- n_mm * n_aa
    solution_out <- solution_out[, -1]
    s <- solution_out[, 1:mm_aa]
    i <- solution_out[, (mm_aa + 1):(2 * mm_aa)]
    r <- solution_out[, (2 * mm_aa + 1):(3 * mm_aa)]

    return(list(s = s, i = i, r = r))
}

ts_to_inc_ <- function(solution_out, n_mm, n_aa, p_reported) {
    singular_ts <- get_singular_ts(solution_out, n_mm, n_aa)
    s <- singular_ts$s

    inc <- -apply(as.data.frame(s), 2, diff) %>%
        as.data.frame() %>%
        mutate(week = (row(.) - 1) %/% 7 + 1) %>%
        group_by(week) %>%
        summarize(across(everything(), sum)) %>%
        select(-starts_with("week")) %>%
        mutate_all(~ round(. * p_reported, 0))

    return(inc)
}

generate_ts <- cmpfun(generate_ts_)
get_singular_ts <- cmpfun(get_singular_ts_)
ts_to_inc <- cmpfun(ts_to_inc_)

SIR_model_ <- function(params) {
    solution_out <- generate_ts(params)
    weekly_inc <- ts_to_inc(solution_out, params$n_mm, params$n_aa, params$p_reported)
    return(weekly_inc)
}

local_ep_mod <- cmpfun(SIR_model_)

bounds_factory <- function() {
    register_bound <- function(list_orig, prefix, lower, upper, num = 1) {
        n_elements <- length(list_orig)
        for (i in 1:num) {
            list_orig[[n_elements + i]] <- list(name = paste0(prefix, "_", i), lower = lower, upper = upper)
        }
        return(list_orig)
    }

    bounds <- list()
    bounds <- register_bound(bounds, "phi_inv", 0.1, 50)
    bounds <- register_bound(bounds, "mu_inv", 0.1, 50)
    bounds <- register_bound(bounds, "log_p_rep", log(0.001), log(0.99))
    bounds <- register_bound(bounds, "log_p_inf_age", log(0.001), log(0.99), 4)
    bounds <- register_bound(bounds, "log_p_inf_patch", log(0.001), log(0.99), 21)
    bounds <- register_bound(bounds, "log_p_rec_age", log(0.001), log(0.9), 4)
    bounds <- register_bound(bounds, "log_p_rec_patch", log(0.001), log(0.9), 21)
    bounds <- register_bound(bounds, "suscep", log(0.0001), log(1), 16)
    return(bounds)
}

local_x2params_ <- function(params, x) {
    params$phi <- 1 / x[1]
    params$mu <- 1 / x[2]
    params$p_reported <- exp(x[3])
    age_prop_inf <- exp(x[4:7])
    patch_prop_inf <- exp(x[8:28])
    age_prop_rec <- exp(x[29:32])
    patch_prop_rec <- exp(x[33:53])
    params$prop_inf <- age_prop_inf %x% patch_prop_inf
    params$prop_rec <- age_prop_rec %x% patch_prop_rec
    params$lambda <- lambda_5(params$n_mm, params$n_aa, exp(x[54:69]))

    return(params)
}

local_bounds <- bounds_factory()
local_x2params <- cmpfun(local_x2params_)
