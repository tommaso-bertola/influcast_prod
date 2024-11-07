library(magrittr)
library(dplyr)
library(tidyr)
library(deSolve)
library(compiler)

source("source/lambda_generator.R")

SIIR_simple_ <- function(times, yini, params) {
    with(as.list(params), {
        mm_aa <- n_mm * n_aa
        s_i <- yini[1:mm_aa]
        x_i <- yini[(mm_aa + 1):(2 * mm_aa)]
        x_ii <- yini[(2 * mm_aa + 1):(3 * mm_aa)]
        r_i <- yini[(3 * mm_aa + 1):(4 * mm_aa)]
        n_i <- s_i + x_i + x_ii + r_i

        ds <- -phi_1 * (lambda %*% (x_i / n_i) * s_i) - phi_2 * (lambda %*% (x_ii / n_i) * s_i) + (alpha * r_i)
        dx <- +phi_1 * (lambda %*% (x_i / n_i) * s_i) - mu_1 * x_i
        dx_i <- +phi_2 * (lambda %*% (x_ii / n_i) * s_i) - mu_2 * x_ii
        dr <- (mu_1 * x_i) + (mu_2 * x_ii) - (alpha * r_i)

        return(list(c(as.vector(ds), as.vector(dx), as.vector(dx_i), as.vector(dr))))
    })
}
SIIR_simple <- cmpfun(SIIR_simple_)

generate_ts_ <- function(params) {
    with(as.list(params), {
        mm_aa <- n_mm * n_aa
        pop_patch <- prop_pop * pop_tot
        pop_inf_1 <- pop_patch * prop_inf_1
        pop_inf_2 <- pop_patch * prop_inf_2
        pop_sus <- pop_patch - pop_inf_1 - pop_inf_2
        pop_rec <- (pop_patch - pop_inf_1 - pop_inf_2) * prop_rec

        yini <- c(pop_sus, pop_inf_1, pop_inf_2, pop_rec)
        solution_out <- rk(y = yini, times = times, func = SIIR_simple, parms = params, method = "rk45dp7")

        return(solution_out)
    })
}

get_singular_ts_ <- function(solution_out, n_mm, n_aa) {
    mm_aa <- n_mm * n_aa
    solution_out <- solution_out[, -1]
    s <- solution_out[, 1:mm_aa]
    i <- solution_out[, (mm_aa + 1):(2 * mm_aa)]
    ii <- solution_out[, (2 * mm_aa + 1):(3 * mm_aa)]
    r <- solution_out[, (3 * mm_aa + 1):(4 * mm_aa)]

    return(list(s = s, i = i + ii, r = r))
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

SIIR_model_ <- function(params) {
    solution_out <- generate_ts(params)
    weekly_inc <- ts_to_inc(solution_out, params$n_mm, params$n_aa, params$p_reported)
    return(weekly_inc)
}

local_ep_mod <- cmpfun(SIIR_model_)

bounds_factory <- function() {
    register_bound <- function(list_orig, prefix, lower, upper, num = 1) {
        n_elements <- length(list_orig)
        for (i in 1:num) {
            list_orig[[n_elements + i]] <- list(name = paste0(prefix, "_", i), lower = lower, upper = upper)
        }
        return(list_orig)
    }

    bounds <- list()
    bounds <- register_bound(bounds, "phi_1_inv", 0.1, 50)
    bounds <- register_bound(bounds, "phi_2_inv", 0.1, 50)
    bounds <- register_bound(bounds, "mu_1_inv", 0.1, 50)
    bounds <- register_bound(bounds, "mu_2_inv", 0.1, 50)
    bounds <- register_bound(bounds, "log_p_rep", log(0.001), log(0.99))
    bounds <- register_bound(bounds, "log_p_inf_age", log(0.001), log(0.99), 4)
    bounds <- register_bound(bounds, "log_p_inf_patch", log(0.001), log(0.99), 21)
    bounds <- register_bound(bounds, "log_p_rec_age", log(0.001), log(0.9), 4)
    bounds <- register_bound(bounds, "log_p_rec_patch", log(0.001), log(0.9), 21)
    bounds <- register_bound(bounds, "log_p_inf_1_2", log(0.001), log(0.99))
    bounds <- register_bound(bounds, "suscep", log(0.0001), log(1), 16)
    return(bounds)
}

local_x2params_ <- function(params, x) {
    params$phi_1 <- 1 / x[1]
    params$phi_2 <- 1 / x[2]
    params$mu_1 <- 1 / x[3]
    params$mu_2 <- 1 / x[4]
    params$p_reported <- exp(x[5])
    age_prop_inf <- exp(x[6:9])
    patch_prop_inf <- exp(x[10:30])
    age_prop_rec <- exp(x[31:34])
    patch_prop_rec <- exp(x[35:55])
    perc_inf_1_2 <- exp(x[56])
    params$prop_inf_1 <- age_prop_inf %x% patch_prop_inf * perc_inf_1_2
    params$prop_inf_2 <- age_prop_inf %x% patch_prop_inf * (1 - perc_inf_1_2)
    params$prop_rec <- age_prop_rec %x% patch_prop_rec
    params$lambda <- lambda_5(params$n_mm, params$n_aa, exp(x[57:72]))

    return(params)
}

local_bounds <- bounds_factory()
local_x2params <- cmpfun(local_x2params_)
