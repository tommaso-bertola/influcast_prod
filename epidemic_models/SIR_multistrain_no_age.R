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
        x_ii <- yini[(2 * mm_aa + 1):(3 * mm_aa)]
        r_i <- yini[(3 * mm_aa + 1):(4 * mm_aa)]
        r_ii <- yini[(4 * mm_aa + 1):(5 * mm_aa)]
        n_i <- s_i + x_i + x_ii + r_i + r_ii

        ds <- -(lambda_1 %*% (x_i / n_i) * s_i) - (lambda_2 %*% (x_ii / n_i) * s_i)
        dx <- +(lambda_1 %*% (x_i / n_i) * s_i) - mu_1 * x_i
        dx_i <- +(lambda_2 %*% (x_ii / n_i) * s_i) - mu_2 * x_ii
        dr_1 <- mu_1 * x_i
        dr_2 <- mu_2 * x_ii

        return(list(c(as.vector(ds), as.vector(dx), as.vector(dx_i), as.vector(dr_1), as.vector(dr_2))))
    })
}
SIR_simple <- cmpfun(SIR_simple_)

generate_ts_ <- function(params) {
    with(as.list(params), {
        mm_aa <- n_mm * n_aa
        pop_patch <- prop_pop * pop_tot
        pop_inf <- pop_patch * prop_inf
        pop_inf_i <- pop_patch * prop_inf_i
        pop_sus <- (pop_patch - pop_inf - pop_inf_i) * (1 - prop_rec)
        pop_rec <- (pop_patch - pop_inf - pop_inf_i) * prop_rec
        pop_rec_i <- pop_rec * prop_rec_12
        pop_rec_ii <- pop_rec * (1 - prop_rec_12)

        yini <- c(pop_sus, pop_inf, pop_inf_i, pop_rec_i, pop_rec_ii)
        solution_out <- rk(y = yini, times = times, func = SIR_simple, parms = params, method = "rk2")

        return(solution_out)
    })
}

get_singular_ts_ <- function(solution_out, n_mm, n_aa) {
    mm_aa <- n_mm * n_aa
    solution_out <- solution_out[, -1]
    s <- solution_out[, 1:mm_aa]
    xi <- solution_out[, (mm_aa + 1):(2 * mm_aa)]
    xii <- solution_out[, (2 * mm_aa + 1):(3 * mm_aa)]
    r_1 <- solution_out[, (3 * mm_aa + 1):(4 * mm_aa)]
    r_2 <- solution_out[, (4 * mm_aa + 1):(5 * mm_aa)]

    return(list(s = s, xi = xi, xii = xii, r_1 = r_1, r_2 = r_2))
}

ts_to_inc_ <- function(solution_out, n_mm, n_aa, p_reported, pop_national, pop_reg_age) {
    singular_ts <- get_singular_ts(solution_out, n_mm, n_aa)
    new_inf_i <- singular_ts$xi
    new_inf_ii <- singular_ts$xii
    new_rec_i <- singular_ts$r_1
    new_rec_ii <- singular_ts$r_2

    strain_1 <- rowSums(new_inf_i + new_rec_i)
    strain_2 <- rowSums(new_inf_ii + new_rec_ii)

    percent_strains <- data.frame(strain_1, strain_2) %>%
        apply(., 2, diff) %>%
        as.data.frame() %>%
        mutate(week = (row(.[1]) - 1) %/% 7 + 1) %>%
        group_by(week) %>%
        summarize(across(c(strain_1, strain_2), ~ sum(.) / sum(strain_1 + strain_2) * 100)) %>%
        as.data.frame() %>%
        select(-week)

    s <- singular_ts$s

    # -(S(t+1) - S(t))
    abs_inc <- -apply(as.data.frame(s), 2, diff) %>%
        as.data.frame() %>%
        mutate(week = (row(.) - 1) %/% 7 + 1) %>%
        group_by(week) %>%
        summarize(across(everything(), sum)) %>%
        select(-starts_with("week")) %>%
        mutate_all(~ round(. * p_reported, 0)) %>%
        as.data.frame()

    abs_national_inc <- rowSums(abs_inc)
    if (nrow(abs_inc) != length(abs_national_inc)) {
        stop("Error in ts_to_inc: inc and national_inc have different number of rows")
        quit()
    }
    national_inc <- abs_national_inc / pop_national * 1000
    inc <- sweep(abs_inc, 2, unlist(pop_reg_age), FUN = "/") * 1000

    return(list(
        abs_inc = abs_inc,
        abs_national_inc = abs_national_inc,
        inc = inc,
        national_inc = national_inc,
        percent_strains = percent_strains
    ))
}

generate_ts <- cmpfun(generate_ts_)
get_singular_ts <- cmpfun(get_singular_ts_)
ts_to_inc <- cmpfun(ts_to_inc_)

SIR_model_ <- function(params) {
    solution_out <- generate_ts(params)
    weekly_inc <- ts_to_inc(solution_out, params$n_mm, params$n_aa, params$p_reported, params$pop_tot, params$population_reg_age)
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
    bounds <- register_bound(bounds, "log_phi", log(0.00001), log(1))
    bounds <- register_bound(bounds, "mu_inv_1", 0.05, 70)
    bounds <- register_bound(bounds, "mu_inv_2", 0.05, 70)
    bounds <- register_bound(bounds, "log_p_rep", log(0.05), log(0.8))
    # bounds <- register_bound(bounds, "log_p_inf_age", log(0.00001), log(0.99), 4)
    bounds <- register_bound(bounds, "log_p_inf_patch", log(0.0001), log(0.99), 21)
    # bounds <- register_bound(bounds, "log_p_inf_age_1", log(0.00001), log(0.99), 4)
    bounds <- register_bound(bounds, "log_p_inf_patch_1", log(0.0001), log(0.99), 21)
    # bounds <- register_bound(bounds, "log_p_rec_age", log(0.000001), log(0.9), 4)
    bounds <- register_bound(bounds, "log_p_rec_patch", log(0.0001), log(0.9), 21)
    bounds <- register_bound(bounds, "q_1", log(0.00001), log(20), 1)
    bounds <- register_bound(bounds, "q_2", log(0.00001), log(20), 1)
    bounds <- register_bound(bounds, "log_prop_rec_12", log(0.00001), log(0.9))
    return(bounds)
}

local_x2params_ <- function(params, x) {
    params$phi <- exp(x[1])
    params$mu_1 <- as.vector(1 / x[2] %x% rep(1, 21))
    params$mu_2 <- as.vector(1 / x[3] %x% rep(1, 21))
    params$p_reported <- exp(x[4])
    params$prop_inf <- exp(x[5:25])
    params$prop_inf_i <- exp(x[26:46])
    params$prop_rec <- exp(x[47:67])
    tmp <- lambda_gen_2(params$phi, exp(x[68:69]), params)
    params$prop_rec_12 <- exp(x[70])
    params$lambda_1 <- tmp$lambda_1
    params$lambda_2 <- tmp$lambda_2
    return(params)
}

local_bounds <- bounds_factory()
local_x2params <- cmpfun(local_x2params_)
