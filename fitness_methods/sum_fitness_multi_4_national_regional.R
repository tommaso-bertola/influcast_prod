library(dplyr)
library(compiler)

fitness_ <- function(weekly_inc_vir, data_inc, fitness_tolerance, params) {
    weekly_inc <- weekly_inc_vir$inc
    weekly_inc_nat <- weekly_inc_vir$national_inc
    weekly_perc <- weekly_inc_vir$percent_strains
    data_inc_reg_age <- data_inc$per_thousand$incidence_reg_age
    data_inc_nat <- data_inc$per_thousand$incidence_nat
    vir_inc <- params$virus_percentage

    res_reg_age <- 0
    res_vir <- 0
    res_nat <- 0

    residuals_df <- abs(weekly_inc - data_inc_reg_age) / (data_inc_reg_age + 0.001)
    merit_factor <- c(rep(10, 21))

    if (ncol(residuals_df) != length(merit_factor)) {
        stop("The number of age groups in the residuals_df does not match the number of merit factors")
        quit()
    }

    for (col_index in seq_len(ncol(residuals_df))) {
        col_tolerance <- ifelse(residuals_df[, col_index] < fitness_tolerance, 0, residuals_df[, col_index] * merit_factor[col_index])
        m <- sum(col_tolerance, na.rm = TRUE)
        if (!is.nan(m)) {
            res_reg_age <- m
        }
    }
    if (typeof(weekly_perc) == "list") {
        residuals_df_vir <- abs(weekly_perc - vir_inc)
        for (col_index in seq_len(ncol(residuals_df_vir))) {
            col_tolerance <- ifelse(residuals_df_vir[, col_index] < 5, 5, residuals_df_vir[, col_index])
            m <- sum(col_tolerance, na.rm = TRUE)
            if (!is.nan(m)) {
                res_vir <- res_vir + m
            }
        }
    } else if (typeof(weekly_perc) == "double") {
        residuals_df_vir <- sum(abs(weekly_perc - vir_inc$A), na.rm = TRUE)
        if (!is.nan(residuals_df_vir)) {
            res_vir <- res_vir + m
        }
    } else {
        stop("Error in fitness: weekly_inc is not a list or a double")
        quit()
    }

    residuals_df_nat <- abs(weekly_inc_nat - data_inc_nat) / (data_inc_nat + 0.001)
    n_weeks <- length(residuals_df_nat$inc)
    # weights <- c(1:n_weeks) / n_weeks
    # weights <- exp(2 * weights)
    ranged_data <- (data_inc_nat - min(data_inc_nat)) / (max(data_inc_nat) - min(data_inc_nat))
    ranged_data <- (ranged_data + 1) * 3
    weights <- rep(10, n_weeks)
    weights <- weights * ranged_data
    weights[length(weights)] <- max(weights)
    # weights[1:3] <- 0
    residuals_df_nat <- residuals_df_nat$inc * weights
    # remove_idx <- seq(2, length(residuals_df_nat) - 1, by = 2)
    # residuals_df_nat[remove_idx] <- residuals_df_nat[remove_idx] * 2
    # residuals_df_nat<-residuals_df_nat[-c(2:8)]
    mmm <- sum(residuals_df_nat, na.rm = TRUE)
    if (!is.nan(mmm)) {
        res_nat <- mmm
    }
    res <- 1 * res_reg_age + 0.5 * res_vir + 100 * res_nat
    return(res)
}

fitness <- cmpfun(fitness_)
