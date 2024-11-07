library(dplyr)
library(compiler)

fitness_ <- function(weekly_inc_vir, data_inc, fitness_tolerance, params) {
    weekly_inc <- weekly_inc_vir$inc
    weekly_perc <- weekly_inc_vir$percent_strains

    vir_inc <- params$virus_percentage

    residuals_df <- abs(weekly_inc - data_inc) / (data_inc + 0.01) # to ensure no division by 0 has to be performed
    res <- 0
    merit_factor <- c(rep(10, 21), rep(1, 21), rep(1, 21), rep(1, 21)) # arbitrarily defined weights for each age group
    if (ncol(residuals_df) != length(merit_factor)) {
        stop("The number of age groups in the residuals_df does not match the number of merit factors")
        quit()
    }
    for (col_index in seq_len(ncol(residuals_df))) {
        col_tolerance <- ifelse(residuals_df[, col_index] < fitness_tolerance, 0, residuals_df[, col_index] * merit_factor[col_index])
        m <- sum(col_tolerance, na.rm = TRUE)
        if (!is.nan(m)) {
            res <- res + m
        }
    }
    res_vir <- 0

    residuals_df_vir <- abs(weekly_perc - vir_inc)
    for (col_index in seq_len(ncol(residuals_df_vir))) {
        col_tolerance <- ifelse(residuals_df_vir[, col_index] < 5, 5, residuals_df_vir[, col_index])
        m <- sum(col_tolerance, na.rm = TRUE)
        if (!is.nan(m)) {
            res_vir <- res_vir + m
        }
    }
    # cat("Fitness: ", res, " - ", res_vir, "\n")
    res <- res + res_vir
    return(res)
}

fitness <- cmpfun(fitness_)
