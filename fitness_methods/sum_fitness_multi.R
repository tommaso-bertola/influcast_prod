library(dplyr)
library(compiler)

fitness_ <- function(weekly_inc, data_inc, fitness_tolerance, params) {
    # weekly_inc <- weekly_inc_vir$inc
    # weekly_perc <- weekly_inc_vir$vir

    vir_inc <- params$virus_percentage

    residuals_df <- abs(weekly_inc - data_inc) / (data_inc + 0.01) # to ensure no division by 0 has to be performed
    res <- 0
    for (col_index in seq_len(ncol(residuals_df))) {
        col_tolerance <- ifelse(residuals_df[, col_index] < fitness_tolerance, 0, residuals_df[, col_index])
        m <- sum(col_tolerance, na.rm = TRUE)
        if (!is.nan(m)) {
            res <- res + m
        }
    }
    res_vir <- 0
    # if (!is.null(vir_inc)) {
    #     residuals_df_vir <- abs(weekly_perc - vir_inc) / (vir_inc + 0.01)
    #     for (col_index in seq_len(ncol(residuals_df_vir))) {
    #         col_tolerance <- ifelse(residuals_df_vir[, col_index] < fitness_tolerance, 1, 1 + residuals_df_vir[, col_index])
    #         m <- sum(col_tolerance, na.rm = TRUE)
    #         if (!is.nan(m)) {
    #             res_vir <- res_vir + m
    #         }
    #     }
    # }
    res <- res + res_vir
    return(res)
}

fitness <- cmpfun(fitness_)
