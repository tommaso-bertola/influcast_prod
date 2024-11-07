library(dplyr)
library(compiler)

fitness_ <- function(weekly_inc, data_inc, fitness_tolerance, params) {
    residuals_df <- abs(weekly_inc - data_inc) / (data_inc + 0.01) # to ensure no division by 0 has to be performed
    res <- 0
    for (col_index in seq_len(ncol(residuals_df))) {
        # col_tolerance <- residuals_df[, col_index] # ifelse(residuals_df[, col_index] < fitness_tolerance, 0, residuals_df[, col_index])
        m <- sum(residuals_df[, col_index], na.rm = TRUE)
        if (!is.nan(m)) {
            res <- res + m
        }
    }
    return(res)
}

fitness <- cmpfun(fitness_)
