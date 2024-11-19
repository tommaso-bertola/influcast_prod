virus_perc <- function(n_week = NULL) {
    # virus_perc_file <- "data/epidemiological/branda/virus_incidence_definitive.rds"
    # virus_perc <- readRDS(virus_perc_file)
    # n_week_virological <- function(n_week) {
    #     corr <- data.frame(a = c(c(45:52), c(1:17)), b = 1:25)
    #     if (is.null(n_week)) {
    #         n_week <- 25
    #     } else {
    #         n_week <- corr[corr$a == n_week, "b"]
    #     }
    #     return(n_week)
    # }
    # virus_percentage <- as.data.frame(virus_perc[virus_perc$week <= n_week_virological(n_week), -1])
    virus_percentage <- data.frame(A = rep(100, n_week), notA = rep(0, n_week))
    return(virus_percentage)
}
