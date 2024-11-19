season_limiter <- function(n_week, season) {
    year <- as.numeric(substr(season, 1, 4))
    year_week_list <- c()
    if (is.null(n_week)) {
        for (i in c(40:52)) {
            year_week_list <- c(year_week_list, paste0(year, "-", i))
        }
        for (i in c(1:9)) {
            year_week_list <- c(year_week_list, paste0(year + 1, "-0", i))
        }
        for (i in c(10:30)) {
            year_week_list <- c(year_week_list, paste0(year + 1, "-", i))
        }
    } else if (n_week > 40) {
        for (i in c(40:n_week)) {
            year_week_list <- c(year_week_list, paste0(year, "-", i))
        }
    } else {
        for (i in c(40:52)) {
            year_week_list <- c(year_week_list, paste0(year, "-", i))
        }
        for (i in c(1:n_week)) {
            if (i < 10) {
                i <- paste0("0", i)
            }
            year_week_list <- c(year_week_list, paste0(year + 1, "-", i))
        }
    }
    return(year_week_list)
}
