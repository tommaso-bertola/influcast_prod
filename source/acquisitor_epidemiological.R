library(dplyr)
library(tidyr)
library(magrittr)

# read the data
influcast_data_acquisitor <- function(max_week_filter = NULL, season = NULL) {
    region_names <- read.csv("data/epidemiological/influcast/regions.txt",
        colClasses = c("character", "character", "character"),
        header = TRUE
    )
    path <- paste0("/home/ubuntu/Influcast/sorveglianza/ILI/", season, "/latest")
    df <- data.frame()
    for (i in seq_len(nrow(region_names))) {
        path_file <- paste0(path, "/", region_names[i, ]$region, "-latest-ILI.csv")
        tmp <- tryCatch(read.csv(path_file),
            error = function(e) {
                data.frame(anno = numeric(0), settimana = numeric(0), numero_casi = numeric(0), numero_assistiti = numeric(0), incidenza = numeric(0), target = numeric(0))
            }
        )
        if (nrow(tmp) == 0) {
            # Add a row with NA values
            tmp <- rbind(tmp, setNames(as.list(rep(NA, ncol(tmp))), names(tmp)))
        }
        tmp$region <- region_names[i, ]$region
        tmp$code <- region_names[i, ]$code
        tmp$nuts2 <- region_names[i, ]$nuts2
        tmp$incidenza[tmp$incidenza == 0] <- NA
        df <- rbind(df, tmp)
    }

    # region incidence
    region_wider <- df %>%
        select(region, code, nuts2, anno, settimana, incidenza) %>%
        arrange(nuts2) %>%
        mutate(
            year_week = paste0(anno, "_", sprintf("%02d", settimana)),
            region_code = paste0(region, "-", code, "-", nuts2)
        ) %>%
        select(region_code, year_week, incidenza) %>%
        pivot_wider(names_from = region_code, values_from = incidenza) %>%
        as.data.frame() %>%
        filter(year_week != "NA_NA")

    year_weeks <- region_wider$year_week

    tmp_italy <- read.csv(paste0("/home/ubuntu/Influcast/sorveglianza/ILI/", season, "/latest/italia-latest-ILI.csv")) %>%
        mutate(year_week = paste0(anno, "_", sprintf("%02d", settimana))) %>%
        select(year_week, incidenza)

    year_weeks_italy <- tmp_italy$year_week

    total_year_weeks <- data.frame(year_week = unique(c(year_weeks, year_weeks_italy)))

    # complete national and regional incidence tables
    italy_incidence_complete <- tmp_italy %>%
        full_join(total_year_weeks) %>%
        # arrange(year_week) %>%
        rename(incidence = incidenza)

    region_incidence_complete <- region_wider %>%
        full_join(total_year_weeks) # %>%
    # arrange(year_week)

    # if (!is.null(max_week_filter)) {
    #     italy_incidence_complete <- italy_incidence_complete %>%
    #         filter(year_week <= max_week_filter)
    #     region_incidence_complete <- region_incidence_complete %>%
    #         filter(year_week <= max_week_filter)
    # }
    current_week <- as.character(italy_incidence_complete$year_week[nrow(italy_incidence_complete)])

    return(list(
        italy_incidence = italy_incidence_complete,
        region_incidence = region_incidence_complete,
        current_week = current_week
    ))
}
