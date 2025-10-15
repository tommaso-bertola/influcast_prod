library(dplyr)
library(tidyr)
library(magrittr)

# read the data
influcast_data_acquisitor <- function(max_week_filter = NULL, signal = NULL, season=NULL) {
    region_names <- read.csv("data/epidemiological/influcast/regions.txt",
        colClasses = c("character", "character", "character"),
        header = TRUE
    )
    if (signal == "A" || signal == "B") {
        tmp_italy <- read.csv(paste0("/home/ubuntu/Influcast/sorveglianza/ILI+_FLU/", season, "/latest/italia-latest-ILI+_FLU_", signal, ".csv")) %>%
            mutate(year_week = paste0(anno, "-", sprintf("%02d", settimana))) %>%
            select(year_week, incidenza) %>%
            mutate(
                perc_a = 100,
                perc_b = 0
            ) %>%
            select(year_week, incidenza, perc_a, perc_b)
    } else {
        stop("Signal not recognized")
        quit(status = 1)
    }

    italy_incidence_complete <- tmp_italy %>%
        select(year_week, incidenza) %>%
        rename(incidence = incidenza)

    # Create a dataframe with identical columns
    tmp_regions <- as.data.frame(replicate(nrow(region_names), tmp_italy$incidenza))
    tmp_regions <- cbind(tmp_italy$year_week, tmp_regions)

    region_code <- region_names %>%
        mutate(region_code = paste0(region, "-", code, "-", nuts2)) %>%
        select(region_code) %>%
        pull()
    colnames(tmp_regions) <- c("year_week", region_code)
    current_week <- as.character(tmp_italy$year_week[nrow(tmp_italy)])
    region_incidence_complete <- tmp_regions

    return(list(
        italy_incidence = italy_incidence_complete,
        region_incidence = region_incidence_complete,
        current_week = current_week,
        perc_ab = data.frame(A = tmp_italy$perc_a, notA = tmp_italy$perc_b)
    ))
}
