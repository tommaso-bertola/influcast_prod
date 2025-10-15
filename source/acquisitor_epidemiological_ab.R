library(dplyr)
library(tidyr)
library(magrittr)

# read the data
influcast_data_acquisitor <- function(max_week_filter = NULL, season = NULL) {
    if (is.null(season)) {
        stop("Season must be specified in acquisitor_epidemiological_ab.R/influcast_data_acquisitor()")
    }
    region_names <- read.csv("data/epidemiological/influcast/regions.txt",
        colClasses = c("character", "character", "character"),
        header = TRUE
    )

    tmp_italy_a <- read.csv(paste0("/home/ubuntu/Influcast/sorveglianza/ILI+_FLU/", season, "/latest/italia-latest-ILI+_FLU_A.csv")) %>%
        mutate(year_week = paste0(anno, "_", sprintf("%02d", settimana))) %>%
        select(year_week, incidenza)
    tmp_italy_b <- read.csv(paste0("/home/ubuntu/Influcast/sorveglianza/ILI+_FLU/", season, "/latest/italia-latest-ILI+_FLU_B.csv")) %>%
        mutate(year_week = paste0(anno, "_", sprintf("%02d", settimana))) %>%
        select(year_week, incidenza)

    tmp_italy <- tmp_italy_a %>%
        left_join(tmp_italy_b, by = c("year_week"), suffix = c("_a", "_b")) %>%
        mutate(
            incidenza = incidenza_a + incidenza_b,
            perc_a = incidenza_a / incidenza * 100,
            perc_b = 100 - perc_a
        ) %>%
        select(year_week, incidenza, perc_a, perc_b)

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
    current_week <- as.character(max(tmp_italy$year_week))
    region_incidence_complete <- tmp_regions

    return(list(
        italy_incidence = italy_incidence_complete,
        region_incidence = region_incidence_complete,
        current_week = current_week,
        perc_ab = data.frame(A = tmp_italy$perc_a, notA = tmp_italy$perc_b)
    ))
}
