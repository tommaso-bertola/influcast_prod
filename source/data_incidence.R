library(dplyr)
library(tidyr)
library(stringr)
library(readr)
library(reshape2)
incidence <- function(season = "2023-2024", n_week = NULL, age_group_names, dcis) {
    source("source/season_limiter.R", local = TRUE)
    # Supporting tables to get the right correspondence
    region_names_correspondence <- data.frame(incidence_db = c("Piedmont", "Aosta Valley", "Lombardy", "AP Bolzano", "AP Trento", "Veneto", "Friuli-Venezia Giulia", "Liguria", "Emilia-Romagna", "Tuscany", "Umbria", "Marche", "Lazio", "Abruzzo", "Molise", "Campania", "Apulia", "Basilicata", "Calabria", "Sicily", "Sardinia"), census_db = c("Piemonte", "Valle d'Aosta / Vallée d'Aoste", "Lombardia", "Provincia Autonoma Bolzano / Bozen", "Provincia Autonoma Trento", "Veneto", "Friuli-Venezia Giulia", "Liguria", "Emilia-Romagna", "Toscana", "Umbria", "Marche", "Lazio", "Abruzzo", "Molise", "Campania", "Puglia", "Basilicata", "Calabria", "Sicilia", "Sardegna"))
    nuts_custom <- data.frame(region_eng = c("Piemonte", "Valle d'Aosta", "Liguria", "Lombardia", "Trentino-Alto Adige", "Trentino-Alto Adige", "Veneto", "Friuli-Venezia Giulia", "Emilia-Romagna", "Toscana", "Umbria", "Marche", "Lazio", "Abruzzo", "Molise", "Campania", "Apulia", "Basilicata", "Calabria", "Sicily", "Sardegna"), region_3 = c("Piedmont", "Aosta Valley", "Liguria", "Lombardy", "AP Bolzano", "AP Trento", "Veneto", "Friuli-Venezia Giulia", "Emilia-Romagna", "Tuscany", "Umbria", "Marche", "Lazio", "Abruzzo", "Molise", "Campania", "Apulia", "Basilicata", "Calabria", "Sicily", "Sardinia"), region = c("Piemonte", "Valle d'Aosta/Vallée d'Aoste", "Liguria", "Lombardia", "PA Bolzano", "PA Trento", "Veneto", "Friuli-Venezia Giulia", "Emilia-Romagna", "Toscana", "Umbria", "Marche", "Lazio", "Abruzzo", "Molise", "Campania", "Puglia", "Basilicata", "Calabria", "Sicilia", "Sardegna"), nuts2 = c("ITC1", "ITC2", "ITC3", "ITC4", "ITD1", "ITD2", "ITD3", "ITD4", "ITD5", "ITE1", "ITE2", "ITE3", "ITE4", "ITF1", "ITF2", "ITF3", "ITF4", "ITF5", "ITF6", "ITG1", "ITG2"))

    # Incidence data
    # file_incidence <- "https://github.com/fbranda/influnet/raw/main/data-aggregated/epidemiological_data/regional_cases.csv"
    file_incidence <- "data/epidemiological/branda/complete_data.csv"
    regional_db_original <- read_csv(file_incidence, show_col_types = FALSE)

    # clean the incidence dataset and select current season
    incidence_all <- regional_db_original %>%
        select(-number_healthcare_workers, -starts_with("cases_")) %>%
        filter(flu_season == season) %>%
        filter(year_week %in% season_limiter(n_week, season)) %>%
        select(-flu_season) %>%
        mutate_all(~ replace(., . == 0, NA)) %>%
        left_join(nuts_custom %>% select(region_3, nuts2), by = c("region" = "region_3")) %>%
        arrange(nuts2)

    if (length(age_group_names) == 4) {
        # apply good namings
        colnames(incidence_all) <- c("year_week", "region", "number_cases", "population", "incidence", age_group_names, "nuts2_tmp")
    } else if (length(age_group_names) == 1) {
        incidence_all <- incidence_all %>%
            select(-starts_with("inc_")) %>%
            rename(nuts2_tmp = nuts2)
    } else {
        stop("age_group_names must be of length 1 or 4")
    }

    # merge census and incidence to have consistent region ordering on data
    incidence_all_census_all <- incidence_all %>%
        left_join(region_names_correspondence, by = c("region" = "incidence_db")) %>%
        left_join(dcis, by = c("census_db" = "region_name")) %>%
        arrange(year_week, nuts2) %>%
        select(-region) %>%
        rename(region = census_db) %>%
        select(year_week, nuts2, -region, number_cases, population, incidence, pop_reg, starts_with("age"))

    # need to compute the marginal abs incidence: remove column and use numbers, then copy from terminal
    abs_inc_reg <- incidence_all_census_all %>%
        mutate(reg_inc = pop_reg * incidence / 1000) %>%
        select(year_week, nuts2, reg_inc) %>%
        mutate(
            nuts2 = str_replace_all(tolower(nuts2), " ", "-"),
        ) %>%
        pivot_wider(values_from = reg_inc, names_from = nuts2) %>%
        select(-year_week) %>%
        as.data.frame() %>%
        {
            colnames(.) <- seq_along(.)
            .
        }

    if (length(age_group_names) == 4) {
        # compute absolute incidence combining census and incidence
        abs_inc_reg_age <- bind_cols(
            incidence_all_census_all[, c(1:2)],
            lapply(age_group_names, function(prefix) {
                f1 <- paste0(prefix, ".x")
                f2 <- paste0(prefix, ".y")
                incidence_all_census_all %>%
                    rowwise() %>%
                    transmute(!!paste0(prefix, ".inc") := !!sym(f1) * !!sym(f2) / 1000)
            })
        ) %>%
            melt() %>%
            mutate(
                nuts2 = str_replace_all(tolower(nuts2), " ", "-"),
                variable = str_sub(str_sub(variable, start = 5), end = -5)
            ) %>%
            pivot_wider(values_from = value, names_from = c("nuts2", "variable")) %>%
            select(-year_week) %>%
            as.data.frame() %>%
            {
                colnames(.) <- seq_along(.)
                .
            }
    } else if (length(age_group_names) == 1) {
        abs_inc_reg_age <- abs_inc_reg
    } else {
        stop("age_group_names must be of length 1 or 4")
        quit(status = 1)
    }

    # some data to output
    n_weeks <- nrow(abs_inc_reg_age)

    # extracting incidence for region
    inc_reg <- incidence_all_census_all %>%
        select(year_week, nuts2, incidence) %>%
        melt() %>%
        select(-variable) %>%
        mutate(nuts2 = str_replace_all(tolower(nuts2), " ", "-")) %>%
        pivot_wider(values_from = value, names_from = nuts2) %>%
        select(-year_week) %>%
        as.data.frame() %>%
        {
            colnames(.) <- seq_along(.)
            .
        }

    if (length(age_group_names) == 4) {
        # extracting incidence for region and age
        inc_reg_age_ <- incidence_all_census_all %>% select(year_week, nuts2, ends_with(".x"))
        colnames(inc_reg_age_) <- c("year_week", "region", age_group_names)

        inc_reg_age <- melt(inc_reg_age_) %>%
            mutate(
                region = str_replace_all(tolower(region), " ", "-"),
                variable = str_sub(str_sub(variable, start = 5), end = -1)
            ) %>%
            pivot_wider(values_from = value, names_from = c("region", "variable")) %>%
            select(-year_week) %>%
            as.data.frame() %>%
            {
                colnames(.) <- seq_along(.)
                .
            }
    } else {
        inc_reg_age <- inc_reg
    }

    inc_nat_tmp <- incidence_all_census_all %>%
        select(year_week, number_cases, population) %>%
        group_by(year_week) %>%
        summarise(tot_cases = sum(number_cases, na.rm = TRUE), tot_pop = sum(population, na.rm = TRUE))

    inc_nat <- inc_nat_tmp %>%
        mutate(incidence_nat = tot_cases / tot_pop * 1000) %>%
        select(incidence_nat) %>%
        as.data.frame() %>%
        {
            colnames(.) <- seq_along(.)
            .
        }

    abs_inc_nat <- inc_nat_tmp %>%
        mutate(abs_incidence_nat = tot_cases) %>%
        select(abs_incidence_nat) %>%
        as.data.frame() %>%
        {
            colnames(.) <- seq_along(.)
            .
        }

    return(list(
        inc_nat = inc_nat,
        abs_inc_nat = abs_inc_nat,
        inc_reg = inc_reg,
        inc_reg_age = inc_reg_age,
        n_weeks = n_weeks,
        abs_inc_reg = abs_inc_reg,
        abs_inc_reg_age = abs_inc_reg_age
    ))
}
