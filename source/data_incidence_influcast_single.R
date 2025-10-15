library(dplyr)
library(tidyr)
library(stringr)
library(readr)
library(reshape2)
incidence <- function(season = NULL, n_week = NULL, italian_population, reg_pop, signal, consolidate = FALSE) {
    # Supporting tables to get the right correspondence
    # region_names_correspondence <- data.frame(incidence_db = c("Piedmont", "Aosta Valley", "Lombardy", "AP Bolzano", "AP Trento", "Veneto", "Friuli-Venezia Giulia", "Liguria", "Emilia-Romagna", "Tuscany", "Umbria", "Marche", "Lazio", "Abruzzo", "Molise", "Campania", "Apulia", "Basilicata", "Calabria", "Sicily", "Sardinia"), census_db = c("Piemonte", "Valle d'Aosta / Vallée d'Aoste", "Lombardia", "Provincia Autonoma Bolzano / Bozen", "Provincia Autonoma Trento", "Veneto", "Friuli-Venezia Giulia", "Liguria", "Emilia-Romagna", "Toscana", "Umbria", "Marche", "Lazio", "Abruzzo", "Molise", "Campania", "Puglia", "Basilicata", "Calabria", "Sicilia", "Sardegna"))
    # nuts_custom <- data.frame(region_eng = c("Piemonte", "Valle d'Aosta", "Liguria", "Lombardia", "Trentino-Alto Adige", "Trentino-Alto Adige", "Veneto", "Friuli-Venezia Giulia", "Emilia-Romagna", "Toscana", "Umbria", "Marche", "Lazio", "Abruzzo", "Molise", "Campania", "Apulia", "Basilicata", "Calabria", "Sicily", "Sardegna"), region_3 = c("Piedmont", "Aosta Valley", "Liguria", "Lombardy", "AP Bolzano", "AP Trento", "Veneto", "Friuli-Venezia Giulia", "Emilia-Romagna", "Tuscany", "Umbria", "Marche", "Lazio", "Abruzzo", "Molise", "Campania", "Apulia", "Basilicata", "Calabria", "Sicily", "Sardinia"), region = c("Piemonte", "Valle d'Aosta/Vallée d'Aoste", "Liguria", "Lombardia", "PA Bolzano", "PA Trento", "Veneto", "Friuli-Venezia Giulia", "Emilia-Romagna", "Toscana", "Umbria", "Marche", "Lazio", "Abruzzo", "Molise", "Campania", "Puglia", "Basilicata", "Calabria", "Sicilia", "Sardegna"), nuts2 = c("ITC1", "ITC2", "ITC3", "ITC4", "ITD1", "ITD2", "ITD3", "ITD4", "ITD5", "ITE1", "ITE2", "ITE3", "ITE4", "ITF1", "ITF2", "ITF3", "ITF4", "ITF5", "ITF6", "ITG1", "ITG2"))

    source("source/acquisitor_epidemiological_single.R", local = TRUE)
    df <- influcast_data_acquisitor(n_week, signal, season)
    inc_nat <- data.frame(inc = df$italy_incidence$incidence)
    inc_reg <- inc_reg_age <- df$region_incidence %>%
        select(-year_week) %>%
        {
            colnames(.) <- seq_along(.)
            .
        }

    # consolidation
    if (consolidate) {
        consolidation_func <- read.csv("/home/ubuntu/influcast_prod/data/epidemiological/consolidation/consolidation_italy_2324_2425.csv")
        consolidation_func <- consolidation_func[1:13, 2]
        consolidation_func[13] <- 100 # last week is not consolidated
        consolidation_func <- rev(consolidation_func)[1:nrow(inc_nat)]
        inc_nat <- inc_nat / (consolidation_func / 100)
        inc_reg <- inc_reg / (consolidation_func / 100)
    } else {
        # does nothing
    }

    abs_inc_nat <- inc_nat * italian_population / 1000

    n_weeks <- nrow(inc_nat)
    abs_inc_reg <- df$region_incidence %>%
        pivot_longer(cols = -year_week, names_to = "region_code_nuts", values_to = "incidence") %>%
        separate(region_code_nuts, into = c("region", "code", "nuts"), sep = "-") %>%
        left_join(reg_pop, by = c("nuts" = "nuts2")) %>%
        mutate(abs_inc = incidence * pop_reg / 1000) %>%
        select(-region, -nuts, -incidence, -pop_reg) %>%
        pivot_wider(names_from = code, values_from = abs_inc) %>%
        select(-year_week) %>%
        as.data.frame() %>%
        {
            colnames(.) <- seq_along(.)
            .
        }
    abs_inc_reg_age <- abs_inc_reg
    current_week <- df$current_week
    return(list(
        inc_nat = inc_nat,
        abs_inc_nat = abs_inc_nat,
        inc_reg = inc_reg,
        inc_reg_age = inc_reg_age,
        n_weeks = n_weeks,
        abs_inc_reg = abs_inc_reg,
        abs_inc_reg_age = abs_inc_reg_age,
        current_week = current_week,
        percent_ab = df$perc_ab
    ))
}
