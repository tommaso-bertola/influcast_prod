library(dplyr)
library(tidyr)
library(stringr)
library(readr)
library(reshape2)

source("source/data_census.R")
source("source/data_mobility.R")

initial_data <- function(season = "2024-2025", n_week = NULL, mobility_type = "radiation", age_groups = NULL, signal = "ILI") {
    census_df <- census(age_groups)
    mobility_matr <- mobility(mobility_type = mobility_type)
    c_matrix_data <- as.matrix(readRDS("data/census/grouped_contact_matrix.rds"))
    if (season == "2023-2024") {
        source("source/data_incidence.R", local = TRUE)
        source("source/data_virus.R", local = TRUE)
        incidence_df <- incidence(season, n_week, census_df$age_group_names, census_df$dcis)
        virus_percentage <- virus_perc(n_week = n_week)
        current_week <- NULL
    } else if (season == "2024-2025") {
        if (signal == "ILI") {
            source("source/data_incidence_influcast.R", local = TRUE)
            source("source/data_virus_influcast.R", local = TRUE)
            incidence_df <- incidence(season, n_week, census_df$italian_population, census_df$region_name_population)
            virus_percentage <- virus_perc(n_week = incidence_df$n_weeks)
            current_week <- incidence_df$current_week
        } else if (signal == "AB") {
            source("source/data_incidence_influcast_ab.R", local = TRUE)

            incidence_df <- incidence(season, n_week, census_df$italian_population, census_df$region_name_population)
            virus_percentage <- incidence_df$percent_ab # virus_perc(n_week = incidence_df$n_weeks)
            current_week <- incidence_df$current_week
        } else {
            stop("Signal not recognized:", signal)
        }
    }

    return(list(
        n_weeks = incidence_df$n_weeks,
        abs_incidence_reg_age = incidence_df$abs_inc_reg_age,
        abs_incidence_reg = incidence_df$abs_inc_reg,
        abs_incidence_nat = incidence_df$abs_inc_nat,
        incidence_nat = incidence_df$inc_nat,
        incidence_reg_age = incidence_df$inc_reg_age,
        incidence_reg = incidence_df$inc_reg,
        initial_fraction = census_df$initial_fraction,
        n_mm = census_df$n_mm,
        n_aa = census_df$n_aa,
        italian_population = census_df$italian_population,
        region_names = census_df$region_names,
        region_names_nuts2 = census_df$region_names_nuts2,
        age_group_names = census_df$age_group_names,
        population_reg_age = census_df$population_reg_age,
        population_reg = census_df$population_reg,
        virus_percentage = virus_percentage,
        mobility = mobility_matr,
        c_matrix = c_matrix_data,
        current_week = current_week
    ))
}
