source("source/data_census.R")
source("source/data_mobility.R")

get_real_data <- function(season = NULL, n_week = NULL, mobility_type = "radiation", age_groups = NULL, signal = NULL, consolidate = FALSE) {
    census_df <- census(age_groups)
    mobility_matr <- mobility(mobility_type = mobility_type)
    c_matrix_data <- as.matrix(readRDS("data/census/grouped_contact_matrix.rds"))
    if (season == "2023-2024") {
        source("source/data_incidence.R", local = TRUE)
        source("source/data_virus.R", local = TRUE)
        incidence_df <- incidence(season, n_week, census_df$age_group_names, census_df$dcis)
        virus_percentage <- virus_perc(n_week = n_week)
        current_week <- NULL
    } else if (season == "2024-2025" || season == "2025-2026") {
        if (signal == "ILI" || signal == "I" || is.null(signal)) {
            print("Using ILI data")
            source("source/data_incidence_influcast.R", local = TRUE)
            source("source/data_virus_influcast.R", local = TRUE)
            incidence_df <- incidence(season, n_week, census_df$italian_population, census_df$region_name_population, consolidate)
            virus_percentage <- virus_perc(n_week = incidence_df$n_weeks)
            current_week <- incidence_df$current_week
        } else if (signal == "AB") {
            print("Using A/B data")
            source("source/data_incidence_influcast_ab.R", local = TRUE)
            incidence_df <- incidence(season, n_week, census_df$italian_population, census_df$region_name_population, consolidate)
            virus_percentage <- incidence_df$percent_ab # virus_perc(n_week = incidence_df$n_weeks)
            current_week <- incidence_df$current_week
        } else if (signal == "A" || signal == "B") {
            print(paste0("Using single strain data: ", signal))
            source("source/data_incidence_influcast_single.R", local = TRUE)
            incidence_df <- incidence(season, n_week, census_df$italian_population, census_df$region_name_population, signal, consolidate)
            virus_percentage <- incidence_df$percent_ab # virus_perc(n_week = incidence_df$n_weeks)
            current_week <- incidence_df$current_week
        } else {
            stop("Signal not recognized")
            quit(status = 1)
        }
    } else if (is.null(season)) {
        stop("Season must be specified: season null in parameters_and_data.R/get_real_data()")
    } else {
        stop("Season not recognized in parameters_and_data.R/get_real_data()")
        quit(status = 1)
    }

    return(list(
        tables = list(
            incidences = list(
                absolute = list(
                    abs_incidence_reg_age = incidence_df$abs_inc_reg_age,
                    abs_incidence_reg = incidence_df$abs_inc_reg,
                    abs_incidence_nat = incidence_df$abs_inc_nat
                ),
                per_thousand = list(
                    incidence_reg_age = incidence_df$inc_reg_age,
                    incidence_reg = incidence_df$inc_reg,
                    incidence_nat = incidence_df$inc_nat
                )
            ),
            population_reg_age = census_df$population_reg_age,
            population_reg = census_df$population_reg,
            virus_percentage = virus_percentage
        ),
        params = list(
            n_weeks_simu = incidence_df$n_weeks,
            times = seq(0, 7 * incidence_df$n_weeks, by = 1),
            alpha = 0,
            n_mm = census_df$n_mm,
            n_aa = census_df$n_aa,
            n_mm_aa = census_df$n_mm * census_df$n_aa,
            pop_tot = census_df$italian_population,
            prop_pop = census_df$initial_fraction,
            population_reg_age = as.vector(census_df$population_reg_age),
            region_names = census_df$region_names,
            region_names_nuts2 = census_df$region_names_nuts2,
            age_group_names = census_df$age_group_names,
            virus_percentage = virus_percentage,
            mobility = mobility_matr,
            pop_reg = census_df$population_reg,
            c_matrix = c_matrix_data,
            current_week = current_week
        )
    ))
}
