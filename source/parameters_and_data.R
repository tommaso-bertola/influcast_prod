get_real_data <- function(season = "2024-2025", n_week = NULL, mobility_type = "radiation", age_groups = NULL, signal = NULL) {
    source("source/data_initial_conditions.R", local = TRUE)
    data <- initial_data(season, n_week, mobility_type, age_groups, signal)
    return(list(
        tables = list(
            incidences = list(
                absolute = list(
                    abs_incidence_reg_age = data$abs_incidence_reg_age,
                    abs_incidence_reg = data$abs_incidence_reg,
                    abs_incidence_nat = data$abs_incidence_nat
                ),
                per_thousand = list(
                    incidence_reg_age = data$incidence_reg_age,
                    incidence_reg = data$incidence_reg,
                    incidence_nat = data$incidence_nat
                )
            ),
            population_reg_age = data$population_reg_age,
            population_reg = data$population_reg,
            virus_percentage = data$virus_percentage
        ),
        params = list(
            n_weeks_simu = data$n_weeks,
            times = seq(0, 7 * data$n_weeks, by = 1),
            alpha = 0,
            n_mm = data$n_mm,
            n_aa = data$n_aa,
            n_mm_aa = data$n_mm * data$n_aa,
            pop_tot = data$italian_population,
            prop_pop = data$initial_fraction,
            population_reg_age = as.vector(data$population_reg_age),
            region_names = data$region_names,
            region_names_nuts2 = data$region_names_nuts2,
            age_group_names = data$age_group_names,
            virus_percentage = data$virus_percentage,
            mobility = data$mobility,
            pop_reg = data$population_reg,
            c_matrix = data$c_matrix,
            current_week = data$current_week
        )
    ))
}
