get_real_data <- function(season = "2023-2024", n_week = NULL, mobility_type = "radiation") {
    source("source/data_initial_conditions.R", local = TRUE)
    data <- initial_data(season, n_week, mobility_type)
    m <- as.data.frame(data$abs_incidence_reg_age)
    m_2 <- as.data.frame(data$incidence_reg_age)
    n <- as.data.frame(data$abs_incidence_reg)
    n_2 <- as.data.frame(data$incidence_reg)
    incid_nat <- as.data.frame(data$incidence_nat)
    virus_percentage <- as.data.frame(data$virus_percentage)
    colnames(m) <- seq_len(ncol(m))
    colnames(m_2) <- seq_len(ncol(m))
    colnames(n) <- seq_len(ncol(n))
    colnames(n_2) <- seq_len(ncol(n))
    colnames(incid_nat) <- seq_len(ncol(incid_nat))
    return(list(
        tables = list(
            abs_incidence_reg_age = m,
            abs_incidence_reg = n,
            incidence_reg_age = m_2,
            incidence_reg = n_2,
            incidence_nat = incid_nat,
            population_reg_age = data$population_reg_age,
            population_reg = data$population_reg,
            virus_percentage = virus_percentage
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
            region_names = data$region_names,
            region_names_nuts2 = data$region_names_nuts2,
            age_group_names = data$age_group_names,
            virus_percentage = virus_percentage,
            mobility = data$mobility,
            pop_reg = data$population_reg,
            c_matrix = data$c_matrix
        )
    ))
}
