library(dplyr)
library(tidyr)
library(stringr)
library(readr)
library(reshape2)

census <- function(age_groups = NULL) {
    # Handle the census data
    file_istat <- "data/census/DCIS_POPRES1_21032024103707419.csv"
    dcis <- utils::read.csv(file_istat, header = TRUE)
    if (age_groups == 4) {
        age_group_names <- c("age_0_4", "age_5_14", "age_15_64", "age_65_end")
        starts <- c(0, 5, 15, 65)
    } else if (age_groups == 1) {
        age_group_names <- c("age_0_end")
        starts <- c(0)
    } else {
        stop("Age groups not recognized ", age_groups, "\nCaused by: census")
    }

    # get the appropriate class depending on the age
    age_groupper_ <- function(x) {
        for (i in seq_along(starts)) {
            min_ <- starts[i]
            if (is.na(starts[i + 1])) {
                max_ <- 120
                max_name <- "end"
            } else {
                max_ <- starts[i + 1]
                max_name <- max_ - 1
            }
            if (min_ <= x && x < max_) {
                age_class <- paste0("age_", min_, "_", max_name)
            }
        }
        return(age_class)
    }

    # wrapper to have a working function for data.frame
    wrapper_age_groupper <- function(x) {
        return(apply(as.matrix(x), 1, age_groupper_))
    }

    dcis <- dcis %>%
        select(-TIPO_DATO15, -Flag.Codes, -Flags, -Tipo.di.indicatore.demografico, -TIME, -Seleziona.periodo) %>%
        filter(Sesso == "totale", Stato.civile == "totale", str_length(ITTER107) == 4, ETA1 != "TOTAL") %>%
        select(-Sesso, -SEXISTAT1, -STATCIV2, -ETA1, -Stato.civile) %>%
        mutate(age = as.numeric(gsub("\\D", "", .[, "Etaa"]))) %>%
        filter(ITTER107 != "ITDA") %>%
        select(-Etaa) %>%
        mutate(age_group = wrapper_age_groupper(age)) %>%
        select(-age) %>%
        group_by(ITTER107, Territorio, age_group) %>%
        summarise(pop = sum(Value), .groups = "drop") %>%
        pivot_wider(names_from = age_group, values_from = pop) %>%
        rename(nuts2 = ITTER107, region_name = Territorio) %>%
        rowwise() %>%
        mutate(pop_reg = sum(across(starts_with("age")))) %>%
        arrange(nuts2)

    # Get the total population
    total_italian_pop <- sum(dcis$pop_reg)

    # Get the fractions
    italian_pop_distrib <- dcis %>%
        mutate(across(starts_with("age"), ~ . / total_italian_pop, .names = "{col}_norm")) %>%
        select(region_name, all_of(ends_with("_norm")))

    # Concatenate the specified columns
    columns_to_concatenate <- paste0(age_group_names, "_norm")
    initial_fraction <- as.vector(unlist(italian_pop_distrib[columns_to_concatenate]))


    # Region order census
    region_order_census <- dcis %>%
        select(region_name) %>%
        unique()

    region_nuts2 <- dcis %>%
        select(nuts2) %>%
        unique()

    # n_mm n_aa
    n_mm <- nrow(dcis)
    n_aa <- length(starts)

    # get data on population from census
    # computes people per region and age group
    population_reg_age <- dcis %>%
        mutate(region_name = str_replace_all(tolower(region_name), " ", "-")) %>%
        select(-nuts2)

    population_reg_age <- data.frame(pop_reg_age = as.vector(unlist(population_reg_age[age_group_names])))

    # computes people per region
    population_reg <- data.frame(pop_reg = dcis$pop_reg)

    return(list(
        dcis = dcis,
        initial_fraction = initial_fraction,
        n_mm = n_mm,
        n_aa = n_aa,
        italian_population = total_italian_pop,
        region_names = as.vector(unlist(region_order_census)),
        region_names_nuts2 = as.vector(unlist(region_nuts2)),
        age_group_names = as.vector(age_group_names),
        population_reg_age = population_reg_age,
        population_reg = population_reg,
        region_name_population = data.frame(nuts = region_nuts2, population = population_reg)
    ))
}
