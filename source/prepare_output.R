library(dplyr)
library(tidyr)
library(jsonlite)
source("source/lambda_generator.R")
source("source/model_chooser.R")

# allow arguments
args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 0) {
    unique_string_ <- args[1]
} else {
    stop("Specify parameters to the script unifying results\n")
    quit(status = 1)
}
# unique_string_ <- "06c49"
file_name <- paste0("output/", unique_string_, "_results.json")

influcast_summariser <- function(dataframe) {
    quantiles <- c(
        0.01, 0.025, 0.05, 0.1,
        0.15, 0.2, 0.25, 0.3, 0.35,
        0.4, 0.45, 0.5, 0.55, 0.6,
        0.65, 0.7, 0.75, 0.8, 0.85,
        0.9, 0.95, 0.975, 0.99
    )
    name_q <- paste0("q", quantiles[1] * 100)
    output <- dataframe %>% summarise(
        {{ name_q }} := quantile(values, quantiles[1], na.rm = TRUE),
        .groups = "drop"
    )

    for (i in c(2:(length(quantiles)))) {
        name_q <- paste0("q", quantiles[i] * 100)
        tmp <- dataframe %>% summarise(
            {{ name_q }} := quantile(values, quantiles[i], na.rm = TRUE),
            .groups = "drop"
        )
        output <- cbind(output, tmp[, ncol(tmp)])
    }
    return(output)
}

transform_param_f_producer <- function(pso_data) {
    function_definition <- pso_data$parameters$simulation_parameters$parameters_transformations

    # new simulations have the transform definition in the json file
    x2params_loaded <- eval(parse(text = paste(function_definition, collapse = "\n")))

    params_new <- function(params, x, i = 1) {
        x <- unname(sapply(x, function(x) x[i]))
        params <- x2params_loaded(params, x)
    }
    return(params_new)
}

prediction_extender <- function(params) {
    times <- params$times
    new_times <- seq(from = max(times) + 1, to = max(times) + 7 * 4, by = 1)
    params$times <- c(times, new_times)
    return(params)
}

results_fn <- function(pso_data) {
    parameter_names <- pso_data$parameters$simulation_parameters$parameter_names
    results <- pso_data$results
    colnames(results) <- parameter_names
    results
}

pop_reg_fn <- function(pso_data) {
    params <- pso_data$complete_list_parameters
    pop_reg <- pso_data$original_data$pop_reg %>%
        mutate(pop_index = row_number()) %>%
        mutate(
            n_patch = ((as.numeric(pop_index) - 1) %% params$n_mm) + 1
        )
    pop_reg
}
pop_reg_age_fn <- function(pso_data) {
    params <- pso_data$complete_list_parameters
    pop_reg_age <- pso_data$original_data$pop_reg_age %>%
        mutate(pop_index = row_number()) %>%
        mutate(
            n_patch = ((as.numeric(pop_index) - 1) %% params$n_mm) + 1,
            n_age = ((as.numeric(pop_index) - 1) %/% params$n_mm) + 1
        )
    pop_reg_age
}
converged_parameters_fn <- function(results, threshold = 1000, only_converged = FALSE) {
    results <- results %>%
        as.data.frame() %>%
        filter(exitvalue <= threshold)
    if (only_converged) {
        results <- results %>%
            filter(exitcode == 0) %>%
            select(-exitvalue, -exitcode)
    } else {
        results <- results %>%
            select(-exitvalue, -exitcode)
    }
    results
}

pso_data <- fromJSON(file_name)
signal <- pso_data$signal
current_week <- pso_data$complete_list_parameters$current_week
results <- results_fn(pso_data)
converged_parameters <- converged_parameters_fn(results, threshold = 4000)
population_reg <- pop_reg_fn(pso_data)
pop_reg_age <- pop_reg_age_fn(pso_data)

pop_national <- pso_data$complete_list_parameters$pop_tot
params <- pso_data$complete_list_parameters
params_transform <- transform_param_f_producer(pso_data)
if (signal == "ILI" || is.null(signal)) {
    epidemic_model_incidence <- model_chooser(pso_data$epidemic_model)$ep_mod
} else if (signal == "AB") {
    epidemic_model_incidence <- model_chooser(pso_data$epidemic_model)$ep_mod_ab
} else {
    stop("Signal not recognized")
    quit(status = 1)
}

melted_incidence <- data.frame()
melted_incidence_a <- data.frame()
melted_incidence_b <- data.frame()
n_iterations <- nrow(converged_parameters)

params <- prediction_extender(params)
for (i in seq_len(n_iterations)) {
    params <- params_transform(params, converged_parameters, i)
    inc_tot <- epidemic_model_incidence(params)
    if (typeof(inc_tot) == "list") {
        if (signal == "AB") {
            inc_a <- inc_tot$abs_inc_a
            inc_b <- inc_tot$abs_inc_b
        } else {
            inc <- inc_tot$abs_inc
        }
    } else {
        inc <- inc_tot
    }
    if (signal == "ILI" || is.null(signal)) {
        if (any(is.na(inc))) {
            cat("\nNA values found in", i, "weeks", params$times, "\nSkipping\n")
            next
        }
        r <- as.data.frame(inc) %>%
            rename_with(~ gsub("X", "", .)) %>%
            mutate(week = seq_len(nrow(.))) %>%
            pivot_longer(cols = -week, values_to = "values", names_to = "patch_age") %>%
            mutate(
                realization = i,
                n_patch = ((as.numeric(patch_age) - 1) %% params$n_mm) + 1,
                n_age = ((as.numeric(patch_age) - 1) %/% params$n_mm) + 1
            )
        melted_incidence <- rbind(melted_incidence, r)
    } else if (signal == "AB") {
        if (any(is.na(inc_a)) || any(is.na(inc_b))) {
            cat("\nNA values found in", i, "weeks", params$times, "\nSkipping\n")
            next
        }
        r_a <- as.data.frame(inc_a) %>%
            rename_with(~ gsub("X", "", .)) %>%
            mutate(week = seq_len(nrow(.))) %>%
            pivot_longer(cols = -week, values_to = "values", names_to = "patch_age") %>%
            mutate(
                realization = i,
                n_patch = ((as.numeric(patch_age) - 1) %% params$n_mm) + 1,
                n_age = ((as.numeric(patch_age) - 1) %/% params$n_mm) + 1
            )
        r_b <- as.data.frame(inc_b) %>%
            rename_with(~ gsub("X", "", .)) %>%
            mutate(week = seq_len(nrow(.))) %>%
            pivot_longer(cols = -week, values_to = "values", names_to = "patch_age") %>%
            mutate(
                realization = i,
                n_patch = ((as.numeric(patch_age) - 1) %% params$n_mm) + 1,
                n_age = ((as.numeric(patch_age) - 1) %/% params$n_mm) + 1
            )
        melted_incidence_a <- rbind(melted_incidence_a, r_a)
        melted_incidence_b <- rbind(melted_incidence_b, r_b)
    } else if (signal == "A" || signal == "B") {
        stop("Signal not recognized")
        quit(status = 1)
    } else {
        stop("Signal not recognized")
        quit(status = 1)
    }
}
if (signal == "ILI") {
    tmp_national <- melted_incidence %>%
        select(-patch_age) %>%
        group_by(week, realization) %>%
        summarize(values = sum(values), .groups = "drop") %>%
        group_by(week)

    tmp_regions <- melted_incidence %>%
        select(-patch_age) %>%
        group_by(week, n_patch, realization) %>%
        summarize(values = sum(values), .groups = "drop") %>%
        group_by(week, n_patch)

    quantiles_regions <- tmp_regions %>%
        influcast_summariser()
    quantiles_national <- tmp_national %>%
        influcast_summariser()

    quant_regional <- quantiles_regions %>%
        left_join(population_reg, by = c("n_patch" = "n_patch")) %>%
        mutate(
            across(starts_with("q"), ~ . / pop_reg * 1000)
        )

    quant_national <- quantiles_national %>%
        mutate(
            across(starts_with("q"), ~ . / pop_national * 1000)
        )

    saveRDS(
        list(
            signal = signal,
            quantiles = quant_regional,
            current_week = current_week
        ),
        paste0("output/regional_quantiles_", unique_string_, ".rds")
    )
    saveRDS(
        list(
            signal = signal,
            quantiles = quant_national,
            current_week = current_week
        ),
        paste0("output/national_quantiles_", unique_string_, ".rds")
    )
} else if (signal == "AB") {
    tmp_national_a <- melted_incidence_a %>%
        select(-patch_age) %>%
        group_by(week, realization) %>%
        summarize(values = sum(values), .groups = "drop") %>%
        group_by(week)
    tmp_national_b <- melted_incidence_b %>%
        select(-patch_age) %>%
        group_by(week, realization) %>%
        summarize(values = sum(values), .groups = "drop") %>%
        group_by(week)

    quantiles_national_a <- tmp_national_a %>%
        influcast_summariser()

    quantiles_national_b <- tmp_national_b %>%
        influcast_summariser()

    quant_national_a <- quantiles_national_a %>%
        mutate(
            across(starts_with("q"), ~ . / pop_national * 1000)
        )
    quant_national_b <- quantiles_national_b %>%
        mutate(
            across(starts_with("q"), ~ . / pop_national * 1000)
        )

    saveRDS(
        list(
            signal = signal,
            quantiles = list(A = quant_national_a, notA = quant_national_b),
            current_week = current_week
        ),
        paste0("output/national_quantiles_", unique_string_, ".rds")
    )
}
