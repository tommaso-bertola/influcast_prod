# The code defines several functions to process and analyze the PSO results, including converting data to long format,
# generating tables with execution info, saving parameter names, computing model diagnostics, filtering converged results,
# computing summary statistics, plotting histograms, computing incidence for each set of parameters, and plotting error time series.
# The program also includes commented-out code for preparing data, creating tables, and generating a final dashboard.

library(jsonlite)
library(dplyr)
library(ggplot2)
library(reshape2)
library(gridExtra)
library(grid)
library(stringr)

source("source/lambda_generator.R")
source("source/model_chooser.R")


list_files <- function() {
    rev(list.files(path = "sim_results/", full.names = FALSE, pattern = "*.json"))
}

get_region_names <- function(pso_data) {
    region_names_listed <- list()
    region_names_labeller <- c()
    region_names <- pso_data$complete_list_parameters$region_names
    if (is.null(region_names)) {
        region_names <- seq_len(pso_data$complete_list_parameters$n_mm)
    }

    for (i in seq_len(pso_data$complete_list_parameters$n_mm)) {
        region_names_listed[[region_names[i]]] <- i
    }
    region_names_labeller <- region_names
    names(region_names_labeller) <- seq_len(pso_data$complete_list_parameters$n_mm)

    region_ids <- seq_len(pso_data$complete_list_parameters$n_mm)
    return(list(
        listed = region_names_listed,
        ids = region_ids,
        labeller = region_names_labeller
    ))
}

get_age_group_names <- function(pso_data) {
    age_group_names_listed <- list()
    age_group_labeller <- c()

    age_group_names <- pso_data$complete_list_parameters$age_group_names
    if (is.null(age_group_names)) {
        age_group_names <- seq_len(pso_data$complete_list_parameters$n_aa)
    }

    for (i in seq_len(pso_data$complete_list_parameters$n_aa)) {
        age_group_names_listed[[age_group_names[i]]] <- i
    }
    age_group_labeller <- age_group_names
    names(age_group_labeller) <- seq(pso_data$complete_list_parameters$n_aa)
    age_group_ids <- seq_len(pso_data$complete_list_parameters$n_aa)

    return(list(
        listed = age_group_names_listed,
        ids = age_group_ids,
        labeller = age_group_labeller
    ))
}

get_params_transformations_fn <- function(pso_data) {
    params_transformations <- pso_data$parameters$simulation_parameters$parameters_transformations
    parse(text = paste(params_transformations, collapse = "\n"))
}

get_params_names <- function(pso_data) {
    params_names_listed <- list()
    params_antilist <- list()
    params_names <- sapply(pso_data$parameters$simulation_parameters$parameter_names, function(x) {
        sub("_[^_]+$", "", x)
    }, USE.NAMES = FALSE)[-c(1, 2)] %>% unique()

    n_pars <- length(params_names)
    for (i in seq_len(n_pars)) {
        params_names_listed[[params_names[i]]] <- i
        params_antilist[[as.character(i)]] <- params_names[i]
    }

    params_ids <- seq_len(n_pars)
    return(list(
        listed = params_names_listed,
        ids = params_ids,
        antilisted = params_antilist
    ))
}

get_epidemic_model <- function(pso_data) {
    epidemic_model <- pso_data$epidemic_model
    epidemic_model
}

get_fitness_method <- function(pso_data) {
    fitness_method <- pso_data$fitness_method
    fitness_method
}

get_season_data <- function(pso_data) {
    season <- pso_data$season_data
    season
}
get_syntehtic_data <- function(pso_data) {
    syntehtic_data <- pso_data$syntehtic_data
    syntehtic_data
}

sim_data_fn <- function(pso_data) {
    exec_time <- pso_data$parameters$simulation_parameters$exec_time
    runs <- pso_data$parameters$simulation_parameters$runs
    fitness_tolerance <- pso_data$parameters$simulation_parameters$fitness_tolerance
    abstol <- pso_data$parameters$swarm_parameters$abstol

    sim_data <- data.frame(
        names = c("time", "runs", "fit_tol", "abstol"),
        values = c(exec_time, runs, fitness_tolerance, abstol)
    )
    sim_data
}

results_fn <- function(pso_data) {
    parameter_names <- pso_data$parameters$simulation_parameters$parameter_names
    results <- pso_data$results
    colnames(results) <- parameter_names
    results
}

abs_incidence_reg_age_fn <- function(pso_data) {
    params <- pso_data$complete_list_parameters
    abs_incidence <- pso_data$original_data$abs_inc_reg_age %>%
        mutate(week = row_number()) %>%
        pivot_longer(cols = -week, names_to = "variable", values_to = "value") %>%
        mutate(
            n_patch = ((as.numeric(variable) - 1) %% params$n_mm) + 1,
            n_age = ((as.numeric(variable) - 1) %/% params$n_mm) + 1
        )
    abs_incidence
}

abs_incidence_reg_fn <- function(pso_data) {
    params <- pso_data$complete_list_parameters
    abs_incidence <- pso_data$original_data$abs_inc_reg %>%
        mutate(week = row_number()) %>%
        pivot_longer(cols = -week, names_to = "variable", values_to = "value") %>%
        mutate(
            n_patch = ((as.numeric(variable) - 1) %% params$n_mm) + 1
        )
    abs_incidence
}

inc_reg_age_fn <- function(pso_data) {
    params <- pso_data$complete_list_parameters
    inc_reg_age <- pso_data$original_data$inc_reg_age %>%
        mutate(week = row_number()) %>%
        pivot_longer(cols = -week, names_to = "variable", values_to = "value") %>%
        mutate(
            n_patch = ((as.numeric(variable) - 1) %% params$n_mm) + 1,
            n_age = ((as.numeric(variable) - 1) %/% params$n_mm) + 1
        )
    inc_reg_age
}

inc_reg_fn <- function(pso_data) {
    params <- pso_data$complete_list_parameters
    inc_reg <- pso_data$original_data$inc_reg %>%
        mutate(week = row_number()) %>%
        pivot_longer(cols = -week, names_to = "variable", values_to = "value") %>%
        mutate(
            n_patch = ((as.numeric(variable) - 1) %% params$n_mm) + 1
        ) %>%
        select(week, n_patch, value)
    inc_reg
}

inc_nat_fn <- function(pso_data) {
    params <- pso_data$complete_list_parameters
    if (!is.null(pso_data$original_data$inc_nat)) {
        inc_nat <- pso_data$original_data$inc_nat %>%
            mutate(week = row_number()) %>%
            pivot_longer(cols = -week, names_to = "variable", values_to = "value") %>%
            select(week, value)
    } else {
        inc_nat <- data.frame(week = c(1:30), value = rep(1, 30))
    }
    inc_nat
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

pop_reg_fn <- function(pso_data) {
    params <- pso_data$complete_list_parameters
    pop_reg <- pso_data$original_data$pop_reg %>%
        mutate(pop_index = row_number()) %>%
        mutate(
            n_patch = ((as.numeric(pop_index) - 1) %% params$n_mm) + 1
        )
    pop_reg
}

# For model diagnostics
exitstatus_results_fn <- function(results) {
    results %>%
        as.data.frame() %>%
        select(exitcode) %>%
        group_by(exitcode) %>%
        count()
}

exitvalue_results_fn <- function(results) {
    results %>%
        as.data.frame() %>%
        select(exitvalue) %>%
        summarize(min = min(exitvalue), max = max(exitvalue))
}

exitvalue_hist_fn <- function(results) {
    results %>%
        as.data.frame() %>%
        ggplot(aes(x = exitvalue)) +
        geom_histogram(aes(y = after_stat(density)), fill = "lightblue", color = "black", bins = 30) +
        geom_density(alpha = 0.5, fill = "#eec790") +
        labs(x = "Exit value", y = "Density")
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
converged_parameters_melted_fn <- function(converged_parameters) {
    converged_parameters_melted <- converged_parameters %>%
        melt(id.vars = NULL)
}

summary_values_fn <- function(converged_parameters_melted) {
    summary_values <- converged_parameters_melted %>%
        group_by(variable) %>%
        summarize(
            mean_val = round(mean(value), 3),
            low = round(quantile(value, 0.05, na.rm = T), 3),
            upp = round(quantile(value, 0.95, na.rm = T), 3)
        )
    colnames(summary_values) <- c("Name", "Mean", "LowCL", "HighCL")
    summary_values
}

params_selected_fn <- function(input, antilisted) {
    selected <- sapply(input, function(x) antilisted[[x]])
    selected
}

hist_plot_fn <- function(converged_parameters_melted, summary_values, prefixes) {
    joined_cls <- left_join(converged_parameters_melted,
        summary_values,
        by = c("variable" = "Name")
    ) %>%
        mutate(variable = as.character(variable)) %>%
        filter(sapply(variable, function(x) any(str_starts(x, as.vector(prefixes)))))

    hist_plot <- ggplot(joined_cls, aes(x = value)) +
        geom_histogram(aes(y = after_stat(density)), fill = "lightblue", color = "black", bins = 30) +
        geom_density(alpha = 0.5, fill = "#eec790", ) +
        labs(x = NULL, y = "Density") +
        geom_vline(aes(xintercept = Mean), color = "red") +
        geom_vline(aes(xintercept = LowCL), color = "blue", linetype = "dashed") +
        geom_vline(aes(xintercept = HighCL), color = "blue", linetype = "dashed") +
        facet_wrap(. ~ variable, scales = "free", ncol = 4)
}

prediction_extender <- function(params) {
    times <- params$times
    new_times <- seq(from = max(times) + 1, to = max(times) + 7 * 4, by = 1)
    params$times <- c(times, new_times)
    return(params)
}

df_long_fn <- function(converged_parameters, pso_data, population_reg, population_reg_age, update_progress = NULL) {
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

    my_summarizer <- function(dataframe) {
        df <- dataframe %>% summarize(
            mean = mean(values, na.rm = TRUE),
            devstd = sd(values, na.rm = TRUE),
            quant_up = quantile(values, 0.9, names = FALSE, na.rm = T),
            quant_do = quantile(values, 0.1, names = FALSE, na.rm = T),
            .groups = "drop"
        )
        return(df)
    }

    influcast_summariser <- function(dataframe) {
        quantiles <- c(0.01, 0.025, 0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.35, 0.4, 0.45, 0.5, 0.55, 0.6, 0.65, 0.7, 0.75, 0.8, 0.85, 0.9, 0.95, 0.975, 0.99)
        name_q <- paste0("q", quantiles[1] * 100)
        output <- dataframe %>% summarise({{ name_q }} := quantile(values, quantiles[1], na.rm = T), .groups = "drop")

        for (i in c(2:(length(quantiles)))) {
            name_q <- paste0("q", quantiles[i] * 100)
            tmp <- dataframe %>% summarise({{ name_q }} := quantile(values, quantiles[i], na.rm = T), .groups = "drop")
            output <- cbind(output, tmp[, ncol(tmp)])
        }
        return(output)
    }

    pop_national <- pso_data$complete_list_parameters$pop_tot
    params <- pso_data$complete_list_parameters
    params_transform <- transform_param_f_producer(pso_data)
    epidemic_model_incidence <- model_chooser(pso_data$epidemic_model)$ep_mod

    melted_incidence <- data.frame()
    n_iterations <- nrow(converged_parameters)

    params <- prediction_extender(params)
    for (i in seq_len(n_iterations)) {
        params <- params_transform(params, converged_parameters, i)
        inc_tot <- epidemic_model_incidence(params)
        if (typeof(inc_tot) == "list") {
            inc <- inc_tot$abs_inc
        } else {
            inc <- inc_tot
        }
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

        if (is.function(update_progress)) {
            text <- paste0(i, " su ", n_iterations)
            update_progress(detail = text, n = n_iterations)
        }
    }

    df_long <- melted_incidence %>%
        select(-patch_age) %>%
        group_by(week, n_patch, n_age) %>%
        my_summarizer()

    tmp_regions <- melted_incidence %>%
        select(-patch_age) %>%
        group_by(week, n_patch, realization) %>%
        summarize(values = sum(values), .groups = "drop") %>%
        group_by(week, n_patch)

    tmp_national <- melted_incidence %>%
        select(-patch_age) %>%
        group_by(week, realization) %>%
        summarize(values = sum(values), .groups = "drop") %>%
        group_by(week)

    df_long_regions <- tmp_regions %>%
        my_summarizer()

    quantiles_regions <- tmp_regions %>%
        influcast_summariser()

    df_long_nation <- tmp_national %>%
        my_summarizer()

    quantiles_national <- tmp_national %>%
        influcast_summariser()

    inc_regions_age <- df_long %>%
        left_join(population_reg_age, by = c("n_patch" = "n_patch", "n_age" = "n_age")) %>%
        mutate(
            across(all_of(c("mean", "devstd", "quant_up", "quant_do")), ~ . / pop_reg_age * 1000)
        )

    inc_regions <- df_long_regions %>%
        left_join(population_reg, by = c("n_patch" = "n_patch")) %>%
        mutate(
            across(all_of(c("mean", "devstd", "quant_up", "quant_do")), ~ . / pop_reg * 1000)
        )

    quant_regional <- quantiles_regions %>%
        left_join(population_reg, by = c("n_patch" = "n_patch")) %>%
        mutate(
            across(starts_with("q"), ~ . / pop_reg * 1000)
        )

    inc_national <- df_long_nation %>%
        mutate(
            across(all_of(c("mean", "devstd", "quant_up", "quant_do")), ~ . / pop_national * 1000)
        )

    quant_national <- quantiles_national %>%
        mutate(
            across(starts_with("q"), ~ . / pop_national * 1000)
        )
    saveRDS(quant_regional, "output/regional_quantiles.rds")
    saveRDS(quant_national, "output/national_quantiles.rds")


    return(list(
        abs_inc_reg_age = df_long,
        abs_inc_reg = df_long_regions,
        inc_reg_age = inc_regions_age,
        inc_reg = inc_regions,
        inc_nat = inc_national,
        inc_nat_quantiles = data.frame(), # quant_national,
        inc_reg_quantiles = data.frame() # quant_regional
    ))
}

plot_error_ts_fn <- function(sim_data, orig_data, pso_data, selected_regions = c(1:100), selected_age_groups = c(1:100)) {
    sim_data <- sim_data %>% filter(
        n_patch %in% selected_regions,
        n_age %in% selected_age_groups
    )
    orig_data <- orig_data %>% filter(
        n_patch %in% selected_regions,
        n_age %in% selected_age_groups
    )
    plot_error_ts <- ggplot(sim_data) +
        geom_bar(data = orig_data, mapping = aes(x = week, y = value, fill = "orange"), stat = "identity", show.legend = TRUE) +
        geom_line(aes(x = week, y = mean, colour = "black")) +
        geom_pointrange(aes(x = week, y = mean, ymin = quant_do, ymax = quant_up, colour = "black"), fatten = 0.5) +
        facet_grid(n_age ~ n_patch, labeller = labeller(
            n_age = get_age_group_names(pso_data)$labeller,
            n_patch = get_region_names(pso_data)$labeller,
            scales = "free_y"
        )) +
        scale_fill_manual(
            values = c("orange" = "orange"),
            labels = c("orange" = "Empirical data")
        ) +
        scale_color_manual(
            values = c("black" = "black"),
            labels = c("black" = "Model prediction (95% CL)")
        ) +
        labs(
            # title = "Incidence from PSO (black line)",
            x = "Epidemic week (progressive counting)",
            y = "Weekly incidence (per thousands individuals)",
            fill = "",
            col = ""
        ) +
        theme(legend.position = "bottom")
    plot_error_ts
}

# Plot the error time series age group groupped
plot_error_ts_fn_regions <- function(sim_data, orig_data, pso_data, selected_regions = c(1:100)) {
    # sim_data <- sim_data %>%
    #     filter(
    #         n_patch %in% selected_regions
    #     )

    # orig_data <- orig_data %>% filter(
    #     n_patch %in% selected_regions
    # )
    plot_error_ts <- ggplot(sim_data) +
        geom_bar(data = orig_data, mapping = aes(x = week, y = value, fill = "orange"), stat = "identity") +
        geom_line(aes(x = week, y = mean, colour = "black")) +
        geom_pointrange(aes(x = week, y = mean, ymin = quant_do, ymax = quant_up, colour = "black"), fatten = 0.5) +
        facet_wrap(~n_patch, labeller = labeller(
            n_patch = get_region_names(pso_data)$labeller,
            scales = "free_y"
        )) +
        scale_fill_manual(
            values = c("orange" = "orange"),
            labels = c("orange" = "Empirical data"),
        ) +
        scale_color_manual(
            values = c("black" = "black"),
            labels = c("black" = "Model prediction (95% CL)")
        ) +
        labs(
            # title = "Incidence from PSO (black line)",
            x = "Epidemic week (progressive counting)",
            y = "Weekly incidence (per 1000 individuals)",
            fill = "",
            col = ""
        ) +
        theme(legend.position = "bottom")

    plot_error_ts
}

# Plot the error time series age group groupped
plot_error_ts_fn_national <- function(sim_data, orig_data) {
    plot_error_ts <- ggplot(sim_data) +
        geom_bar(data = orig_data, mapping = aes(x = week, y = value, fill = "orange"), stat = "identity") +
        geom_line(aes(x = week, y = mean, colour = "black")) +
        geom_pointrange(aes(x = week, y = mean, ymin = quant_do, ymax = quant_up, colour = "black"), fatten = 0.5) +
        scale_fill_manual(
            values = c("orange" = "orange"),
            labels = c("orange" = "Empirical data")
        ) +
        scale_color_manual(
            values = c("black" = "black"),
            labels = c("black" = "Model prediction (95% CL)")
        ) +
        labs(
            title = "Incidence from PSO (black line)",
            x = "Epidemic week (progressive counting)",
            y = "Weekly incidence",
            fill = "",
            col = ""
        ) +
        theme(legend.position = "bottom")

    plot_error_ts
}

# # Prepare all the data
# pso_data <- fromJSON("/Users/tommasobertola/Git/influcast_prod/code_influenza/particles_swarm_optimization/sim_results/sim_res_20240605_091715.json")
# # pso_data <- fromJSON("/Users/tommasobertola/Git/influcast_prod/code_influenza/particles_swarm_optimization/sim_results/sim_res_20240604_173902.json")
# results <- results_fn(pso_data)
# pop_reg_age <- pop_reg_age_fn(pso_data)
# pop_reg <- pop_reg_fn(pso_data)
# converged_parameters <- converged_parameters_fn(results)
# df_long <- df_long_fn(converged_parameters, pso_data, pop_reg, pop_reg_age)


# abs_incidence_reg_age <- abs_incidence_reg_age_fn(pso_data)
# # abs_incidence_reg <- abs_incidence_reg_fn(pso_data)
# # incidence_reg_age <- inc_reg_age_fn(pso_data)
# # incidence_reg <- inc_reg_fn(pso_data)

# sim_data <- sim_data_fn(pso_data)


# # exitstatus_results <- exitstatus_results_fn(results)
# # exitstatus_hist <- exitvalue_hist_fn(results)
# # summary_values <- summary_values_fn(converged_parameters)
# # hist_plot <- hist_plot_fn(converged_parameters, summary_values)

# # # Plotting the timeseries


# plot_error_ts <- plot_error_ts_fn(df_long$regions_ages, abs_incidence_reg_age, pso_data)
# plot_error_ts_2 <- plot_error_ts_fn(df_long$inc_regions_age, incidence_reg_age, pso_data)
# plot_error_ts_reg <- plot_error_ts_fn_regions(df_long$regions, abs_incidence_reg, pso_data)
# plot_error_ts_2_reg <- plot_error_ts_fn_regions(df_long$inc_regions, incidence_reg, pso_data)
