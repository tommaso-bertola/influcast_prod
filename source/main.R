library(dplyr)
library(pso)
library(compiler)
library(iterators)
library(jsonlite)
library(future)
library(foreach)
library(doFuture)
library(parallel)
library(digest)

# allow arguments
args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 0) {
    unique_string_ <- args[1]
    comment_ <- args[2] # comment to add to the results
    epi_fit_age_ <- args[3]
    epidemic_model_ <- strsplit(epi_fit_age_, "-")[[1]][1]
    fitness_method_ <- strsplit(epi_fit_age_, "-")[[1]][2]
    age_groups_ <- as.numeric(strsplit(epi_fit_age_, "-")[[1]][3])
    counter_ <- as.numeric(args[4]) # dummy index to compute more iterations
    weeks_ <- args[5] # index to check how many weeks to include
    maxit_ <- as.numeric(args[6]) # maximum number of iterations
    runs_ <- as.numeric(args[7]) # number of runs
    swarmsize_ <- as.numeric(args[8]) # change swarm size
    season_ <- args[9]
    signal_ <- args[10]
} else {
    stop("Specify parameters to the script\n")
    quit(status = 1)
}

# %dopar% parallelizes via future
doFuture::registerDoFuture()

# set to maximum number
n_workers <- detectCores()

# forked parallel processing (via 'parallel')
future::plan(multisession, workers = n_workers)

options(doFuture.rng.onMisuse = "ignore")

source("source/parameters_and_data.R")
source("source/model_chooser.R")
source("source/fitness_method_chooser.R")


model_builder <- function(epidemic_model, fitness_evaluation_method) {
    fit_ev_func <- fitness_methods_chooser(fitness_evaluation_method)
    model <- model_chooser(epidemic_model)

    ep_mod_func <- model$ep_mod
    x2params <- model$x2params
    x2params_ <- model$x2params_
    param_bounds <- model$bounds
    fitness_fnc <- function(x, fitness_tolerance, data_inc, params) {
        params <- x2params(params, x)
        weekly_inc <- ep_mod_func(params)
        fit_ev_func(weekly_inc, data_inc, fitness_tolerance, params)
    }
    return(list(
        fitness = fitness_fnc,
        bounds = param_bounds,
        param_transform = x2params_
    ))
}

parallel_PSO <- function(
    desc = comment_,
    gnu_parallel_string = unique_string_,
    epidemic_model = epidemic_model_,
    fitness_method = fitness_method_,
    season_data = season_,
    n_week = weeks_,
    mobility_type = "radiation",
    fitness_tolerance = 0.1,
    runs = runs_,
    maxit = maxit_,
    abstol = 0.1,
    swarmsize = swarmsize_,
    inertia = 0.5,
    c.p = 0.4,
    c.g = 0.6,
    age_groups = age_groups_,
    signal = signal_) {
    # create the fitness function depending on the epidemic model
    model <- model_builder(epidemic_model, fitness_method)
    fitness_compiled <- cmpfun(model$fitness)
    parameter_bounds <- model$bounds
    param_transform <- model$param_transform

    n_pars <- length(parameter_bounds)
    cat("N params: ", n_pars, "\n")
    lower_bound <- upper_bound <- parameter_names <- c()
    for (par in seq_len(n_pars)) {
        lower_bound <- c(lower_bound, parameter_bounds[[par]]$lower)
        upper_bound <- c(upper_bound, parameter_bounds[[par]]$upper)
        parameter_names <- c(parameter_names, parameter_bounds[[par]]$name)
    }

    # get real data
    real_data_and_pars <- get_real_data(season_data, n_week, mobility_type, age_groups, signal)
    real_data <- real_data_and_pars$tables
    params <- real_data_and_pars$params
    data_inc <- real_data_and_pars$tables$incidences
    unique_run_string <- paste0(gnu_parallel_string, "_", substr(digest(date(), algo = "md5"), 1, 5))

    # create progress bar
    time <- system.time({
        results <- foreach(x = icount(runs), .combine = rbind, .options.future = list(chunk.size = 1)) %dofuture% {
            time_inner_loop <- system.time({
                x <- psoptim(rep(NA, n_pars),
                    fn = fitness_compiled,
                    fitness_tolerance = fitness_tolerance,
                    data_inc = data_inc,
                    params = params,
                    lower = lower_bound,
                    upper = upper_bound,
                    control = list(
                        maxit = maxit,
                        s = swarmsize,
                        w = inertia,
                        c.p = c.p,
                        c.g = c.g,
                        abstol = abstol
                    )
                )
            })
            json_file <- paste0(
                "tmp_res_",
                format(Sys.time(), "%Y%m%d_%H%M%S"), "_", unique_run_string, ".json"
            )
            writeLines(
                toJSON(
                    list(
                        unique_run_string = unique_run_string,
                        description = desc,
                        signal = signal,
                        epi_model = epidemic_model,
                        fit_method = fitness_method,
                        time_single_run = time_inner_loop[3],
                        maxit = maxit,
                        swarmsize = swarmsize,
                        fit_tolerance = fitness_tolerance,
                        exitvalue = x$value,
                        converged = x$convergence,
                        n_week = n_week,
                        params = x$par
                    ),
                    pretty = TRUE, digits = 8, na = "string"
                ),
                paste0("sim_results/tmp_res/", json_file)
            )
            c(x$value, x$convergence, time_inner_loop[3], x$par)
        }
    })
    # print info
    cat("Runs:", runs, "\nTook ", time[3], "sec\nSummary:", colMeans(results[, -c(1, 2, 3)], na.rm = TRUE), "\n")

    # aggregate to save final file
    rownames(results) <- c()
    pso_results <- list(
        signal = signal,
        description = desc,
        unique_run_string = unique_run_string,
        epidemic_model = epidemic_model,
        fitness_method = fitness_method,
        season_data = season_data,
        syntehtic_data = FALSE,
        parameters = list(
            simulation_parameters = list(
                runs = runs,
                lower_bound = lower_bound,
                upper_bound = upper_bound,
                exec_time = time[3],
                workers = n_workers,
                fitness_tolerance = fitness_tolerance,
                parameter_names = c("exitvalue", "exitcode", parameter_names),
                n_pars = n_pars,
                parameters_transformations = deparse(param_transform)
            ),
            swarm_parameters = list(
                maxit = maxit,
                s = swarmsize,
                w = inertia,
                c.p = c.p,
                c.g = c.g,
                abstol = abstol
            )
        ),
        summary_results = list(
            mean_par = colMeans(results, na.rm = TRUE),
            sd_par = apply(results, 2, sd, na.rm = TRUE)
        ),
        results = results[, -3],
        timing = results[, 3],
        complete_list_parameters = params,
        original_data = list(
            abs_inc_reg_age = real_data$incidences$absolute$abs_incidence_reg_age,
            abs_inc_reg = real_data$incidences$absolute$abs_incidence_reg,
            abs_inc_nat = real_data$incidences$absolute$abs_incidence_nat,
            inc_reg_age = real_data$incidences$per_thousand$incidence_reg_age,
            inc_reg = real_data$incidences$per_thousand$incidence_reg,
            inc_nat = real_data$incidences$per_thousand$incidence_nat,
            pop_reg_age = real_data$population_reg_age,
            pop_reg = real_data$population_reg
        )
    )

    json_file <- paste0("sim_res_", format(Sys.time(), "%Y%m%d_%H%M%S"), "_", unique_run_string, ".json")
    rds_file <- paste0("sim_res_", format(Sys.time(), "%Y%m%d_%H%M%S"), "_", unique_run_string, ".rds")
    writeLines(toJSON(pso_results, pretty = TRUE, digits = 8, na = "string"), paste0("sim_results/", json_file))
    saveRDS(pso_results, paste0("sim_results/", rds_file))
    return(results)
}

# real business happening here
res <- parallel_PSO()


#' Run parallel particle swarm optimization (PSO) algorithm
#'
#' This function runs a parallel implementation of the particle swarm optimization (PSO) algorithm
#' to optimize a given fitness function. It uses multiple parallel processes to speed up the optimization process.
