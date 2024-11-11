fitness_methods_chooser <- function(fitness_evaluation_method) {
    fitness_files <- list(
        "standard" = "fitness_methods/standard_fitness.R",
        "sum" = "fitness_methods/sum_fitness.R",
        "sum_multi" = "fitness_methods/sum_fitness_multi.R",
        "sum_multi_2" = "fitness_methods/sum_fitness_multi_2.R",
        "sum_multi_3" = "fitness_methods/sum_fitness_multi_3.R",
        "sum_no_thr" = "fitness_methods/sum_no_thr_fitness.R",
        "peak" = "fitness_methods/peak_fitness.R",
        "peak_squared" = "fitness_methods/peak_squared_fitness.R",
        "peak_squared_no_thr" = "fitness_methods/peak_squared_no_thr_fitness.R",
        "peak_squared_no_thr_4" = "fitness_methods/peak_squared_no_thr_fitness_4.R"
    )

    if (!fitness_evaluation_method %in% names(fitness_files)) {
        stop("Fitness evaluation method not recognized:", fitness_evaluation_method)
    }

    source(fitness_files[[fitness_evaluation_method]], local = TRUE)
    fit_method <- fitness
    fit_method
}
