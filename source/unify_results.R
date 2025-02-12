library(jsonlite)
library(dplyr)
library(tidyr)

# allow arguments
args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 0) {
    unique_string_ <- args[1]
} else {
    stop("Specify parameters to the script unifying results\n")
    quit(status = 1)
}
# unique_string_ <- "4bc66"
target_files <- list.files("sim_results",
    pattern = paste0("*", unique_string_, ".*.json"),
    full.names = TRUE,
    include.dirs = TRUE
)

# read all files
for (file in target_files) {
    df <- fromJSON(file)
    if (exists("results")) {
        results <- rbind(results, df$results)
    } else {
        results <- df$results
    }
}

parameters <- df$parameters
epidemic_model <- df$epidemic_model
complete_list_parameters <- df$complete_list_parameters
current_week <- df$complete_list_parameters$current_week
original_data <- df$original_data
# write to file
writeLines(
    toJSON(list(
        parameters = parameters,
        complete_list_parameters = complete_list_parameters,
        epidemic_model = epidemic_model,
        original_data = original_data,
        results = results
    ), pretty = TRUE, digits = 8, na = "string"),
    paste0("output/", unique_string_, "_results.json")
)
