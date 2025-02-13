library(dplyr)
library(ggplot2)
library(tidyr)
library(magrittr)
library(stringr)
library(reshape2)

# Read regions data
regions_influcast <- read.csv("/home/ubuntu/influcast_prod/data/epidemiological/influcast/regions.txt",
    colClasses = c("character", "character", "character"),
    header = TRUE
) %>%
    arrange(nuts2) %>%
    mutate(order_census = 1:nrow(.))

# Get arguments
args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 0) {
    stop("Specify parameters to the script unifying results\n")
    quit(status = 1)
}
unique_string_ <- args[1]
# unique_string_ <- "1f4a2"

# Read national data
national_file_name <- list.files(path = "output", pattern = paste0("national_quantiles_", unique_string_, ".rds"), full.names = TRUE)
national_df <- readRDS(national_file_name)



# Main processing logic
# Process data based on signal type
if (national_df$signal == "ILI") {
    source("source/output_parser_ili.R")
} else if (national_df$signal == "AB") {
    source("source/output_parser_ab.R")
} else if (national_df$signal %in% c("A", "B")) {
    source("source/output_parser_single.R")
} else {
    stop("Signal not recognized")
    quit(status = 1)
}
