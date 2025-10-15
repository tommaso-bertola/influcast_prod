# get current season from command line arguments
args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 0) {
    stop("Specify current season as the first argument to the script\n")
    quit(status = 1)
}
current_season <- args[1]
# current_season <- "2024-2025"

# source file of weeks to compute again
path <- "/home/ubuntu/influcast_prod/retrospective/missing_rounds_comunipd-mobnetSI2R_working_copy.csv"
missing_rounds <- read.csv(path, header = FALSE)
colnames(missing_rounds) <- c("name", "output_file", "target", "url")

# get first row info
first_row <- missing_rounds[1, ]

# get the target
target <- substr(first_row$target, nchar(first_row$target), nchar(first_row$target))

# check that season is correct
if (!grepl(current_season, first_row$url)) {
    stop(paste0("Season in the first row (", first_row$output_file, ") does not match current season (", current_season, ")"))
}

# list of regions
regions <- c(
    "abruzzo", "basilicata",
    "calabria", "campania",
    "emilia_romagna", "friuli_venezia_giulia",
    "italia", "lazio",
    "liguria", "lombardia",
    "marche", "molise",
    "pa_bolzano", "pa_trento",
    "piemonte", "puglia",
    "sardegna", "sicilia",
    "toscana", "umbria",
    "valle_d_aosta", "veneto"
)

# get the remote data
# it is always the national one, it just changes the target
remote_data <- read.csv(first_row$url)

# recover regional latest data if signal=I
# and determine the output path
if (target == "I") {
    target <- "ILI"
    output_path <- paste0("/home/ubuntu/Influcast/sorveglianza/ILI/", current_season, "/latest/italia-latest-ILI.csv")

    for (region in regions) {
        region_path <- paste0("/home/ubuntu/Influcast/sorveglianza/ILI/", current_season, "/latest/", region)
        region_url <- gsub("/italia-", paste0("/", region, "-"), first_row$url)
        remote_df <- tryCatch(
            {
                read.csv(region_url)
            },
            error = function(e) {
                if (grepl("cannot open URL", e$message)) {
                    message(paste0("Could not open file at URL (possibly 404):", region_url))
                }
                # NULL # return NULL on failure
                data.frame(
                    anno = numeric(0), settimana = numeric(0),
                    numero_casi = numeric(0), numero_assistiti = numeric(0), incidenza = numeric(0), target = numeric(0)
                ) # return empty data frame on failure
            }
        )
        write.csv(remote_df, paste0(region_path, "-latest-ILI.csv"), row.names = FALSE, quote = FALSE)
    }
} else {
    output_path <- paste0("/home/ubuntu/Influcast/sorveglianza/ILI+_FLU/", current_season, "/latest/italia-latest-ILI+_FLU_", target, ".csv")
    for (region in regions) {
        write.csv(remote_data, paste0("/home/ubuntu/Influcast/sorveglianza/ILI+_FLU/", current_season, "/latest/", region, "-latest-ILI+_FLU_", target, ".csv"), row.names = FALSE, quote = FALSE)
    }
}

#  write the "new" data depending on the missing_round file
write.csv(remote_data, output_path, row.names = FALSE, quote = FALSE)

# update the signal file
signal_file <- "/home/ubuntu/influcast_prod/uploading_predictions/current_signal.txt"
writeLines(target, signal_file)

# update the current week file
current_week <- substr(first_row$output_file, 1, nchar(first_row$output_file) - 4)
writeLines(current_week, "/home/ubuntu/influcast_prod/uploading_predictions/current_week_retrospective.txt")

# remove the first row from the missing rounds file
colnames(missing_rounds) <- NULL
write.csv(missing_rounds[-1, ], path, row.names = FALSE, quote = FALSE)
