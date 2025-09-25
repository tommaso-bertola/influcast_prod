# source file of weeks to compute again
path <- "/home/ubuntu/influcast_prod/retrospective/missing_rounds_comunipd-mobnetSI2R_working_copy.csv"
missing_rounds <- read.csv(path, header = FALSE)
colnames(missing_rounds) <- c("name", "output_file", "target", "url")

# get first row info
first_row <- missing_rounds[1, ]

# get the target
target <- substr(first_row$target, nchar(first_row$target), nchar(first_row$target))

# recover regional latest data if signal=I
# and determine the output path
if (target == "I") {
    target <- "ILI"
    output_path <- paste0("/home/ubuntu/Influcast/sorveglianza/ILI/2024-2025/latest/italia-latest-ILI.csv")
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
    for (region in regions) {
        region_path <- paste0("/home/ubuntu/Influcast/sorveglianza/ILI/2024-2025/latest/", region)
        region_url <- gsub("/italia-", paste0("/", region, "-"), first_row$url)
        remote_df <- tryCatch(
            {
                read.csv(region_url)
            },
            error = function(e) {
                if (grepl("cannot open URL", e$message)) {
                    warning(paste0("Could not open file at URL (possibly 404):", region_url))
                }
                NULL # return NULL on failure
            }
        )
        # remote_df <- read.csv(region_url)
        write.csv(remote_df, paste0(region_path, "-latest-ILI.csv"), row.names = FALSE, quote = FALSE)
    }
} else {
    output_path <- paste0("/home/ubuntu/Influcast/sorveglianza/ILI+_FLU/2024-2025/latest/italia-latest-ILI+_FLU_", target, ".csv")
}

# download and write the "new" data
remote_data <- read.csv(first_row$url)
write.csv(remote_data, output_path, row.names = FALSE, quote = FALSE)

# update the signal file
signal_file <- "/home/ubuntu/influcast_prod/uploading_predictions/current_signal.txt"
writeLines(target, signal_file)

# remove the first row
colnames(missing_rounds) <- NULL
write.csv(missing_rounds[-1, ], path, row.names = FALSE, quote = FALSE)
