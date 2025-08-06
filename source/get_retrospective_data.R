path <- "/home/ubuntu/influcast_prod/retrospective/missing_rounds_comunipd-mobnetSI2R_working_copy.csv"

missing_rounds <- read.csv(path, header = FALSE)
colnames(missing_rounds) <- c("name", "output_file", "target", "url")

first_row <- missing_rounds[1, ]
target <- substr(first_row$target, nchar(first_row$target), nchar(first_row$target))
output_path <- paste0("/home/ubuntu/Influcast/sorveglianza/ILI+_FLU/2024-2025/latest/italia-latest-ILI+_FLU_", target, ".csv")

remote_data <- read.csv(first_row$url)
write.csv(remote_data, output_path, row.names = FALSE, quote = FALSE)
signal_file <- "/home/ubuntu/influcast_prod/uploading_predictions/current_signal.txt"

writeLines(target, signal_file)
colnames(missing_rounds) <- NULL
write.csv(missing_rounds[-1, ], path, row.names = FALSE, quote = FALSE)
