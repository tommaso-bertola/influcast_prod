library(dplyr)
library(ggplot2)
library(tidyr)
library(magrittr)
library(stringr)
# allow arguments
args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 0) {
    unique_string_ <- args[1]
} else {
    stop("Specify parameters to the script unifying results\n")
    quit(status = 1)
}
# unique_string_ <- "4fb73"
national_file_name <- list.files(path = "output", pattern = paste0("national_quantiles_", unique_string_, ".rds"), full.names = TRUE)
regional_file_name <- list.files(path = "output", pattern = paste0("regional_quantiles_", unique_string_, ".rds"), full.names = TRUE)


national_df <- readRDS(national_file_name)
regional_df <- readRDS(regional_file_name)
current_week_national <- national_df$current_week
current_week_regional <- regional_df$current_week
if (current_week_national != current_week_regional) {
    stop("Current week in national and regional data is different or missing")
    quit(status = 1)
} else {
    current_week <- gsub("-", "_", current_week_national)
}

national <- national_df$quantiles %>%
    as.data.frame() %>%
    slice_tail(n = 6) %>%
    pivot_longer(cols = -week, names_to = "quantiles", values_to = "incidence") %>%
    mutate(id_valore = as.numeric(stringr::str_replace_all(quantiles, "q", "")) / 100) %>%
    select(-quantiles) %>%
    mutate(
        target = "ILI",
        anno = 2024,
        settimana = 45,
        tipo_valore = "quantile",
        luogo = "IT",
        orizzonte = week - max(week) + 4,
        incidence = round(incidence, 4)
    ) %>%
    rename(
        valore = incidence,
    ) %>%
    select(anno, settimana, luogo, tipo_valore, id_valore, orizzonte, valore, target) %>%
    arrange(anno, settimana, id_valore, orizzonte)


write.table(national, paste0("uploading_predictions/", current_week, ".csv"), sep = ",", row.names = FALSE, col.names = TRUE, quote = FALSE)


file_conn <- file("uploading_predictions/current_week.txt", "w")
writeLines(current_week, file_conn)
close(file_conn)


plot_national <- national %>%
    mutate(id_valore = as.factor(id_valore)) %>%
    ggplot() +
    geom_line(aes(x = orizzonte, y = valore, group = id_valore, color = id_valore)) +
    labs(title = current_week, x = "Weeks ahead", y = "ILI incidence", color = "quantile")

ggsave(paste0("uploading_predictions/", current_week, ".png"), plot_national, width = 10, height = 6, dpi = 300)
