library(dplyr)
library(ggplot2)
library(tidyr)
library(magrittr)
library(stringr)
library(reshape2)

cons <- readLines("/home/ubuntu/influcast_prod/uploading_predictions/consolidation.txt")
if (cons == "TRUE") {
    consolidation_path <- "consolidated/squared/"
} else {
    consolidation_path <- "not_consolidated/squared/"
}

raw_incidence_national_a <- read.csv("/home/ubuntu/Influcast/sorveglianza/ILI+_FLU/2024-2025/latest/italia-latest-ILI+_FLU_A.csv") %>%
    mutate(ori = ifelse(settimana < 40, settimana + 52, settimana), orizzonte = ori - max(ori)) %>%
    select(-ori) %>%
    select(orizzonte, anno, settimana, incidenza) %>%
    mutate(target = "ILI+_FLU_A")

raw_incidence_national_b <- read.csv("/home/ubuntu/Influcast/sorveglianza/ILI+_FLU/2024-2025/latest/italia-latest-ILI+_FLU_B.csv") %>%
    mutate(ori = ifelse(settimana < 40, settimana + 52, settimana), orizzonte = ori - max(ori)) %>%
    select(-ori) %>%
    select(orizzonte, anno, settimana, incidenza) %>%
    mutate(target = "ILI+_FLU_B")

raw_incidence_national <- bind_rows(raw_incidence_national_a, raw_incidence_national_b)

joint <- raw_incidence_national %>%
    group_by(orizzonte, anno, settimana) %>%
    summarise(incidenza = sum(incidenza)) %>%
    ungroup() %>%
    mutate(target = "ILI+_FLU_AB")
raw_incidence_national <- bind_rows(raw_incidence_national, joint)
# Read data
national_df <- readRDS(national_file_name)
current_week_national <- national_df$current_week
current_week_national <- gsub("-", "_", current_week_national)

# Extract current year and week
current_year <- strsplit(current_week_national, "_")[[1]][1]
current_week <- strsplit(current_week_national, "_")[[1]][2]

# Helper function to process data
process_data <- function(data, target_label) {
    data %>%
        as.data.frame() %>%
        pivot_longer(cols = -week, names_to = "quantiles", values_to = "incidence") %>%
        mutate(
            id_valore = as.numeric(str_replace_all(quantiles, "q", "")) / 100,
            target = target_label,
            anno = current_year,
            settimana = current_week,
            tipo_valore = "quantile",
            luogo = "IT",
            orizzonte = week - max(week) + 4,
            incidence = round(incidence, 4)
        ) %>%
        select(anno, settimana, luogo, tipo_valore, id_valore, orizzonte, incidence, target) %>%
        rename(valore = incidence) %>%
        arrange(anno, settimana, id_valore, orizzonte)
}

# Process data for FLU_A and FLU_B
national_a <- process_data(national_df$quantiles$A, "ILI+_FLU_A")
national_b <- process_data(national_df$quantiles$notA, "ILI+_FLU_B")

# Combine and filter data
total_table <- bind_rows(national_a, national_b) %>% filter(orizzonte >= -1)

# Write data to file
write.table(total_table,
    paste0("uploading_predictions/", consolidation_path, current_week_national, "_", national_df$signal, ".csv"),
    sep = ",",
    row.names = FALSE,
    col.names = TRUE,
    quote = FALSE
)

# Write current week to file
writeLines(current_week_national, "uploading_predictions/current_week.txt")
writeLines(national_df$signal, "uploading_predictions/current_signal.txt")

# Prepare data for plotting
national <- bind_rows(national_a, national_b) %>%
    mutate(id_valore = 100 * id_valore) %>%
    pivot_wider(names_from = id_valore, values_from = valore)

# Plot data
plot_national <- ggplot(data = national, aes(x = orizzonte)) +
    geom_ribbon(aes(ymin = `1`, ymax = `99`, fill = target), alpha = 0.1) +
    geom_ribbon(aes(ymin = `2.5`, ymax = `97.5`, fill = target), alpha = 0.1) +
    geom_ribbon(aes(ymin = `5`, ymax = `95`, fill = target), alpha = 0.1) +
    geom_ribbon(aes(ymin = `10`, ymax = `90`, fill = target), alpha = 0.1) +
    geom_ribbon(aes(ymin = `15`, ymax = `85`, fill = target), alpha = 0.1) +
    geom_ribbon(aes(ymin = `20`, ymax = `80`, fill = target), alpha = 0.1) +
    geom_ribbon(aes(ymin = `25`, ymax = `75`, fill = target), alpha = 0.1) +
    geom_ribbon(aes(ymin = `30`, ymax = `70`, fill = target), alpha = 0.1) +
    geom_ribbon(aes(ymin = `35`, ymax = `65`, fill = target), alpha = 0.1) +
    geom_ribbon(aes(ymin = `40`, ymax = `60`, fill = target), alpha = 0.1) +
    geom_ribbon(aes(ymin = `45`, ymax = `55`, fill = target), alpha = 0.1) +
    geom_linerange(aes(ymin = `2.5`, ymax = `97.5`, color = target), position = position_nudge(x = 0.1)) +
    geom_linerange(aes(ymin = `5`, ymax = `95`, color = target), position = position_nudge(x = 0.05)) +
    geom_linerange(aes(ymin = `25`, ymax = `75`, color = target), position = position_nudge(x = 0.0)) +
    geom_vline(xintercept = 0, linetype = "dashed") +
    geom_point(data = raw_incidence_national, aes(x = orizzonte, y = incidenza, color = target)) +
    geom_line(data = raw_incidence_national, aes(x = orizzonte, y = incidenza, color = target)) +
    labs(
        title = paste("Prediction for Italy at", current_week_national, "(dashed line)", national_df$signal),
        x = "Weeks ahead", y = "ILI incidence"
    )

# Save plots
ggsave(paste0("uploading_predictions/", consolidation_path, current_week_national, "_", national_df$signal, "_national.png"), plot_national, width = 10, height = 6, dpi = 300)
ggsave(paste0("uploading_predictions/", consolidation_path, current_week_national, "_", national_df$signal, "_regional.png"), plot_national, width = 10, height = 6, dpi = 300)
