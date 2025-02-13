library(dplyr)
library(ggplot2)
library(tidyr)
library(magrittr)
library(stringr)
library(reshape2)


raw_incidence_national <- read.csv(paste0("/home/ubuntu/Influcast/sorveglianza/ILI+_FLU/2024-2025/latest/italia-latest-ILI+_FLU_", signal, ".csv")) %>%
    mutate(ori = ifelse(settimana < 40, settimana + 52, settimana), orizzonte = ori - max(ori)) %>%
    select(-ori) %>%
    select(orizzonte, anno, settimana, incidenza)

files <- list.files("/home/ubuntu/Influcast/sorveglianza/ILI/2024-2025/latest", pattern = ".*\\.csv", full.names = TRUE)
files <- files[!grepl("italia-latest-ILI.csv", files)]

raw_incidence_regional <- files %>%
    lapply(function(file) {
        read.csv(file) %>%
            mutate(region = basename(file)) %>%
            separate(region, into = c("region", "latest", "csv"), sep = "-") %>%
            select(-csv, -latest, -target, -numero_casi, -numero_assistiti, -incidenza) %>%
            left_join(raw_incidence_national, by = c("settimana", "anno")) %>%
            filter(!is.na(incidenza))
    }) %>%
    bind_rows() %>%
    pivot_wider(names_from = region, values_from = incidenza) %>%
    mutate(ori = ifelse(settimana < 40, settimana + 52, settimana), orizzonte = ori - max(ori)) %>%
    select(-ori) %>%
    melt(id.vars = c("orizzonte", "anno", "settimana")) %>%
    left_join(regions_influcast, by = c("variable" = "region")) %>%
    arrange(nuts2) %>%
    rename(region = variable)



regional_file_name <- list.files(path = "output", pattern = paste0("regional_quantiles_", unique_string_, ".rds"), full.names = TRUE)
regional_df <- readRDS(regional_file_name)
current_week_national <- national_df$current_week
current_week_regional <- regional_df$current_week
if (current_week_national != current_week_regional) {
    stop("Current week in national and regional data is different or missing")
    quit(status = 1)
} else {
    current_year_week <- gsub("-", "_", current_week_national)
}

current_year <- strsplit(current_year_week, "_")[[1]][1]
current_week <- strsplit(current_year_week, "_")[[1]][2]

national <- national_df$quantiles %>%
    as.data.frame() %>%
    slice_tail(n = 6) %>%
    pivot_longer(cols = -week, names_to = "quantiles", values_to = "incidence") %>%
    mutate(id_valore = as.numeric(stringr::str_replace_all(quantiles, "q", "")) / 100) %>%
    select(-quantiles) %>%
    mutate(
        target = "ILI",
        anno = current_year,
        settimana = current_week,
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


regional <- regional_df$quantiles %>%
    as.data.frame() %>%
    group_by(pop_index) %>%
    slice_tail(n = 6) %>%
    ungroup() %>%
    left_join(regions_influcast, by = c("n_patch" = "order_census")) %>%
    select(-pop_reg, -pop_index) %>%
    group_by(n_patch) %>%
    mutate(
        week = week - max(week) + 4,
        region_code_patch = paste0(region, "-", code, "-", n_patch)
    ) %>%
    select(!starts_with("q"), starts_with("q")) %>%
    ungroup()

regional_to_save <- regional %>%
    select(week, code, starts_with("q")) %>%
    pivot_longer(cols = -c(week, code), names_to = "quantiles", values_to = "incidence") %>%
    mutate(id_valore = as.numeric(stringr::str_replace_all(quantiles, "q", "")) / 100) %>%
    select(-quantiles) %>%
    mutate(
        target = "ILI",
        anno = current_year,
        settimana = current_week,
        tipo_valore = "quantile",
        orizzonte = week - max(week) + 4,
        incidence = round(incidence, 4)
    ) %>%
    rename(
        valore = incidence,
        luogo = code
    ) %>%
    select(anno, settimana, luogo, tipo_valore, id_valore, orizzonte, valore, target) %>%
    arrange(anno, settimana, luogo, id_valore, orizzonte)


total_table <- rbind(national, regional_to_save)
write.table(total_table,
    paste0("uploading_predictions/", current_year_week, "_", national_df$signal, ".csv"),
    sep = ",",
    row.names = FALSE,
    col.names = TRUE,
    quote = FALSE
)

writeLines(current_year_week, "uploading_predictions/current_week.txt")
writeLines(national_df$signal, "uploading_predictions/current_signal.txt")



plot_regional <- regional %>%
    ggplot() +
    geom_ribbon(aes(x = week, ymin = q1, ymax = q99), fill = "#6b6a6a", alpha = 0.2) +
    geom_ribbon(aes(x = week, ymin = q2.5, ymax = q97.5), fill = "#6b6a6a", alpha = 0.2) +
    geom_ribbon(aes(x = week, ymin = q5, ymax = q95), fill = "#6b6a6a", alpha = 0.2) +
    geom_ribbon(aes(x = week, ymin = q10, ymax = q90), fill = "#6b6a6a", alpha = 0.2) +
    geom_ribbon(aes(x = week, ymin = q15, ymax = q85), fill = "#6b6a6a", alpha = 0.2) +
    geom_ribbon(aes(x = week, ymin = q20, ymax = q80), fill = "#6b6a6a", alpha = 0.2) +
    geom_ribbon(aes(x = week, ymin = q25, ymax = q75), fill = "#6b6a6a", alpha = 0.2) +
    geom_ribbon(aes(x = week, ymin = q30, ymax = q70), fill = "#6b6a6a", alpha = 0.2) +
    geom_ribbon(aes(x = week, ymin = q35, ymax = q65), fill = "#6b6a6a", alpha = 0.2) +
    geom_ribbon(aes(x = week, ymin = q40, ymax = q60), fill = "#6b6a6a", alpha = 0.2) +
    geom_ribbon(aes(x = week, ymin = q45, ymax = q55), fill = "#6b6a6a", alpha = 0.2) +
    geom_linerange(aes(x = week, ymin = q2.5, ymax = q97.5), color = "black", position = position_nudge(x = 0.15)) +
    geom_linerange(aes(x = week, ymin = q5, ymax = q95), color = "black", position = position_nudge(x = 0.07)) +
    geom_linerange(aes(x = week, ymin = q25, ymax = q75), color = "black", position = position_nudge(x = 0.0)) +
    geom_point(data = raw_incidence_regional, aes(x = orizzonte, y = value), color = "black") +
    geom_line(data = raw_incidence_regional, aes(x = orizzonte, y = value), color = "black") +
    geom_vline(xintercept = 0, linetype = "dashed") +
    facet_wrap(~region) +
    theme_light() +
    labs(y = "incidence", x = "weeks ahead", title = paste("Regional prediction", current_year_week, "(dashed line)", national_df$signal))

plot_national <- national %>%
    mutate(id_valore = 100 * id_valore) %>%
    pivot_wider(names_from = id_valore, values_from = valore) %>%
    ggplot(aes(x = orizzonte)) +
    geom_ribbon(aes(ymin = `1`, ymax = `99`), fill = "#ff0000", alpha = 0.1) +
    geom_ribbon(aes(ymin = `2.5`, ymax = `97.5`), fill = "#ff0000", alpha = 0.1) +
    geom_ribbon(aes(ymin = `5`, ymax = `95`), fill = "#ff0000", alpha = 0.1) +
    geom_ribbon(aes(ymin = `10`, ymax = `90`), fill = "#ff0000", alpha = 0.1) +
    geom_ribbon(aes(ymin = `15`, ymax = `85`), fill = "#ff0000", alpha = 0.1) +
    geom_ribbon(aes(ymin = `20`, ymax = `80`), fill = "#ff0000", alpha = 0.1) +
    geom_ribbon(aes(ymin = `25`, ymax = `75`), fill = "#ff0000", alpha = 0.1) +
    geom_ribbon(aes(ymin = `30`, ymax = `70`), fill = "#ff0000", alpha = 0.1) +
    geom_ribbon(aes(ymin = `35`, ymax = `65`), fill = "#ff0000", alpha = 0.1) +
    geom_ribbon(aes(ymin = `40`, ymax = `60`), fill = "#ff0000", alpha = 0.1) +
    geom_ribbon(aes(ymin = `45`, ymax = `55`), fill = "#ff0000", alpha = 0.1) +
    geom_linerange(aes(ymin = `2.5`, ymax = `97.5`), color = "black", position = position_nudge(x = 0.1)) +
    geom_linerange(aes(ymin = `5`, ymax = `95`), color = "black", position = position_nudge(x = 0.05)) +
    geom_linerange(aes(ymin = `25`, ymax = `75`), color = "black", position = position_nudge(x = 0.0)) +
    geom_vline(xintercept = 0, linetype = "dashed") +
    geom_point(data = raw_incidence_national, aes(x = orizzonte, y = incidenza), color = "black") +
    geom_line(data = raw_incidence_national, aes(x = orizzonte, y = incidenza), color = "black") +
    labs(title = paste("prediction for Italy at", current_year_week, "(dashed line)", national_df$signal), x = "Weeks ahead", y = "ILI incidence")



ggsave(paste0("uploading_predictions/", current_year_week, "_", national_df$signal, "_national.png"), plot_national, width = 10, height = 6, dpi = 300)
ggsave(paste0("uploading_predictions/", current_year_week, "_", national_df$signal, "_regional.png"), plot_regional, width = 10, height = 6, dpi = 300)
