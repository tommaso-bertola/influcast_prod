library(dplyr)
library(ggplot2)
library(tidyr)
library(magrittr)
library(stringr)
national <- readRDS("output/national_quantiles_2024_11_19_21_21_42.rds") %>%
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
        orizzonte = week - max(week) + 4
    ) %>%
    rename(
        valore = incidence,
    ) %>%
    select(anno, settimana, luogo, tipo_valore, id_valore, orizzonte, valore, target) %>%
    arrange(anno, settimana, id_valore, orizzonte)


write.table(national, "2024_45.csv", sep = ",", row.names = FALSE, col.names = TRUE, quote = FALSE)

national %>%
    mutate(id_valore = as.factor(id_valore)) %>%
    ggplot() +
    geom_line(aes(x = orizzonte, y = valore, group = id_valore, color = id_valore)) +
    labs(title = "2024 week 45", x = "Weeks ahead", y = "ILI incidence", color = "quantile")
ggsave("uploading_predictions/2024_45.pdf", width = 10, height = 6, dpi = 300)
