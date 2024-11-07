library(tidyverse)
library(magrittr)
library(igraph)
library(fuzzyjoin)
library(ggplot2)
library(reshape2)
library(gridExtra)
library(readxl)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
library(dplyr)
library(paletteer)

# Load data from cuebiq and istat namings
prov <- as.data.frame(read.csv("data/data_generators/raw_data_sources/id_provinces_it.csv", na.strings = "--"))
mov <- as.data.frame(read.csv("data/data_generators/raw_data_sources/od_matrix_daily_flows_norm_full_2020_01_18_2020_06_26.csv"))
istat_namings <- read_excel("data/data_generators/raw_data_sources/Codici-statistici-e-denominazioni-al-01_01_2020.xls", sheet = "CODICI al 01012020")

nuts_custom <- data.frame(
    region_eng = c(
        "Piemonte",
        "Valle d'Aosta",
        "Liguria",
        "Lombardia",
        "Trentino-Alto Adige",
        "Trentino-Alto Adige",
        "Veneto",
        "Friuli-Venezia Giulia",
        "Emilia-Romagna",
        "Toscana",
        "Umbria",
        "Marche",
        "Lazio",
        "Abruzzo",
        "Molise",
        "Campania",
        "Apulia",
        "Basilicata",
        "Calabria",
        "Sicily",
        "Sardegna"
    ),
    region = c(
        "Piemonte",
        "Valle d'Aosta/Vallée d'Aoste",
        "Liguria",
        "Lombardia",
        "PA Bolzano",
        "PA Trento",
        "Veneto",
        "Friuli-Venezia Giulia",
        "Emilia-Romagna",
        "Toscana",
        "Umbria",
        "Marche",
        "Lazio",
        "Abruzzo",
        "Molise",
        "Campania",
        "Puglia",
        "Basilicata",
        "Calabria",
        "Sicilia",
        "Sardegna"
    ),
    nuts2 = c(
        "ITC1",
        "ITC2",
        "ITC3",
        "ITC4",
        "ITD1",
        "ITD2",
        "ITD3",
        "ITD4",
        "ITD5",
        "ITE1",
        "ITE2",
        "ITE3",
        "ITE4",
        "ITF1",
        "ITF2",
        "ITF3",
        "ITF4",
        "ITF5",
        "ITF6",
        "ITG1",
        "ITG2"
    )
)

# select what you need from istat dataset
istat_namings <- istat_namings %>%
    select(sigla = `Sigla automobilistica`, region = `Denominazione Regione`) %>%
    unique()

# left join and fixing sudtirol
prov <- prov %>%
    left_join(istat_namings, by = c("SIGLA" = "sigla")) %>%
    select(COD_PROV, region)
prov[prov$COD_PROV == 21, ]$region <- "PA Bolzano"
prov[prov$COD_PROV == 22, ]$region <- "PA Trento"

# select the days before the lockdown
days_before <- colnames(mov)[-c(1:2)][1:35] # choose form Jan 18 to Feb 21

# compute the mean of days of not lockdown, join with correct names and summarize fluxes by region as a sum
mov_complete <- mov %>%
    mutate(avg_pre = rowMeans(.[, days_before])) %>%
    select(p1, p2, avg_pre) %>%
    left_join(prov, by = c("p1" = "COD_PROV")) %>%
    left_join(prov, by = c("p2" = "COD_PROV"), suffix = c("", "_to")) %>%
    select(from = region, to = region_to, avg_pre) %>%
    group_by(from, to) %>%
    summarize(flux = sum(avg_pre), .groups = "drop") %>%
    left_join(nuts_custom, by = c("from" = "region")) %>%
    left_join(nuts_custom, by = c("to" = "region")) %>%
    select(flux, nuts2.x, nuts2.y) %>%
    rename(from = nuts2.x, to = nuts2.y) %>%
    select(from, to, flux)

# get the adjacency matrix for the flux
gr <- graph_from_data_frame(mov_complete, directed = TRUE)
adj_mat <- as_adjacency_matrix(gr, attr = "flux", sparse = FALSE)

adj_mat_with_zero_diag <- adj_mat
diag(adj_mat_with_zero_diag) <- 0
adj_mat_with_zero_diag <- proportions(adj_mat_with_zero_diag, 1)
adj_mat_with_zero_diag <- adj_mat_with_zero_diag[nuts_custom$nuts2, nuts_custom$nuts2]

# save with 0 in diagonal for successive use
saveRDS(adj_mat_with_zero_diag, "data/data_generators/check_files/mobility_matrix_regions.rds")


# graphical purpose code only beyond this point

adj_mat_no_zero <- proportions(adj_mat, 1) # normalize by row

# Replace 0 values in adj_matrix_during with NA
adj_mat_no_zero[adj_mat_no_zero == 0] <- NA
adj_matrix_melt <- melt(adj_mat_no_zero, na.rm = TRUE)
adj_matrix_melt <- adj_matrix_melt %>%
    mutate(
        Var1 = factor(Var1, levels = nuts_custom$nuts2, labels = nuts_custom$nuts2),
        Var2 = factor(Var2, levels = nuts_custom$nuts2, labels = nuts_custom$nuts2)
    )

adj_matrix_melt %>%
    pivot_wider(names_from = Var2, values_from = value) %>%
    as.data.frame()

plot1 <- ggplot(adj_matrix_melt, aes(x = Var2, y = Var1)) +
    geom_raster(aes(fill = log(value))) +
    geom_hline(yintercept = c(2.5, 8.5, 12.5, 17.5), color = "black", size = 1) +
    geom_vline(xintercept = c(4.5, 9.5, 13.5, 19.5), color = "black", size = 1) +
    scale_fill_paletteer_c("viridis::plasma", limits = c(-15, 0)) +
    labs(x = "Destination region", y = "Origin region", fill = "Flux (log)") +
    theme_bw() +
    theme(
        axis.text.x = element_text(size = 14, angle = 90, hjust = 0),
        axis.text.y = element_text(size = 14, hjust = 0),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        aspect.ratio = 1,
    ) +
    scale_y_discrete(drop = FALSE, limits = rev(levels(adj_matrix_melt$Var2))) +
    scale_x_discrete(drop = FALSE, position = "top")

ggsave("data/data_generators/check_files/matrix_od.png", plot1, width = 15, height = 15, units = "cm")
