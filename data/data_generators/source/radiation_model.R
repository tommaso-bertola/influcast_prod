library(dplyr)
library(magrittr)
library(igraph)
library(ggplot2)
library(paletteer)
library(geosphere)
library(compiler)
library(rnaturalearth)
library(sf)
library(stringr)
library(tidyr)
library(reshape2)

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
radiation <- function(mappa, p_going = 0.5) {
    radiation_model <- function(pop_i, pop_j, s_ij) {
        return((pop_i * pop_j) / ((pop_i + s_ij) * (pop_i + pop_j + s_ij)))
    }

    travel_rates <- matrix(0, nrow = nrow(mappa), ncol = nrow(mappa))
    s <- matrix(0, nrow = nrow(mappa), ncol = nrow(mappa))
    distances <- matrix(0, nrow = nrow(mappa), ncol = nrow(mappa))
    rownames(travel_rates) <- mappa$patch_name
    colnames(travel_rates) <- mappa$patch_name

    # Compute travel rates
    for (i in 1:nrow(mappa)) {
        for (j in 1:nrow(mappa)) {
            if (i != j) {
                distances[i, j] <- distGeo(
                    c(mappa$longitude[i], mappa$latitude[i]),
                    c(mappa$longitude[j], mappa$latitude[j])
                )
            }
        }
    }
    for (i in 1:nrow(mappa)) {
        for (j in 1:nrow(mappa)) {
            if (i != j) {
                s[i, j] <- sum(mappa$population[distances[i, ] < distances[i, j]]) - mappa$population[i]
            }
        }
    }
    for (i in 1:nrow(mappa)) {
        for (j in 1:nrow(mappa)) {
            if (i != j) {
                travel_rates[i, j] <- radiation_model(
                    mappa$population[i],
                    mappa$population[j],
                    s[i, j]
                )
            }
        }
    }
    colnames(travel_rates) <- mappa$name
    rownames(travel_rates) <- mappa$name
    travel_rates <- proportions(travel_rates, 1) * p_going
    diag(travel_rates) <- 1 - p_going
    return(travel_rates)
}

italy <- ne_states(country = "Italy", returnclass = "sf")
italy_provinces <- italy %>% select(name, type, latitude, longitude, region)

italy_centroids <- st_centroid(italy_provinces) %>%
    st_coordinates() %>%
    as.data.frame() %>%
    rename(longitude = X, latitude = Y)

italy_provinces_2 <- cbind(italy_provinces, italy_centroids)

ggplot(italy_provinces_2) +
    geom_sf(aes(fill = region)) +
    geom_point(aes(x = longitude, y = latitude), size = 4) +
    scale_fill_hue()

file_istat <- "data/census/DCIS_POPRES1_21032024103707419.csv"
dcis <- utils::read.csv(file_istat, header = TRUE)


population <- dcis %>%
    filter(Sesso == "totale", Stato.civile == "totale", ETA1 != "TOTAL", str_length(ITTER107) == 5) %>%
    select(-Tipo.di.indicatore.demografico, -Seleziona.periodo, -Stato.civile) %>%
    group_by(ITTER107, Territorio) %>%
    summarise(population = sum(Value), .groups = "drop") %>%
    rename(province = Territorio, nuts3 = ITTER107)



italy_complete <- italy_provinces_2 %>% left_join(population, by = c("name" = "province"))

italy_complete[italy_complete$name == "Aoste", c("nuts3", "population")] <- c(NA, 123018)
italy_complete[italy_complete$name == "Bozen", c("nuts3", "population")] <- c(NA, 536933)
italy_complete[italy_complete$name == "Turin", c("nuts3", "population")] <- c(NA, 2203353)
italy_complete[italy_complete$name == "Barletta-Andria Trani", c("nuts3", "population")] <- c(NA, 377973)
italy_complete[italy_complete$name == "Crotene", c("nuts3", "population")] <- c(NA, 161733)
italy_complete[italy_complete$name == "Reggio Calabria", c("nuts3", "population")] <- c(NA, 515046)
italy_complete[italy_complete$name == "Carbonia-Iglesias", c("nuts3", "population")] <- c(NA, 117921)
italy_complete[italy_complete$name == "Olbia-Tempio", c("nuts3", "population")] <- c(NA, 158491)
italy_complete[italy_complete$name == "Oristrano", c("nuts3", "population")] <- c(NA, 151089)
italy_complete[italy_complete$name == "Medio Campidano", c("nuts3", "population")] <- c(NA, 91697)
italy_complete[italy_complete$name == "Ogliastra", c("nuts3", "population")] <- c(NA, 54416)
italy_complete[italy_complete$name == "Monza e Brianza", c("nuts3", "population")] <- c(NA, 877680)
italy_complete[italy_complete$name == "Reggio Emilia", c("nuts3", "population")] <- c(NA, 529261)

italy_complete %>%
    ggplot() +
    geom_sf(aes(fill = population)) +
    geom_point(aes(x = longitude, y = latitude, size = population), color = "red") +
    theme_minimal()


rad <- radiation(italy_complete)

italy_complete_simple <- italy_complete %>%
    select(name, region) %>%
    as.data.frame() %>%
    select(-geometry) %>%
    left_join(nuts_custom %>% select(-region), by = c("region" = "region_eng")) %>%
    filter(!(name == "Trento" & nuts2 == "ITD1")) %>%
    filter(!(name == "Bozen" & nuts2 == "ITD2")) %>%
    arrange(nuts2, region)

order_region <- italy_complete_simple %>% select(name)

travel_rate_df <- as.data.frame(as.table(rad))
colnames(travel_rate_df) <- c("Origin", "Destination", "TravelRate")
travel_rate_df <- travel_rate_df %>%
    left_join(italy_complete_simple, by = c("Origin" = "name")) %>%
    left_join(italy_complete_simple, by = c("Destination" = "name"), suffix = c(".origin", ".destination"))

plot_provinces_radiation <- travel_rate_df %>%
    arrange(nuts2.origin, nuts2.destination) %>%
    mutate(Origin = factor(Origin, levels = unique(Origin))) %>%
    mutate(Destination = factor(Destination, levels = rev(unique(Destination)))) %>%
    ggplot(aes(x = Origin, y = Destination)) +
    geom_raster(aes(fill = log(TravelRate))) +
    scale_fill_viridis_c(option = "inferno") +
    # scale_x_discrete(limits = rev(levels(travel_rate_df$Destination))) +
    scale_x_discrete(drop = FALSE, position = "top") +
    labs(x = "Origin patch", y = "Destination patch", fill = "Travel rate (log)") +
    theme(axis.text.x = element_text(angle = 90, hjust = 0)) +
    coord_fixed(ratio = 1)

ggsave("data/data_generators/check_files/radiation_model_provinces.png", plot_provinces_radiation, width = 10, height = 10)

travel_rate_df_regions <- travel_rate_df %>%
    group_by(nuts2.origin, nuts2.destination) %>%
    summarise(travel_rate = sum(TravelRate), .groups = "drop") %>%
    pivot_wider(names_from = nuts2.destination, values_from = travel_rate) %>%
    as.data.frame()

prob_going <- 1
regions_order <- travel_rate_df_regions$nuts2.origin
travel_rate_df_regions <- travel_rate_df_regions[, -1]
diag(travel_rate_df_regions) <- 0
travel_rate_df_regions <- as.matrix(travel_rate_df_regions)
travel_rate_df_regions <- proportions(travel_rate_df_regions, 1) * prob_going
diag(travel_rate_df_regions) <- 1 - prob_going
colnames(travel_rate_df_regions) <- regions_order
rownames(travel_rate_df_regions) <- regions_order

saveRDS(travel_rate_df_regions, "data/data_generators/check_files/radiation_model_regions.rds")

melted_travel_rate_df_regions <- melt(travel_rate_df_regions)
melted_travel_rate_df_regions$type <- "Radiation model"

travel_rates_rad_regions <- ggplot(melted_travel_rate_df_regions, aes(x = Var2, y = Var1)) +
    geom_raster(aes(fill = log(value))) +
    geom_hline(yintercept = c(2.5, 8.5, 12.5, 17.5), color = "black", size = 1) +
    geom_vline(xintercept = c(4.5, 9.5, 13.5, 19.5), color = "black", size = 1) +
    scale_fill_paletteer_c("viridis::plasma", limits = c(-15, 0)) +
    labs(x = "Destination region", y = "Origin region", fill = "Flux (log)") +
    theme(axis.text.x = element_text(angle = 90, hjust = 0)) +
    coord_fixed(ratio = 1) +
    scale_y_discrete(drop = FALSE, limits = rev(levels(melted_travel_rate_df_regions$Var2))) +
    scale_x_discrete(drop = FALSE, position = "top") +
    theme_bw() +
    theme(
        axis.text.x = element_text(size = 14, angle = 90, hjust = 0),
        axis.text.y = element_text(size = 14, hjust = 0),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        aspect.ratio = 1,
    )
ggsave("data/data_generators/check_files/radiation_model_regions.png", travel_rates_rad_regions, width = 15, height = 15, units = "cm")
