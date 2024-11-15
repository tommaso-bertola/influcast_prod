library(ggplot2)
library(reshape2)
library(magrittr)
library(dplyr)
library(gridExtra)
library(ggh4x)
library(tidyr)
library(wesanderson)
library(colorspace)
library(paletteer)

virus <- read.csv("data/data_generators/raw_data_sources/andamento-settimanale-de.csv",
    header = TRUE,
    sep = ","
)

virus <- rbind(virus[1, ], virus)
virus <- virus[, -c(1, ncol(virus))]

abs <- cbind(virus[, 1] + virus[, 2] + virus[, 3] + virus[, 4], rowSums(virus[, -c(1, 2, 3, 4)]))
colnames(abs) <- c("A+B", "notA+B")
rel <- abs / rowSums(abs) * 100

rel <- as.data.frame(cbind(week = 1:nrow(rel), rel))

test <- rel %>%
    as.data.frame() %>%
    mutate(week = row_number()) %>%
    melt(id.vars = "week") %>%
    mutate(variable = factor(variable, levels = rev(c("A+B", "notA+B")))) %>%
    ggplot() +
    geom_bar(stat = "identity", aes(x = week, y = value, fill = variable), position = "stack")

ggsave("data/data_generators/check_files/virus_incidence.jpg", test, width = 10, height = 6)
# saveRDS(rel, "data/data_generators/check_files/virus_incidence_definitive.rds")
saveRDS(rel, "data/data_generators/check_files/virus_incidence_definitive_ab.rds")
