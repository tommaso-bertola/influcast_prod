library(tidyverse)
library(purrr)

files_a <- list.files(path = "A", pattern = "*.csv", full.names = TRUE)
files_b <- list.files(path = "B", pattern = "*.csv", full.names = TRUE)

big_data <- data.frame()

for (file in files_a) {
    data <- read.csv(file)
    big_data <- rbind(big_data, data)
}

for (file in files_b) {
    data <- read.csv(file)
    big_data <- rbind(big_data, data)
}

big_data %>%
    mutate(filename = paste0(anno, "_", sprintf("%02d", settimana), ".csv")) %>%
    split(.$filename) %>%
    map(~ .x %>% select(-filename)) %>%
    iwalk(~ write_csv(.x, paste0('joint/', .y)))
