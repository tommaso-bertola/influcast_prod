mobility <- function(mobility_type = "radiation") {
    if (mobility_type == "radiation") {
        mobility_matr <- as.matrix(readRDS("data/census/radiation_model_regions.rds"))
    } else {
        mobility_matr <- as.matrix(readRDS("data/census/mobility_matrix_regions.rds"))
    }

    return(mobility_matr)
}
