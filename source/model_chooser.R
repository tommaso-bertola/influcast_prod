model_chooser <- function(epidemic_model) {
    models <- list(
        "SIR" = "epidemic_models/SIR.R",
        "SEIR" = "epidemic_models/SEIR.R",
        "SIIR" = "epidemic_models/SIIR.R",
        "SIR_multistrain_2" = "epidemic_models/SIR_multistrain_2.R",
        "SIR_multistrain_3" = "epidemic_models/SIR_multistrain_3.R",
        "SIR_multistrain_4" = "epidemic_models/SIR_multistrain_4.R",
        "SIR_multistrain_5" = "epidemic_models/SIR_multistrain_5.R",
        "SIR_multistrain_no_age" = "epidemic_models/SIR_multistrain_no_age.R"
    )

    if (!epidemic_model %in% names(models)) {
        stop("Epidemic model not recognized:", epidemic_model)
    }

    # loads the correct model locally
    source(models[[epidemic_model]], local = TRUE)

    return(list(
        ep_mod = local_ep_mod,
        x2params_ = local_x2params_,
        x2params = local_x2params,
        bounds = local_bounds,
        ep_mod_ab = local_ep_mod_ab
    ))
}
