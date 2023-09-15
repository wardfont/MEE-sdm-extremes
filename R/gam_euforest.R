gam_euforest <- function(euforest_model,
                              threshold,
                              fold_name) {

  data <- euforest_model$data
  predictors <- names(data)[grepl(euforest_model$predictor_pattern, names(data))]

  gam <- fit_gam(data = data,
                 response = "pr_ab",
                 predictors = predictors,
                 partition = fold_name,
                 thr = threshold,
                 fit_formula = NULL,
                 k = -1)

  return(list(gam = gam,
              fold_name = fold_name,
              sblock_cell_size = euforest_model$sblock_cell_size,
              sblock_folds = euforest_model$sblock_folds,
              species = euforest_model$species,
              model = euforest_model$model,
              predictor_pattern = euforest_model$predictor_pattern,
              rep = euforest_model$rep))
}
