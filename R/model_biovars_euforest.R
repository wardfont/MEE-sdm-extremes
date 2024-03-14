#' .. content for \description{} (no empty lines) ..
#' Set all data in a nice list
#' .. content for \details{} ..
#'
#' @title
#' @param euforest_partition
#' @param model
#' @param predictor_pattern
#' @return
#' @author Ward Fonteyn
#' @export
model_biovars_euforest <- function(euforest_partition, model, predictor_pattern, biovars_file) {
  data <- euforest_partition$data

  biovars_data <- terra::extract(rast(biovars_file), vect(data, geom = c("x", "y"))) %>%
    select(-ID)

  data_all <- cbind(data, biovars_data)

  predictor_pattern_full <- paste(predictor_pattern, paste0("bio", c(1:4, 7:16, 18:19), collapse = "|"), sep = "|")

  predictors <- names(data_all)[grepl(predictor_pattern_full, names(data_all))]

  data_out <- data_all %>%
    select(x, y, pr_ab, sblock.part, random.part, all_of(predictors)) %>%
    select(-bio17)

  return(list(
    data = data_out,
    species = euforest_partition$species,
    model = model,
    predictor_pattern = predictor_pattern_full,
    rep = euforest_partition$rep,
    sblock_cell_size = euforest_partition$sblock_cell_size,
    sblock_folds = euforest_partition$sblock_folds
  ))
}
