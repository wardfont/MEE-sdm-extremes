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
model_euforest <- function(euforest_partition, model, predictor_pattern) {

    data <- euforest_partition$data 
    
    predictors <- names(data)[grepl(predictor_pattern, names(data))]

    data_out <- data %>%
        select(x, y, pr_ab, sblock.part, random.part, all_of(predictors))

    return(list(data = data_out,
                species = euforest_partition$species,
                model = model,
                predictor_pattern = predictor_pattern,
                rep = euforest_partition$rep,
                sblock_cell_size = euforest_partition$sblock_cell_size,
                sblock_folds = euforest_partition$sblock_folds))
}
