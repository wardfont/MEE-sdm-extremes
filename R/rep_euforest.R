rep_euforest <- function(euforest, reps) {

    set.seed(1996)    
    data <- part_random(data = euforest$data,
                        pr_ab = "pr_ab",
                        method = c(method = "kfold", folds = max(reps))) %>%
        group_by(.part)

    return(list(data = data,
                species = euforest$species))
}
