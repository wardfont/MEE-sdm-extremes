gam_ensemble <- function(gam, model, species, env_file){

  predictions <- lapply(species, function(sp){
    models <- lapply(subset(gam,
                            sapply(gam, function(fg){
                              return(grepl(model, fg$model) & fg$species == sp)
                            }))
                    ,function(x){
                      return(x$gam)})

    if(model == "stewart"){
      name_TSS <- do.call(rbind,
                          lapply(models, function(m){
                            return(data.frame(name = as.character(m$predictors[2]),
                                              TSS = as.numeric(m$performance$TSS_mean)))
                          }))

      best_mean_TSS_name <- name_TSS %>%
        group_by(name) %>%
        summarise(mean_TSS = mean(TSS)) %>%
        slice_max(order_by = mean_TSS, n=1) %>%
        pull(name)

      print(paste0("best is", best_mean_TSS_name))

      models <- subset(models, name_TSS$name == best_mean_TSS_name)
    }

    ensemble <- fit_ensemble(models,
                             ens_method = "mean",
                             thr = "max_sens_spec")

    predict <- wrap(sdm_predict(models = ensemble,
                                pred = rast(env_file),
                                thr = "max_sens_spec")[[1]])
  })

  return(predictions)
}
