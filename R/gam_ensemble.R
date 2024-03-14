gam_ensemble <- function(gam, model, species, env_files) {
  env <- c(rast(env_files[1]), crop(rast(env_files[2]), rast(env_files[1])))

  predictions <- lapply(species, function(sp) {
    models <- lapply(
      subset(
        gam,
        sapply(gam, function(fg) {
          return(grepl(model, fg$model) & fg$species == sp)
        })
      ),
      function(x) {
        return(x$gam)
      }
    )

    if (model == "stewart") {
      name_AUC <- do.call(
        rbind,
        lapply(models, function(m) {
          return(data.frame(
            name = as.character(m$predictors[2]),
            AUC = as.numeric(m$performance$AUC_mean)
          ))
        })
      )

      best_mean_AUC_name <- name_AUC %>%
        group_by(name) %>%
        summarise(mean_AUC = mean(AUC)) %>%
        slice_max(order_by = mean_AUC, n = 1) %>%
        pull(name)

      print(paste0("best is", best_mean_AUC_name))

      models <- subset(models, name_AUC$name == best_mean_AUC_name)
    }

    ensemble <- fit_ensemble(models,
      ens_method = "mean",
      thr = "max_sens_spec"
    )

    predict <- wrap(sdm_predict(
      models = ensemble,
      pred = env,
      thr = NULL
    )[[1]])
  })

  return(predictions)
}

gam_first_rep <- function(gam, model, species, env_files) {
  env <- c(rast(env_files[1]), crop(rast(env_files[2]), rast(env_files[1])))

  predictions <- lapply(species, function(sp) {
    models <- lapply(
      subset(
        gam,
        sapply(gam, function(fg) {
          return(grepl(model, fg$model) & fg$species == sp & fg$rep == 1)
        })
      ),
      function(x) {
        return(x$gam)
      }
    )

    if (model == "stewart") {
      name_AUC <- do.call(
        rbind,
        lapply(models, function(m) {
          return(data.frame(
            name = as.character(m$predictors[2]),
            AUC = as.numeric(m$performance$AUC_mean)
          ))
        })
      )

      best_mean_AUC_name <- name_AUC %>%
        group_by(name) %>%
        summarise(mean_AUC = mean(AUC)) %>%
        slice_max(order_by = mean_AUC, n = 1) %>%
        pull(name)

      print(paste0("best is", best_mean_AUC_name))

      models <- subset(models, name_AUC$name == best_mean_AUC_name)
    }

    predict <- wrap(sdm_predict(
      models = models[[1]],
      pred = env,
      thr = NULL
    )[[1]])
  })

  return(predictions)
}
