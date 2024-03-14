metrics_gam <- function(gam) {
  if (is.null(gam$gam)) {
    performance <- as_tibble(t(rep(NA, 12)))
    names(performance) <- c(
      "n_presences", "n_absences", "TSS_mean", "AUC_mean",
      "IMAE_mean", "TPR_mean", "TNR_mean", "SORENSEN_mean",
      "JACCARD_mean", "FPB_mean", "OR_mean", "BOYCE_mean"
    )
    aic <- NA
  } else {
    performance <- gam$gam$performance
    aic <- gam$gam$model$aic
  }

  return(data.frame(
    rep = gam$rep,
    species = gam$species,
    model = gam$model,
    n_presences = performance$n_presences,
    n_absences = performance$n_absences,
    TSS = performance$TSS_mean,
    AUC = performance$AUC_mean,
    IMAE = performance$IMAE_mean,
    TPR = performance$TPR_mean,
    TNR = performance$TNR_mean,
    Sorensen = performance$SORENSEN_mean,
    Jaccard = performance$JACCARD_mean,
    FBP = performance$FPB_mean,
    OR = performance$OR_mean,
    BOYCE = performance$BOYCE_mean,
    AIC = aic
  ))
}
