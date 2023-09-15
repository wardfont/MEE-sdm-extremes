metrics_gam <- function(gam) {

  performance <- gam$gam$performance

  return(data.frame(rep = gam$rep,
                    species = gam$species,
                    model = gam$model,
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
                    AIC = gam$gam$model$aic))
}
