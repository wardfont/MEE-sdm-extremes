normal_gev_aic_compare <- function(terraclimate_katz, terraclimate_normal) {
  delta_aics <- lapply(
    c("extmaxhm", "extmincm", "expptqdq"),
    function(x) {
      delta_aic <- rast(terraclimate_normal$files[x])[["aic"]] -
        rast(terraclimate_katz$files[x])[["aic"]]
    }
  )

  delta_aic <- rast(delta_aics)
  names(delta_aic) <- c("tmaxhm", "tmincm", "pptqdq")

  delta_file <- "./output/terraclimate/historical/aic/delta_aic.tif"
  writeRaster(delta_aic, delta_file, overwrite = T)
  return(delta_file)
}
