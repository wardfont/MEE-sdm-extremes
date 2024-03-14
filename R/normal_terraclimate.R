#' .. content for \description{} (no empty lines) ..
#' Calculate empirical quantile paramters from derived data.
#' .. content for \details{} ..
#'
#' @title
#' @param tc_dv
#' @return
#' @author Ward Fonteyn
#' @export
normal_terraclimate <- function(tc_dv) {
  terraclimate_normal_dir <- paste0(
    "./output/terraclimate/",
    tc_dv$scenario,
    "/normal/"
  )

  normal_extremes <- function(x) {
    library(MASS)
    if (!anyNA(x)) {
      fit <- fitdistr(x, densfun = "normal")

      return(c(
        fit$estimate["mean"],
        fit$estimate["sd"],
        AIC(fit)[1]
      ))
    } else {
      return(rep(NA, 3))
    }
  }

  cl <- makeCluster(10)

  #### Tmax ####

  tmaxhm <- rast(tc_dv$files["tmaxhm"])
  extmaxhm <- app(tmaxhm,
    normal_extremes,
    cores = cl
  )
  names(extmaxhm) <- c(
    "mean_tmaxhm", "sd_tmaxhm", "aic"
  )
  extmaxhm_file <- paste0(terraclimate_normal_dir, "extmaxhm.tif")
  writeRaster(extmaxhm, extmaxhm_file, overwrite = T)

  #### Tmin ####

  tmincm <- rast(tc_dv$files["tmincm"])
  extmincm <- app(tmincm,
    normal_extremes,
    cores = cl
  )
  names(extmincm) <- c(
    "mean_tmincm", "sd_tmincm", "aic"
  )

  extmincm_file <- paste0(terraclimate_normal_dir, "extmincm.tif")
  writeRaster(extmincm, extmincm_file, overwrite = T)

  #### Ppt ####

  pptqdq <- rast(tc_dv$files["pptqdq"])
  expptqdq <- app(pptqdq,
    normal_extremes,
    cores = cl
  )
  names(expptqdq) <- c(
    "mean_pptqdq", "sd_pptqdq", "aic"
  )
  expptqdq_file <- paste0(terraclimate_normal_dir, "expptqdq.tif")
  writeRaster(expptqdq, expptqdq_file, overwrite = T)

  #### Return ####

  stopCluster(cl)

  normal_files <- c(
    extmaxhm = extmaxhm_file,
    extmincm = extmincm_file,
    expptqdq = expptqdq_file
  )

  return(list(
    files = normal_files,
    vars = tc_dv$vars,
    scenario = tc_dv$scenario,
    years = tc_dv$years,
    md5_hashes = sapply(normal_files,
      digest,
      file = T, algo = "md5"
    )
  ))
}
