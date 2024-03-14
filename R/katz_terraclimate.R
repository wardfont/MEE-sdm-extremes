#' .. content for \description{} (no empty lines) ..
#' Calculate GEV parameters from derived data
#' .. content for \details{} ..
#'
#' @title
#' @param tc_dv
#' @param return_periods
#' @return
#' @author Ward Fonteyn
#' @export
katz_terraclimate <- function(tc_dv, return_periods) {


  terraclimate_katz_dir <- paste0("./output/terraclimate/",
                                  tc_dv$scenario,
                                  "/katz/")

  cl <- makeCluster(10)
  clusterEvalQ(cl, library(extRemes))

#### Tmax ####

  tmaxhm <- rast(tc_dv$files["tmaxhm"])
  extmaxhm <- app(tmaxhm, katz_extremes, side = "max", ret_per = return_periods, cores = cl)
  names(extmaxhm) <- c("location_tmaxhm", "scale_tmaxhm", "shape_tmaxhm",
                       "se_location_tmaxhm", "se_scale_tmaxhm", "se_shape_tmaxhm",
                       paste0("rl", return_periods, "yr_tmaxhm"),
                       "nllh", "aic", "bic")
  extmaxhm_file <- paste0(terraclimate_katz_dir,"extmaxhm.tif")
  writeRaster(extmaxhm, extmaxhm_file, overwrite = T)

#### Tmin ####

  tmincm <- rast(tc_dv$files["tmincm"])
  extmincm <- app(tmincm, katz_extremes, side = "min", ret_per = return_periods, cores = cl)
  names(extmincm) <- c("location_tmincm", "scale_tmincm", "shape_tmincm",
                       "se_location_tmincm", "se_scale_tmincm", "se_shape_tmincm",
                       paste0("rl", return_periods, "yr_tmincm"),
                       "nllh", "aic", "bic")
  extmincm_file <- paste0(terraclimate_katz_dir, "extmincm.tif")
  writeRaster(extmincm, extmincm_file, overwrite = T)

#### Pptq ####

  pptqdq <- rast(tc_dv$files["pptqdq"])
  expptqdq <- app(pptqdq, katz_extremes, side = "min", ret_per = return_periods, cores = cl)
  names(expptqdq) <- c("location_pptqdq", "scale_pptqdq", "shape_pptqdq",
                       "se_location_pptqdq", "se_scale_pptqdq", "se_shape_pptqdq",
                       paste0("rl", return_periods, "yr_pptqdq"),
                       "nllh", "aic", "bic")
                                        # Set rare pixels which return negative pptqdq to NA
  expptqdq[[7:(7+length(return_periods)-1)]][expptqdq[[7:(7+length(return_periods)-1)]] < 0] <- NA
  expptqdq_file <- paste0(terraclimate_katz_dir, "expptqdq.tif")
  writeRaster(expptqdq, expptqdq_file, overwrite = T)

#### Return ####

  stopCluster(cl)

  katz_files <- c(extmaxhm = extmaxhm_file,
                  extmincm = extmincm_file,
                  expptqdq = expptqdq_file)

  return(list(files = katz_files,
              vars = tc_dv$vars,
              scenario = tc_dv$scenario,
              years = tc_dv$years,
              return_periods = return_periods,
              md5_hashes = sapply(katz_files,
                                  digest, file = T, algo = "md5")))
}
