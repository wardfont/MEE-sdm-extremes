stewart_terraclimate <- function(tc_dv, return_periods) {

  terraclimate_stewart_dir <- paste0("./output/terraclimate/",
                                     tc_dv$scenario,
                                     "/stewart/")

  stewart_extremes <- function(x, ret_per = return_periods, side){
    if(any(is.na(x))){
      return(rep(NA,2+(length(ret_per))))
    } else {
      u <- mean(x)
      sd <- sd(x)
      if(side == "max"){
        p = 1-(1/ret_per)
      } else if(side == "min"){
        p = 1/ret_per
      }
      q = quantile(x,probs = p)
      return(c(u,sd,q))
    }
  }

  cl <- makeCluster(10)

#### Tmax ####

  tmaxhm <- rast(tc_dv$files["tmaxhm"])
  extmaxhm <- app(tmaxhm, stewart_extremes, side = "max",
                  ret_per = return_periods, cores = cl)
  names(extmaxhm) <- c("mean_tmaxhm", "sd_tmaxhm",
                       paste0("rl", return_periods, "yr_tmaxhm"))
  extmaxhm_file <- paste0(terraclimate_stewart_dir, "extmaxhm.tif")
  writeRaster(extmaxhm, extmaxhm_file, overwrite = T)

#### Tmin ####

  tmincm <- rast(tc_dv$files["tmincm"])
  extmincm <- app(tmincm, stewart_extremes, side = "min",
                  ret_per = return_periods, cores = cl)
  names(extmincm) <- c("mean_tmincm", "sd_tmincm",
                       paste0("rl", return_periods, "yr_tmincm"))
  extmincm_file <- paste0(terraclimate_stewart_dir, "extmincm.tif")
  writeRaster(extmincm, extmincm_file, overwrite = T)


#### Ppt ####

  pptqdq <- rast(tc_dv$files["pptqdq"])
  expptqdq <- app(pptqdq, stewart_extremes, side = "min",
                  ret_per = return_periods, cores = cl)
  names(expptqdq) <- c("mean_pptqdq", "sd_pptqdq",
                       paste0("rl", return_periods, "yr_pptqdq"))
  expptqdq_file <- paste0(terraclimate_stewart_dir, "expptqdq.tif")
  writeRaster(expptqdq, expptqdq_file, overwrite = T)

#### Return ####

  stopCluster(cl)

  stewart_files <- c(extmaxhm = extmaxhm_file,
                     extmincm = extmincm_file,
                     expptqdq = expptqdq_file)

  return(list(files = stewart_files,
              vars = tc_dv$vars,
              scenario = tc_dv$scenario,
              years = tc_dv$years,
              return_periods = return_periods,
              md5_hashes = sapply(stewart_files,
                                  digest, file = T, algo = "md5")))
}
