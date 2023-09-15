deriv_terraclimate <- function(tc_dl) {

    terraclimate_deriv_dir <- paste0("./output/terraclimate/",
                                     tc_dl$scenario,
                                     "/deriv/")
#### Tmax ####
    
    tmax <- rast(tc_dl$files[grepl("tmax", tc_dl$files)])  
    tmaxhm <- tapp(tmax,
                   rep(tc_dl$years, each = 12),
                   max)
    tmaxhm_file <- paste0(terraclimate_deriv_dir, "tmaxhm.tif")
    writeRaster(tmaxhm, tmaxhm_file, overwrite = T)

#### Tmin ####
    
    tmin_all <- rast(tc_dl$files[grepl("tmin", tc_dl$files)])  
    ## Shift tmin half year to calculate for whole winter
    tmin <- subset(tmin_all, 7:(dim(tmin_all)[3]-6))
    tmincm <- tapp(tmin,
                   rep(tc_dl$years[-length(tc_dl$years)], each = 12),
                   min)
    tmincm_file <- paste0(terraclimate_deriv_dir, "tmincm.tif")
    writeRaster(tmincm, tmincm_file, overwrite = T)

#### Ppt  ####

    ## Rolling quarter precipitation sum
    ppt <- rast(tc_dl$files[grepl("ppt", tc_dl$files)])
    ppt_na <- ppt[[1]]
    ppt_na[] <- NA
    ppt_minus1 <- c(ppt_na, ppt[[-dim(ppt)[3]]])
    ppt_minus2 <- c(ppt_na, ppt_minus1[[-dim(ppt_minus1)[3]]])
    pptq_all <- sum(ppt, ppt_minus1, ppt_minus2)
    ## Discard first year, since we don't have complete data, and driest quarter could be in first three months of first year, which cannot be calculated.
    pptq <- subset(pptq_all, 13:dim(pptq_all)[3])
    pptqdq <- tapp(pptq,
                   rep(tc_dl$years[-1], each = 12),
                   min)
    pptqdq_file <- paste0(terraclimate_deriv_dir, "pptqdq.tif")
    writeRaster(pptqdq, pptqdq_file, overwrite = T)

#### Return ####

    deriv_files <- c(tmaxhm = tmaxhm_file, tmincm = tmincm_file, pptqdq = pptqdq_file)
        
    return(list(files = deriv_files,
                vars = tc_dl$vars,
                scenario = tc_dl$scenario,
                years = tc_dl$years,
                md5_hashes = sapply(deriv_files, digest, file = T, algo = "md5")))
}
