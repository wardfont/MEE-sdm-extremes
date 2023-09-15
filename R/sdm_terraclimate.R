sdm_terraclimate <- function(terraclimate_stewart, terraclimate_katz, extent) {

    stewart <- rast(terraclimate_stewart$files)

    rl_to_anom <- function(var, rast){
        mean <- subset(rast, paste0("mean_", var))
        sd <- subset(rast, paste0("sd_", var))
        rl <- subset(rast, names(rast)[grepl(paste0("yr_", var), names(rast))])
        anom <- rl - mean
        names(anom) <- gsub("rl", "anom", names(rl))
        return(c(mean, sd, anom))
    }

    stewart_anom <- rast(lapply(c("tmaxhm", "tmincm", "pptqdq"),
                                function(var){rl_to_anom(var = var, rast = stewart)}))
                
    katz <- rast(terraclimate_katz$files)
    katz_sub <- subset(katz, c("location_tmaxhm", "scale_tmaxhm", "shape_tmaxhm",
                               "location_tmincm", "scale_tmincm", "shape_tmincm",
                               "location_pptqdq", "scale_pptqdq", "shape_pptqdq"))

    sdm_rast <- crop(c(stewart_anom, katz_sub), ext(extent))
    sdm_rast_file <- paste0("./output/terraclimate/",
                            terraclimate_stewart$scenario,
                            "/sdm/sdm.tif")
    writeRaster(sdm_rast, sdm_rast_file, overwrite = T)

    return(list(file = sdm_rast_file,
                vars = terraclimate_stewart$vars,
                scenario = terraclimate_stewart$scenario,
                years = terraclimate_stewart$years,
                return_periods = terraclimate_stewart$return_periods,
                md5_hashes = digest(sdm_rast_file, file = T, algo = "md5")))
}
