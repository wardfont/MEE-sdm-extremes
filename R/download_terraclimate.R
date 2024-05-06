#' .. content for \description{} (no empty lines) ..
#' Download a cropped version of the terraclimate dataset.
#' .. content for \details{} ..
#' See https://www.climatologylab.org/
#' The function saves the full .nc raster in a temporary folder before cropping and saving crop as .tif. The temorary file is removed afterwards. If the cropped file already exists, the function makes sure the extent matches the wished study area extent. 
#' @title
#' @param terraclimate_vars Can be aet, def, pet, ppt, q, soil, srad, swe, tmax, tmin, vap, ws, vpd or pdsi. Provided in a vector as character strings. 
#' @param terraclimate_grid vector with scenario, being either "historical", "plus2c" or "plus4c", and clim_years specifying the years to be downloaded as "YYYY-YYYY".
#' @param wkt Defined in well-known-text format
#' @return
#' @author Ward Fonteyn
#' @export
download_terraclimate <- function(terraclimate_vars, terraclimate_grid, wkt) {

    scenario <- terraclimate_grid$scenario
    clim_years <- substr(terraclimate_grid$clim_years, 1, 4):substr(terraclimate_grid$clim_years, 6, 9)
    
    if(scenario == "historical"){
        
        terraclimate_dir <- "./data/terraclimate/historical/"
        base_url <- "https://climate.northwestknowledge.net/TERRACLIMATE-DATA/"
        file_names <- paste(
        rep(paste("TerraClimate", terraclimate_vars, sep = "_"), each = length(clim_years)),
        clim_years, sep = "_")
        
    } else if(scenario == "plus2c"){
        
        terraclimate_dir <- "./data/terraclimate/plus2c/"
        base_url <- "http://thredds.northwestknowledge.net:8080/thredds/fileServer/TERRACLIMATE_ALL/data_plus2C/"
        file_names <- paste(
            rep(paste("TerraClimate_2c", terraclimate_vars, sep = "_"),
                each = length(clim_years)),
            clim_years, sep = "_")
        
    } else if(scenario == "plus4c"){
        
        terraclimate_dir <- "./data/terraclimate/plus4c/"
        base_url <- "http://thredds.northwestknowledge.net:8080/thredds/fileServer/TERRACLIMATE_ALL/data_plus4C/"
        file_names <- paste(
            rep(paste("TerraClimate_4c", terraclimate_vars, sep = "_"),
                each = length(clim_years)),
            clim_years, sep = "_")
        
    }
        
    options(timeout=1000) # Increase to avoid timeouts

    crop_polygon <- vect(wkt)
    cropped_file_names <- paste0(terraclimate_dir, "cropped_", file_names, ".tif")

    ## Wihout this, extents of vect and rast can still differ after comma
    if (!file.exists(paste0(terraclimate_dir, "extent/", "correct.nc"))){
        download.file(url = paste0(base_url, file_names[1], ".nc"),
                      destfile = paste0(terraclimate_dir, "extent/", "correct.nc"))
    }
    
    correct_extent <- ext(mask(crop(rast(paste0(terraclimate_dir, "extent/", "correct.nc")), crop_polygon), crop_polygon))
    
    lapply(file_names,
           function(file_name){
               if (!file.exists(paste0(terraclimate_dir, "cropped_", file_name, ".tif")) ||
                   (ext(rast(paste0(terraclimate_dir, "cropped_", file_name, ".tif"))) !=  correct_extent)){

                   ## Downloading first and then cropping is faster than reading raster directly from remote
                   download.file(url = paste0(base_url, file_name, ".nc"),
                                 destfile = paste0(terraclimate_dir, "temp/", file_name, ".nc"))
                   terraclimate_rast <- mask(crop(rast(paste0(terraclimate_dir, "temp/", file_name, ".nc")),
                                             crop_polygon), crop_polygon)

                   writeRaster(terraclimate_rast,
                               paste0(terraclimate_dir, "cropped_", file_name, ".tif"),
                               overwrite = T)

                   file.remove(paste0(terraclimate_dir, "temp/", file_name, ".nc"))
               }
           })

    options(timeout=60) # Revert to default

    return(list(files = cropped_file_names,
                vars = terraclimate_vars,
                scenario = scenario,
                years = clim_years,
                md5_hashes = sapply(cropped_file_names, digest, file = T, algo = "md5")))
}
