#' .. content for \description{} (no empty lines) ..
#' Derive biovars
#' .. content for \details{} ..
#' @title
#' @param tc_dl Return from the terracliamte_download funtion.
#' @return
#' @author Ward Fonteyn
#' @export
biovars_terraclimate <- function(tc_dl) {

  terraclimate_biovar_dir <- paste0("./output/terraclimate/",
                                    tc_dl$scenario,
                                    "/biovars/")
#### Tmax ####

  tmax <- rast(tc_dl$files[grepl("tmax", tc_dl$files)])
  tmax_mean <- tapp(tmax,
                    rep(1:12, length(tc_dl$years)),
                    mean)

#### Tmin ####

  tmin <- rast(tc_dl$files[grepl("tmin", tc_dl$files)])
  tmin_mean <- tapp(tmin,
                    rep(1:12, length(tc_dl$years)),
                    mean)

#### Ppt  ####

  ## Rolling quarter precipitation sum
  ppt <- rast(tc_dl$files[grepl("ppt", tc_dl$files)])
  ppt_mean <- tapp(ppt,
                    rep(1:12, length(tc_dl$years)),
                    mean)

#### Biovar ####

  bvars <- biovars(prec = stack(ppt_mean),
                   tmin = stack(tmin_mean),
                   tmax = stack(tmax_mean))

  bvars_file <- paste0(terraclimate_biovar_dir, "biovars.tif")
  writeRaster(rast(bvars), bvars_file, overwrite = T)
#### Return ####

  return(list(file = bvars_file,
              vars = tc_dl$vars,
              scenario = tc_dl$scenario,
              years = tc_dl$years,
              md5_hashes = sapply(bvars_file, digest, file = T, algo = "md5")))
}
