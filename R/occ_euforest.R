occ_euforest <- function(euforest_csv, selected_species, env_file, extent,
                         buffer_km, epsg) {

  ## Summarise for species
  data <- read.csv(euforest_csv) %>%
    mutate(pr_ab = case_when(SPECIES.NAME == selected_species ~ 1, T ~ 0),
           x = as.numeric(X), y = as.numeric(Y)) %>%
    select(x, y, pr_ab) %>%
    group_by(x, y) %>%
    summarise(pr_ab = max(pr_ab))

  ## Project
  env <- crop(rast(env_file), ext(extent))
  data_vect <- crop(project(
    vect(data, geom = c("x", "y"), crs = epsg),
    env), extent)

  ## Calibration area
  data_pr <- as.data.frame(data_vect, geom = "XY") %>% filter(pr_ab == 1)
  ca <- calib_area(data = data_pr,
                   x = "x",
                   y = "y",
                   method = c("buffer", width = buffer_km*1000),
                   crs = crs(env))

  data_ca_vect <- crop(data_vect, ca)
  ## plot(data_vect_ca)
  ## plot(vect(data_pr, geom = c("x", "y"), crs = crs(env)), add = T, col = "green")

  data_ca <- as.data.frame(data_ca_vect, geom = "XY") %>%
    mutate(cell = cells(env, data_ca_vect)[,"cell"]) %>%
    group_by(cell) %>%
    summarise(pr_ab = max(pr_ab)) %>%
    mutate(x = xFromCell(env, cell), y = yFromCell(env, cell), id = 1:length(cell)) %>%
    select(x, y, pr_ab)

  data_ca_e <- terra::extract(env, vect(data_ca, geom = c("x", "y"))) %>%
    select(-ID)

  data_out <- cbind(data_ca, data_ca_e) %>% drop_na()

  print(paste("Buffer of", buffer_km,"km kept", dim(data_out)[1], "records for", selected_species, "with", dim(data_pr)[1], "presences"))

  return(list(data = data_out,
              ca = wrap(ca),
              buffer = buffer_km,
              species = selected_species))

}
