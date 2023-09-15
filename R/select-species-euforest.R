select_species_euforest <- function(euforest_csv, env_file, extent,
                         buffer_km, epsg, n_lim = 4000) {

  ## Summarise for species
  data <- read.csv(euforest_csv) %>%
    as_tibble() %>%
    mutate(x = as.numeric(X), y = as.numeric(Y))

  # At least 1000 presences before going to test aggregated amount
  species_names <- data  %>%
    group_by(SPECIES.NAME) %>%
    summarise(n = n()) %>%
    arrange(desc(n)) %>%
    filter(n >= n_lim) %>%
    pull(SPECIES.NAME)

  ns <- lapply(species_names, function(selected_species){

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

    n <- sum(data_ca$pr_ab)
  })

  selected_species <- tibble(sp = species_names, n = unlist(ns)) %>%
    filter(n >= n_lim) %>%
    arrange(sp) %>%
    pull(sp)

  return(selected_species)
}
