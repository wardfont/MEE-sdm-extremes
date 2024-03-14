species_characteristics <- function(euforest_partition,
                                    terraclimate_biovars,
                                    terraclimate_sdm) {
  biovars <- rast(terraclimate_biovars[[1]]$file)
  sdm <- rast(terraclimate_sdm[[1]]$file)
  ## delta_aic <- rast(terraclimate_delta_aic)

  pr_only <- euforest_partition$data %>%
    filter(pr_ab == 1)

  bios <- tibble(extract(biovars, vect(pr_only, geom = c("x", "y")))) %>%
    select(bio1, bio12)

  convex_hull <- pr_only %>% slice(chull(x, y))

  convex_area <- as.matrix(convex_hull) %>% areapl()

  data <- tibble(cbind(pr_only, bios)) %>%
    summarise(
      bio1_mean = mean(bio1),
      bio12_mean = mean(bio12),
      tmaxhm_location_mean = mean(location_tmaxhm),
      tmincm_location_mean = mean(location_tmincm),
      pptqdq_location_mean = mean(location_pptqdq),
      tmaxhm_scale_mean = mean(scale_tmaxhm),
      tmincm_scale_mean = mean(scale_tmincm),
      pptqdq_scale_mean = mean(scale_pptqdq),
      tmaxhm_shape_mean = mean(shape_tmaxhm),
      tmincm_shape_mean = mean(shape_tmincm),
      pptqdq_shape_mean = mean(shape_pptqdq)
    ) %>%
    mutate(
      species = euforest_partition$species,
      rep = euforest_partition$rep,
      convex_area = convex_area
    )

  return(data)
}
