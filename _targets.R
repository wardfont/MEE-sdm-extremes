## Load your packages, e.g. library(targets).
source("./packages.R")

## Load your R files
lapply(list.files("./R", full.names = TRUE), source)

tar_plan(

  #### Settings ####

  ## Polygon outlining europe, excluding cyprus
  eu_poly_wkt = "POLYGON ((25.280527 74.385866, 34 74.516506, 34 35.773364, 32.861584 35.588421, 26.375071 34.609982, 22.816449 34.504241, 13.627633 36.690862, 10.001544 38.719154, -0.356862 36.404548, -9.323257 35.627079, -12.292937 57.408982, 25.280527 74.385866))",
  return_periods = c(10, 15, 20, 25),
  palette = create_palette(),

  #### TerraClimate ####

  ## Grid specifying the iterations for the following process in each row
  terraclimate_grid = data.frame(
    scenario = c("historical"),
    clim_years = c("1986-2015")
  ),
  terraclimate_vars = c("tmax", "tmin", "ppt"),
  tar_target(terraclimate_download,
    download_terraclimate(
      terraclimate_vars,
      terraclimate_grid,
      eu_poly_wkt
    ),
    pattern = map(terraclimate_grid), iteration = "list"
  ),
  tar_target(terraclimate_biovars,
    biovars_terraclimate(terraclimate_download),
    pattern = map(terraclimate_download), iteration = "list"
  ),
  tar_target(terraclimate_deriv,
    deriv_terraclimate(terraclimate_download),
    pattern = map(terraclimate_download), iteration = "list"
  ),
  tar_target(terraclimate_katz,
    katz_terraclimate(terraclimate_deriv, return_periods),
    pattern = map(terraclimate_deriv), iteration = "list"
  ),
  tar_target(terraclimate_stewart,
    stewart_terraclimate(terraclimate_deriv, return_periods),
    pattern = map(terraclimate_deriv), iteration = "list"
  ),
  tar_target(terraclimate_sdm,
    sdm_terraclimate(terraclimate_stewart, terraclimate_katz, euforest_extent),
    pattern = map(terraclimate_stewart, terraclimate_katz),
    iteration = "list"
  ),
  tar_target(terraclimate_normal,
    normal_terraclimate(terraclimate_deriv),
    pattern = map(terraclimate_deriv),
    iteration = "list"
  ),
  tar_target(terraclimate_delta_aic,
    normal_gev_aic_compare(terraclimate_katz, terraclimate_normal),
    pattern = map(terraclimate_katz, terraclimate_normal),
    iteration = "list"
  ),
  delta_aic_plot = per_pixel_delta_aic_plot(terraclimate_delta_aic[[1]]),

  #### EU-Forest ####

  ## EU-Forest settings
  euforest_extent = c(-11, 34, 35.5, 72),
  euforest_epsg = "EPSG:3035",
  buffer_km = 500, # in km

  ## Data repetitions settings
  repetitions = 1:10,

  ## Cross validation settings
  random_folds = 10,
  sblock_folds = 10,
  min_occ_prop = 0.01, # Each sblock fold must at least have this proportion of total presences


  ## EU-Forest data file
  tar_target(euforest_csv, "./data/euforest/EUForestspecies.csv", format = "file"),

  ## Select which speies have enough observations after aggregation, n_lim is threshold
  species = select_species_euforest(euforest_csv,
    env_file = terraclimate_sdm[[1]]$file,
    extent = euforest_extent,
    buffer_km = buffer_km,
    epsg = euforest_epsg,
    n_lim = 4000
  ),

  ## Split over species: Aggregate pr_ab per species, buffer area
  tar_target(euforest, occ_euforest(euforest_csv, species,
    env_file = terraclimate_sdm[[1]]$file,
    extent = euforest_extent,
    buffer_km = buffer_km,
    epsg = euforest_epsg
  ),
  pattern = map(species),
  iteration = "list"
  ),

  ## Create repetitions for each species
  tar_target(euforest_rep, rep_euforest(euforest, reps = repetitions),
    pattern = map(euforest), iteration = "list"
  ),

  ## ## Fit an spatial block for all predictors per species and repetition
  tar_target(euforest_partition_biovars, partition_euforest_biovars(euforest_rep,
    rep = repetitions,
    env_files = c(terraclimate_sdm[[1]]$file, terraclimate_biovars[[1]]$file),
    min_occ_prop = min_occ_prop,
    k_sblock = sblock_folds,
    k_random = random_folds
  ),
  pattern = cross(euforest_rep, repetitions),
  iteration = "list"
  ),

  #### Modelling ####

  ## Model settings
  models = c("base", "zimmerman", paste0("stewart", return_periods), "katz2", "katz"),
  predictor_patterns = c(
    "mean", "mean|sd",
    paste0("mean|", "anom", return_periods, "yr"),
    "location|scale",
    "location|scale|shape"
  ),
  threshold = "max_sens_spec", # Maximize TSS

  ## GAMs with more variables
  biovars_models = models[models %in% c("base", paste0("stewart", return_periods), "zimmerman", "katz")],
  biovars_predictor_patterns = predictor_patterns[models %in% c("base", paste0("stewart", return_periods), "zimmerman", "katz")],
  tar_target(euforest_biovars_model, model_biovars_euforest(euforest_partition_biovars,
    model = biovars_models,
    predictor_pattern = biovars_predictor_patterns,
    biovars_file = terraclimate_biovars[[1]]$file
  ),
  pattern = cross(euforest_partition_biovars, map(biovars_models, biovars_predictor_patterns)),
  iteration = "list"
  ),
  tar_target(euforest_gam_biovars,
    gam_euforest(euforest_biovars_model,
      thr = threshold,
      fold_name = "sblock.part"
    ),
    pattern = map(euforest_biovars_model),
    iteration = "list",
    deployment = "worker",
    garbage_collection = T
  ),
  tar_target(species_properties_file,
    "./data/species_properties/data.csv",
    format = "file"
  ),
  species_properties = read.csv(species_properties_file),

  ## Calculate metrics
  tar_target(euforest_gam_biovars_sblock_metrics,
    metrics_gam(euforest_gam_biovars),
    pattern = map(euforest_gam_biovars)
  ),
  tar_target(euforest_species_characteristics,
    species_characteristics(
      euforest_partition_biovars,
      terraclimate_biovars,
      terraclimate_sdm
    ),
    pattern = map(euforest_partition_biovars)
  ),
  AUC_model_data = tibble(euforest_gam_biovars_sblock_metrics) %>%
    drop_na() %>%
    left_join(euforest_species_characteristics, by = c("species", "rep")) %>%
    select(
      rep, species, model,
      AUC, n_presences, n_absences,
      bio1_mean, bio12_mean,
      contains("tmaxhm"),
      contains("tmincm"),
      contains("pptqdq"),
      convex_area
    ) %>%
    pivot_wider(names_from = model, values_from = AUC) %>%
    left_join(species_properties, by = "species") %>%
    drop_na() %>%
    rename(AUC_G = katz, AUC_B = base) %>%
    mutate(delta_AUC = AUC_G - AUC_B) %>%
    select(-zimmerman, -contains("stewart")),

  #### Plotting ####

  ## Define target species
  display_species = c(
    "Acer pseudoplatanus",
    "Quercus pubescens",
    "Abies alba",
    "Picea abies"
  ),
  
  ## Metrics difference table and boxplot
  AIC_and_AUC_gam_biovars_sblock_diff_to_B = performance_difference_to_B_comparison_boxplot(
    euforest_gam_biovars_sblock_metrics,
    palette,
    species_properties
  ),
  AIC_and_AUC_gam_biovars_sblock_diff_to_G = performance_difference_to_G_comparison_boxplot(
    euforest_gam_biovars_sblock_metrics,
    palette,
    species_properties
  ),
  AIC_and_AUC_gam_biovars_sblock_all_species = performance_difference_all_species_boxplot(
    euforest_gam_biovars_sblock_metrics,
    palette,
    species_properties
  ),

  ## ## Ensemble predictions for biovar models
  tar_target(
    katz_ensemble_gam_biovars_predictions,
    gam_ensemble(euforest_gam_biovars,
      model = "katz",
      display_species,
      env_files = c(terraclimate_sdm[[1]]$file, terraclimate_biovars[[1]]$file)
    ),
    pattern = map(display_species),
    garbage_collection = T
  ),
  tar_target(
    base_ensemble_gam_biovars_predictions,
    gam_ensemble(euforest_gam_biovars,
      model = "base",
      display_species,
      env_files = c(terraclimate_sdm[[1]]$file, terraclimate_biovars[[1]]$file)
    ),
    pattern = map(display_species),
    garbage_collection = T
  ),
  tar_target(
    zimmerman_ensemble_gam_biovars_predictions,
    gam_ensemble(euforest_gam_biovars,
      model = "zimmerman",
      display_species,
      env_files = c(terraclimate_sdm[[1]]$file, terraclimate_biovars[[1]]$file)
    ),
    pattern = map(display_species),
    garbage_collection = T
  ),
  tar_target(
    stewart_ensemble_gam_biovars_predictions,
    gam_ensemble(euforest_gam_biovars,
      model = "stewart",
      display_species,
      env_files = c(terraclimate_sdm[[1]]$file, terraclimate_biovars[[1]]$file)
    ),
    pattern = map(display_species),
    garbage_collection = T
  ),

  ## Plot spatial differences
  one_spatial_plot_gam_biovars = spatial_plot_new_ensemble(
    katz_ensemble_gam_biovars_predictions,
    base_ensemble_gam_biovars_predictions,
    stewart_ensemble_gam_biovars_predictions,
    zimmerman_ensemble_gam_biovars_predictions,
    euforest,
    species,
    display_species,
    palette,
    terraclimate_sdm,
    limit = 0.5
  ),
  
  ## Correlation plot
  cor_plot = get_cor_plot(terraclimate_sdm[[1]]$file),

  ## Info per partition for SI
  summary_species_records = species_records_summary(
    euforest,
    euforest_partition_biovars
  ),

  ## Occurrence plot
  occurrence_plots = plot_pr_ab(euforest, species, species, env_file = terraclimate_sdm[[1]]$file)
  
)
