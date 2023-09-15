## Load your packages, e.g. library(targets).
source("./packages.R")

## Load your R files
lapply(list.files("./R", full.names = TRUE), source)

options(clustermq.scheduler = "multiprocess")

library(future)
library(future.callr)
plan(callr)

tar_option_set(memory = "transient", garbage_collection = TRUE)
tar_option_set(storage = "worker", retrieval = "worker")

## tar_plan supports drake-style targets and also tar_target()
tar_plan(

#### Settings ####

  ## Polygon outlining europe, excluding cyprus
  eu_poly_wkt =  "POLYGON ((25.280527 74.385866, 34 74.516506, 34 35.773364, 32.861584 35.588421, 26.375071 34.609982, 22.816449 34.504241, 13.627633 36.690862, 10.001544 38.719154, -0.356862 36.404548, -9.323257 35.627079, -12.292937 57.408982, 25.280527 74.385866))",

  return_periods = c(10, 15, 20, 25),

  palette = create_palette(),

#### TerraClimate ####

  ## Grid specifying the iterations for the following process in each row
  terraclimate_grid = data.frame(scenario = c("historical"),
                                 clim_years = c("1986-2015")),
  terraclimate_vars = c("tmax", "tmin", "ppt"),

  tar_target(terraclimate_download,
             download_terraclimate(terraclimate_vars,
                                   terraclimate_grid,
                                   eu_poly_wkt),
             pattern = map(terraclimate_grid), iteration = "list"),

  tar_target(terraclimate_biovars,
             biovars_terraclimate(terraclimate_download),
             pattern = map(terraclimate_download), iteration = "list"),
  tar_target(terraclimate_deriv,
             deriv_terraclimate(terraclimate_download),
             pattern = map(terraclimate_download), iteration = "list"),
  tar_target(terraclimate_katz,
             katz_terraclimate(terraclimate_deriv, return_periods),
             pattern = map(terraclimate_deriv), iteration = "list"),
  tar_target(terraclimate_stewart,
             stewart_terraclimate(terraclimate_deriv, return_periods),
             pattern = map(terraclimate_deriv), iteration = "list"),
  tar_target(terraclimate_sdm,
             sdm_terraclimate(terraclimate_stewart, terraclimate_katz, euforest_extent),
             pattern = map(terraclimate_stewart, terraclimate_katz),
             iteration = "list"),

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
                                    n_lim = 4000),

  ## Split over species: Aggregate pr_ab per species, buffer area
  tar_target(euforest, occ_euforest(euforest_csv, species,
                                    env_file = terraclimate_sdm[[1]]$file,
                                    extent = euforest_extent,
                                    buffer_km = buffer_km,
                                    epsg = euforest_epsg),
             pattern = map(species),
             iteration = "list"),

  ## Create repetitions for each species
  tar_target(euforest_rep, rep_euforest(euforest, reps = repetitions),
             pattern = map(euforest), iteration = "list"),

  ## Fit an spatial block for all predictors per species and repetition
  tar_target(euforest_partition, partition_euforest(euforest_rep,
                                                    rep = repetitions,
                                                    env_file = terraclimate_sdm[[1]]$file,
                                                    min_occ_prop = min_occ_prop,
                                                    k_sblock = sblock_folds,
                                                    k_random = random_folds),
             pattern = cross(euforest_rep, repetitions),
             iteration = "list"),

#### Modelling ####

  ## Model settings
  models = c("base", "zimmerman", paste0("stewart", return_periods), "katz2", "katz"),
  predictor_patterns = c("mean", "mean|sd",
                         paste0("mean|", "anom", return_periods, "yr"),
                         "location|scale",
                         "location|scale|shape"),
  threshold = "max_sens_spec", # Maximize TSS

  ## Split over repetitions and models
  tar_target(euforest_model, model_euforest(euforest_partition,
                                            model = models,
                                            predictor_pattern = predictor_patterns),
             pattern = cross(euforest_partition, map(models, predictor_patterns)),
             iteration = "list"),

  ## GAMs
  tar_target(euforest_gam,
             gam_euforest(euforest_model,
                          thr = threshold,
                          fold_name = "sblock.part"),
             pattern = map(euforest_model),
             iteration = "list"),

  ## GAMs with more variables
  biovars_models = models[models %in% c("base", "katz")],
  biovars_predictor_patterns = predictor_patterns[models %in% c("base", "katz")],

  tar_target(euforest_biovars_model, model_biovars_euforest(euforest_partition,
                                                            model = biovars_models,
                                                            predictor_pattern = biovars_predictor_patterns,
                                                            biovars_file = terraclimate_biovars[[1]]$file),
             pattern = cross(euforest_partition, map(biovars_models, biovars_predictor_patterns)),
             iteration = "list"),

  tar_target(euforest_gam_biovars,
             gam_euforest(euforest_biovars_model,
                          thr = threshold,
                          fold_name = "sblock.part"),
             pattern = map(euforest_biovars_model),
             iteration = "list"),

  ## Calculate metrics
  tar_target(euforest_gam_sblock_metrics,
             metrics_gam(euforest_gam),
             pattern = map(euforest_gam)),

 tar_target(euforest_gam_biovars_sblock_metrics,
             metrics_gam(euforest_gam_biovars),
             pattern = map(euforest_gam_biovars)),

#### Plotting ####

  ## Define target species
  display_species = c("Abies alba", "Betula pendula", "Fagus sylvatica", "Picea abies", "Quercus ilex"),

  ## Boxplot comparison metrics
  AIC_AUC_and_TSS_vs_GEV_gam_sblock = ggarrange(
    performance_comparison_boxplot(euforest_gam_sblock_metrics, variable = "AIC", display_species, palette, alt = "greater"),
    performance_comparison_boxplot(euforest_gam_sblock_metrics, variable = "AUC", display_species, palette),
    performance_comparison_boxplot(euforest_gam_sblock_metrics, variable = "TSS", display_species, palette),
    ncol = 1, labels = c("(a)", "(b)", "(c)")),

  G2_AIC_AUC_and_TSS_vs_GEV_gam_sblock = ggarrange(
    performance_comparison_boxplot_2(euforest_gam_sblock_metrics, variable = "AIC", display_species, palette, alt = "greater", incl = c("G2", "G")),
    performance_comparison_boxplot_2(euforest_gam_sblock_metrics, variable = "AUC", display_species, palette, incl = c("G2", "G")),
    performance_comparison_boxplot_2(euforest_gam_sblock_metrics, variable = "TSS", display_species, palette, incl = c("G2", "G")),
    ncol = 1, labels = c("(a)", "(b)", "(c)")),

  AIC_AUC_and_TSS_vs_GEV_gam_biovars_sblock = ggarrange(
    performance_comparison_boxplot_2(euforest_gam_biovars_sblock_metrics, variable = "AIC", display_species, palette, alt = "greater",
                                     incl = c("B", "G"), names = c("B-Bioclim", "G-Bioclim")),
    performance_comparison_boxplot_2(euforest_gam_biovars_sblock_metrics, variable = "AUC", display_species, palette,
                                     incl = c("B", "G"), names = c("B-Bioclim", "G-Bioclim")),
    performance_comparison_boxplot_2(euforest_gam_biovars_sblock_metrics, variable = "TSS", display_species, palette,
                                     incl = c("B", "G"), names = c("B-Bioclim", "G-Bioclim")),
    ncol = 1, labels = c("(a)", "(b)", "(c)")),


  ## Metrics difference table and boxplot
  AUC_TSS_median_tables = performance_comparison_table(euforest_gam_sblock_metrics, display_species, palette),

  AIC_AUC_and_TSS_vs_GEV_gam_sblock_diff_GN = performance_difference_comparison_boxplot(euforest_gam_sblock_metrics,
                                                                                        comparison = c("G", "N")),

  ## Ensemble predictions
  tar_target(katz_ensemble_gam_predictions,
             gam_ensemble(euforest_gam,
                          model = "katz",
                          display_species,
                          env_file = terraclimate_sdm[[1]]$file)),

  tar_target(zimmerman_ensemble_gam_predictions,
             gam_ensemble(euforest_gam,
                          model = "zimmerman",
                          display_species,
                          env_file = terraclimate_sdm[[1]]$file)),

  tar_target(base_ensemble_gam_predictions,
             gam_ensemble(euforest_gam,
                          model = "base",
                          display_species,
                          env_file = terraclimate_sdm[[1]]$file)),

  tar_target(stewart_ensemble_gam_predictions,
             gam_ensemble(euforest_gam,
                          model = "stewart",
                          display_species,
                          env_file = terraclimate_sdm[[1]]$file)),

  ## Plot spatial differences
  one_spatial_plot = spatial_plot_one(katz_ensemble_gam_predictions,
                                      base_ensemble_gam_predictions,
                                      stewart_ensemble_gam_predictions,
                                      zimmerman_ensemble_gam_predictions,
                                      euforest,
                                      species,
                                      display_species,
                                      palette),

  ## Correlation plot
  cor_plot = get_cor_plot(terraclimate_sdm[[1]]$file),

  ## Occurrence plot
  occurrence_plots = plot_pr_ab(euforest, species, display_species, env_file = terraclimate_sdm[[1]]$file),

  ## Venn diagram plot
  venn_plot = plot_venn(katz_ensemble_gam_predictions,
                        base_ensemble_gam_predictions,
                        stewart_ensemble_gam_predictions,
                        zimmerman_ensemble_gam_predictions,
                        euforest,
                        species,
                        display_species,
                        palette)
)
