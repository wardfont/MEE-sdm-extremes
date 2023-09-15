create_palette <- function(){
  source_palette <- palette.colors(palette = "Okabe-Ito")
  palette <- c("B" = source_palette[8],
              "E" = source_palette[2],
              "N" = source_palette[3],
              "G" = source_palette[4])
  names(palette) <- c("B", "E", "N", "G")

  return(palette)

}

name_models <- function(x){

  model_names <- ordered(case_when(x == "base" ~ "B",
                           x == "zimmerman" ~ "N",
                           x == "stewart10" ~ "E10",
                           x == "stewart15" ~ "E15",
                           x == "stewart20" ~ "E20",
                           x == "stewart25" ~ "E25",
                           x == "katz" ~ "G",
                           x == "katz2" ~ "G2"),
                         levels = c("B", "E10", "E15", "E20", "E25", "N", "G2", "G"))
}

performance_comparison_table <- function(metrics, display_species, palette,
                                         alt = "less",
                                         incl = c("B", "E10", "E15", "E20", "E25", "N", "G2", "G")){
  data <- metrics %>%
    mutate(id = factor(paste0(rep)))
  all_species <- data %>% mutate(rep = paste0(rep, species),
                                 species = "All species, n = 28")

  tables <- lapply(c("AUC", "TSS"), function(var){

    test_data <- rbind(data %>% filter(species %in% display_species), all_species) %>%
      mutate(model = ordered(name_models(model), levels = c("G", "B", "E10", "E15", "E20", "E25", "N", "G2")),
             approach = ordered(substr(model,1,1), levels = c("B", "E", "N", "G")),
             species = ordered(species, levels = c(unique(data$species), "All species, n = 28"))) %>%
      arrange(species, rep, model) %>%
      filter(model %in% incl) %>%
      pivot_wider(id_cols = c("rep", "species"),
                  names_from = "model", values_from = contains(var)) %>%
      mutate_at(vars(-matches("rep"), -matches("species")), list(dif = ~ . - G)) %>%#, perdif = ~ (. - G)/.)) %>%
      select(-c("G", "B", "E10", "E15", "E20", "E25", "N", "G2")) %>%
      group_by(species) %>%
      summarise_at(vars(-matches("rep"), -matches("species")), list(med = ~ median(.))) %>%
      select(-G_dif_med)#, -G_perdif_med)

    colnames(test_data) <- c("Species", "B - G", "E10 - G", "E15 - G", "E20 - G",
                             "E25 - G", "N - G", "G2 - G")
    return(test_data)
  })

  return(tables)
}

performance_comparison_boxplot <- function(metrics, variable, display_species, palette,
                                           alt = "less",
                                           incl = c("B", "E10", "E15", "E20", "E25", "N", "G")){
  data <- metrics %>%
    mutate(id = factor(paste0(rep))) %>%
    dplyr::rename(var = all_of(variable))
  all_species <- data %>% mutate(rep = paste0(rep, species),
                                 species = "All species, n = 28")


  test_data <- rbind(data %>% filter(species %in% display_species), all_species) %>%
    mutate(model = ordered(name_models(model), levels = c("G", "B", "E10", "E15", "E20", "E25", "N")),
           approach = ordered(substr(model,1,1), levels = c("B", "E", "N", "G")),
           species = ordered(species, levels = c(unique(data$species), "All species, n = 28"))) %>%
    arrange(species, rep, model) %>%
    filter(model %in% incl)

  test_results <- test_data %>%
    group_by(species) %>%
    mutate(fr_p = friedmanTest(var, groups = model, blocks = rep, alternative = alt)$p.value,
           fr_ny_p = rep(c(NA, frdManyOneNemenyiTest(var, groups = model, blocks = rep, alternative = alt)$p.value), length(unique(rep))),
           n = length(unique(rep))) %>%
    group_by(species, model) %>%
    summarise(fr_p = first(fr_p),
              fr_ny_p = first(fr_ny_p),
              n = first(n)) %>%
    mutate(label_fr = paste0("Friedman, ", ifelse(fr_p < 0.05, "p < 0.05", paste0("p = ", round(fr_p, digits = 2))), ", n = ", n),
           label_fr_ny = paste0(ifelse(replace_na(fr_ny_p, 1) < 0.05, "\u2217", "")))

  plot_data <- test_data %>%
    mutate(model = ordered(model, levels = c("B", "E10", "E15", "E20", "E25", "N", "G")))

  ggpaired(data = plot_data, x = "model", y = "var", id = "rep", facet.by = "species", col = "approach",
           palette = palette,
           xlab = "Model", ylab = variable,
           line.size = 0.05, line.color = "grey",
           ylim = c(min(plot_data$var) - 0.04, max(plot_data$var) + 0.02)#,
           ) +
    geom_text(data = test_results %>% group_by(species) %>% summarise(label_fr = first(label_fr)), aes(label = label_fr, x = 4, y = min(plot_data$var) - 0.025)) +
    geom_text(data = test_results, aes(label = label_fr_ny, y = max(plot_data$var) + 0.025), size = 8) +
    theme_pubr(legend = "none") +
    theme(strip.text = element_text(face = "italic"))
}

performance_difference_comparison_boxplot <- function(metrics, comparison = c("G", "N")){

  variables <- c("AIC", "AUC", "TSS")

 datas <- lapply(variables, function(variable){

    data <- metrics %>%
      mutate(id = factor(paste0(rep))) %>%
      rename(var = all_of(variable)) %>%
      mutate(model = name_models(model)) %>%
      filter(model %in% comparison) %>%
      select(rep, species, var, id, model) %>%
      pivot_wider(names_from = model, values_from = var) %>%
      mutate(diff = (get(comparison[1]) - get(comparison[2]))) %>% #/get(comparison[2]))
      mutate(metric = variable)

  })

  species_order <- datas[[2]] %>%
    group_by(species) %>%
    summarise(median = median(diff)) %>%
    arrange(-median) %>%
    pull(species)

  plot_data <- do.call(rbind, datas)

  ggplot(data = plot_data %>% mutate(species = ordered(species, levels = species_order)), aes(x = species, y = diff)) +
    geom_hline(yintercept = 0) +
    geom_boxplot() +
    labs(x = NULL) +
    scale_x_discrete(guide = guide_axis(angle = 60)) +
    theme_pubr() +
    theme(axis.text.x = element_text(face = "italic")) +
    facet_wrap(~ metric, strip.position = "left", scales = "free_y", ncol = 1) +
    ylab(NULL) +
    theme(strip.background = element_blank(),
          strip.placement = "outside")
 }



spatial_plot_ref_vs_compare <- function(ref_predictions, comp_predictions,
                                        ref_name, comp_name,
                                        euforest, species, palette){
  layer <- "max_sens_spec"

  layers <- lapply(1:length(ref_predictions), function(x){
    ref_trunc <- mask(subset(rast(ref_predictions[[x]]), layer), vect(euforest[[x]]$ca))
    comp_trunc <- mask(subset(rast(comp_predictions[[x]]), layer), vect(euforest[[x]]$ca))
    europe <- subset(rast(ref_predictions[[x]]), layer)

    rast <- ref_trunc - comp_trunc
    rast[ref_trunc != 1 & comp_trunc != 1] <- 2
    rast[is.na(rast) & !is.na(europe)] <- -2
    names(rast) <- species[x]
    rast <- classify(rast, c(-2.1,-1.1,-0.9, 0.1,1.1,2.1))
  })

  labels <- data.frame(lyr = species, label = species)

  plot <- ggplot() +
    geom_spatraster(data = rast(layers)) +
    facet_wrap(~lyr, ncol = 8) +
    geom_text(data = labels, aes(x = -10, y = 68, label = label),
              size = 3.5, hjust = 0, vjust = 0, fontface = "italic") +
    scale_fill_manual(values =  c("#EEEEEE",  as.character(palette[comp_name]), "#FDF9B9", as.character(palette[ref_name]), "#CCCCCC"),
                      labels = c("Outside calibration area", paste("Only", comp_name), "Both", paste("Only", ref_name), "Neither"), na.translate = F) +
    theme_pubr() +
    theme(legend.title = element_blank(),
          axis.title = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          axis.line = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          strip.background = element_blank(),
          panel.border = element_blank(),
          strip.text.x = element_blank())

  return(plot)
}

plot_pr_ab <- function(euforest, species, display_species, env_file){

  eu <- rast(env_file)
  eu_background <- eu[[1]]
  eu_background[!is.na(eu_background)] <- 0
  levels(eu_background) <- "bg"

  is <- species %in% display_species
  euforest <- subset(euforest,is)


  plots <- lapply(euforest, function(sp_euforest){
    pr <- vect(sp_euforest$data %>% filter(pr_ab == 1),
                      geom = c("x", "y"), crs = "EPSG:4326")
    ab <- vect(sp_euforest$data %>% filter(pr_ab == 0),
                      geom = c("x", "y"), crs = "EPSG:4326")

    plot <- ggplot() +
      geom_spatraster(data = eu_background, show.legend = F) +
      scale_fill_manual(values  = c("light grey"), na.value = "transparent") +
      geom_spatvector(data = ab, size = 0.001, aes(col = "Absence")) +
      geom_spatvector(data = pr, size = 0.001, aes(col = "Presence")) +
      scale_colour_manual(values = c("black", "dark green")) +
      theme_pubr() +
      ggtitle(sp_euforest$species) +
      theme(legend.title = element_blank(),
            axis.title = element_blank(),
            axis.text = element_blank(),
            axis.ticks = element_blank(),
            axis.line = element_blank()) +
      border() +
     guides(color = guide_legend(override.aes = list(size = 3)))
  })

  output_plot <- ggarrange(plotlist = plots, ncol = 5, nrow = 1, common.legend = T)
  return(ggplotGrob(output_plot))
}

spatial_plot_one <- function(katz_ensemble_predictions,
                             base_ensemble_predictions,
                             stewart_ensemble_predictions,
                             zimmerman_ensemble_predictions,
                             euforest, species, display_species, palette){

  is <- species %in% display_species

  katz_vs_base = spatial_plot_ref_vs_compare(katz_ensemble_predictions,
                                             base_ensemble_predictions,
                                             ref_name = "G" , comp_name = "B",
                                             subset(euforest,is),
                                             display_species, palette)
  katz_vs_stewart = spatial_plot_ref_vs_compare(katz_ensemble_predictions,
                                                stewart_ensemble_predictions,
                                                ref_name = "G", comp_name = "E",
                                                subset(euforest,is),
                                                display_species, palette)
  katz_vs_zimmerman = spatial_plot_ref_vs_compare(katz_ensemble_predictions,
                                                  zimmerman_ensemble_predictions,
                                                  ref_name = "G", comp_name = "N",
                                                  subset(euforest,is),
                                                  display_species, palette)

  legend <- ggplot(data = data.frame(value = c("Only B",
                                               "Only E",
                                               "Only N",
                                               "Only G",
                                               "Both",
                                               "Neither",
                                               "Outside calibration area"),
                                     x = rep(1,7))) +
    geom_bar(aes(x = x, fill = value)) +
    scale_fill_manual(name = NULL,
                      breaks = c("Only B",
                                 "Only E",
                                 "Only N",
                                 "Only G",
                                 "Both",
                                 "Neither",
                                 "Outside calibration area"),
                      values = c("Only B" = as.character(palette["B"]),
                                 "Only E" = as.character(palette["E"]),
                                 "Only N" = as.character(palette["N"]),
                                 "Only G" = as.character(palette["G"]),
                                 "Both" = "#FDF9B9",
                                 "Neither" = "#CCCCCC",
                                 "Outside calibration area" = "#EEEEEE")) +
    theme(legend.position = "top") +
    guides(fill = guide_legend(nrow = 1))

  one <- ggarrange(ggarrange(katz_vs_base + theme(legend.position = "none")),
                   ggarrange(katz_vs_stewart + theme(legend.position = "none")),
                   ggarrange(katz_vs_zimmerman + theme(legend.position = "none")),
                   legend.grob = get_legend(legend),
                   ncol = 1,
                   labels = c("(a)", "(b)", "(c)"))
  one

  return(ggplotGrob(one))
}


plot_venn <- function(katz_ensemble_predictions,
                      base_ensemble_predictions,
                      stewart_ensemble_predictions,
                      zimmerman_ensemble_predictions,
                      euforest,
                      species,
                      display_species,
                      palette){



  is <- species %in% display_species
  euforest <- subset(euforest,is)

  mark <-  function(x){
    e <- unwrap(x)[["max_sens_spec"]]
    cells(e,c(1))
  }

  gs <- lapply(katz_ensemble_predictions, mark)
  bs <- lapply(base_ensemble_predictions, mark)
  es <- lapply(stewart_ensemble_predictions, mark)
  ns <- lapply(zimmerman_ensemble_predictions, mark)

  venns <- lapply(1:length(gs), function(i){

    x  <- list(
      B = as.character(bs[[i]][[1]]),
      E = as.character(es[[i]][[1]]),
      N = as.character(ns[[i]][[1]]),
      G = as.character(gs[[i]][[1]]))

    colorGroups <- as.character(palette)
    colfunc <- colorRampPalette(colorGroups)
    col <- colfunc(15)

    d <- process_data(Venn(x))
    percentage <- paste0(round(d@region$count/sum(d@region$count)*100, digits = 0),"%")
    percentage_num <- round(d@region$count/sum(d@region$count)*100, digits = 0)

    ggplot() +
      geom_sf(aes(fill = percentage_num), data = venn_region(d), show.legend = F) +
      geom_sf(aes(color = name), data = venn_setedge(d)) +
      geom_sf_text(aes(label = percentage), data = venn_region(d)) +
      scale_fill_gradient(low = "white", high = "red", name = "Frequency") +
      theme_void() +
      scale_color_manual(name = "Approach",breaks = c("B",
                                                      "E",
                                                      "N",
                                                      "G"),
                         values = c("B" = as.character(palette["B"]),
                                                       "E" = as.character(palette["E"]),
                                                       "N" = as.character(palette["N"]),
                                                       "G" = as.character(palette["G"]))) +
      labs(title = display_species[i]) +
      theme(plot.title = element_text(face = "italic"),
            legend.title = element_blank())

  })

  venn <- ggarrange(plotlist = venns, ncol = 5, nrow = 1, common.legend = T)

  return(ggplotGrob(venn))
}

performance_comparison_boxplot_2 <- function(metrics, variable, display_species, palette,
                                           alt = "less",
                                           incl = c("G2", "G"),
                                           names = c("G2", "G")){
  data <- metrics %>%
    mutate(id = factor(paste0(rep))) %>%
    rename(var = all_of(variable))
  all_species <- data %>% mutate(rep = paste0(rep, species),
                                 species = "All species")


  test_data <- rbind(data %>% filter(species %in% display_species), all_species) %>%
    mutate(model = ordered(name_models(model), levels = incl),
           approach = ordered(substr(model,1,1), levels = c("B", "E", "N", "G")),
           species = ordered(species, levels = c(unique(data$species), "All species"))) %>%
    arrange(species, rep, model) %>%
    filter(model %in% incl)

  test_results <- test_data %>%
    group_by(species) %>%
    mutate(fr_p = friedmanTest(var, groups = model, blocks = rep, alternative = alt)$p.value,
           ## fr_ny_p = rep(c(NA, frdManyOneNemenyiTest(var, groups = model, blocks = rep, alternative = alt)$p.value), length(unique(rep))),
           n = length(unique(rep))) %>%
    group_by(species, model) %>%
    summarise(fr_p = first(fr_p),
              ## fr_ny_p = first(fr_ny_p),
              n = first(n)) %>%
    mutate(label_fr = paste0("Friedman, ", ifelse(fr_p < 0.05, "p < 0.05", paste0("p = ", round(fr_p, digits = 2))), ", n = ", n))

  plot_data <- test_data %>%
    mutate(model = case_when(model == incl[1] ~ names[1],
                             model == incl[2] ~ names[2],
                             T ~ model)) %>%
    mutate(model = ordered(model, levels = c("B", "E10", "E15", "E20", "E25", "N", "G2", "G", "B-Bioclim", "G-Bioclim")))

  ggpaired(data = plot_data, x = "model", y = "var", id = "rep", facet.by = "species", col = "approach",
           palette = palette,
           xlab = "Model", ylab = variable,
           line.size = 0.05, line.color = "grey",
           ylim = c(min(plot_data$var) - 0.04, max(plot_data$var) + 0.02)#,
           ) +
    geom_text(data = test_results %>% group_by(species) %>% summarise(label_fr = first(label_fr)), aes(label = label_fr, x = 1.5, y = min(plot_data$var) - 0.025)) +
    ## geom_text(data = test_results, aes(label = label_fr_ny, y = max(plot_data$var) + 0.025), size = 8) +
    theme_pubr(legend = "none") +
    theme(strip.text = element_text(face = "italic"))
}

get_cor_plot <- function(sdm_file){

  r <- rast(sdm_file)
  plots <- lapply(c("tmaxhm", "tmincm", "pptqdq"), function(var){

    cor <- layerCor(subset(r, grepl(var, names(r))),
                    "pearson", na.rm = T, maxcell = 100000)$pearson

    nms <- c("Mean",
             "Std. Dev",
             "10 year anomaly",
             "15 year anomaly",
             "20 year anomaly",
             "25 year anomaly",
             "Location",
             "Scale",
             "Shape")

    colnames(cor) <- nms
    rownames(cor) <- nms
    ggcorrplot(cor, method = "circle",
               title = case_when(var == "tmaxhm" ~ "Max. temperature hottest month",
                                 var == "tmincm" ~ "Min. temperature coldest month",
                                 var == "pptqdq" ~ "Precipitation driest quarter"))

  })

  ggarrange(plotlist = plots, ncol = 2, nrow = 2, common.legend = T)
}
