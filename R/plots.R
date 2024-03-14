create_palette <- function() {
  source_palette <- palette.colors(palette = "Okabe-Ito")
  palette <- c(
    "B" = source_palette[8],
    "E" = source_palette[2],
    "N" = source_palette[3],
    "G" = source_palette[4]
  )
  names(palette) <- c("B", "E", "N", "G")

  return(palette)
}

name_models <- function(x) {
  model_names <- ordered(
    case_when(
      x == "base" ~ "B",
      x == "zimmerman" ~ "N",
      x == "stewart10" ~ "E10",
      x == "stewart15" ~ "E15",
      x == "stewart20" ~ "E20",
      x == "stewart25" ~ "E25",
      x == "katz" ~ "G",
      x == "katz2" ~ "G2"
    ),
    levels = c("B", "E10", "E15", "E20", "E25", "N", "G2", "G")
  )
}

performance_comparison_table <- function(metrics, display_species, palette,
                                         alt = "less",
                                         incl = c("B", "E10", "E15", "E20", "E25", "N", "G2", "G")) {
  data <- metrics %>%
    mutate(id = factor(paste0(rep)))
  all_species <- data %>% mutate(
    rep = paste0(rep, species),
    species = "All species, n = 28"
  )

  tables <- lapply(c("AUC", "TSS"), function(var) {
    test_data <- rbind(data %>% filter(species %in% display_species), all_species) %>%
      mutate(
        model = ordered(name_models(model), levels = c("G", "B", "E10", "E15", "E20", "E25", "N", "G2")),
        approach = ordered(substr(model, 1, 1), levels = c("B", "E", "N", "G")),
        species = ordered(species, levels = c(unique(data$species), "All species, n = 28"))
      ) %>%
      arrange(species, rep, model) %>%
      filter(model %in% incl) %>%
      pivot_wider(
        id_cols = c("rep", "species"),
        names_from = "model", values_from = contains(var)
      ) %>%
      mutate_at(vars(-matches("rep"), -matches("species")), list(dif = ~ . - G)) %>% # , perdif = ~ (. - G)/.)) %>%
      select(-c("G", "B", "E10", "E15", "E20", "E25", "N", "G2")) %>%
      group_by(species) %>%
      summarise_at(vars(-matches("rep"), -matches("species")), list(med = ~ median(.))) %>%
      select(-G_dif_med) # , -G_perdif_med)

    colnames(test_data) <- c(
      "Species", "B - G", "E10 - G", "E15 - G", "E20 - G",
      "E25 - G", "N - G", "G2 - G"
    )
    return(test_data)
  })

  return(tables)
}

performance_comparison_boxplot <- function(metrics, variable, display_species, palette,
                                           alt = "less",
                                           incl = c("B", "E10", "E15", "E20", "E25", "N", "G")) {
  if (anyNA(metrics)) {
    nas <- metrics %>%
      filter(is.na(AIC)) %>%
      mutate(rep = paste0(rep, species))

    data <- tibble(metrics) %>%
      mutate(id = factor(paste0(rep))) %>%
      dplyr::rename(var = all_of(variable))
    all_species <- data %>%
      mutate(
        rep = paste0(rep, species),
        species = "All species, n = 28"
      ) %>%
      filter(rep != nas$rep)
  } else {
    data <- tibble(metrics) %>%
      mutate(id = factor(paste0(rep))) %>%
      dplyr::rename(var = all_of(variable))
    all_species <- data %>%
      mutate(
        rep = paste0(rep, species),
        species = "All species, n = 28"
      )
  }


  test_data <- rbind(data %>% filter(species %in% display_species), all_species) %>%
    mutate(
      model = ordered(name_models(model), levels = c("G", "B", "E10", "E15", "E20", "E25", "N")),
      approach = ordered(substr(model, 1, 1), levels = c("B", "E", "N", "G")),
      species = ordered(species, levels = c(unique(data$species), "All species, n = 28"))
    ) %>%
    arrange(species, rep, model) %>%
    filter(model %in% incl)

  test_results <- test_data %>%
    group_by(species) %>%
    mutate(
      fr_p = friedmanTest(var, groups = model, blocks = rep, alternative = alt)$p.value,
      fr_ny_p = rep(c(NA, frdManyOneNemenyiTest(var, groups = model, blocks = rep, alternative = alt)$p.value), length(unique(rep))),
      n = length(unique(rep))
    ) %>%
    group_by(species, model) %>%
    summarise(
      fr_p = first(fr_p),
      fr_ny_p = first(fr_ny_p),
      n = first(n)
    ) %>%
    mutate(
      label_fr = paste0("Friedman, ", ifelse(fr_p < 0.05, "p < 0.05", paste0("p = ", round(fr_p, digits = 2))), ", n = ", n),
      label_fr_ny = paste0(ifelse(replace_na(fr_ny_p, 1) < 0.05, "\u2217", ""))
    ) %>%
    mutate(approach = ordered(substr(model, 1, 1), levels = c("B", "E", "N", "G")))

  plot_data <- test_data %>%
    mutate(model = ordered(model, levels = c("B", "E10", "E15", "E20", "E25", "N", "G")))

  ## ggpaired(
  ##   data = plot_data, x = "model", y = "var", id = "rep", facet.by = "species", col = "approach",
  ##   palette = palette,
  ##   xlab = "Model", ylab = variable,
  ##   line.size = 0.05, line.color = "grey",
  ##   ylim = c(min(plot_data$var) - 0.04, max(plot_data$var) + 0.02) # ,
  ## ) +
  ##   geom_text(data = test_results %>% group_by(species) %>% summarise(label_fr = first(label_fr)), aes(label = label_fr, x = 4, y = min(plot_data$var) - 0.025)) +
  ##   geom_text(data = test_results, aes(label = label_fr_ny, y = max(plot_data$var) + 0.025), size = 8) +
  ##   theme_pubr(legend = "none") +
  ##   theme(strip.text = element_text(face = "italic"))

  labelposition <- plot_data %>%
    group_by(species) %>%
    summarise(var_min = min(var))

  starposition <- plot_data %>%
    group_by(species, model) %>%
    summarise(var_max = max(var)) %>%
    mutate(model = ordered(model, levels = c("G", "B", "E10", "E15", "E20", "E25", "N")))

  ggplot(
    data = plot_data,
    aes(x = model, y = var, col = approach)
  ) +
    geom_boxplot() +
    geom_text(
      data = test_results %>%
        group_by(species) %>%
        summarise(label_fr = first(label_fr)) %>%
        left_join(labelposition, by = c("species")),
      aes(label = label_fr, x = 4, y = var_min), vjust = 1.5, col = "black"
    ) +
    geom_text(
      data = test_results %>%
        left_join(starposition, by = c("species", "model")),
      aes(label = label_fr_ny, y = var_max), vjust = -0.25, col = "black",
      size = 8
    ) +
    scale_color_manual(values = palette) +
    facet_wrap(~species, scales = "free") +
    scale_y_continuous(expand = expansion(mult = 0.3)) +
    theme_pubr(legend = "none") +
    theme(strip.text = element_text(face = "italic")) +
    labs(y = variable)
}

performance_difference_to_B_comparison_boxplot <- function(metrics, palette, species_properties) {
  variables <- c("AIC", "AUC")

  best_E <- metrics %>%
    tibble() %>%
    mutate(model = name_models(model)) %>%
    filter(model %in% c("E10", "E15", "E20", "E25")) %>%
    group_by(model, species) %>%
    summarise(mean_AUC = mean(AUC, na.rm = T)) %>%
    group_by(species) %>%
    filter(mean_AUC == max(mean_AUC, na.rm = T)) %>%
    mutate(comb = paste0(species, model))

  datas <- lapply(variables, function(variable) {
    data <- metrics %>%
      tibble() %>%
      mutate(id = factor(paste0(rep))) %>%
      rename(var = all_of(variable)) %>%
      mutate(model = name_models(model)) %>%
      filter(model %in% c("G", "N", "B") | paste0(species, model) %in% best_E$comb) %>%
      select(rep, species, var, id, model) %>%
      mutate(model = case_when(
        model %in% c("G", "N", "B") ~ model,
        T ~ "E"
      )) %>%
      pivot_wider(names_from = model, values_from = var) %>%
      mutate(
        diff_NB = N - B,
        diff_GB = G - B,
        diff_EB = E - B,
        diff_BG = B - G,
        diff_NG = N - G,
        diff_EG = E - G
      ) %>% # /get(comparison[2]))
      mutate(metric = variable)
  })

  summary_table <-
    rbind(
      datas[[1]] %>%
        mutate(metric = "AIC"),
      datas[[2]] %>%
        mutate(metric = "AUC")
    ) %>%
    group_by(species, metric) %>%
    summarise(
      B = median(B),
      G = median(G),
      E = median(E),
      N = median(N),
      diff_NB = median(diff_NB),
      diff_GB = median(diff_GB),
      diff_EB = median(diff_EB),
      diff_BG = median(diff_BG),
      diff_NG = median(diff_NG),
      diff_EG = median(diff_EG)
    )

  write.csv(summary_table, "./output/summary/summary-table.csv")

  tests <- lapply(variables, function(variable) {
    test <- metrics %>%
      tibble() %>%
      mutate(id = factor(paste0(rep))) %>%
      rename(var = all_of(variable)) %>%
      mutate(model = name_models(model)) %>%
      filter(model %in% c("G", "N", "B") | paste0(species, model) %in% best_E$comb) %>%
      select(rep, species, var, id, model) %>%
      mutate(model = case_when(
        model %in% c("G", "N", "B") ~ model,
        T ~ "E"
      )) %>%
      group_by(species) %>%
      mutate(model = ordered(model, levels = c("B", "G", "N", "E"))) %>%
      arrange(species, rep, model) %>%
      mutate(
        fr_p_diff_B = friedmanTest(var,
          groups = model, blocks = rep,
          alternative = "two.sided"
        )$p.value,
        fr_ny_p_diff_B = rep(c(
          NA,
          frdManyOneNemenyiTest(var,
            groups = model,
            blocks = rep,
            alternative = "two.sided"
          )$p.value
        ), length(unique(rep))),
        n = length(unique(rep))
      ) %>%
      mutate(model = ordered(model, levels = c("G", "B", "E", "N"))) %>%
      arrange(species, rep, model) %>%
      mutate(
        fr_p_diff_G = friedmanTest(var,
          groups = model, blocks = rep,
          alternative = "two.sided"
        )$p.value,
        fr_ny_p_diff_G = rep(c(
          NA,
          frdManyOneNemenyiTest(var,
            groups = model,
            blocks = rep,
            alternative = "two.sided"
          )$p.value
        ), length(unique(rep))),
        n = length(unique(rep))
      ) %>%
      select(-rep, -var, -id) %>%
      distinct() %>%
      mutate(
        sign_B = fr_p_diff_B <= 0.05 & fr_ny_p_diff_B <= 0.05,
        sign_G = fr_p_diff_G <= 0.05 & fr_ny_p_diff_G <= 0.05
      ) %>%
      select(species, model, sign_B, sign_G) %>%
      rename(comparison = model) %>%
      mutate(metric = variable)
  })

  species_order <- datas[[2]] %>%
    group_by(species) %>%
    summarise(median = median(diff_GB)) %>%
    left_join(species_properties, by = "species") %>%
    arrange(Leaf.type, -median) %>%
    pull(species)

  plot_data <- do.call(rbind, datas) %>%
    left_join(species_properties, by = "species") %>%
    mutate(Leaf.type = case_when(
      Leaf.type == "B" ~ "Broadleaf",
      Leaf.type == "N" ~ "Needle"
    )) %>%
    pivot_longer(c("diff_NB", "diff_GB", "diff_EB"),
      names_to = "comparison",
      values_to = "diff"
    ) %>%
    mutate(comparison = case_when(
      comparison == "diff_NB" ~ "N",
      comparison == "diff_GB" ~ "G",
      comparison == "diff_EB" ~ "E"
    )) %>%
    select(species, Leaf.type, diff, comparison, metric) %>%
    left_join(do.call(rbind, tests) %>% select(species, comparison, metric, sign_B),
      by = c("species", "comparison", "metric")
    ) %>%
    mutate(metric = factor(metric,
      levels = c("AIC", "AUC"),
      labels = c(
        expression(Delta * " AIC"),
        expression(Delta * " AUC")
      )
    )) %>%
    mutate(
      comparison = ordered(comparison,
        levels = c("G", "N", "E")
      ),
      sign_B = factor(sign_B, levels = c(TRUE, FALSE))
    )

  plot <- ggplot(
    data = plot_data %>%
      mutate(species = ordered(species, levels = species_order)),
    aes(x = interaction(species, Leaf.type), y = diff, col = comparison, fill = sign_B)
  ) +
    geom_hline(yintercept = 0) +
    geom_boxplot() +
    labs(x = NULL) +
    theme_pubr() +
    facet_wrap(~metric,
      labeller = label_parsed,
      strip.position = "left", scales = "free_y", ncol = 1
    ) +
    ylab(NULL) +
    theme(
      strip.background = element_blank(),
      strip.placement = "outside",
      strip.text = element_text(face = "bold", size = 12)
    ) +
    scale_color_manual(
      values = palette,
      labels = c("G-B", "N-B", "E-B")
    ) +
    scale_fill_manual(
      values = c("grey75", "white"),
      labels = c("Sign.", "Not sign."),
      drop = T
    ) +
    labs(col = NULL, fill = NULL) +
    scale_x_discrete(guide = "axis_nested") +
    theme(
      axis.text.x = element_text(face = "italic"),
      panel.background = element_rect(colour = "black", size = 0.25)
    ) +
    rotate_x_text(angle = 90)

  return(list(
    plot = plot,
    summary_table = summary_table
  ))
}

performance_difference_to_G_comparison_boxplot <- function(metrics, palette, species_properties) {
  variables <- c("AIC", "AUC")

  best_E <- metrics %>%
    tibble() %>%
    mutate(model = name_models(model)) %>%
    filter(model %in% c("E10", "E15", "E20", "E25")) %>%
    group_by(model, species) %>%
    summarise(mean_AUC = mean(AUC, na.rm = T)) %>%
    group_by(species) %>%
    filter(mean_AUC == max(mean_AUC, na.rm = T)) %>%
    mutate(comb = paste0(species, model))

  datas <- lapply(variables, function(variable) {
    data <- metrics %>%
      tibble() %>%
      mutate(id = factor(paste0(rep))) %>%
      rename(var = all_of(variable)) %>%
      mutate(model = name_models(model)) %>%
      filter(model %in% c("G", "N", "B") | paste0(species, model) %in% best_E$comb) %>%
      select(rep, species, var, id, model) %>%
      mutate(model = case_when(
        model %in% c("G", "N", "B") ~ model,
        T ~ "E"
      )) %>%
      pivot_wider(names_from = model, values_from = var) %>%
      mutate(
        diff_BG = B - G,
        diff_NG = N - G,
        diff_EG = E - G
      ) %>% # /get(comparison[2]))
      mutate(metric = variable)
  })

  tests <- lapply(variables, function(variable) {
    test <- metrics %>%
      tibble() %>%
      mutate(id = factor(paste0(rep))) %>%
      rename(var = all_of(variable)) %>%
      mutate(model = name_models(model)) %>%
      filter(model %in% c("G", "N", "B") | paste0(species, model) %in% best_E$comb) %>%
      select(rep, species, var, id, model) %>%
      mutate(model = case_when(
        model %in% c("G", "N", "B") ~ model,
        T ~ "E"
      )) %>%
      group_by(species) %>%
      mutate(model = ordered(model, levels = c("B", "G", "N", "E"))) %>%
      arrange(species, rep, model) %>%
      mutate(
        fr_p_diff_B = friedmanTest(var,
          groups = model, blocks = rep,
          alternative = "two.sided"
        )$p.value,
        fr_ny_p_diff_B = rep(c(
          NA,
          frdManyOneNemenyiTest(var,
            groups = model,
            blocks = rep,
            alternative = "two.sided"
          )$p.value
        ), length(unique(rep))),
        n = length(unique(rep))
      ) %>%
      mutate(model = ordered(model, levels = c("G", "B", "E", "N"))) %>%
      arrange(species, rep, model) %>%
      mutate(
        fr_p_diff_G = friedmanTest(var,
          groups = model, blocks = rep,
          alternative = "two.sided"
        )$p.value,
        fr_ny_p_diff_G = rep(c(
          NA,
          frdManyOneNemenyiTest(var,
            groups = model,
            blocks = rep,
            alternative = "two.sided"
          )$p.value
        ), length(unique(rep))),
        n = length(unique(rep))
      ) %>%
      select(-rep, -var, -id) %>%
      distinct() %>%
      mutate(
        sign_B = fr_p_diff_B <= 0.05 & fr_ny_p_diff_B <= 0.05,
        sign_G = fr_p_diff_G <= 0.05 & fr_ny_p_diff_G <= 0.05
      ) %>%
      select(species, model, sign_B, sign_G) %>%
      rename(comparison = model) %>%
      mutate(metric = variable)
  })

  species_order <- datas[[2]] %>%
    group_by(species) %>%
    summarise(median = median(diff_BG)) %>%
    left_join(species_properties, by = "species") %>%
    arrange(Leaf.type, median) %>%
    pull(species)

  plot_data <- do.call(rbind, datas) %>%
    left_join(species_properties, by = "species") %>%
    mutate(Leaf.type = case_when(
      Leaf.type == "B" ~ "Broadleaf",
      Leaf.type == "N" ~ "Needle"
    )) %>%
    pivot_longer(c("diff_BG", "diff_NG", "diff_EG"),
      names_to = "comparison",
      values_to = "diff"
    ) %>%
    mutate(
      comparison =
        case_when(
          comparison == "diff_BG" ~ "B",
          comparison == "diff_NG" ~ "N",
          comparison == "diff_EG" ~ "E"
        )
    ) %>%
    select(species, Leaf.type, diff, comparison, metric) %>%
    left_join(do.call(rbind, tests) %>% select(species, comparison, metric, sign_G),
      by = c("species", "comparison", "metric")
    ) %>%
    mutate(metric = factor(metric,
      levels = c("AIC", "AUC"),
      labels = c(
        expression(Delta * " AIC"),
        expression(Delta * " AUC")
      )
    )) %>%
    mutate(
      comparison = ordered(comparison,
        levels = c("B", "E", "N")
      ),
      sign_G = factor(sign_G, levels = c(TRUE, FALSE))
    )

  plot <- ggplot(
    data = plot_data %>%
      mutate(species = ordered(species, levels = species_order)),
    aes(x = interaction(species, Leaf.type), y = diff, col = comparison, fill = sign_G)
  ) +
    geom_hline(yintercept = 0) +
    geom_boxplot() +
    labs(x = NULL) +
    theme_pubr() +
    facet_wrap(~metric,
      labeller = label_parsed,
      strip.position = "left", scales = "free_y", ncol = 1
    ) +
    ylab(NULL) +
    theme(
      strip.background = element_blank(),
      strip.placement = "outside",
      strip.text = element_text(face = "bold", size = 12)
    ) +
    scale_color_manual(
      values = palette,
      labels = c("B-G", "E-G", "N-G")
    ) +
    scale_fill_manual(
      values = c("grey75", "white"),
      labels = c("Sign.", "Not sign."),
      drop = T
    ) +
    labs(col = NULL, fill = NULL) +
    scale_x_discrete(guide = "axis_nested") +
    theme(
      axis.text.x = element_text(face = "italic"),
      panel.background = element_rect(colour = "black", size = 0.25)
    ) +
    rotate_x_text(angle = 90)

  return(list(
    plot = plot
  ))
}

spatial_plot_ref_vs_compare <- function(ref_predictions, comp_predictions,
                                        ref_name, comp_name,
                                        euforest, species, palette) {
  layer <- "max_sens_spec"

  layers <- lapply(1:length(ref_predictions), function(x) {
    ref_trunc <- mask(subset(rast(ref_predictions[[x]]), layer), vect(euforest[[x]]$ca))
    comp_trunc <- mask(subset(rast(comp_predictions[[x]]), layer), vect(euforest[[x]]$ca))
    europe <- subset(rast(ref_predictions[[x]]), layer)

    rast <- ref_trunc - comp_trunc
    rast[ref_trunc != 1 & comp_trunc != 1] <- 2
    rast[is.na(rast) & !is.na(europe)] <- -2
    names(rast) <- species[x]
    rast <- classify(rast, c(-2.1, -1.1, -0.9, 0.1, 1.1, 2.1))
  })

  labels <- data.frame(lyr = species, label = species)

  plot <- ggplot() +
    geom_spatraster(data = rast(layers)) +
    facet_wrap(~lyr, ncol = 8) +
    geom_text(
      data = labels, aes(x = -10, y = 68, label = label),
      size = 3.5, hjust = 0, vjust = 0, fontface = "italic"
    ) +
    scale_fill_manual(
      values = c("#EEEEEE", as.character(palette[comp_name]), "#FDF9B9", as.character(palette[ref_name]), "#CCCCCC"),
      labels = c("Outside calibration area", paste("Only", comp_name), "Both", paste("Only", ref_name), "Neither"), na.translate = F
    ) +
    theme_pubr() +
    theme(
      legend.title = element_blank(),
      axis.title = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      axis.line = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      strip.background = element_blank(),
      panel.border = element_blank(),
      strip.text.x = element_blank()
    )

  return(plot)
}

plot_pr_ab <- function(euforest, species, display_species, env_file) {
  eu <- rast(env_file)
  eu_background <- eu[[1]]
  eu_background[!is.na(eu_background)] <- 0
  levels(eu_background) <- "bg"

  is <- species %in% display_species
  euforest <- subset(euforest, is)


  plots <- lapply(euforest, function(sp_euforest) {
    pr <- vect(sp_euforest$data %>% filter(pr_ab == 1),
      geom = c("x", "y"), crs = "EPSG:4326"
    )
    ab <- vect(sp_euforest$data %>% filter(pr_ab == 0),
      geom = c("x", "y"), crs = "EPSG:4326"
    )

    plot <- ggplot() +
      geom_spatraster(data = eu_background, show.legend = F) +
      scale_fill_manual(values = c("light grey"), na.value = "transparent") +
      geom_spatvector(data = ab, size = 0.001, aes(col = "Absence")) +
      geom_spatvector(data = pr, size = 0.001, aes(col = "Presence")) +
      scale_colour_manual(values = c("black", "dark green")) +
      theme_pubr() +
      ggtitle(sp_euforest$species) +
      theme(
        legend.title = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_blank()
      ) +
      border() +
      guides(color = guide_legend(override.aes = list(size = 3)))
  })

  output_plot <- ggarrange(plotlist = plots, common.legend = T)
  return(ggplotGrob(output_plot))
}

spatial_plot_one <- function(katz_ensemble_predictions,
                             base_ensemble_predictions,
                             stewart_ensemble_predictions,
                             zimmerman_ensemble_predictions,
                             euforest, species, display_species, palette) {
  is <- species %in% display_species

  katz_vs_base <- spatial_plot_ref_vs_compare(katz_ensemble_predictions,
    base_ensemble_predictions,
    ref_name = "G", comp_name = "B",
    subset(euforest, is),
    display_species, palette
  )
  katz_vs_stewart <- spatial_plot_ref_vs_compare(katz_ensemble_predictions,
    stewart_ensemble_predictions,
    ref_name = "G", comp_name = "E",
    subset(euforest, is),
    display_species, palette
  )
  katz_vs_zimmerman <- spatial_plot_ref_vs_compare(katz_ensemble_predictions,
    zimmerman_ensemble_predictions,
    ref_name = "G", comp_name = "N",
    subset(euforest, is),
    display_species, palette
  )

  legend <- ggplot(data = data.frame(
    value = c(
      "Only B",
      "Only E",
      "Only N",
      "Only G",
      "Both",
      "Neither",
      "Outside calibration area"
    ),
    x = rep(1, 7)
  )) +
    geom_bar(aes(x = x, fill = value)) +
    scale_fill_manual(
      name = NULL,
      breaks = c(
        "Only B",
        "Only E",
        "Only N",
        "Only G",
        "Both",
        "Neither",
        "Outside calibration area"
      ),
      values = c(
        "Only B" = as.character(palette["B"]),
        "Only E" = as.character(palette["E"]),
        "Only N" = as.character(palette["N"]),
        "Only G" = as.character(palette["G"]),
        "Both" = "#FDF9B9",
        "Neither" = "#CCCCCC",
        "Outside calibration area" = "#EEEEEE"
      )
    ) +
    theme(legend.position = "top") +
    guides(fill = guide_legend(nrow = 1))

  one <- ggarrange(ggarrange(katz_vs_base + theme(legend.position = "none")),
    ggarrange(katz_vs_stewart + theme(legend.position = "none")),
    ggarrange(katz_vs_zimmerman + theme(legend.position = "none")),
    legend.grob = get_legend(legend),
    ncol = 1,
    labels = c("(a)", "(b)", "(c)")
  )
  one

  return(ggplotGrob(one))
}

spatial_plot_new_ensemble <- function(katz_ensemble_gam_biovars_predictions,
                                      base_ensemble_gam_biovars_predictions,
                                      stewart_ensemble_gam_biovars_predictions,
                                      zimmerman_ensemble_gam_biovars_predictions,
                                      euforest, species, display_species, palette,
                                      terraclimate_sdm,
                                      limit) {
  katz <- rast(lapply(
    1:length(display_species),
    function(i) {
      mask(
        rast(katz_ensemble_gam_biovars_predictions[[i]]),
        vect(euforest[[which(species == display_species[[i]])]]$ca)
      )
    }
  ))
  names(katz) <- display_species
  base <- rast(lapply(
    1:length(display_species),
    function(i) {
      mask(
        rast(base_ensemble_gam_biovars_predictions[[i]]),
        vect(euforest[[which(species == display_species[[i]])]]$ca)
      )
    }
  ))
  stewart <- rast(lapply(
    1:length(display_species),
    function(i) {
      mask(
        rast(stewart_ensemble_gam_biovars_predictions[[i]]),
        vect(euforest[[which(species == display_species[[i]])]]$ca)
      )
    }
  ))
  names(stewart) <- display_species
  zimmerman <- rast(lapply(
    1:length(display_species),
    function(i) {
      mask(
        rast(zimmerman_ensemble_gam_biovars_predictions[[i]]),
        vect(euforest[[which(species == display_species[[i]])]]$ca)
      )
    }
  ))
  names(zimmerman) <- display_species

  pal <- wes_palette("Zissou1") # 5 en 1
  low_ind <- 5
  high_ind <- 1
  ## pal <- palette_okabe_ito() # 6 en 3
  ## low_ind <- 6
  ## high_ind <- 3

  eu <- rast(terraclimate_sdm[[1]]$file)
  eu[!is.na(eu)] <- 1
  eu_pol <- as.polygons(eu)

  sp_limits <- c(
    0.2,
    0.2,
    0.2,
    0.3
  )

  plots_base <- lapply(1:length(display_species), function(i) {
    ca <- vect(euforest[[which(species == display_species[[i]])]]$ca)
    pol <- crop(ca, eu_pol)

    ggplot() +
      geom_spatraster(data = katz[[i]]) +
      scale_fill_gradient(
        low = "white",
        high = "grey25",
        na.value = NA
      ) +
      new_scale_fill() +
      geom_spatraster(data = katz[[i]] - base[[i]]) +
      scale_fill_gradient2(
        low = pal[low_ind],
        mid = NA,
        high = pal[high_ind],
        midpoint = 0,
        limits = c(-1, 1) * sp_limits[i],
        na.value = NA
      ) +
      geom_spatvector(data = pol, fill = NA) +
      labs(
        y = display_species[i],
        x = "GEV approach"
      ) +
      theme_pubr() +
      theme(
        axis.title.y = element_text(face = "bold.italic"),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_blank()
      )
  })

  heights <- sapply(1:length(display_species), function(i) {
    ca <- vect(euforest[[which(species == display_species[[i]])]]$ca)
    ext(ca)[4] - ext(ca)[3]
  })

  plots_stewart_B <- lapply(1:length(display_species), function(i) {
    ca <- vect(euforest[[which(species == display_species[[i]])]]$ca)
    pol <- crop(ca, eu_pol)

    ggplot() +
      geom_spatraster(data = stewart[[i]]) +
      scale_fill_gradient(
        low = "white",
        high = "grey25",
        na.value = NA
      ) +
      new_scale_fill() +
      geom_spatraster(data = stewart[[i]] - base[[i]]) +
      scale_fill_gradient2(
        low = pal[low_ind],
        mid = NA,
        high = pal[high_ind],
        midpoint = 0,
        limits = c(-1, 1) * sp_limits[i],
        na.value = NA
      ) +
      geom_spatvector(data = pol, fill = NA) +
      labs(x = "Empirical quantile appraoch") +
      theme_pubr() +
      theme(
        legend.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_blank()
      )
  })

  plots_zimmerman_B <- lapply(1:length(display_species), function(i) {
    ca <- vect(euforest[[which(species == display_species[[i]])]]$ca)
    pol <- crop(ca, eu_pol)

    ggplot() +
      geom_spatraster(data = zimmerman[[i]]) +
      scale_fill_gradient(
        low = "white",
        high = "grey25",
        na.value = NA
      ) +
      ## labs(y = "G - B") +
      new_scale_fill() +
      geom_spatraster(data = zimmerman[[i]] - base[[i]]) +
      scale_fill_gradient2(
        low = pal[low_ind],
        mid = NA,
        high = pal[high_ind],
        midpoint = 0,
        limits = c(-1, 1) * sp_limits[i],
        na.value = NA
      ) +
      labs(x = "Normal approach") +
      geom_spatvector(data = pol, fill = NA) +
      theme_pubr() +
      theme(
        legend.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_blank()
      )
  })

  leg <- get_legend(plots_base[[1]])

  one <- ggarrange(
    ggarrange(plotlist = plots_base, legend = "none", ncol = 1),
    ggarrange(plotlist = plots_stewart_B, legend = "none", ncol = 1),
    ggarrange(plotlist = plots_zimmerman_B, legend = "none", ncol = 1),
    ncol = 3,
    legend.grob = leg
  )

  ggsave("../sdm-extremes-paper/plot/one_spatial/final_one_zissou.svg", one,
    scale = 1, height = 297, width = 210, units = "mm"
  ) # A4

  return(ggplotGrob(one))
}



performance_comparison_boxplot_2 <- function(metrics, variable, display_species, palette,
                                             alt = "less",
                                             incl = c("G2", "G"),
                                             names = c("G2", "G")) {
  data <- metrics %>%
    mutate(id = factor(paste0(rep))) %>%
    rename(var = all_of(variable))
  all_species <- data %>% mutate(
    rep = paste0(rep, species),
    species = "All species"
  )

  test_data <- rbind(data %>% filter(species %in% display_species), all_species) %>%
    mutate(
      model = ordered(name_models(model), levels = incl),
      approach = ordered(substr(model, 1, 1), levels = c("B", "E", "N", "G")),
      species = ordered(species, levels = c(unique(data$species), "All species"))
    ) %>%
    arrange(species, rep, model) %>%
    filter(model %in% incl)

  test_results <- test_data %>%
    group_by(species) %>%
    mutate(
      fr_p = friedmanTest(var, groups = model, blocks = rep, alternative = alt)$p.value,
      ## fr_ny_p = rep(c(NA, frdManyOneNemenyiTest(var, groups = model, blocks = rep, alternative = alt)$p.value), length(unique(rep))),
      n = length(unique(rep))
    ) %>%
    group_by(species, model) %>%
    summarise(
      fr_p = first(fr_p),
      ## fr_ny_p = first(fr_ny_p),
      n = first(n)
    ) %>%
    mutate(label_fr = paste0("Friedman, ", ifelse(fr_p < 0.05, "p < 0.05", paste0("p = ", round(fr_p, digits = 2))), ", n = ", n))

  plot_data <- test_data %>%
    mutate(model = case_when(
      model == incl[1] ~ names[1],
      model == incl[2] ~ names[2],
      T ~ model
    )) %>%
    mutate(model = ordered(model, levels = c("B", "E10", "E15", "E20", "E25", "N", "G2", "G", "B-Bioclim", "G-Bioclim")))

  ggpaired(
    data = plot_data, x = "model", y = "var", id = "rep", facet.by = "species", col = "approach",
    palette = palette,
    xlab = "Model", ylab = variable,
    line.size = 0.05, line.color = "grey",
    ylim = c(min(plot_data$var) - 0.04, max(plot_data$var) + 0.02) # ,
  ) +
    geom_text(data = test_results %>% group_by(species) %>% summarise(label_fr = first(label_fr)), aes(label = label_fr, x = 1.5, y = min(plot_data$var) - 0.025)) +
    ## geom_text(data = test_results, aes(label = label_fr_ny, y = max(plot_data$var) + 0.025), size = 8) +
    theme_pubr(legend = "none") +
    theme(strip.text = element_text(face = "italic"))
}

get_cor_plot <- function(sdm_file) {
  r <- rast(sdm_file)
  plots <- lapply(c("tmaxhm", "tmincm", "pptqdq"), function(var) {
    cor <- layerCor(subset(r, grepl(var, names(r))),
      "pearson",
      na.rm = T, maxcell = 100000
    )$pearson

    nms <- c(
      "Mean",
      "Std. Dev",
      "10 year anomaly",
      "15 year anomaly",
      "20 year anomaly",
      "25 year anomaly",
      "Location",
      "Scale",
      "Shape"
    )

    colnames(cor) <- nms
    rownames(cor) <- nms
    ggcorrplot(cor,
      method = "circle",
      title = case_when(
        var == "tmaxhm" ~ "Max. temperature hottest month",
        var == "tmincm" ~ "Min. temperature coldest month",
        var == "pptqdq" ~ "Precipitation driest quarter"
      )
    )
  })

  ggarrange(plotlist = plots, ncol = 2, nrow = 2, common.legend = T)
}

per_pixel_delta_aic_plot <- function(terraclimate_delta_aic) {
  r <- rast(terraclimate_delta_aic)

  pal <- wes_palette("Zissou1")

  tmax <- ggplot() +
    geom_spatraster(data = r[[1]]) +
    theme_pubr() +
    scale_fill_gradient2(
      high = pal[1],
      mid = "grey90",
      low = pal[5],
      na.value = NA,
      limits = c(-1, 1) * as.numeric(global(abs(r[[1]]), fun = "max", na.rm = T)[1])
    ) +
    labs(title = "TxHm", fill = expression(Delta * "AIC")) +
    theme(
      legend.position = "bottom",
      plot.title = element_text(hjust = 0.5),
      axis.title = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      axis.line = element_blank()
    )

  tmin <- ggplot() +
    geom_spatraster(data = r[[2]]) +
    theme_pubr() +
    scale_fill_gradient2(
      high = pal[1],
      mid = "grey90",
      low = pal[5],
      na.value = NA,
      limits = c(-1, 1) * as.numeric(global(abs(r[[2]]), fun = "max", na.rm = T)[1])
    ) +
    labs(title = "TnCm", fill = expression(Delta * "AIC")) +
    theme(
      legend.position = "bottom",
      plot.title = element_text(hjust = 0.5),
      axis.title = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      axis.line = element_blank()
    )

  ppt <- ggplot() +
    geom_spatraster(data = r[[3]]) +
    theme_pubr() +
    scale_fill_gradient2(
      high = pal[1],
      mid = "grey90",
      low = pal[5],
      na.value = NA,
      limits = c(-1, 1) * as.numeric(global(abs(r[[3]]), fun = "max", na.rm = T)[1]),
      breaks = c(-150, 0, 150)
    ) +
    labs(title = "PDq", fill = expression(Delta * "AIC")) +
    theme(
      legend.position = "bottom",
      plot.title = element_text(hjust = 0.5),
      axis.title = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      axis.line = element_blank()
    )

  p_delta_aic <- ggarrange(
    tmax, tmin, ppt,
    ncol = 3,
    labels = "auto"
  )

  return(ggplotGrob(p_delta_aic))
}

performance_difference_all_species_boxplot <- function(metrics, palette, species_properties) {
  variables <- c("AIC", "AUC")

  best_E <- metrics %>%
    tibble() %>%
    mutate(model = name_models(model)) %>%
    filter(model %in% c("E10", "E15", "E20", "E25")) %>%
    group_by(model, species) %>%
    summarise(mean_AUC = mean(AUC, na.rm = T)) %>%
    group_by(species) %>%
    filter(mean_AUC == max(mean_AUC, na.rm = T)) %>%
    mutate(comb = paste0(species, model))

  datas <- lapply(variables, function(variable) {
    data <- metrics %>%
      tibble() %>%
      mutate(id = factor(paste0(rep, species))) %>%
      rename(var = all_of(variable)) %>%
      mutate(model = name_models(model)) %>%
      filter(model %in% c("G", "N", "B") | paste0(species, model) %in% best_E$comb) %>%
      select(rep, species, var, id, model) %>%
      mutate(model = case_when(
        model %in% c("G", "N", "B") ~ model,
        T ~ "E"
      )) %>%
      pivot_wider(names_from = model, values_from = var) %>%
      mutate(
        diff_NB = N - B,
        diff_GB = G - B,
        diff_EB = E - B,
        diff_BG = B - G,
        diff_NG = N - G,
        diff_EG = E - G
      ) %>% # /get(comparison[2]))
      mutate(metric = variable)
  })

  all_species_tests <- lapply(variables, function(variable) {
    test <- metrics %>%
      tibble() %>%
      mutate(id = factor(paste0(rep, species))) %>%
      rename(var = all_of(variable)) %>%
      mutate(model = name_models(model)) %>%
      filter(model %in% c("G", "N", "B") | paste0(species, model) %in% best_E$comb) %>%
      select(rep, species, var, id, model) %>%
      mutate(model = case_when(
        model %in% c("G", "N", "B") ~ model,
        T ~ "E"
      )) %>%
      select(-species) %>%
      mutate(model = ordered(model, levels = c("B", "G", "N", "E"))) %>%
      arrange(id, model) %>%
      mutate(
        fr_p_diff_B = friedmanTest(var,
          groups = model, blocks = id,
          alternative = "two.sided"
        )$p.value,
        fr_ny_p_diff_B = rep(c(
          NA,
          frdManyOneNemenyiTest(var,
            groups = model,
            blocks = id,
            alternative = "two.sided"
          )$p.value
        ), length(unique(id))),
        n = length(unique(id))
      ) %>%
      mutate(model = ordered(model, levels = c("G", "B", "E", "N"))) %>%
      arrange(id, model) %>%
      mutate(
        fr_p_diff_G = friedmanTest(var,
          groups = model, blocks = id,
          alternative = "two.sided"
        )$p.value,
        fr_ny_p_diff_G = rep(c(
          NA,
          frdManyOneNemenyiTest(var,
            groups = model,
            blocks = id,
            alternative = "two.sided"
          )$p.value
        ), length(unique(id))),
        n = length(unique(id))
      ) %>%
      select(-rep, -var, -id) %>%
      distinct() %>%
      mutate(
        sign_B = fr_p_diff_B <= 0.05 & fr_ny_p_diff_B <= 0.05,
        sign_G = fr_p_diff_G <= 0.05 & fr_ny_p_diff_G <= 0.05
      ) %>%
      select(model, sign_B, sign_G) %>%
      rename(comparison = model) %>%
      mutate(metric = variable)
  })

  B_plot_data <- do.call(rbind, datas) %>%
    pivot_longer(c("diff_NB", "diff_GB", "diff_EB"),
      names_to = "comparison",
      values_to = "diff"
    ) %>%
    mutate(comparison = case_when(
      comparison == "diff_NB" ~ "N",
      comparison == "diff_GB" ~ "G",
      comparison == "diff_EB" ~ "E"
    )) %>%
    select(species, diff, comparison, metric) %>%
    left_join(do.call(rbind, all_species_tests) %>% select(comparison, metric, sign_B),
      by = c("comparison", "metric")
    ) %>%
    mutate(metric = factor(metric,
      levels = c("AIC", "AUC"),
      labels = c(
        expression(Delta * " AIC"),
        expression(Delta * " AUC")
      )
    )) %>%
    mutate(
      comparison = ordered(comparison,
        levels = c("G", "N", "E")
      ),
      sign_B = factor(sign_B, levels = c(TRUE, FALSE))
    )

  B_plot <- ggplot(
    data = B_plot_data,
    aes(y = diff, col = comparison, fill = sign_B)
  ) +
    geom_hline(yintercept = 0) +
    geom_boxplot() +
    labs(x = NULL) +
    theme_pubr() +
    facet_wrap(~metric,
      labeller = label_parsed,
      strip.position = "left", scales = "free_y", ncol = 1
    ) +
    ylab(NULL) +
    theme(
      strip.background = element_blank(),
      strip.placement = "outside"
    ) +
    scale_color_manual(
      values = palette,
      labels = c("G-B", "N-B", "E-B")
    ) +
    scale_fill_manual(
      values = c("grey75", "white"),
      labels = c("Sign.", "Not sign."),
      drop = T
    ) +
    labs(col = NULL, fill = NULL) +
    scale_x_discrete(guide = "axis_nested") +
    theme(axis.text.x = element_text(face = "italic")) +
    rotate_x_text(angle = 90)


  G_plot_data <- do.call(rbind, datas) %>%
    pivot_longer(c("diff_BG", "diff_NG", "diff_EG"),
      names_to = "comparison",
      values_to = "diff"
    ) %>%
    mutate(
      comparison =
        case_when(
          comparison == "diff_BG" ~ "B",
          comparison == "diff_NG" ~ "N",
          comparison == "diff_EG" ~ "E"
        )
    ) %>%
    select(species, diff, comparison, metric) %>%
    left_join(do.call(rbind, all_species_tests) %>% select(comparison, metric, sign_G),
      by = c("comparison", "metric")
    ) %>%
    mutate(metric = factor(metric,
      levels = c("AIC", "AUC"),
      labels = c(
        expression(Delta * " AIC"),
        expression(Delta * " AUC")
      )
    )) %>%
    mutate(
      comparison = ordered(comparison,
        levels = c("B", "E", "N")
      ),
      sign_G = factor(sign_G, levels = c(TRUE, FALSE))
    )

  G_plot <- ggplot(
    data = G_plot_data,
    aes(y = diff, col = comparison, fill = sign_G)
  ) +
    geom_hline(yintercept = 0) +
    geom_boxplot() +
    labs(x = NULL) +
    theme_pubr() +
    facet_wrap(~metric,
      labeller = label_parsed,
      strip.position = "left", scales = "free_y", ncol = 1
    ) +
    ylab(NULL) +
    theme(
      strip.background = element_blank(),
      strip.placement = "outside"
    ) +
    scale_color_manual(
      values = palette,
      labels = c("B-G", "E-G", "N-G")
    ) +
    scale_fill_manual(
      values = c("grey75", "white"),
      labels = c("Sign.", "Not sign."),
      drop = T
    ) +
    labs(col = NULL, fill = NULL) +
    scale_x_discrete(guide = "axis_nested") +
    theme(axis.text.x = element_text(face = "italic")) +
    rotate_x_text(angle = 90)

  plot <- ggarrange(B_plot, G_plot)
}
