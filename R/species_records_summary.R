species_records_summary <- function(euforest,
                                    euforest_partition) {
  summaries_rep <- lapply(euforest_partition, function(x) {
    species <- x$species
    pr_rep <- sum(x$data$pr_ab == 1)
    ab_rep <- sum(x$data$pr_ab == 0)
    sblock_rep <- max(x$data$sblock.part)
    blocksize_rep <- x$sblock_cell_size
    rep <- x$rep

    pr_ab_rep <- x$data %>%
      group_by(sblock.part) %>%
      summarise(
        pr = sum(pr_ab == 1),
        ab = sum(pr_ab == 0)
      ) %>%
      summarise(
        min_pr = min(pr),
        max_pr = max(pr),
        min_ab = min(ab),
        max_ab = max(ab)
      )


    summary <- tibble(
      species = species,
      pr_min_rep = pr_ab_rep$min_pr,
      pr_max_rep = pr_ab_rep$max_pr,
      ab_min_rep = pr_ab_rep$min_ab,
      ab_max_rep = pr_ab_rep$max_ab,
      sblock_rep = sblock_rep,
      rep = rep
    )
  })

  summary_rep <- do.call(rbind, summaries_rep) %>%
    group_by(species) %>%
    summarise(
      sblock_range = paste0(min(sblock_rep), "-", max(sblock_rep)),
      pr_range = paste0(min(pr_min_rep), "-", max(pr_max_rep)),
      ab_range = paste0(min(ab_min_rep), "-", max(ab_max_rep))
    )



  summaries_sp <- lapply(euforest, function(x) {
    summary <- tibble(
      species = x$species,
      presences = sum(x$data$pr_ab == 1),
      absences = sum(x$data$pr_ab == 0)
    )
  })

  summary_sp <- do.call(rbind, summaries_sp)

  summary <- summary_sp %>%
    left_join(summary_rep, by = "species") %>%
    rename(
      Species = species,
      Presences = presences,
      Absences = absences,
      "Range of number of CV partitions" = sblock_range,
      "Range of CV partition presences" = pr_range,
      "Range of CV partition absences" = ab_range
    )

  return(summary)
}
