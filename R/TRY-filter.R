TRY_filter <- function() {
  library(rtry)
  library(tidyverse)

  ## Convert latin-1... encoding to utf-8 and remove all mutlibytes in terminal: iconv -f iso-8859-1 -t utf-8 31778.txt > 31778-utf8.txt

  data <- read.csv("./data/species_properties/31778_16022024002623/31778-utf8.txt", sep = "\t")

  # Look at all TraitsIDs/Names and corresponsing DAataIDs

  complete_DataIDs <- tibble(data) %>%
    group_by(AccSpeciesID, TraitID, TraitName, DataID, DataName) %>%
    summarise(mean = mean(as.numeric(OrigValueStr), na.rm = T)) %>%
    group_by(TraitID, TraitName, DataID, DataName) %>%
    summarise(n = n()) %>%
    filter(n == 28)

  write.csv(complete_DataIDs, "complete_DataIDs.csv")

  Final <- tibble(data) %>%
    filter(DataID %in% c(
      48, # Leaf type
      4013, # root type
      20 # Plant height maximum
    ))

  Height <- tibble(data) %>%
    filter(DataID %in% c(20)) %>%
    mutate(height = case_when(
      OrigUnitStr == "m" ~ as.numeric(OrigValueStr),
      OrigUnitStr == "cm" ~ as.numeric(OrigValueStr) / 100,
      T ~ NA
    )) %>%
    group_by(AccSpeciesName, DataID) %>%
    summarise(
      median_height = median(height, na.rm = T),
      max_height = max(height, na.rm = T),
      q95_height = quantile(height, probs = 0.95, na.rm = T)
    )



  write.csv(Height, "Heigh.csv")
}
