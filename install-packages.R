packages <- c("targets","tarchetypes", "conflicted",
              "terra", "tidyverse", "dismo", "extRemes",
              "digest", "fitdistrplus", "devtools",
              "future", "future.callr", "clustermq",
              "parallel", "RcolorBrewer", "gbm",
              "tidyterra", "ggpubr", "PMCMRplus",
              "xtable", "ggVennDiagram")

lapply(packages, function(somepackage){
if(!require(somepackage, character.only = T)){install.packages(somepackage)}
})

devtools::install_github("sjevelazco/flexsdm@HEAD")
