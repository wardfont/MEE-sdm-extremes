# Incorporating climatic extremes in species distribution models using the generalised extreme value distribution: Code and Data
---

This dataset contains necessary data and code to reproduce the results presented in the research article. The data is either included directly or can be downloaded using the provided code. The code contains a reproducible workflow based on the `targets` R package. More info on the package and its syntax can be found [here](https://books.ropensci.org/targets/).

The `targets` workflow assumes the following file structure and might throw errors if some of these locations do not exist on your system:

```bash
..
.
├── data
│   ├── euforest
│   │   └── EUForestspecies.csv
│   ├── species_properties
│   │   └── data.csv
│   └── terraclimate
│       └── historical
│           ├── extent
│           └── tmp
├── install-packages.R
├── output
│   ├── summary
│   └── terraclimate
│       └── historical
│           ├── deriv
│           ├── katz
│           ├── sdm
│           └── stewart
├── packages.R
├── plot
├── R
│   ├── biovar-terraclimate.R
│   ├── deriv_terraclimate.R
│   ├── download_terraclimate.R
│   ├── gam_ensemble.R
│   ├── gam_euforest.R
│   ├── katz_extremes.R
│   ├── katz_terraclimate.R
│   ├── metrics_gam.R
│   ├── model_biovars_euforest.R
│   ├── model_euforest.R
│   ├── normal_gev_aic.R
│   ├── normal_terraclimate.R
│   ├── occ_euforest.R
│   ├── partition_euforest.R
│   ├── plots.R
│   ├── rep_euforest.R
│   ├── sdm_terraclimate.R
│   ├── select-species-euforest.R
│   ├── species_characteristics.R
│   ├── species_records_summary.R
│   ├── stewart_terraclimate.R
│   └── TRY-filter.R
├── README.md
├── _targets
└── _targets.R
```

The workflow can be visualised by running the following R code in the root directory:

```R
library(targets)
tar_visnetwork()
```

and can be run using the following:

```R
library(targets)
tar_make()
```

Running the complete workflow will take considerable time and requires at least 30GB of free disk space.

All analysis is contained within the `_targets.R` file and the R-files in the `./R` folder.

Data used in this analysis comes from the following sources:
  * [EU-Forest dataset](https://www.nature.com/articles/sdata2016123)
  * [Terracliamte dataset](https://www.climatologylab.org/terraclimate.html)

