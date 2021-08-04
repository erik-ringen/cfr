# cfr
This repo contains the data file, processing and analysis scripts behind "Child Foraging Returns: A Meta-Analysis"

## Data
`paper_data` and `paper_data_round2` folders contain the raw data (including tables and screenshots of plots) extracted from studies, as well as R scripts to clean those individual chunks of foraging data. `processing.R` wrangles all of the data from individual studies (as well as child foraging retursn from [cchunts](https://github.com/rmcelreath/cchunts) and then produces the integrated dataset used in our analysis, simply called `data.csv`.

## Analysis
`model_fitting.R` fits the Stan program in the `stan_models` folder and then saves the fitted object for future use, so that it only needs to be run once. Results are visualized using scritps in the `plots` folder. `cfr_functions.R` contains convienience functions and code snippets to support working with the fitted stan model and making plots.

## Text
LaTeX code behind the manuscript and supplementary materials is stored in the `text` folder.
