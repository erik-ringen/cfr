# cfr
This repo contains the data file, processing and analysis scripts behind "Foraging complexity and the evolution of childhood" (Pretelli, Ringen, and Lew-Levy, 2022).

## Data
`paper_data` and `paper_data_round2` folders contain the raw data (including tables and screenshots of plots) extracted from studies, as well as R scripts to clean those individual chunks of foraging data. `processing.R` wrangles all of the data from individual studies (as well as child foraging returns from [cchunts](https://github.com/rmcelreath/cchunts) and then produces the integrated dataset used in our analysis, simply called `data.csv`.

Column | Description
---------|-------------
study | id for source study; first author lastname_yearpublished
outcome | id for foraging return outcomes, nested within study
id | individual forager id, where applicable
sex | 0 = f, 1 = m. values between 0 and 1 indicate prop m
age | forager age, best estimate
age_error | type of measurement error on forager age
age_sd | std error of age
age_lower | lower bound of age
age_upper | upper bound of age
resource | type of food resource
units | reported quantity/rate (e.g., kg, kcal/h)
raw_return | child return quantity
raw_sd | child return std. dev (when given as summary stat)
raw_se | child return SE (when given as summary stat)
adult_return | matched adult mean return
adult_sd | adult return std. dev
adult_se | adult return SE

## Analysis
`model_fitting.R` fits the Stan program in the `stan_models` folder and then saves the fitted object for future use, so that it only needs to be run once. Results are visualized using scritps in the `plots` folder. `cfr_functions.R` contains convienience functions and code snippets to support working with the fitted stan model and making plots.

## Text
LaTeX code behind the manuscript and supplementary materials is stored in the `text` folder.
