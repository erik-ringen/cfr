library(tidyverse)
library(rethinking)

# Get all extracted data files in the subdirectories
paper_data <- list.files(pattern = "^data_", recursive = TRUE)

# Combine them into a single dataframe
d <- paper_data %>% 
  purrr::map(read_csv) %>% 
  purrr::reduce(rbind)

# Prep data for Stan
N <- nrow(d)
N_studies <- length(unique(d$study))
N_outcomes <- length(unique(d$outcome))
N_id <- length(unique(d$id))

#### Index whether it is possible to estimate sex diff in linear model
d$male <- ifelse(d$sex == "male", 1, 0)

outcome_sex_diff <- d %>% group_by(outcome) %>% summarise(sex_diff = ifelse(sum(male)==0, 0, 1))

d$male <- ifelse(d$sex == "both", 2, d$male)

d <- left_join(d, outcome_sex_diff)

d$sex_diff <- coerce_index( ifelse( d$sex_diff == 1, d$outcome, NA ) )
d$sex_diff <- ifelse(is.na(d$sex_diff), -99, d$sex_diff)
########################################################
#### Index whether it is possible to estiamte individual diffs
outcome_id_diff <- d %>% group_by(outcome) %>% summarise(id_diff = ifelse(sum(is.na(id)) == n(), 0, 1) )

d <- left_join(d, outcome_id_diff)

d$id_diff <- coerce_index( ifelse( d$id_diff == 1, d$outcome, NA) )
d$id_diff <- ifelse( is.na(d$id_diff), -99, d$id_diff )
########################################################

# making additional indices
study <- coerce_index(d$study)
outcome <- coerce_index(d$outcome)
id <- coerce_index(d$id)
id <- ifelse(is.na(id), -99, id)

# Organize age data. flag NA's as -99 for Stan and divide by max age (20)
age <- ifelse(is.na(d$age), -99, d$age / 20)
age_lower <- ifelse(is.na(d$age_lower), -99, d$age_lower / 20)
age_upper <- ifelse(is.na(d$age_upper), -99, d$age_upper / 20)
age_sd <- ifelse(is.na(d$age_sd), -99, d$age_sd / 20 )

# when data are given as summaries, estimate log-normal parameters using method of moments
ln_MM <- function( mean, sd, stat="mu" ) {
  mu = log( (mean^2)/(sqrt(sd^2 + mean^2)))
  var = log( (sd^2)/(mean^2) + 1 )
  
  if (stat == "mu") return(mu)
  if (stat == "sd") return(sqrt(var))
}

returns <- d$raw_return /  d$adult_return # scaling by adult mean

lmu_child <- ifelse( is.na(d$raw_sd), -99, ln_MM(returns, d$raw_sd/d$adult_return, stat="mu") )
lsd_child <- ifelse( is.na(d$raw_sd), -99, ln_MM(returns, d$raw_sd/d$adult_return, stat="sd") )

lmu_adult <- ifelse( is.na(d$adult_sd), -99, ln_MM(d$adult_return, d$adult_sd, stat="mu") )
lsd_adult <- ifelse( is.na(d$adult_sd), -99, ln_MM(d$adult_return, d$adult_sd, stat="sd") )

# Index unique outcomes with unknown variance
d$outcome_var <- coerce_index( ifelse(is.na(d$raw_sd), d$outcome, NA) )
d$outcome_var <- ifelse( is.na(d$outcome_var), -99, d$outcome_var)

# Organize into a list for Stan
data_list <- list(
  N = N,
  N_studies = N_studies,
  N_outcomes = N_outcomes,
  N_id = N_id,
  outcome = outcome,
  outcome_var = d$outcome_var,
  study = study,
  id = id,
  age = age,
  age_lower = age_lower,
  age_upper = age_upper,
  age_sd = age_sd,
  returns = returns,
  lmu_child = lmu_child,
  lsd_child = lsd_child,
  lmu_adult = lmu_adult,
  lsd_adult = lsd_adult,
  mu_adult = d$adult_return,
  returns_error = returns_error,
  male = d$male,
  sex_diff = d$sex_diff,
  id_diff = d$id_diff
)

fit0 <- stan( file="stan_models/model_nl.stan", data=data_list, chains=4, cores=4, iter=2000, init="0", control=list(adapt_delta=0.95) )

post <- extract.samples(fit0)
