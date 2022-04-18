library(tidyverse)
library(rethinking)

d <- read_csv("data.csv")

#########################################################
# Prep data for Stan
N <- nrow(d)
N_studies <- length(unique(d$study))
N_outcomes <- length(unique(d$outcome))
N_id <- length(unique(d$id))

########################################################
#### Index whether child returns are summary stats #######
child_summary_returns <- ifelse( is.na(d$raw_sd), 0, 1)
child_summary_returns <- sapply( 1:length(child_summary_returns),
                                function(n) child_summary_returns[n]*sum(child_summary_returns[1:n]))

#########################################################
#### set sex = 0.5 for missing
d$sex <- ifelse(is.na(d$sex), 0.5, d$sex)

########################################################
# additional indices
study <- match(d$study, unique(d$study))

id <- coerce_index(d$id)

id <- ifelse(is.na(id), -99, id) # flag NA's as -99 for Stan

d$outcome_id <- match(d$outcome, unique(d$outcome))

d$resource_id <- match(d$resource_cat, unique(d$resource_cat))

# Organize age data and divide by max age (20)
age <- ifelse(is.na(d$age), (d$age_lower + d$age_upper)/2, d$age) / 20

# If age given as sd
age_sd <- ifelse(is.na(d$age_sd), -99, d$age_sd / 20 )

# If age given as intervals
age_range <- (d$age_upper - d$age_lower) / 20
age_sd <- ifelse(!is.na(age_range), age_range/2, age_sd)

# Index unique outcomes with unknown variance
d$outcome_var <- coerce_index( ifelse(is.na(d$raw_sd), d$outcome, NA) )
d$outcome_var <- ifelse( is.na(d$outcome_var), -99, d$outcome_var)

se_child <- ifelse(is.na(d$scaled_se), -99, d$scaled_se)

# Organize into a list for Stan
data_list <- list(
  N = N,
  N_studies = N_studies,
  N_outcomes = N_outcomes,
  N_resource = max(d$resource_id),
  N_id = N_id,
  outcome = d$outcome_id,
  resource = d$resource_id,
  outcome_var = d$outcome_var,
  study = study,
  id = id,
  age = age,
  age_sd = age_sd,
  returns = d$scaled_return,
  se_child = se_child,
  sex = d$sex,
  child_summary_returns = child_summary_returns
)

### compile model 
stan_model <- stan_model("stan_models/model_cfr.stan")

### run MCMC program
fit <- sampling( stan_model, data=data_list, chains=8, cores=8, iter=6000, warmup=500, init="0", control=list(adapt_delta=0.99) )

### save fit model for use with other scripts
saveRDS(fit, "fit_cfr.rds")

