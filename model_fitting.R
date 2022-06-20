library(tidyverse)
library(rethinking)

d <- read_csv("data.csv")

#########################################################
#remove duplicated datasets
d$resource <- str_replace(d$resource, "fruits", "fruit")
d <- filter(d, outcome != "BliegeBird_2002a_fig2")
d <- filter(d, outcome != "BliegeBird_2002a_fig5c")
d <- filter(d, outcome != "Froehle_2018_fig6b")
d <- filter(d, outcome != "Hawkes_1995_Table_6_tin_measured_rates_Cal.h")

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

#########################################################
#########################################################
# Run analysis with non-pooled returns only

d_np <- d %>% filter(pooled == 0)

#########################################################
# Prep data for Stan
N <- nrow(d_np)
N_studies <- length(unique(d_np$study))
N_outcomes <- length(unique(d_np$outcome))
N_id <- length(unique(d_np$id))

########################################################
#### Index whether child returns are summary stats #######
child_summary_returns <- ifelse( is.na(d_np$raw_sd), 0, 1)
child_summary_returns <- sapply( 1:length(child_summary_returns),
                                 function(n) child_summary_returns[n]*sum(child_summary_returns[1:n]))

#########################################################
#### set sex = 0.5 for missing
d_np$sex <- ifelse(is.na(d_np$sex), 0.5, d_np$sex)

########################################################
# additional indices
study <- match(d_np$study, unique(d_np$study))

id <- coerce_index(d_np$id)

id <- ifelse(is.na(id), -99, id) # flag NA's as -99 for Stan

d_np$outcome_id <- match(d_np$outcome, unique(d_np$outcome))

d_np$resource_id <- match(d_np$resource_cat, unique(d_np$resource_cat))

# Organize age data and divide by max age (20)
age <- ifelse(is.na(d_np$age), (d_np$age_lower + d_np$age_upper)/2, d_np$age) / 20

# If age given as sd
age_sd <- ifelse(is.na(d_np$age_sd), -99, d_np$age_sd / 20 )

# If age given as intervals
age_range <- (d_np$age_upper - d_np$age_lower) / 20
age_sd <- ifelse(!is.na(age_range), age_range/2, age_sd)

# Index unique outcomes with unknown variance
d_np$outcome_var <- coerce_index( ifelse(is.na(d_np$raw_sd), d_np$outcome, NA) )
d_np$outcome_var <- ifelse( is.na(d_np$outcome_var), -99, d_np$outcome_var)

se_child <- ifelse(is.na(d_np$scaled_se), -99, d_np$scaled_se)


data_list_np <- list(
  N = N,
  N_studies = N_studies,
  N_outcomes = N_outcomes,
  N_resource = max(d_np$resource_id),
  N_id = N_id,
  outcome = d_np$outcome_id,
  resource = d_np$resource_id,
  outcome_var = d_np$outcome_var,
  study = study,
  id = id,
  age = age,
  age_sd = age_sd,
  returns = d_np$scaled_return,
  se_child = se_child,
  sex = d_np$sex,
  child_summary_returns = child_summary_returns
)

### run MCMC program
fit_np <- sampling( stan_model, data=data_list_np, chains=10, cores=10, iter=1000, init="0", control=list(adapt_delta=0.98) )

### save fit model for use with other scripts
saveRDS(fit_np, "fit_np_cfr.rds")

                                
#### Checking mcmc diagnostics ########################
# write.csv(precis(fit, depth=3), "model_precis.csv")
# prec <- read_csv("model_precis.csv")

## Verify that all population parameters (not necessarily all random effects) have Rhat < 1.01, ESS > 1000.
# prec %>% filter(Rhat4 > 1.01)
# prec %>% filter(n_eff < 1000)
