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

# making indices
study <- coerce_index(d$study)
outcome <- coerce_index(d$outcome)
id <- coerce_index(d$id)
id <- ifelse(is.na(id), -99, id)

# Organize age data. flag NA's as -99 for Stan and divide by max age (20)
age <- ifelse(is.na(d$age), -99, d$age / 20)
age_lower <- ifelse(is.na(d$age_lower), -99, d$age_lower / 20)
age_upper <- ifelse(is.na(d$age_upper), -99, d$age_upper / 20)
age_sd <- ifelse(is.na(d$age_sd), -99, d$age_sd / 20 )

# log returns ratio
lRR_mean <- d$lRR_mean
lRR_sd <- ifelse( is.na(d$lRR_sd), -99, d$lRR_sd)

# Organize into a list for Stan
data_list <- list(
  N = N,
  N_studies = N_studies,
  N_outcomes = N_outcomes,
  N_id = N_id,
  outcome = outcome,
  study = study,
  id = id,
  age = age,
  age_lower = age_lower,
  age_upper = age_upper,
  age_sd = age_sd,
  lRR_mean = lRR_mean,
  lRR_sd = lRR_sd
)

fit0 <- stan( file="stan_models/model0.stan", data=data_list, chains=4, cores=4, iter=2000, init="0", control=list(adapt_delta=0.95) )

post <- extract.samples(fit0)

#save data for text
write.csv(d, "text/data/d_all_data.csv")
