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

outcome_sex_diff <- d %>% group_by(outcome) %>% summarise(sex_diff = ifelse(var(male)==0 | is.na(var(male)), 0, 1))

d$male <- ifelse(d$sex == "both", 0.5, d$male)

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
study <- match(d$study, unique(d$study))
outcome <- match(d$outcome, unique(d$outcome))
id <- coerce_index(d$id)
id <- ifelse(is.na(id), -99, id)

# Organize age data. flag NA's as -99 for Stan and divide by max age (20)
age <- ifelse(is.na(d$age), (d$age_lower + d$age_upper)/2, d$age) / 20
age_lower <- ifelse(is.na(d$age_lower), -99, d$age_lower / 20)
age_upper <- ifelse(is.na(d$age_upper), -99, d$age_upper / 20)
age_sd <- ifelse(is.na(d$age_sd), -99, d$age_sd / 20 )


# Index unique outcomes with unknown variance
d$outcome_var <- coerce_index( ifelse(is.na(d$raw_sd), d$outcome, NA) )
d$outcome_var <- ifelse( is.na(d$outcome_var), -99, d$outcome_var)

sd_child <- ifelse(is.na(d$raw_sd), -99, d$raw_sd)

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
  returns = d$raw_return,
  sd_child = sd_child,
  mu_adult = d$adult_return,
  se_adult = d$adult_se,
  male = d$male,
  sex_diff = d$sex_diff,
  id_diff = d$id_diff
)

fit <- stan( file="stan_models/meta_analysis.stan", data=data_list, chains=4, cores=4, iter=1000, init="0", control=list(adapt_delta=0.95) )

post <- extract.samples(fit)
n_samps <- length(post$lp__)

#### Convienience function to plot predictions
pred_fun <- function( outcome=NA, male=0, id=NA, resp="returns", age=14 ) {
  
  if (!is.na(outcome)) sd <- post$sd_merged[,match(outcome, data_list$outcome)]
  else sd <- median(post$sd_outcome)
  
  if (!is.na(outcome)) outcome_v <- post$outcome_v[,outcome,]
  else outcome_v <- matrix(0, nrow=n_samps, ncol=16)
  
  if (!is.na(id)) id_v <- post$id_v[,id,]
  else id_v <- matrix(0, nrow=n_samps, ncol=2)
  
  n_preds <- length(age)
    
    k <- matrix(NA, n_samps, 2)
    b <- k
    eta <- k
    S <- array(NA, dim=c(n_samps, n_preds,2))
    mu_p <- matrix(NA, n_samps, n_preds)
    mu_r <- mu_p
    
    for (q in 1:2) {
      ticker <- 0
      
      k[,q] = exp( post$a_k[,male+1,q] + outcome_v[,(ticker + 1 + q - 1)] + outcome_v[,(ticker + 3 + q - 1)]*male );
      ticker <- ticker + 4 # update index position
      
      b[,q] = exp( post$a_b[,male+1,q] + outcome_v[,(ticker + 1 + q - 1)] + outcome_v[,(ticker + 3 + q - 1)]*male );
      ticker <- ticker + 4
      
      eta[,q] = exp( post$a_eta[,male+1,q] + outcome_v[,(ticker + 1 + q - 1)] + outcome_v[,(ticker + 3 + q - 1)]*male );
      ticker <- ticker + 4
      
      for (n in 1:n_preds) S[,n,q] = ( 1 - exp(-k[,q] * (age[n]/20) ))^b[,q];
    
    p = exp( post$a_p[,1] + post$a_p[,2]*male + id_v[,1] + outcome_v[,ticker + 1] + outcome_v[,ticker + 2]*male );
    ticker <- ticker + 2
    
    alpha = exp( post$a_alpha[,1] + post$a_alpha[,2]*male + id_v[,2] + outcome_v[,ticker + 1] + outcome_v[,ticker + 2]*male );
    }
    
    for (n in 1:n_preds) {
    mu_p[,n] = (S[,n,1]^eta[,1]) * p; 
    mu_r[,n] = (S[,n,2]^eta[,2]) * alpha;
    }
    
    if (resp == "S_returns") return( S[,,2] )
    if (resp == "returns") return( mu_r )
}

age_seq <- seq(from=0,to=20, length.out = 50)

preds <- pred_fun(age=age_seq, resp="returns", male=0)
plot(x=age_seq, y=apply(preds, 2, median), ylim=c(0,max(preds)), type="l", col="slategray", lwd=2)

shade(apply(preds, 2, PI, prob=0.9), age_seq, col=col.alpha("slategray", 0.15))
#shade(apply(preds, 2, PI, prob=0.6), age_seq, col=col.alpha("slategray", 0.1))
#shade(apply(preds, 2, PI, prob=0.3), age_seq, col=col.alpha("slategray", 0.1))

preds <- pred_fun(age=age_seq, resp="returns", male=1)
lines(x=age_seq, y=apply(preds, 2, median), col="orange", lwd=2)
shade(apply(preds, 2, PI, prob=0.9), age_seq, col=col.alpha("orange", 0.15))
#shade(apply(preds, 2, PI, prob=0.6), age_seq, col=col.alpha("orange", 0.1))
#shade(apply(preds, 2, PI, prob=0.3), age_seq, col=col.alpha("orange", 0.1))

