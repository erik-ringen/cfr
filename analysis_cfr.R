library(tidyverse)
library(rethinking)

d <- read_csv("data.csv")

########################################################
#### Drop a few problematic cases ######################
d <- d %>% 
  filter(outcome != "Hawkes_1995_Table_4_Tafabe_stashing_rates_g.h") %>% # zero-return AND no adult value
  filter(outcome != "Bird_2002b_table2_Trid. gigas") %>% # zero-return summary stat
  filter(adult_return != 0 | is.na(adult_return) ) %>% 
  filter( !(!is.na(adult_return) & is.na(adult_se)) )

## For now, drop summary stats
#d <- filter(d, is.na(raw_se))

#########################################################
#### Scale data by maximum in each outcome ##############
d <- d %>% 
  group_by(outcome) %>% 
  mutate(scaled_return = raw_return / ifelse( is.na(adult_return), max(raw_return, na.rm=T), adult_return),
         scaled_se = raw_se / ifelse( is.na(adult_return), max(raw_return, na.rm=T), adult_return),
         ) %>% 
  ungroup()

#########################################################
# Prep data for Stan
N <- nrow(d)
N_studies <- length(unique(d$study))
N_outcomes <- length(unique(d$outcome))
N_id <- length(unique(d$id))
########################################################
#### Index whether it is possible to estimate sex diff
outcome_sex_diff <- d %>%
  group_by(outcome) %>%
  summarise(sex_diff = ifelse(var(sex)==0 | is.na(var(sex)), 0, 1))

d <- left_join(d, outcome_sex_diff)

d$sex_diff <- ifelse( d$sex_diff == 1, d$outcome, NA )
d$sex_diff <- match(d$sex_diff, unique(d$sex_diff))
d$sex_diff <- ifelse(is.na(d$sex_diff), -99, d$sex_diff)

########################################################
#### Index whether it is possible to estimate individual diffs
outcome_id_diff <- d %>% group_by(outcome) %>%
  summarise(id_diff = ifelse(sum(is.na(id)) == n(), 0, 1) )

d <- left_join(d, outcome_id_diff)

d$id_diff <- coerce_index( ifelse( d$id_diff == 1, d$outcome, NA) )
d$id_diff <- ifelse( is.na(d$id_diff), -99, d$id_diff )

########################################################
#### Index missing values for adult mean returns #######
adult_values_present <- ifelse( is.na(d$adult_return), 0, 1)
adult_values_present <- sapply( 1:length(adult_values_present),
                                function(n) adult_values_present[n]*sum(adult_values_present[1:n]))

########################################################
#### Index whether child returns are summary stats #######
child_summary_returns <- ifelse( is.na(d$raw_sd), 0, 1)
child_summary_returns <- sapply( 1:length(child_summary_returns),
                                function(n) child_summary_returns[n]*sum(child_summary_returns[1:n]))

#########################################################
#### BlurtonJones, set sex = 0.5 for missing?
d$sex <- ifelse(is.na(d$sex), 0.5, d$sex)

########################################################
# making additional indices
study <- match(d$study, unique(d$study))
outcome <- match(d$outcome, unique(d$outcome))
id <- coerce_index(d$id)
id <- ifelse(is.na(id), -99, id)

d$resource_id <- match(d$resource, unique(d$resource))

# Organize age data. flag NA's as -99 for Stan and divide by max age (20)
age <- ifelse(is.na(d$age), (d$age_lower + d$age_upper)/2, d$age) / 20
age_lower <- ifelse(is.na(d$age_lower), -99, d$age_lower / 20)
age_upper <- ifelse(is.na(d$age_upper), -99, d$age_upper / 20)
age_sd <- ifelse(is.na(d$age_sd), -99, d$age_sd / 20 )

# Index unique outcomes with unknown variance
d$outcome_var <- coerce_index( ifelse(is.na(d$raw_sd), d$outcome, NA) )
d$outcome_var <- ifelse( is.na(d$outcome_var), -99, d$outcome_var)

se_child <- ifelse(is.na(d$scaled_se), -99, d$scaled_se)

# Organize into a list for Stan
data_list <- list(
  N = N,
  N_studies = N_studies,
  N_outcomes = N_outcomes,
  N_resource = max(resource),
  N_id = N_id,
  outcome = outcome,
  resource = d$resource_id,
  outcome_var = d$outcome_var,
  study = study,
  id = id,
  age = age,
  age_lower = age_lower,
  age_upper = age_upper,
  age_sd = age_sd,
  returns = d$scaled_return,
  se_child = se_child,
  sex = d$sex,
  child_summary_returns = child_summary_returns
)

ggplot(filter(d, scaled_return > 0), aes(x = age, y = scaled_return)) + facet_wrap(~resource) + geom_point() + geom_smooth() + scale_y_continuous(limits=c(0,2.5))

d_r <- d %>% 
  group_by(resource) %>% 
  summarise(id = unique(resource_id))

stan_model <- stan_model("stan_models/model_v3_noadult.stan")

fit <- sampling( stan_model, data=data_list, chains=6, cores=6, iter=100, init="0" )

post <- extract.samples(fit)

n_samps <- length(post$lp__)

#### Convienience function to plot predictions
pred_fun <- function( outcome=NA, male=0, id=NA, resource=NA, resp="returns", age=14 ) {
  
  if (!is.na(resource)) resource_v <- post$resource_v[,resource,]
  else resource_v <- matrix(0, nrow=n_samps, ncol=9)
  
  if (!is.na(outcome)) sd <- exp(post$a_sd_outcome[,1] + post$a_sd_outcome[,2]*male + resource_v[,9])
  else sd <- exp(post$a_sd_outcome[,1] + post$a_sd_outcome[,2]*male + resource_v[,9])

  if (!is.na(outcome)) outcome_v <- post$outcome_v[,outcome,]
  else outcome_v <- matrix(0, nrow=n_samps, ncol=9)
    
  if (!is.na(id)) id_v <- post$id_v[,id,]
  else id_v <- matrix(0, nrow=n_samps, ncol=2)
  
  n_preds <- length(age)
    
    k <- matrix(NA, n_samps, 2); b <- k; eta <- k
    S <- array(NA, dim=c(n_samps, n_preds,2))
    mu_p <- matrix(NA, n_samps, n_preds)
    mu_r <- mu_p
    
    for (q in 1:2) {
      ticker <- 0
      
      k[,q] = exp( post$a_k[,male+1,q] + outcome_v[,(ticker + q)] + resource_v[,(ticker + q)]);
      ticker <- ticker + 2 # update index position
      
      b[,q] = exp( post$a_b[,male+1,q] + outcome_v[,(ticker + q)] + resource_v[,(ticker + q)]);
      ticker <- ticker + 2
      
      eta[,q] = exp( post$a_eta[,male+1,q] + outcome_v[,(ticker + q)] + resource_v[,(ticker + q)]);
      ticker <- ticker + 2
      
      for (n in 1:n_preds) S[,n,q] = ( 1 - exp(-k[,q] * (age[n]/20) ))^b[,q];
    
    p = exp( post$a_p[,1] + post$a_p[,2]*male + id_v[,1] + outcome_v[,7] + resource_v[,7]);
    
    alpha = exp( post$a_alpha[,1] + post$a_alpha[,2]*male + id_v[,2] + outcome_v[,8] + resource_v[,8] );
    }
    
    for (n in 1:n_preds) {
    mu_p[,n] = (S[,n,1]^eta[,1]) * p; 
    mu_r[,n] = (S[,n,2]^eta[,2]) * alpha;
    }
    
    if (resp == "S_returns") return( S[,,2] )
    if (resp == "returns") return( (2*(inv_logit(mu_p) - 0.5)) *  exp( log(mu_r) + (sd^2)/2) )
}

age_seq <- seq(from=0,to=20, length.out = 50)


resource_cols <- c("#046C9A", "#CB2313", "#1E1E1E", "#0C775E", "#EBCC2A")

plot(NULL, ylim=c(0,3), xlim=c(0,20), ylab="Expected Child Return / Expected Adult Return", xlab="Age")

for (i in 1:N) points(y=data_list$returns[i], x = jitter(data_list$age[i]*20), col=col.alpha(resource_cols[resource[i]], 0.2), pch=16)

for (r in 1:max(resource)) {
  preds_female <- pred_fun(age=age_seq, resp="returns", resource = r, male=0)
  preds_male <- pred_fun(age=age_seq, resp="returns", resource = r, male=1)
  preds_both <- (preds_female + preds_male)/2
  
  lines(apply(preds_both, 2, mean), x=age_seq, lwd=3, col=resource_cols[r])
}

legend(x=0, y=3, legend=c("Marine", "Game", "Mixed/Other", "Fruit", "Underground Storage Organs"), lwd=3, col=resource_cols, bty='n')




plot(x=age_seq, y=apply(preds_both, 2, median), ylim=c(0,1.2), type="l", col="slategray", lwd=2, ylab=c("Expected Child Return / Expected Adult Return"))

shade(apply(preds_both, 2, PI, prob=0.9), age_seq, col=col.alpha("slategray", 0.05))
shade(apply(preds_both, 2, PI, prob=0.6), age_seq, col=col.alpha("slategray", 0.05))
shade(apply(preds_both, 2, PI, prob=0.3), age_seq, col=col.alpha("slategray", 0.05))

preds <- pred_fun(age=age_seq, resp="returns", male=1)
lines(x=age_seq, y=apply(preds, 2, median), col="orange", lwd=2)
shade(apply(preds, 2, PI, prob=0.9), age_seq, col=col.alpha("orange", 0.15))
#shade(apply(preds, 2, PI, prob=0.6), age_seq, col=col.alpha("orange", 0.1))
#shade(apply(preds, 2, PI, prob=0.3), age_seq, col=col.alpha("orange", 0.1))

