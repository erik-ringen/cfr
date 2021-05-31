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
d$outcome_id <- match(d$outcome, unique(d$outcome))
id <- coerce_index(d$id)
id <- ifelse(is.na(id), -99, id)

d$resource_id <- match(d$resource, unique(d$resource))

# Organize age data. flag NA's as -99 for Stan and divide by max age (20)
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

d_r <- d %>% 
  group_by(resource) %>% 
  summarise(id = unique(resource_id))

d_outcome <- d %>% 
  group_by(outcome) %>% 
  summarise(id = unique(outcome_id)) %>% 
  mutate(short_name = str_extract(outcome, "[^_]+"))

stan_model <- stan_model("stan_models/model_v3_noadult.stan")

fit <- sampling( stan_model, data=data_list, chains=6, cores=6, iter=250, init="0" )

post <- extract.samples(fit)

n_samps <- length(post$lp__)



#### Convenience function to plot predictions
pred_fun <- function( outcome=NA, male=0, id=NA, resource=NA, resp="returns", age=14 ) {
  
  if (!is.na(resource)) resource_v <- post$resource_v[,resource,]
  else resource_v <- matrix(0, nrow=n_samps, ncol=7)
  
  if (!is.na(outcome)) sd <- exp(post$a_sd_outcome[,1] + post$a_sd_outcome[,2]*male + resource_v[,7])
  else sd <- exp(post$a_sd_outcome[,1] + post$a_sd_outcome[,2]*male + resource_v[,7])

  if (!is.na(outcome)) outcome_v <- post$outcome_v[,outcome,]
  else outcome_v <- matrix(0, nrow=n_samps, ncol=7)
    
  if (!is.na(id)) id_v <- post$id_v[,id,]
  else id_v <- matrix(0, nrow=n_samps, ncol=2)
  
  n_preds <- length(age)
    
    eta <- matrix(NA, n_samps, 2); k <- rep(NA, n_samps); b <- k
    S <- matrix(NA, n_samps, n_preds)
    mu_p <- matrix(NA, n_samps, n_preds)
    mu_r <- mu_p
      
      k = exp( post$a_k[,1] + post$a_k[,2]*male + outcome_v[,1] + resource_v[,1])
      
      b = exp( post$a_b[,1] + post$a_b[,2]*male + outcome_v[,2] + resource_v[,2])
      
      eta[,1] = exp( post$a_eta[,1,1] + post$a_eta[,2,1]*male + outcome_v[,3] + resource_v[,3])
      eta[,2] = exp( post$a_eta[,1,2] + post$a_eta[,2,2]*male + outcome_v[,4] + resource_v[,4])
      
      for (n in 1:n_preds) S[,n] = ( 1 - exp(-k * (age[n]/20) ))^b
    
    p = exp( post$a_p[,1] + post$a_p[,2]*male + id_v[,1] + outcome_v[,5] + resource_v[,5])
    
    alpha = exp( post$a_alpha[,1] + post$a_alpha[,2]*male + id_v[,2] + outcome_v[,6] + resource_v[,6] )
    
    for (n in 1:n_preds) {
    mu_p[,n] = (S[,n]^eta[,1]) * p
    mu_r[,n] = (S[,n]^eta[,2]) * alpha
    }
    
    if (resp == "S_returns") return( S )
    if (resp == "dim_returns") return(  2*(inv_logit(mu_p) - 0.5) * exp( log(mu_r) + (sd^2)/2) )
    if (resp == "nodim_returns") return( mu_r/alpha )
}

resource_cols <- c("#046C9A", "#CB2313", "#1E1E1E", "#0C775E", "#EBCC2A")

age_seq <- seq(from=2, to=20, length.out = 40)

par(mfrow=c(1,3), cex=1)

plot(NULL, ylim=c(0,3), xlim=c(2,20), ylab="E(Child Returns)/E(Adult Returns)", xlab="Age")
mtext("Measurement Scale")
legend(x=2, y=3, legend=c("Marine", "Game", "Mixed/Other", "Fruit", "Underground Storage Organs"), lwd=3, col=resource_cols, bty='n', cex=0.7)

# plot raw data?
for (i in 1:N) points(y=data_list$returns[i], x = jitter(data_list$age[i]*20), col=col.alpha(resource_cols[data_list$resource[i]], 0.2), pch=16)

for (r in 1:max(data_list$resource)) {
  
  preds_female <- pred_fun(age=age_seq, resp="dim_returns", resource = r, male=0)
  preds_male <- pred_fun(age=age_seq, resp="dim_returns", resource = r, male=1)
  preds_both <- (preds_male + preds_female)/2
  
  lines(apply(preds_both, 2, median), x=age_seq, lwd=3, col=resource_cols[r])
}


plot(NULL, ylim=c(0,1), xlim=c(2,20), ylab="Returns", xlab="Age", yaxt='n')
axis(2, at=c(0,1), labels=c("Min","Max"))
mtext("Dimensionless Returns")

for (r in 1:max(data_list$resource)) {
  
  preds_female <- pred_fun(age=age_seq, resp="nodim_returns", resource = r, male=0)
  preds_male <- pred_fun(age=age_seq, resp="nodim_returns", resource = r, male=1)
  preds_both <- (preds_male + preds_female)/2
  
  lines(apply(preds_both, 2, median), x=age_seq, lwd=3, col=resource_cols[r])
}


plot(NULL, ylim=c(0,1), xlim=c(2,20), ylab="Skill", xlab="Age", yaxt='n')
mtext("Dimensionless Latent Skill")
axis(2, at=c(0,1), labels=c("Min","Max"))

for (r in 1:max(data_list$resource)) {
  
  preds_female <- pred_fun(age=age_seq, resp="S_returns", resource = r, male=0)
  preds_male <- pred_fun(age=age_seq, resp="S_returns", resource = r, male=1)
  preds_both <- (preds_male + preds_female)/2
  
  lines(apply(preds_both, 2, median), x=age_seq, lwd=3, col=resource_cols[r])
}

#####################################################
### Sex differences #################################
par(mfrow=c(1,3))O

plot(NULL, ylim=c(0,3), xlim=c(2,20), ylab="E(Child Returns)/E(Adult Returns)", xlab="Age")
axis(2, at=c(0,1), labels=c("Min","Max"), yaxt='n')
mtext("Measurement Scale")
legend(x=2, y=3, legend=c("Female", "Male"), lwd=3, col=c("slategray","orange"), bty='n')

preds_female <- pred_fun(age=age_seq, resp="dim_returns", male=0)
preds_male <- pred_fun(age=age_seq, resp="dim_returns", male=1)
  
lines(apply(preds_female, 2, median), x=age_seq, lwd=3, col="slategray")
shade(apply(preds_female, 2, PI, prob=0.9), age_seq, lwd=3, col=col.alpha("slategray",0.1))

lines(apply(preds_male, 2, median), x=age_seq, lwd=3, col="orange")
shade(apply(preds_male, 2, PI, prob=0.9), age_seq, lwd=3, col=col.alpha("orange",0.1))

## Dimensionless returns
plot(NULL, ylim=c(0,1), xlim=c(2,20), ylab="", xlab="Age", yaxt='n')
axis(2, at=c(0,1), labels=c("Min","Max"))
mtext("Dimensionless Returns")
legend(x=2, y=3, legend=c("Female", "Male"), lwd=3, col=c("slategray","orange"), bty='n', cex=0.7)

preds_female <- pred_fun(age=age_seq, resp="nodim_returns", male=0)
preds_male <- pred_fun(age=age_seq, resp="nodim_returns", male=1)

lines(apply(preds_female, 2, median), x=age_seq, lwd=3, col="slategray")
shade(apply(preds_female, 2, PI, prob=0.9), age_seq, lwd=3, col=col.alpha("slategray",0.1))

lines(apply(preds_male, 2, median), x=age_seq, lwd=3, col="orange")
shade(apply(preds_male, 2, PI, prob=0.9), age_seq, lwd=3, col=col.alpha("orange",0.1))

## Dimensionless skill
plot(NULL, ylim=c(0,1), xlim=c(2,20), ylab="", xlab="Age", yaxt='n')
axis(2, at=c(0,1), labels=c("Min","Max"))
mtext("Dimensionless Latent Skill")
legend(x=2, y=3, legend=c("Female", "Male"), lwd=3, col=c("slategray","orange"), bty='n', cex=0.7)

preds_female <- pred_fun(age=age_seq, resp="S_returns", male=0)
preds_male <- pred_fun(age=age_seq, resp="S_returns", male=1)

lines(apply(preds_female, 2, median), x=age_seq, lwd=3, col="slategray")
shade(apply(preds_female, 2, PI, prob=0.9), age_seq, lwd=3, col=col.alpha("slategray",0.1))

lines(apply(preds_male, 2, median), x=age_seq, lwd=3, col="orange")
shade(apply(preds_male, 2, PI, prob=0.9), age_seq, lwd=3, col=col.alpha("orange",0.1))

#### Compare parameters between males and females
par(mfrow=c(1,5))
dens( exp(post$a_k[,1] + post$a_k[,2]), col="orange", lwd=2, yaxt='n', xlab='k', ylab="", xlim=c(0,5.5)) 
dens( exp(post$a_k[,1]), col="slategray", lwd=2, yaxt='n', xlab='k', ylab="", add=T) 
legend(x=2, y=3, legend=c("Female", "Male"), lwd=3, col=c("slategray","orange"), bty='n')
mtext("k")


dens( exp(post$a_b[,1] + post$a_b[,2]), col="orange", lwd=2, yaxt='n', xlab='b', ylab="") 
dens( exp(post$a_b[,1]), col="slategray", lwd=2, yaxt='n', xlab='b', ylab="", add=T) 
mtext("b")

dens( exp(post$a_eta[,1,2] + post$a_eta[,2,2]), col="orange", lwd=2, yaxt='n', xlab='eta', ylab="") 
dens( exp(post$a_eta[,1,2]), col="slategray", lwd=2, yaxt='n', xlab='eta', ylab="", add=T) 
mtext(expression(eta))

dens( exp(post$a_alpha[,1]), col="slategray", lwd=2, yaxt='n', xlab='alpha', ylab="") 
dens( exp(post$a_alpha[,1] + post$a_alpha[,2]), col="orange", lwd=2, yaxt='n', xlab='eta', ylab="", add=T) 

mtext(expression(alpha))


dens( exp(post$a_sd_outcome[,1]), col="slategray", lwd=2, yaxt='n', xlab='sigma', ylab="") 
dens( exp(post$a_sd_outcome[,1] + post$a_sd_outcome[,2]), col="orange", lwd=2, yaxt='n', xlab='eta', ylab="", add=T) 

mtext(expression(sigma))

################################
#### Outcome differences #######

k_outcome <- apply(post$outcome_v[,,1], 2, median)
b_outcome <- apply(post$outcome_v[,,2], 2, median)
eta_outcome <- apply(post$outcome_v[,,4], 2, median)
alpha_outcome <- apply(post$outcome_v[,,6], 2, median)
sigma_outcome <- apply(post$outcome_v[,,7], 2, median)


d_outcome %>% arrange(id)


plot()

















