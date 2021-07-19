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
  summarise(id = unique(outcome_id),
            resource = unique(resource_id),
            age_min = ifelse( is.na(min(age)), unique(age_lower), min(age)),
            age_max = ifelse( is.na(max(age)), unique(age_upper), max(age))
            ) %>% 
  mutate(short_name = str_extract(outcome, "[^_]+"))

### intialize and fit stan model
stan_model <- stan_model("stan_models/model_v3_noadult.stan")

fit <- sampling( stan_model, data=data_list, chains=6, cores=6, iter=10, init="0" )

# extract posterior samples
post <- extract.samples(fit)
n_samps <- length(post$lp__)

#### Convenience function to plot predictions
source("cfr_pred.R")

resource_cols <- c("#046C9A", "#CB2313", "#0C775E", "#EBCC2A") # color palette to denote resources
resource_names <- c("Marine", "Game/Mixed", "Fruit", "USOs") # pub-friendly labels
age_seq <- seq(from=0, to=20, length.out = 20)

## Plot each outcome age curve

## Stratify plots by resource type
pdf(file = "resource_skill.pdf", width = 8.5, height= 11)
par(mfrow=c(2,2),
    pty='s',
    oma=c(2,2,2,2)
    )

for (r in 1:max(data_list$resource)) {
  
  d_outcome_temp <- filter(d_outcome, resource == r)
  plot(NULL, ylim=c(0,1), xlim=c(0,20), ylab="", xlab="", axes=F)
  axis(1, at=c(0,5,10,15,20))
  mtext(resource_names[r], adj=0, cex=1.25)
  
  #### Study-specific curves ########
  for (s in 1:nrow(d_outcome_temp)) {

    preds_female <- cfr_pred(age=age_seq, resp="S_returns", resource = r, outcome = d_outcome_temp$id[s], male=0)
    preds_male <- cfr_pred(age=age_seq, resp="S_returns", resource = r, outcome = d_outcome_temp$id[s], male=1)
    preds_both <- (preds_male + preds_female)/2
    
    lines(apply(preds_both, 2, median), x=age_seq, lwd=1, col=col.alpha(resource_cols[r], 0.4))
  }
  
  #### Average curve ################
  preds_female <- cfr_pred(age=age_seq, resp="S_returns", resource = r, male=0)
  preds_male <- cfr_pred(age=age_seq, resp="S_returns", resource = r, male=1)
  preds_both <- (preds_male + preds_female)/2
  
  lines(apply(preds_both, 2, median), x=age_seq, lwd=3, col = resource_cols[r])
  
  ## lines to connect different ages ####
  # 0 to 5 diff 
  lines(x = rep(0,2), y = c(0, median(preds_both[,5])), col=resource_cols[r], lty="dashed", lwd=2)
  lines(x = c(0, 5), y = rep(median(preds_both[,5]),2), col=resource_cols[r], lty="dashed", lwd=2)
  
  # 5 to 10 diff 
  lines(x = rep(5,2), y = c(median(preds_both[,5]), median(preds_both[,10])), col=resource_cols[r], lty="dashed", lwd=2)
  lines(x = c(5, 10), y = rep(median(preds_both[,10]),2), col=resource_cols[r], lty="dashed", lwd=2)
  
  # 10 to 20 diff
  lines(x = rep(10,2), y = c(median(preds_both[,10]), median(preds_both[,20])), col=resource_cols[r], lty="dashed", lwd=2)
  lines(x = c(10, 20), y = rep(median(preds_both[,20]),2), col=resource_cols[r], lty="dashed", lwd=2)
  
  
}
dev.off()


pdf(file = "resource_return.pdf", width = 8, height= 8)
par(mfrow=c(2,2),
    pty='s',
    oma=c(0,0,0,0),
    mai = c(0.5,0.5,0.5,0.5)
)

for (r in 1:max(data_list$resource)) {
  
  d_outcome_temp <- filter(d_outcome, resource == r)
  
  #### Average curve ################
  preds_female <- cfr_pred(age=age_seq, resp="nodim_returns", resource = r, male=0)
  preds_male <- cfr_pred(age=age_seq, resp="nodim_returns", resource = r, male=1)
  preds_both <- (preds_male + preds_female)/2
  
  plot(NULL, ylim=c(0,max(apply(preds_both, 2, median))+0.1), xlim=c(0,20), ylab="", xlab="", axes=F)
  
  axis(1, at=c(0,5,10,15,20), tck=-0.02, labels=NA)
  axis(1, at=c(0,5,10,15,20), tck=0, lwd=0, line=-0.5)
  
  mtext(resource_names[r], cex=1.25)
  mtext(ifelse(r %in% c(1,3), "Returns", ""), side=2, cex=1.25, line=1)
  
  lines(apply(preds_both, 2, median), x=age_seq, lwd=3, col = resource_cols[r])
  
  #### Study-specific curves ########
  for (s in 1:nrow(d_outcome_temp)) {
    
    preds_female <- cfr_pred(age=age_seq, resp="nodim_returns", resource = r, outcome = d_outcome_temp$id[s], male=0)
    preds_male <- cfr_pred(age=age_seq, resp="nodim_returns", resource = r, outcome = d_outcome_temp$id[s], male=1)
    preds_both <- (preds_male + preds_female)/2
    
    lines(apply(preds_both, 2, median), x=age_seq, lwd=1, col=col.alpha(resource_cols[r], 0.4))
  }
  
  
  ## lines to connect different ages ####
  # 0 to 5 diff 
  lines(x = rep(0,2), y = c(0, median(preds_both[,5])), col=resource_cols[r], lty="dashed", lwd=2)
  lines(x = c(0, 5), y = rep(median(preds_both[,5]),2), col=resource_cols[r], lty="dashed", lwd=2)
  
  # 5 to 10 diff 
  lines(x = rep(5,2), y = c(median(preds_both[,5]), median(preds_both[,10])), col=resource_cols[r], lty="dashed", lwd=2)
  lines(x = c(5, 10), y = rep(median(preds_both[,10]),2), col=resource_cols[r], lty="dashed", lwd=2)
  
  # 10 to 20 diff
  lines(x = rep(10,2), y = c(median(preds_both[,10]), median(preds_both[,20])), col=resource_cols[r], lty="dashed", lwd=2)
  lines(x = c(10, 20), y = rep(median(preds_both[,20]),2), col=resource_cols[r], lty="dashed", lwd=2)
  
  
}
dev.off()

###########################################
##### Coefficient of Variation #############
cov_male <- matrix(NA, nrow=n_samps, ncol = max(data_list$resource)+1)
cov_female <- cov_male

for (r in 1:max(data_list$resource)) {
 cov_female[,r] = cfr_pred(resp="CoV", male=0, resource = r)
 cov_male[,r] = cfr_pred(resp="CoV", male=1, resource = r)
}

# overall CoV
cov_female[,max(data_list$resource)+1] = cfr_pred(resp="CoV", male=0)
cov_male[,max(data_list$resource)+1] = cfr_pred(resp="CoV", male=1)




###########################################

par(mfrow=c(1,3), cex=1, pty='s')

plot(NULL, ylim=c(0,3), xlim=c(2,20), ylab="E(Child Returns)/E(Adult Returns)", xlab="Age")
mtext("Measurement Scale")
legend(x=2, y=3, legend=c("Marine", "Game", "Mixed/Other", "Fruit", "Underground Storage Organs"), lwd=3, col=resource_cols, bty='n', cex=0.7)

# plot raw data?
for (i in 1:N) points(y=data_list$returns[i], x = jitter(data_list$age[i]*20), col=col.alpha(resource_cols[data_list$resource[i]], 0.2), pch=16)

for (r in 1:max(data_list$resource)) {
  
  preds_female <- cfr_pred(age=age_seq, resp="dim_returns", resource = r, male=0)
  preds_male <- cfr_pred(age=age_seq, resp="dim_returns", resource = r, male=1)
  preds_both <- (preds_male + preds_female)/2
  
  lines(apply(preds_both, 2, median), x=age_seq, lwd=3, col=resource_cols[r])
}


plot(NULL, ylim=c(0,1), xlim=c(2,20), ylab="Returns", xlab="Age", yaxt='n')
axis(2, at=c(0,1), labels=c("Min","Max"))
mtext("Dimensionless Returns")

for (r in 1:max(data_list$resource)) {
  
  preds_female <- cfr_pred(age=age_seq, resp="nodim_returns", resource = r, male=0)
  preds_male <- cfr_pred(age=age_seq, resp="nodim_returns", resource = r, male=1)
  preds_both <- (preds_male + preds_female)/2
  
  lines(apply(preds_both, 2, median), x=age_seq, lwd=3, col=resource_cols[r])
}


plot(NULL, ylim=c(0,1), xlim=c(2,20), ylab="Skill", xlab="Age", yaxt='n')
mtext("Dimensionless Latent Skill")
axis(2, at=c(0,1), labels=c("Min","Max"))

for (r in 1:max(data_list$resource)) {
  
  preds_female <- cfr_pred(age=age_seq, resp="S_returns", resource = r, male=0)
  preds_male <- cfr_pred(age=age_seq, resp="S_returns", resource = r, male=1)
  preds_both <- (preds_male + preds_female)/2
  
  lines(apply(preds_both, 2, median), x=age_seq, lwd=3, col=resource_cols[r])
}

#####################################################
### Sex differences #################################
par(mfrow=c(1,3))

plot(NULL, ylim=c(0,3), xlim=c(2,20), ylab="E(Child Returns)/E(Adult Returns)", xlab="Age")
axis(2, at=c(0,1), labels=c("Min","Max"), yaxt='n')
mtext("Measurement Scale")
legend(x=2, y=3, legend=c("Female", "Male"), lwd=3, col=c("slategray","orange"), bty='n')

preds_female <- cfr_pred(age=age_seq, resp="dim_returns", male=0)
preds_male <- cfr_pred(age=age_seq, resp="dim_returns", male=1)
  
lines(apply(preds_female, 2, median), x=age_seq, lwd=3, col="slategray")
shade(apply(preds_female, 2, PI, prob=0.9), age_seq, lwd=3, col=col.alpha("slategray",0.1))

lines(apply(preds_male, 2, median), x=age_seq, lwd=3, col="orange")
shade(apply(preds_male, 2, PI, prob=0.9), age_seq, lwd=3, col=col.alpha("orange",0.1))

## Dimensionless returns
plot(NULL, ylim=c(0,1), xlim=c(2,20), ylab="", xlab="Age", yaxt='n')
axis(2, at=c(0,1), labels=c("Min","Max"))
mtext("Dimensionless Returns")
legend(x=2, y=3, legend=c("Female", "Male"), lwd=3, col=c("slategray","orange"), bty='n', cex=0.7)

preds_female <- cfr_pred(age=age_seq, resp="nodim_returns", male=0)
preds_male <- cfr_pred(age=age_seq, resp="nodim_returns", male=1)

lines(apply(preds_female, 2, median), x=age_seq, lwd=3, col="slategray")
shade(apply(preds_female, 2, PI, prob=0.9), age_seq, lwd=3, col=col.alpha("slategray",0.1))

lines(apply(preds_male, 2, median), x=age_seq, lwd=3, col="orange")
shade(apply(preds_male, 2, PI, prob=0.9), age_seq, lwd=3, col=col.alpha("orange",0.1))

## Dimensionless skill
plot(NULL, ylim=c(0,1), xlim=c(2,20), ylab="", xlab="Age", yaxt='n')
axis(2, at=c(0,1), labels=c("Min","Max"))
mtext("Dimensionless Latent Skill")
legend(x=2, y=3, legend=c("Female", "Male"), lwd=3, col=c("slategray","orange"), bty='n', cex=0.7)

preds_female <- cfr_pred(age=age_seq, resp="S_returns", male=0)
preds_male <- cfr_pred(age=age_seq, resp="S_returns", male=1)

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

















