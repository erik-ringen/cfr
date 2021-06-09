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
  summarise(id = unique(outcome_id), resource = unique(resource_id)) %>% 
  mutate(short_name = str_extract(outcome, "[^_]+"))

stan_model <- stan_model("stan_models/model_v3_noadult.stan")

fit <- sampling( stan_model, data=data_list, chains=6, cores=6, iter=10, init="0" )

post <- extract.samples(fit)

n_samps <- length(post$lp__)

##### Coverage of resources by age #######
d$age_round <- round(d$age)

dummy_asr <- data.frame(
  age = rep(seq(from=2, to=20), each = 8),
  sex_bin = rep( rep( c(0,1), each = 4), 19 ),
  resource = rep( rep( unique(d$resource), 2), 19),
  outcome_count = 0
)

for (i in 1:nrow(dummy_asr)) {
  
  res = dummy_asr$resource[i]
  sex = c(d$sex[d$sex > 0 & d$sex < 1],  dummy_asr$sex_bin[i])
  age = dummy_asr$age[i]
  
  d_sub <- d[ d$resource == res & d$sex %in% sex & d$age_round == age, ]
  
  dummy_asr$outcome_count[i] <- length(unique(d_sub$outcome))
}


resource_cols <- c("#0C775E", "#CB2313","#046C9A", "#EBCC2A")

dummy_asr$sex <- ifelse(dummy_asr$sex == 0, "Female", "Male")

ggplot(dummy_asr, aes(x = age, y = outcome_count, fill = resource, color=resource)) + 
  facet_wrap(~sex) +
  geom_area(alpha=0.8) + 
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0)) +
  scale_fill_manual(values = resource_cols) +
  scale_color_manual(values = resource_cols) +
  theme_bw(base_size = 14) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),strip.background = element_blank(),
        legend.title = element_blank(),
        panel.spacing = unit(2, "lines")) +
  ylab("Number of Studies") +
  xlab("Age")

###########################################
##### Orchard plots for key parameters ####

### k #####
avg_k <- exp( (post$a_k[,1] + post$a_k[,2])*0.5 )

outcome_k <- matrix(NA, nrow = n_samps, ncol = N_outcomes)

for (j in 1:N_outcomes) {
  outcome_k[,j] <- exp( (post$a_k[,1] + post$a_k[,2])*0.5 + post$outcome_v[,j,1] + post$resource_v[,d_outcome$resource[match(j, d_outcome$id)],1] )
}

precision_k <- 1/apply(outcome_k, 2, sd)
median_k <- apply(outcome_k, 2, mean)

k_df <- data.frame(median = median_k,
                   precision = precision_k,
                   resource = d_outcome$resource[match(1:N_outcomes, d_outcome$id)]
                   )

k_df$resource <- factor(k_df$resource, labels = d_r$resource[match(1:max(d_r$id),d_r$id )])

avg_k <- data.frame(
  median = mean(avg_k),
  sd = sd(avg_k),
  lower = HPDI(avg_k, 0.9)[1],
  upper = HPDI(avg_k, 0.9)[2]
)

### b #####
avg_b <- exp( (post$a_b[,1] + post$a_b[,2])*0.5 )

outcome_b <- matrix(NA, nrow = n_samps, ncol = N_outcomes)

for (j in 1:N_outcomes) {
  outcome_b[,j] <- exp( (post$a_b[,1] + post$a_b[,2])*0.5 + post$outcome_v[,j,2] + post$resource_v[,d_outcome$resource[match(j, d_outcome$id)],2] )
}

precision_b <- 1/apply(outcome_b, 2, sd)
median_b <- apply(outcome_b, 2, mean)

b_df <- data.frame(median = median_b,
                   precision = precision_b,
                   resource = d_outcome$resource[match(1:N_outcomes, d_outcome$id)]
)

b_df$resource <- factor(b_df$resource, labels = d_r$resource[match(1:max(d_r$id),d_r$id )])

avg_b <- data.frame(
  median = median(avg_b),
  sd = sd(avg_b),
  lower = HPDI(avg_b, 0.9)[1],
  upper = HPDI(avg_b, 0.9)[2]
)
###########

### eta_p #####
avg_eta_p <- exp( (post$a_eta[,1,1] + post$a_eta[,2,1])*0.5 )

outcome_eta_p <- matrix(NA, nrow = n_samps, ncol = N_outcomes)

for (j in 1:N_outcomes) {
  outcome_eta_p[,j] <- exp( (post$a_eta[,1,1] + post$a_eta[,2,1])*0.5 + post$outcome_v[,j,3] + post$resource_v[,d_outcome$resource[match(j, d_outcome$id)],3] )
}

precision_eta_p <- 1/apply(outcome_eta_p, 2, sd)
median_eta_p <- apply(outcome_eta_p, 2, mean)

eta_p_df <- data.frame(median = median_eta_p,
                   precision = precision_eta_p,
                   resource = d_outcome$resource[match(1:N_outcomes, d_outcome$id)]
)

eta_p_df$resource <- factor(eta_p_df$resource, labels = d_r$resource[match(1:max(d_r$id),d_r$id )])

avg_eta_p <- data.frame(
  median = mean(avg_eta_p),
  sd = sd(avg_eta_p),
  lower = HPDI(avg_eta_p, 0.9)[1],
  upper = HPDI(avg_eta_p, 0.9)[2]
)
######

### eta_r #####
avg_eta_r <- exp( (post$a_eta[,1,2] + post$a_eta[,2,2])*0.5 )

outcome_eta_r <- matrix(NA, nrow = n_samps, ncol = N_outcomes)

for (j in 1:N_outcomes) {
  outcome_eta_r[,j] <- exp( (post$a_eta[,1,2] + post$a_eta[,2,2])*0.5 + post$outcome_v[,j,4] + post$resource_v[,d_outcome$resource[match(j, d_outcome$id)],4] )
}

precision_eta_r <- 1/apply(outcome_eta_r, 2, sd)
median_eta_r <- apply(outcome_eta_r, 2, mean)

eta_r_df <- data.frame(median = median_eta_r,
                       precision = precision_eta_r,
                       resource = d_outcome$resource[match(1:N_outcomes, d_outcome$id)]
)

eta_r_df$resource <- factor(eta_r_df$resource, labels = d_r$resource[match(1:max(d_r$id),d_r$id )])

avg_eta_r <- data.frame(
  median = mean(avg_eta_r),
  sd = sd(avg_eta_r),
  lower = HPDI(avg_eta_r, 0.9)[1],
  upper = HPDI(avg_eta_r, 0.9)[2]
)
######


######

### Compose orchard plots 

resource_cols <- c("#046C9A", "#CB2313", "#0C775E", "#EBCC2A")

k_plot <- ggplot(k_df, aes(x = median, y = 0)) + 
  geom_jitter(aes(color = resource, size = precision), alpha=0.6, width=0, height=0.2) + 
  geom_errorbarh(data = avg_k, aes(xmin = lower, xmax = upper), lwd=1, height=0, color="darkgrey") + 
  geom_errorbarh(data = avg_k, aes(xmin = median - sd, xmax = median + sd), height=0, lwd=2, color="black") +
  geom_point(data = avg_k, aes(x = median, y = 0), size=4, color="seashell1") +
  geom_point(data = avg_k, aes(x = median, y = 0), size=4, color="black", shape=1) +
  scale_y_continuous(limits=c(-0.5, 0.5)) +
  scale_size(name = "Precision") +
  theme_bw(base_size=14) + 
  scale_color_manual(values = resource_cols, guide=F) + 
  xlab("k") +
  ylab("")  + 
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = c(0.7, 0.1),
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.title = element_text(size=8),
        legend.text = element_text(size=8),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.key.size = unit(0.5, 'lines')) + 
  guides(size = guide_legend(nrow = 1,title.position = "left"))


b_plot <- ggplot(b_df, aes(x = median, y = 0)) + 
  geom_jitter(aes(color = resource, size = precision), alpha=0.6, width=0, height=0.2) + 
  geom_errorbarh(data = avg_b, aes(xmin = lower, xmax = upper), lwd=1, height=0, color="darkgrey") + 
  geom_errorbarh(data = avg_b, aes(xmin = median - sd, xmax = median + sd), height=0, lwd=2, color="black") + 
  geom_point(data = avg_b, aes(x = median, y = 0), size=4, color="seashell1") +
  geom_point(data = avg_b, aes(x = median, y = 0), size=4, color="black", shape=1) +
  scale_y_continuous(limits=c(-0.5, 0.5)) +
  theme_bw(base_size=14) + 
  scale_color_manual(values = resource_cols, guide=F) + 
  scale_size(breaks=c(1.5,2,2.5), name = "Precision") +
  xlab("b") +
  ylab("") + 
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = c(0.7, 0.1),
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.title = element_text(size=8),
        legend.text = element_text(size=8),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
  guides(size = guide_legend(nrow = 1,title.position = "left"))


eta_p_plot <- ggplot(eta_p_df, aes(x = median, y = 0)) + 
  geom_jitter(aes(color = resource, size = precision), alpha=0.6, width=0, height=0.2) + 
  geom_errorbarh(data = avg_eta_p, aes(xmin = lower, xmax = upper), lwd=1, height=0, color="darkgrey") + 
  geom_errorbarh(data = avg_eta_p, aes(xmin = median - sd, xmax = median + sd), height=0, lwd=2, color="black") + 
  geom_point(data = avg_eta_p, aes(x = median, y = 0), size=4, color="seashell1") +
  geom_point(data = avg_eta_p, aes(x = median, y = 0), size=4, color="black", shape=1) +
  scale_y_continuous(limits=c(-0.5, 0.5)) +
  theme_bw(base_size=14) + 
  scale_color_manual(values = resource_cols, guide=F) + 
  scale_size(breaks=c(0.75,1.25,1.75), name = "Precision") +
  xlab(expression(eta["p"])) +
  ylab("") + 
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = c(0.7, 0.1),
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.title = element_text(size=8),
        legend.text = element_text(size=8),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
  guides(size = guide_legend(nrow = 1,title.position = "left"))


eta_r_plot <- ggplot(eta_r_df, aes(x = median, y = 0)) + 
  geom_jitter(aes(color = resource, size = precision), alpha=0.6, width=0, height=0.2) + 
  geom_errorbarh(data = avg_eta_r, aes(xmin = lower, xmax = upper), lwd=1, height=0, color="darkgrey") + 
  geom_errorbarh(data = avg_eta_r, aes(xmin = median - sd, xmax = median + sd), height=0, lwd=2, color="black") + 
  geom_point(data = avg_eta_r, aes(x = median, y = 0), size=4, color="seashell1") +
  geom_point(data = avg_eta_r, aes(x = median, y = 0), size=4, color="black", shape=1) +
  scale_y_continuous(limits=c(-0.5, 0.5)) +
  theme_bw(base_size=14) + 
  scale_color_manual(values = resource_cols, guide=F) + 
  scale_size(name = "Precision") +
  xlab(expression(eta["r"])) +
  ylab("") + 
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = c(0.7, 0.1),
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.title = element_text(size=8),
        legend.text = element_text(size=8),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
  guides(size = guide_legend(nrow = 1,title.position = "left"))


orchard <- k_plot / b_plot / eta_p_plot / eta_r_plot

ggsave("orchard.pdf", plot = orchard, width=4, height=6.5, dpi=600)


b_plot <- ggplot(b_df, aes(x = median, y = 0)) + 
  geom_jitter(aes(color = resource, size = precision), alpha=0.6, width=0.001, height=0.2) + 
  scale_y_continuous(limits=c(-0.5, 0.5)) +
  theme_bw(base_size=14) + 
  scale_color_manual(values = resource_cols) + 
  xlab("k") +
  ylab("")

# Area chat of resouce type, by sex and across ages


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

















