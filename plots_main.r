library(tidyverse)
library(rethinking)
source("cfr_functions.R")

##### Read in study data
d <- read_csv("data.csv")

d <- d %>% 
  group_by(outcome) %>% 
  mutate(scaled_return = raw_return /  max(raw_return, na.rm=T),
         scaled_se = raw_se / max(raw_return, na.rm=T),
  ) %>% 
  ungroup() %>% 
  filter( !(is.na(scaled_return)) & ( is.na(raw_se) | raw_se > 0) )

### Re-create indices for resource and outcome
d$resource_id <- match(d$resource, unique(d$resource))
d$outcome_id <- match(d$outcome, unique(d$outcome))

### Get resource id to link model fit indices with dataset
d_r <- d %>% 
  group_by(resource) %>% 
  summarise(id = unique(resource_id))

### Get outcome id, resource type, and age range for each study*outcome
d_outcome <- d %>% 
  group_by(outcome) %>% 
  summarise(id = unique(outcome_id),
            resource = unique(resource_id),
            age_min = ifelse( is.na(min(age)), unique(age_lower), min(age)),
            age_max = ifelse( is.na(max(age)), unique(age_upper), max(age))
            ) %>% 
  mutate(short_name = str_extract(outcome, "[^_]+"))

##### Read in previously fit stan model
fit <- readRDS("fit_cfr.rds")

# extract posterior samples
post <- extract.samples(fit)
n_samps <- length(post$lp__)

#####################################################
#### Skill ~ age plot, marginal mean ################
pdf(file = "resource_skill_avg.pdf", width = 6, height= 8)
par(pty='s',
    oma=c(0,0,0,0),
    mai = c(0.5,0.5,0.5,0.5),
    cex=1.3
)

preds_both <- cfr_pred(age=age_seq, resp="S_returns")

plot(NULL, ylim=c(0,max(apply(preds_both, 2, median))+0.3), xlim=c(0,20), ylab="", xlab="", axes=F)
lines(apply(preds_both, 2, median), x=age_seq, lwd=3)
shade(apply(preds_both, 2, PI, prob=0.9), age_seq, col=col.alpha("black",0.05))
shade(apply(preds_both, 2, PI, prob=0.6), age_seq, col=col.alpha("black",0.05))
shade(apply(preds_both, 2, PI, prob=0.3), age_seq, col=col.alpha("black",0.05))

axis(1, at=c(0,5,10,15,20), tck=-0.02, labels=NA)
axis(1, at=c(0,5,10,15,20), tck=0, lwd=0, line=-0.5)

  ## lines to connect different ages ####
  # 0 to 5 diff 
  lines(x = rep(0,2), y = c(0, median(preds_both[,6])), lty="dashed", lwd=2)
  lines(x = c(0, 5), y = rep(median(preds_both[,6]),2), lty="dashed", lwd=2)
  
  # 5 to 10 diff 
  lines(x = rep(5,2), y = c(median(preds_both[,6]), median(preds_both[,11])), lty="dashed", lwd=2)
  lines(x = c(5, 10), y = rep(median(preds_both[,11]),2), lty="dashed", lwd=2)
  
  # 10 to 20 diff
  lines(x = rep(10,2), y = c(median(preds_both[,11]), median(preds_both[,21])), lty="dashed", lwd=2)
  lines(x = c(10, 20), y = rep(median(preds_both[,21]),2), lty="dashed", lwd=2)

dev.off()

#####################################################
#### Skill ~ age plot, broken down by resource ######
pdf(file = "resource_skill.pdf", width = 8.5, height = 11)
par(mfrow=c(2,2),
    pty='s',
    oma=c(0,0,0,0),
    cex=1.3
    )

# loop over resource type
for (r in 1:length(unique(d$resource))) {
  
  d_outcome_temp <- filter(d_outcome, resource == r)
  # Set up plot area
  plot(NULL, ylim=c(0,1), xlim=c(0,20), ylab="", xlab="", axes=F)
  axis(1, at=c(0,5,10,15,20), tck=-0.02, labels=NA)
  axis(1, at=c(0,5,10,15,20), tck=0, lwd=0, line=-0.5)
  mtext(resource_names[r], adj=0, cex=1.25)
  
  ## Loop over each study outcome
  for (s in 1:nrow(d_outcome_temp)) {

  ## generate model predictions    
  preds_both <- cfr_pred(
      age=age_seq,
       resp="S_returns",
        resource = r,
         outcome = d_outcome_temp$id[s]
         )

  ## plot age trajectories (posterior median)
  lines(apply(preds_both, 2, median),
   x=age_seq, lwd=1,
    col=col.alpha(resource_cols[r], 0.4)
    )
  } # end loop over study outcomes
  
  #### Average curve ################
  preds_both <- cfr_pred(age=age_seq, resp="S_returns", resource = r)
  lines(apply(preds_both, 2, median), x=age_seq, lwd=3, col = resource_cols[r])
  
  ## lines to connect different ages ####
  # 0 to 5 diff 
  lines(x = rep(0,2), y = c(0, median(preds_both[,6])), col=resource_cols[r], lty="dashed", lwd=2)
  lines(x = c(0, 5), y = rep(median(preds_both[,6]),2), col=resource_cols[r], lty="dashed", lwd=2)
  
  # 5 to 10 diff 
  lines(x = rep(5,2), y = c(median(preds_both[,6]), median(preds_both[,11])), col=resource_cols[r], lty="dashed", lwd=2)
  lines(x = c(5, 10), y = rep(median(preds_both[,11]),2), col=resource_cols[r], lty="dashed", lwd=2)
  
  # 10 to 20 diff
  lines(x = rep(10,2), y = c(median(preds_both[,11]), median(preds_both[,21])), col=resource_cols[r], lty="dashed", lwd=2)
  lines(x = c(10, 20), y = rep(median(preds_both[,21]),2), col=resource_cols[r], lty="dashed", lwd=2)
  
  
} # end loop over resource type
dev.off()
################################################
#### Returns ~ age plot, marginal mean ###########
pdf(file = "resource_return_avg.pdf", width = 6, height= 8)
par(pty='s',
    oma=c(0,0,0,0),
    mai = c(0.5,0.5,0.5,0.5),
    cex=1.3
)

preds_both <- cfr_pred(age=age_seq, resp="nodim_returns")

plot(NULL, ylim=c(0,max(apply(preds_both, 2, median))+0.3), xlim=c(0,20), ylab="", xlab="", axes=F)
lines(apply(preds_both, 2, median), x=age_seq, lwd=3)
shade(apply(preds_both, 2, PI, prob=0.9), age_seq, col=col.alpha("black",0.05))
shade(apply(preds_both, 2, PI, prob=0.6), age_seq, col=col.alpha("black",0.05))
shade(apply(preds_both, 2, PI, prob=0.3), age_seq, col=col.alpha("black",0.05))

axis(1, at=c(0,5,10,15,20), tck=-0.02, labels=NA)
axis(1, at=c(0,5,10,15,20), tck=0, lwd=0, line=-0.5)

 ## lines to connect different ages ####
  # 0 to 5 diff 
  lines(x = rep(0,2), y = c(0, median(preds_both[,6])), lty="dashed", lwd=2)
  lines(x = c(0, 5), y = rep(median(preds_both[,6]),2), lty="dashed", lwd=2)
  
  # 5 to 10 diff 
  lines(x = rep(5,2), y = c(median(preds_both[,6]), median(preds_both[,11])), lty="dashed", lwd=2)
  lines(x = c(5, 10), y = rep(median(preds_both[,11]),2), lty="dashed", lwd=2)
  
  # 10 to 20 diff
  lines(x = rep(10,2), y = c(median(preds_both[,11]), median(preds_both[,21])), lty="dashed", lwd=2)
  lines(x = c(10, 20), y = rep(median(preds_both[,21]),2), lty="dashed", lwd=2)

dev.off()

#####################################################
#### Returns ~ age plot, broken down by resource ####
pdf(file = "resource_return.pdf", width = 8, height= 8)
par(mfrow=c(2,2),
    pty='s',
    oma=c(0,0,0,0),
    mai = c(0.5,0.5,0.5,0.5),
    cex=1.3
)

for (r in 1:length(unique(d$resource))) {
  
  d_outcome_temp <- filter(d_outcome, resource == r)
  
  #### Average curve ################
  preds_both <- cfr_pred(age=age_seq, resp="nodim_returns", resource = r)
  
  plot(NULL, ylim=c(0,max(apply(preds_both, 2, median))+0.3), xlim=c(0,20), ylab="", xlab="", axes=F)
  
  axis(1, at=c(0,5,10,15,20), tck=-0.02, labels=NA)
  axis(1, at=c(0,5,10,15,20), tck=0, lwd=0, line=-0.5)
  
  mtext(resource_names[r], cex=1.25)
  mtext(ifelse(r %in% c(1,3), "Returns", ""), side=2, cex=1.25, line=1)
  
  lines(apply(preds_both, 2, median), x=age_seq, lwd=3, col = resource_cols[r])
  
  ## lines to connect different ages ####
  # 0 to 5 diff 
  lines(x = rep(0,2), y = c(0, median(preds_both[,6])), col=resource_cols[r], lty="dashed", lwd=2)
  lines(x = c(0, 5), y = rep(median(preds_both[,6]),2), col=resource_cols[r], lty="dashed", lwd=2)
  
  # 5 to 10 diff 
  lines(x = rep(5,2), y = c(median(preds_both[,6]), median(preds_both[,11])), col=resource_cols[r], lty="dashed", lwd=2)
  lines(x = c(5, 10), y = rep(median(preds_both[,11]),2), col=resource_cols[r], lty="dashed", lwd=2)
  
  # 10 to 20 diff
  lines(x = rep(10,2), y = c(median(preds_both[,11]), median(preds_both[,21])), col=resource_cols[r], lty="dashed", lwd=2)
  lines(x = c(10, 20), y = rep(median(preds_both[,21]),2), col=resource_cols[r], lty="dashed", lwd=2)

  #### Study-specific curves ########
  for (s in 1:nrow(d_outcome_temp)) {
    
    preds_both <- cfr_pred(age=age_seq, resp="nodim_returns", resource = r, outcome = d_outcome_temp$id[s])
    lines(apply(preds_both, 2, median), x=age_seq, lwd=1, col=col.alpha(resource_cols[r], 0.4))
  }
  
}
dev.off()

#####################################################
#### Skill ~ age plot, broken down by sex ###########

pdf(file = "male_skill_avg.pdf", width = 6, height= 8)
par(pty='s',
    oma=c(0,0,0,0),
    mai = c(0.5,0.5,0.5,0.5),
    cex=1.3
)

preds_both <- cfr_pred(age=age_seq, resp="S_returns", male=1)

plot(NULL, ylim=c(0,max(apply(preds_both, 2, median))+0.3), xlim=c(0,20), ylab="", xlab="", axes=F)
lines(apply(preds_both, 2, median), x=age_seq, lwd=3, col=m_col)
shade(apply(preds_both, 2, PI, prob=0.9), age_seq, col=col.alpha(m_col,0.05))
shade(apply(preds_both, 2, PI, prob=0.6), age_seq, col=col.alpha(m_col,0.05))
shade(apply(preds_both, 2, PI, prob=0.3), age_seq, col=col.alpha(m_col,0.05))

axis(1, at=c(0,5,10,15,20), tck=-0.02, labels=NA)
axis(1, at=c(0,5,10,15,20), tck=0, lwd=0, line=-0.5)

 ## lines to connect different ages ####
  # 0 to 5 diff 
  lines(x = rep(0,2), y = c(0, median(preds_both[,6])), lty="dashed",col=m_col, lwd=2)
  lines(x = c(0, 5), y = rep(median(preds_both[,6]),2), lty="dashed",col=m_col, lwd=2)
  
  # 5 to 10 diff 
  lines(x = rep(5,2), y = c(median(preds_both[,6]), median(preds_both[,11])), lty="dashed",col=m_col, lwd=2)
  lines(x = c(5, 10), y = rep(median(preds_both[,11]),2), lty="dashed",col=m_col, lwd=2)
  
  # 10 to 20 diff
  lines(x = rep(10,2), y = c(median(preds_both[,11]), median(preds_both[,21])), lty="dashed",col=m_col, lwd=2)
  lines(x = c(10, 20), y = rep(median(preds_both[,21]),2), lty="dashed",col=m_col, lwd=2)

dev.off()

pdf(file = "female_skill_avg.pdf", width = 6, height= 8)
par(pty='s',
    oma=c(0,0,0,0),
    mai = c(0.5,0.5,0.5,0.5),
    cex=1.3
)

preds_both <- cfr_pred(age=age_seq, resp="S_returns", male=0)

plot(NULL, ylim=c(0,max(apply(preds_both, 2, median))+0.3), xlim=c(0,20), ylab="", xlab="", axes=F)
lines(apply(preds_both, 2, median), x=age_seq, lwd=3, col=f_col)
shade(apply(preds_both, 2, PI, prob=0.9), age_seq, col=col.alpha(f_col,0.05))
shade(apply(preds_both, 2, PI, prob=0.6), age_seq, col=col.alpha(f_col,0.05))
shade(apply(preds_both, 2, PI, prob=0.3), age_seq, col=col.alpha(f_col,0.05))

axis(1, at=c(0,5,10,15,20), tck=-0.02, labels=NA)
axis(1, at=c(0,5,10,15,20), tck=0, lwd=0, line=-0.5)

 ## lines to connect different ages ####
  # 0 to 5 diff 
  lines(x = rep(0,2), y = c(0, median(preds_both[,6])), lty="dashed",col=f_col, lwd=2)
  lines(x = c(0, 5), y = rep(median(preds_both[,6]),2), lty="dashed",col=f_col, lwd=2)
  
  # 5 to 10 diff 
  lines(x = rep(5,2), y = c(median(preds_both[,6]), median(preds_both[,11])), lty="dashed",col=f_col, lwd=2)
  lines(x = c(5, 10), y = rep(median(preds_both[,11]),2), lty="dashed",col=f_col, lwd=2)
  
  # 10 to 20 diff
  lines(x = rep(10,2), y = c(median(preds_both[,11]), median(preds_both[,21])), lty="dashed",col=f_col, lwd=2)
  lines(x = c(10, 20), y = rep(median(preds_both[,21]),2), lty="dashed",col=f_col, lwd=2)

dev.off()

#####################################################
#### Different source of variance ###################

sigma_k <- data.frame(
  sex = post$sigma_sex[,1],
  outcome = post$sigma_outcome[,1],
  outcome_sex = (post$sigma_outcome[,8] + post$sigma_outcome[,15])/2,
  resource = post$sigma_resource[,1],
  resource_sex = (post$sigma_resource[,8] + post$sigma_resource[,15])/2
)

sigma_b <- data.frame(
  sex = post$sigma_sex[,2],
  outcome = post$sigma_outcome[,2],
  outcome_sex = (post$sigma_outcome[,9] + post$sigma_outcome[,16])/2,
  resource = post$sigma_resource[,2],
  resource_sex = (post$sigma_resource[,9] + post$sigma_resource[,16])/2
)

sigma_eta_p <- data.frame(
  sex = post$sigma_sex[,3],
  outcome = post$sigma_outcome[,3],
  outcome_sex = (post$sigma_outcome[,10] + post$sigma_outcome[,17])/2,
  resource = post$sigma_resource[,3],
  resource_sex = (post$sigma_resource[,10] + post$sigma_resource[,17])/2
)

sigma_eta_mu <- data.frame(
  sex = post$sigma_sex[,4],
  outcome = post$sigma_outcome[,4],
  resource = post$sigma_resource[,4],
  samp = 1:n_samps
)

sigma_eta_mu_long <- sigma_eta_mu %>% pivot_longer(-samp)

ggplot(sigma_eta_mu_long, aes(x = value)) + geom_density(aes(color=name))

# 1 = marine
# 2 = game/mixed
# 3 = fruit
# 4 = USOs

k_marine <- exp(post$a[,1] + post$resource_v[,1,1])
k_game <- exp(post$a[,1] + post$resource_v[,2,1])
k_fruit <- exp(post$a[,1] + post$resource_v[,3,1])
k_USO <- exp(post$a[,1] + post$resource_v[,4,1])

b_marine <- exp(post$a[,2] + post$resource_v[,1,2])
b_game <- exp(post$a[,2] + post$resource_v[,2,2])
b_fruit <- exp(post$a[,2] + post$resource_v[,3,2])
b_USO <- exp(post$a[,2] + post$resource_v[,4,2])

eta_marine <- exp(post$a[,4] + post$resource_v[,1,4])
eta_game <- exp(post$a[,4] + post$resource_v[,2,4])
eta_fruit <- exp(post$a[,4] + post$resource_v[,3,4])
eta_USO <- exp(post$a[,4] + post$resource_v[,4,4])









