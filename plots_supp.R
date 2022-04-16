library(tidyverse)
library(rethinking)
library(ggridges)
library(patchwork)
library(scales)
source("cfr_functions.R")

##### Read in study data
d <- read_csv("data.csv")

### Re-create indices for resource and outcome
d$resource_id <- match(d$resource_cat, unique(d$resource_cat))
d$outcome_id <- match(d$outcome, unique(d$outcome))

### Get resource id to link model fit indices with dataset
d_r <- d %>% 
  group_by(resource_cat) %>% 
  summarise(id = unique(resource_id))

### Get outcome id, resource type, and age range for each study*outcome
d_outcome <- d %>% 
  group_by(outcome) %>% 
  summarise(id = unique(outcome_id),
            resource_label = unique(resource),
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

##### Like Figure 4, but for skill ###################################
## 4a: overall skill ~ age
pdf(file = "skill_avg.pdf", width = 6, height = 8)
par(pty='s',
    oma=c(0,0,0,0),
    mai = c(0.5,0.5,0.5,0.5),
    cex=1.3
)

# Get model predictions across ages
preds_both <- cfr_pred(age=age_seq, resp="S_returns")

# Set up plot window
plot(NULL, ylim=c(0,max(apply(preds_both, 2, median))+0.4), xlim=c(0,20), ylab="", xlab="", axes=F)
# Plot posterior median age seq, then PI
lines(apply(preds_both, 2, median), x=age_seq, lwd=3)
shade(apply(preds_both, 2, PI, prob=0.9), age_seq, col=col.alpha("black",0.05))
shade(apply(preds_both, 2, PI, prob=0.6), age_seq, col=col.alpha("black",0.05))
shade(apply(preds_both, 2, PI, prob=0.3), age_seq, col=col.alpha("black",0.05))

# Axis labels and ticks
axis(1, at=c(0,5,10,15,20), tck=-0.02, labels=NA)
axis(1, at=c(0,5,10,15,20), tck=0, lwd=0, line=-0.5)

# Now, draw differentials (dashed lines)
# 0 to 5 diff 
lines(x = rep(0,2), y = c(0, median(preds_both[,6])), lty="dashed", lwd=2)
lines(x = c(0, 5), y = rep(median(preds_both[,6]),2), lty="dashed", lwd=2)

# 5 to 10 diff 
lines(x = rep(5,2), y = c(median(preds_both[,6]), median(preds_both[,11])), lty="dashed", lwd=2)
lines(x = c(5, 10), y = rep(median(preds_both[,11]),2), lty="dashed", lwd=2)

# 10 to 20 diff
lines(x = rep(10,2), y = c(median(preds_both[,11]), median(preds_both[,21])), lty="dashed", lwd=2)
lines(x = c(10, 20), y = rep(median(preds_both[,21]),2), lty="dashed", lwd=2)

dev.off() # end plot

## 4b: Returns ~ age*resource
pdf(file = "resource_skill.pdf", width = 8, height = 8)
par(mfrow=c(2,2),
    pty='s',
    oma=c(0,0,0,0),
    mai = c(0.5,0.5,0.5,0.5),
    cex=1.3
)

for (r in 1:4) {
  
  d_outcome_temp <- filter(d_outcome, resource == resource_seq[r])
  
  #### Average curve ################
  preds_both <- cfr_pred(age=age_seq, resp="S_returns", resource = resource_seq[r])
  
  max_height <- max(apply(preds_both, 2, median))
  preds_both <- preds_both / max_height
  
  plot(NULL, ylim=c(0,1), xlim=c(0,20), ylab="", xlab="", axes=F)
  
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
    
    preds_both <- cfr_pred(age=age_seq, resp="S_returns", resource = resource_seq[r], outcome = d_outcome_temp$id[s])
    
    max_height <- max(apply(preds_both, 2, median))
    preds_both <- preds_both / max_height
    
    lines(apply(preds_both, 2, median), x=age_seq, lwd=1, col=col.alpha(resource_cols[r], 0.4))
  }
  
}
dev.off()

## 4c: Differentials by resource type
age_5 <- matrix(NA, nrow=n_samps, ncol=5)
age_10 <- matrix(NA, nrow=n_samps, ncol=5)
age_20 <- matrix(NA, nrow=n_samps, ncol=5)

preds <- cfr_pred(age=age_seq, resp="S_returns")

for (i in 1:nrow(preds)) preds[i,] <- preds[i,] / max(preds[i,])
age_5[,1] = preds[,6]
age_10[,1] = preds[,11] - preds[,6]
age_20[,1] = preds[,21] - preds[,11]

for (r in 1:4) {
  preds <- cfr_pred(age=age_seq, resp="S_returns", resource=resource_seq[r])
  
  for (i in 1:nrow(preds)) preds[i,] <- preds[i,] / max(preds[i,])
  age_5[,r+1] = preds[,6]
  age_10[,r+1] = preds[,11] - preds[,6]
  age_20[,r+1] = preds[,21] - preds[,11]
}

age_5 <- as.data.frame(age_5); age_10 <- as.data.frame(age_10); age_20 <- as.data.frame(age_20)

names(age_5) <- c("Average", "Fish/Shellfish", "Game", "Fruit", "USOs"); names(age_10) <- names(age_5); names(age_20) <- names(age_5)

age_5$samp <- 1:n_samps
age_10$samp <- 1:n_samps
age_20$samp <- 1:n_samps

age_5_long <- age_5 %>% pivot_longer(-samp)
age_10_long <- age_10 %>% pivot_longer(-samp)
age_20_long <- age_20 %>% pivot_longer(-samp)

age_long <- bind_rows(age_5_long, age_10_long)
age_long <- bind_rows(age_long, age_20_long)

age_long$age <- rep(c("5", "10", "20"), each=nrow(age_5_long))

age_long_summary <- age_long %>% 
  group_by(age, name) %>% 
  summarise(med_diff = median(value),
            lower = HPDI(value, prob=0.9)[1],
            upper = HPDI(value, prob=0.9)[2]
  )

age_long_summary$name <- factor(age_long_summary$name, levels=rev(c("Game","USOs","Average","Fish/Shellfish","Fruit")))
age_long_summary$age <- factor(age_long_summary$age, levels=c("5","10","20"), labels=c("Ages 0 - 5", "Ages 5 - 10", "Ages 10 - 20"))

age_returns_plot <- ggplot(age_long_summary, aes(x = med_diff, y = name)) +
  facet_wrap(~age) + 
  geom_point(size=3, aes(color=name)) +
  geom_errorbarh(aes(xmin=lower, xmax=upper, y=name, color=name), height=0, lwd=1.5) +
  scale_x_continuous( labels = label_percent()) +
  scale_color_manual(values=c(resource_cols[3],resource_cols[1], "black",resource_cols[4],resource_cols[2])) +
  theme_bw(base_size=16) +
  theme(legend.position = 'none',
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        strip.background = element_rect(colour="black", fill="white"),
        panel.spacing = unit(2, "lines")) +
  xlab("% Increase in Foraging Skill") +
  ylab("")

ggsave("skill_age_plot.pdf", width=11, height=6, dpi=600)

#####################################################
#### Like Figure 4, but for Males (returns) #########
## 4a: overall skill ~ age
pdf(file = "returns_avg_male.pdf", width = 6, height = 8)
par(pty='s',
    oma=c(0,0,0,0),
    mai = c(0.5,0.5,0.5,0.5),
    cex=1.3
)

# Get model predictions across ages
preds_both <- cfr_pred(age=age_seq, resp="nodim_returns", male=1)

# Set up plot window
plot(NULL, ylim=c(0,max(apply(preds_both, 2, median))+0.4), xlim=c(0,20), ylab="", xlab="", axes=F)
# Plot posterior median age seq, then PI
lines(apply(preds_both, 2, median), x=age_seq, lwd=3)
shade(apply(preds_both, 2, PI, prob=0.9), age_seq, col=col.alpha("black",0.05))
shade(apply(preds_both, 2, PI, prob=0.6), age_seq, col=col.alpha("black",0.05))
shade(apply(preds_both, 2, PI, prob=0.3), age_seq, col=col.alpha("black",0.05))

# Axis labels and ticks
axis(1, at=c(0,5,10,15,20), tck=-0.02, labels=NA)
axis(1, at=c(0,5,10,15,20), tck=0, lwd=0, line=-0.5)

# Now, draw differentials (dashed lines)
# 0 to 5 diff 
lines(x = rep(0,2), y = c(0, median(preds_both[,6])), lty="dashed", lwd=2)
lines(x = c(0, 5), y = rep(median(preds_both[,6]),2), lty="dashed", lwd=2)

# 5 to 10 diff 
lines(x = rep(5,2), y = c(median(preds_both[,6]), median(preds_both[,11])), lty="dashed", lwd=2)
lines(x = c(5, 10), y = rep(median(preds_both[,11]),2), lty="dashed", lwd=2)

# 10 to 20 diff
lines(x = rep(10,2), y = c(median(preds_both[,11]), median(preds_both[,21])), lty="dashed", lwd=2)
lines(x = c(10, 20), y = rep(median(preds_both[,21]),2), lty="dashed", lwd=2)

dev.off() # end plot

## 4b: Returns ~ age*resource
pdf(file = "resource_returns_male.pdf", width = 8, height= 8)
par(mfrow=c(2,2),
    pty='s',
    oma=c(0,0,0,0),
    mai = c(0.5,0.5,0.5,0.5),
    cex=1.3
)

for (r in 1:4) {
  
  d_outcome_temp <- filter(d_outcome, resource == resource_seq[r])
  
  #### Average curve ################
  preds_both <- cfr_pred(age=age_seq, resp="nodim_returns", male=1, resource = resource_seq[r])
  
  max_height <- max(apply(preds_both, 2, median))
  preds_both <- preds_both / max_height
  
  plot(NULL, ylim=c(0,1), xlim=c(0,20), ylab="", xlab="", axes=F)
  
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
    
    preds_both <- cfr_pred(age=age_seq, resp="nodim_returns", male=1, resource = resource_seq[r], outcome = d_outcome_temp$id[s])
    
    max_height <- max(apply(preds_both, 2, median))
    preds_both <- preds_both / max_height
    
    lines(apply(preds_both, 2, median), x=age_seq, lwd=1, col=col.alpha(resource_cols[r], 0.4))
  }
  
}
dev.off()

## 4c: Differentials by resource type
age_5 <- matrix(NA, nrow=n_samps, ncol=5)
age_10 <- matrix(NA, nrow=n_samps, ncol=5)
age_20 <- matrix(NA, nrow=n_samps, ncol=5)

preds <- cfr_pred(age=age_seq, resp="nodim_returns", male=1)

for (i in 1:nrow(preds)) preds[i,] <- preds[i,] / max(preds[i,])
age_5[,1] = preds[,6]
age_10[,1] = preds[,11] - preds[,6]
age_20[,1] = preds[,21] - preds[,11]

for (r in 1:4) {
  preds <- cfr_pred(age=age_seq, resp="nodim_returns", male=1, resource=resource_seq[r])
  
  for (i in 1:nrow(preds)) preds[i,] <- preds[i,] / max(preds[i,])
  age_5[,r+1] = preds[,6]
  age_10[,r+1] = preds[,11] - preds[,6]
  age_20[,r+1] = preds[,21] - preds[,11]
}

age_5 <- as.data.frame(age_5); age_10 <- as.data.frame(age_10); age_20 <- as.data.frame(age_20)

names(age_5) <- c("Average", "Fish/Shellfish", "Game", "Fruit", "USOs"); names(age_10) <- names(age_5); names(age_20) <- names(age_5)

age_5$samp <- 1:n_samps
age_10$samp <- 1:n_samps
age_20$samp <- 1:n_samps

age_5_long <- age_5 %>% pivot_longer(-samp)
age_10_long <- age_10 %>% pivot_longer(-samp)
age_20_long <- age_20 %>% pivot_longer(-samp)

age_long <- bind_rows(age_5_long, age_10_long)
age_long <- bind_rows(age_long, age_20_long)

age_long$age <- rep(c("5", "10", "20"), each=nrow(age_5_long))

age_long_summary <- age_long %>% 
  group_by(age, name) %>% 
  summarise(med_diff = median(value),
            lower = HPDI(value, prob=0.9)[1],
            upper = HPDI(value, prob=0.9)[2]
  )

age_long_summary$name <- factor(age_long_summary$name, levels=rev(c("Game","USOs","Average","Fish/Shellfish","Fruit")))
age_long_summary$age <- factor(age_long_summary$age, levels=c("5","10","20"), labels=c("Ages 0 - 5", "Ages 5 - 10", "Ages 10 - 20"))

age_returns_plot <- ggplot(age_long_summary, aes(x = med_diff, y = name)) +
  facet_wrap(~age) + 
  geom_point(size=3, aes(color=name)) +
  geom_errorbarh(aes(xmin=lower, xmax=upper, y=name, color=name), height=0, lwd=1.5) +
  scale_x_continuous( labels = label_percent()) +
  scale_color_manual(values=c(resource_cols[3],resource_cols[1], "black",resource_cols[4],resource_cols[2])) +
  theme_bw(base_size=16) +
  theme(legend.position = 'none',
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        strip.background = element_rect(colour="black", fill="white"),
        panel.spacing = unit(2, "lines")) +
  xlab("% Increase in Foraging Skill") +
  ylab("")

ggsave("returns_age_plot_male.pdf", width=11, height=6, dpi=600)
#####################################################

#### Fig 4, but females (returns) ###################
pdf(file = "returns_avg_female.pdf", width = 6, height = 8)
par(pty='s',
    oma=c(0,0,0,0),
    mai = c(0.5,0.5,0.5,0.5),
    cex=1.3
)

# Get model predictions across ages
preds_both <- cfr_pred(age=age_seq, resp="nodim_returns", male=0)

# Set up plot window
plot(NULL, ylim=c(0,max(apply(preds_both, 2, median))+0.4), xlim=c(0,20), ylab="", xlab="", axes=F)
# Plot posterior median age seq, then PI
lines(apply(preds_both, 2, median), x=age_seq, lwd=3)
shade(apply(preds_both, 2, PI, prob=0.9), age_seq, col=col.alpha("black",0.05))
shade(apply(preds_both, 2, PI, prob=0.6), age_seq, col=col.alpha("black",0.05))
shade(apply(preds_both, 2, PI, prob=0.3), age_seq, col=col.alpha("black",0.05))

# Axis labels and ticks
axis(1, at=c(0,5,10,15,20), tck=-0.02, labels=NA)
axis(1, at=c(0,5,10,15,20), tck=0, lwd=0, line=-0.5)

# Now, draw differentials (dashed lines)
# 0 to 5 diff 
lines(x = rep(0,2), y = c(0, median(preds_both[,6])), lty="dashed", lwd=2)
lines(x = c(0, 5), y = rep(median(preds_both[,6]),2), lty="dashed", lwd=2)

# 5 to 10 diff 
lines(x = rep(5,2), y = c(median(preds_both[,6]), median(preds_both[,11])), lty="dashed", lwd=2)
lines(x = c(5, 10), y = rep(median(preds_both[,11]),2), lty="dashed", lwd=2)

# 10 to 20 diff
lines(x = rep(10,2), y = c(median(preds_both[,11]), median(preds_both[,21])), lty="dashed", lwd=2)
lines(x = c(10, 20), y = rep(median(preds_both[,21]),2), lty="dashed", lwd=2)

dev.off() # end plot

## 4b: Returns ~ age*resource
pdf(file = "resource_returns_female.pdf", width = 8, height= 8)
par(mfrow=c(2,2),
    pty='s',
    oma=c(0,0,0,0),
    mai = c(0.5,0.5,0.5,0.5),
    cex=1.3
)

for (r in 1:4) {
  
  d_outcome_temp <- filter(d_outcome, resource == resource_seq[r])
  
  #### Average curve ################
  preds_both <- cfr_pred(age=age_seq, resp="nodim_returns", male=0, resource = resource_seq[r])
  
  max_height <- max(apply(preds_both, 2, median))
  preds_both <- preds_both / max_height
  
  plot(NULL, ylim=c(0,1), xlim=c(0,20), ylab="", xlab="", axes=F)
  
  axis(1, at=c(0,5,10,15,20), tck=-0.02, labels=NA)
  axis(1, at=c(0,5,10,15,20), tck=0, lwd=0, line=-0.5)
  
  mtext(resource_names[r], cex=1.25)
  mtext(ifelse(r %in% c(1,3), "Skill", ""), side=2, cex=1.25, line=1)
  
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
    
    preds_both <- cfr_pred(age=age_seq, resp="nodim_returns", male=0, resource = resource_seq[r], outcome = d_outcome_temp$id[s])
    
    max_height <- max(apply(preds_both, 2, median))
    preds_both <- preds_both / max_height
    
    lines(apply(preds_both, 2, median), x=age_seq, lwd=1, col=col.alpha(resource_cols[r], 0.4))
  }
  
}
dev.off()

## 4c: Differentials by resource type
age_5 <- matrix(NA, nrow=n_samps, ncol=5)
age_10 <- matrix(NA, nrow=n_samps, ncol=5)
age_20 <- matrix(NA, nrow=n_samps, ncol=5)

preds <- cfr_pred(age=age_seq, resp="nodim_returns", male=0)

for (i in 1:nrow(preds)) preds[i,] <- preds[i,] / max(preds[i,])
age_5[,1] = preds[,6]
age_10[,1] = preds[,11] - preds[,6]
age_20[,1] = preds[,21] - preds[,11]

for (r in 1:4) {
  preds <- cfr_pred(age=age_seq, resp="nodim_returns", male=0, resource=resource_seq[r])
  
  for (i in 1:nrow(preds)) preds[i,] <- preds[i,] / max(preds[i,])
  age_5[,r+1] = preds[,6]
  age_10[,r+1] = preds[,11] - preds[,6]
  age_20[,r+1] = preds[,21] - preds[,11]
}

age_5 <- as.data.frame(age_5); age_10 <- as.data.frame(age_10); age_20 <- as.data.frame(age_20)

names(age_5) <- c("Average", "Fish/Shellfish", "Game", "Fruit", "USOs"); names(age_10) <- names(age_5); names(age_20) <- names(age_5)

age_5$samp <- 1:n_samps
age_10$samp <- 1:n_samps
age_20$samp <- 1:n_samps

age_5_long <- age_5 %>% pivot_longer(-samp)
age_10_long <- age_10 %>% pivot_longer(-samp)
age_20_long <- age_20 %>% pivot_longer(-samp)

age_long <- bind_rows(age_5_long, age_10_long)
age_long <- bind_rows(age_long, age_20_long)

age_long$age <- rep(c("5", "10", "20"), each=nrow(age_5_long))

age_long_summary <- age_long %>% 
  group_by(age, name) %>% 
  summarise(med_diff = median(value),
            lower = HPDI(value, prob=0.9)[1],
            upper = HPDI(value, prob=0.9)[2]
  )

age_long_summary$name <- factor(age_long_summary$name, levels=rev(c("Game","USOs","Average","Fish/Shellfish","Fruit")))
age_long_summary$age <- factor(age_long_summary$age, levels=c("5","10","20"), labels=c("Ages 0 - 5", "Ages 5 - 10", "Ages 10 - 20"))

age_returns_plot <- ggplot(age_long_summary, aes(x = med_diff, y = name)) +
  facet_wrap(~age) + 
  geom_point(size=3, aes(color=name)) +
  geom_errorbarh(aes(xmin=lower, xmax=upper, y=name, color=name), height=0, lwd=1.5) +
  scale_x_continuous( labels = label_percent()) +
  scale_color_manual(values=c(resource_cols[3],resource_cols[1], "black",resource_cols[4],resource_cols[2])) +
  theme_bw(base_size=16) +
  theme(legend.position = 'none',
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        strip.background = element_rect(colour="black", fill="white"),
        panel.spacing = unit(2, "lines")) +
  xlab("% Increase in Foraging Skill") +
  ylab("")

ggsave("returns_age_plot_female.pdf", width=11, height=6, dpi=600)





#####################################################
##### Like Figure 4, but for skill and MALES ########
## 4a: overall skill ~ age
pdf(file = "skill_avg_male.pdf", width = 6, height = 8)
par(pty='s',
    oma=c(0,0,0,0),
    mai = c(0.5,0.5,0.5,0.5),
    cex=1.3
)

# Get model predictions across ages
preds_both <- cfr_pred(age=age_seq, resp="S_returns", male=1)

# Set up plot window
plot(NULL, ylim=c(0,max(apply(preds_both, 2, median))+0.4), xlim=c(0,20), ylab="", xlab="", axes=F)
# Plot posterior median age seq, then PI
lines(apply(preds_both, 2, median), x=age_seq, lwd=3)
shade(apply(preds_both, 2, PI, prob=0.9), age_seq, col=col.alpha("black",0.05))
shade(apply(preds_both, 2, PI, prob=0.6), age_seq, col=col.alpha("black",0.05))
shade(apply(preds_both, 2, PI, prob=0.3), age_seq, col=col.alpha("black",0.05))

# Axis labels and ticks
axis(1, at=c(0,5,10,15,20), tck=-0.02, labels=NA)
axis(1, at=c(0,5,10,15,20), tck=0, lwd=0, line=-0.5)

# Now, draw differentials (dashed lines)
# 0 to 5 diff 
lines(x = rep(0,2), y = c(0, median(preds_both[,6])), lty="dashed", lwd=2)
lines(x = c(0, 5), y = rep(median(preds_both[,6]),2), lty="dashed", lwd=2)

# 5 to 10 diff 
lines(x = rep(5,2), y = c(median(preds_both[,6]), median(preds_both[,11])), lty="dashed", lwd=2)
lines(x = c(5, 10), y = rep(median(preds_both[,11]),2), lty="dashed", lwd=2)

# 10 to 20 diff
lines(x = rep(10,2), y = c(median(preds_both[,11]), median(preds_both[,21])), lty="dashed", lwd=2)
lines(x = c(10, 20), y = rep(median(preds_both[,21]),2), lty="dashed", lwd=2)

dev.off() # end plot

## 4b: Returns ~ age*resource
pdf(file = "resource_skill_male.pdf", width = 8, height= 8)
par(mfrow=c(2,2),
    pty='s',
    oma=c(0,0,0,0),
    mai = c(0.5,0.5,0.5,0.5),
    cex=1.3
)

for (r in 1:4) {
  
  d_outcome_temp <- filter(d_outcome, resource == resource_seq[r])
  
  #### Average curve ################
  preds_both <- cfr_pred(age=age_seq, resp="S_returns", male=1, resource = resource_seq[r])
  
  max_height <- max(apply(preds_both, 2, median))
  preds_both <- preds_both / max_height
  
  plot(NULL, ylim=c(0,1), xlim=c(0,20), ylab="", xlab="", axes=F)
  
  axis(1, at=c(0,5,10,15,20), tck=-0.02, labels=NA)
  axis(1, at=c(0,5,10,15,20), tck=0, lwd=0, line=-0.5)
  
  mtext(resource_names[r], cex=1.25)
  mtext(ifelse(r %in% c(1,3), "Skill", ""), side=2, cex=1.25, line=1)
  
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
    
    preds_both <- cfr_pred(age=age_seq, resp="S_returns", male=1, resource = resource_seq[r], outcome = d_outcome_temp$id[s])
    
    max_height <- max(apply(preds_both, 2, median))
    preds_both <- preds_both / max_height
    
    lines(apply(preds_both, 2, median), x=age_seq, lwd=1, col=col.alpha(resource_cols[r], 0.4))
  }
  
}
dev.off()

## 4c: Differentials by resource type
age_5 <- matrix(NA, nrow=n_samps, ncol=5)
age_10 <- matrix(NA, nrow=n_samps, ncol=5)
age_20 <- matrix(NA, nrow=n_samps, ncol=5)

preds <- cfr_pred(age=age_seq, resp="S_returns", male=1)

for (i in 1:nrow(preds)) preds[i,] <- preds[i,] / max(preds[i,])
age_5[,1] = preds[,6]
age_10[,1] = preds[,11] - preds[,6]
age_20[,1] = preds[,21] - preds[,11]

for (r in 1:4) {
  preds <- cfr_pred(age=age_seq, resp="S_returns", male=1, resource=resource_seq[r])
  
  for (i in 1:nrow(preds)) preds[i,] <- preds[i,] / max(preds[i,])
  age_5[,r+1] = preds[,6]
  age_10[,r+1] = preds[,11] - preds[,6]
  age_20[,r+1] = preds[,21] - preds[,11]
}

age_5 <- as.data.frame(age_5); age_10 <- as.data.frame(age_10); age_20 <- as.data.frame(age_20)

names(age_5) <- c("Average", "Fish/Shellfish", "Game", "Fruit", "USOs"); names(age_10) <- names(age_5); names(age_20) <- names(age_5)

age_5$samp <- 1:n_samps
age_10$samp <- 1:n_samps
age_20$samp <- 1:n_samps

age_5_long <- age_5 %>% pivot_longer(-samp)
age_10_long <- age_10 %>% pivot_longer(-samp)
age_20_long <- age_20 %>% pivot_longer(-samp)

age_long <- bind_rows(age_5_long, age_10_long)
age_long <- bind_rows(age_long, age_20_long)

age_long$age <- rep(c("5", "10", "20"), each=nrow(age_5_long))

age_long_summary <- age_long %>% 
  group_by(age, name) %>% 
  summarise(med_diff = median(value),
            lower = HPDI(value, prob=0.9)[1],
            upper = HPDI(value, prob=0.9)[2]
  )

age_long_summary$name <- factor(age_long_summary$name, levels=rev(c("Game","USOs","Average","Fish/Shellfish","Fruit")))
age_long_summary$age <- factor(age_long_summary$age, levels=c("5","10","20"), labels=c("Ages 0 - 5", "Ages 5 - 10", "Ages 10 - 20"))

age_returns_plot <- ggplot(age_long_summary, aes(x = med_diff, y = name)) +
  facet_wrap(~age) + 
  geom_point(size=3, aes(color=name)) +
  geom_errorbarh(aes(xmin=lower, xmax=upper, y=name, color=name), height=0, lwd=1.5) +
  scale_x_continuous( labels = label_percent()) +
  scale_color_manual(values=c(resource_cols[3],resource_cols[1], "black",resource_cols[4],resource_cols[2])) +
  theme_bw(base_size=16) +
  theme(legend.position = 'none',
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        strip.background = element_rect(colour="black", fill="white"),
        panel.spacing = unit(2, "lines")) +
  xlab("% Increase in Foraging Skill") +
  ylab("")

ggsave("skill_age_plot_male.pdf", width=11, height=6, dpi=600)

#### Fig 4, but for skill and females ###################
#####################################################
pdf(file = "skill_avg_female.pdf", width = 6, height = 8)
par(pty='s',
    oma=c(0,0,0,0),
    mai = c(0.5,0.5,0.5,0.5),
    cex=1.3
)

# Get model predictions across ages
preds_both <- cfr_pred(age=age_seq, resp="S_returns", male=0)

# Set up plot window
plot(NULL, ylim=c(0,max(apply(preds_both, 2, median))+0.4), xlim=c(0,20), ylab="", xlab="", axes=F)
# Plot posterior median age seq, then PI
lines(apply(preds_both, 2, median), x=age_seq, lwd=3)
shade(apply(preds_both, 2, PI, prob=0.9), age_seq, col=col.alpha("black",0.05))
shade(apply(preds_both, 2, PI, prob=0.6), age_seq, col=col.alpha("black",0.05))
shade(apply(preds_both, 2, PI, prob=0.3), age_seq, col=col.alpha("black",0.05))

# Axis labels and ticks
axis(1, at=c(0,5,10,15,20), tck=-0.02, labels=NA)
axis(1, at=c(0,5,10,15,20), tck=0, lwd=0, line=-0.5)

# Now, draw differentials (dashed lines)
# 0 to 5 diff 
lines(x = rep(0,2), y = c(0, median(preds_both[,6])), lty="dashed", lwd=2)
lines(x = c(0, 5), y = rep(median(preds_both[,6]),2), lty="dashed", lwd=2)

# 5 to 10 diff 
lines(x = rep(5,2), y = c(median(preds_both[,6]), median(preds_both[,11])), lty="dashed", lwd=2)
lines(x = c(5, 10), y = rep(median(preds_both[,11]),2), lty="dashed", lwd=2)

# 10 to 20 diff
lines(x = rep(10,2), y = c(median(preds_both[,11]), median(preds_both[,21])), lty="dashed", lwd=2)
lines(x = c(10, 20), y = rep(median(preds_both[,21]),2), lty="dashed", lwd=2)

dev.off() # end plot

## 4b: Returns ~ age*resource
pdf(file = "resource_skill_female.pdf", width = 8, height= 8)
par(mfrow=c(2,2),
    pty='s',
    oma=c(0,0,0,0),
    mai = c(0.5,0.5,0.5,0.5),
    cex=1.3
)

for (r in 1:4) {
  
  d_outcome_temp <- filter(d_outcome, resource == resource_seq[r])
  
  #### Average curve ################
  preds_both <- cfr_pred(age=age_seq, resp="S_returns", male=0, resource = resource_seq[r])
  
  max_height <- max(apply(preds_both, 2, median))
  preds_both <- preds_both / max_height
  
  plot(NULL, ylim=c(0,1), xlim=c(0,20), ylab="", xlab="", axes=F)
  
  axis(1, at=c(0,5,10,15,20), tck=-0.02, labels=NA)
  axis(1, at=c(0,5,10,15,20), tck=0, lwd=0, line=-0.5)
  
  mtext(resource_names[r], cex=1.25)
  mtext(ifelse(r %in% c(1,3), "Skill", ""), side=2, cex=1.25, line=1)
  
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
    
    preds_both <- cfr_pred(age=age_seq, resp="S_returns", male=0, resource = resource_seq[r], outcome = d_outcome_temp$id[s])
    
    max_height <- max(apply(preds_both, 2, median))
    preds_both <- preds_both / max_height
    
    lines(apply(preds_both, 2, median), x=age_seq, lwd=1, col=col.alpha(resource_cols[r], 0.4))
  }
  
}
dev.off()

## 4c: Differentials by resource type
age_5 <- matrix(NA, nrow=n_samps, ncol=5)
age_10 <- matrix(NA, nrow=n_samps, ncol=5)
age_20 <- matrix(NA, nrow=n_samps, ncol=5)

preds <- cfr_pred(age=age_seq, resp="S_returns", male=0)

for (i in 1:nrow(preds)) preds[i,] <- preds[i,] / max(preds[i,])
age_5[,1] = preds[,6]
age_10[,1] = preds[,11] - preds[,6]
age_20[,1] = preds[,21] - preds[,11]

for (r in 1:4) {
  preds <- cfr_pred(age=age_seq, resp="S_returns", male=0, resource=resource_seq[r])
  
  for (i in 1:nrow(preds)) preds[i,] <- preds[i,] / max(preds[i,])
  age_5[,r+1] = preds[,6]
  age_10[,r+1] = preds[,11] - preds[,6]
  age_20[,r+1] = preds[,21] - preds[,11]
}

age_5 <- as.data.frame(age_5); age_10 <- as.data.frame(age_10); age_20 <- as.data.frame(age_20)

names(age_5) <- c("Average", "Fish/Shellfish", "Game", "Fruit", "USOs"); names(age_10) <- names(age_5); names(age_20) <- names(age_5)

age_5$samp <- 1:n_samps
age_10$samp <- 1:n_samps
age_20$samp <- 1:n_samps

age_5_long <- age_5 %>% pivot_longer(-samp)
age_10_long <- age_10 %>% pivot_longer(-samp)
age_20_long <- age_20 %>% pivot_longer(-samp)

age_long <- bind_rows(age_5_long, age_10_long)
age_long <- bind_rows(age_long, age_20_long)

age_long$age <- rep(c("5", "10", "20"), each=nrow(age_5_long))

age_long_summary <- age_long %>% 
  group_by(age, name) %>% 
  summarise(med_diff = median(value),
            lower = HPDI(value, prob=0.9)[1],
            upper = HPDI(value, prob=0.9)[2]
  )

age_long_summary$name <- factor(age_long_summary$name, levels=rev(c("Game","USOs","Average","Fish/Shellfish","Fruit")))
age_long_summary$age <- factor(age_long_summary$age, levels=c("5","10","20"), labels=c("Ages 0 - 5", "Ages 5 - 10", "Ages 10 - 20"))

age_returns_plot <- ggplot(age_long_summary, aes(x = med_diff, y = name)) +
  facet_wrap(~age) + 
  geom_point(size=3, aes(color=name)) +
  geom_errorbarh(aes(xmin=lower, xmax=upper, y=name, color=name), height=0, lwd=1.5) +
  scale_x_continuous( labels = label_percent()) +
  scale_color_manual(values=c(resource_cols[3],resource_cols[1], "black",resource_cols[4],resource_cols[2])) +
  theme_bw(base_size=16) +
  theme(legend.position = 'none',
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        strip.background = element_rect(colour="black", fill="white"),
        panel.spacing = unit(2, "lines")) +
  xlab("% Increase in Foraging Skill") +
  ylab("")

ggsave("skill_age_plot_female.pdf", width=11, height=6, dpi=600)


#####################################################
##########################################
#### Elasticity parameters (outcome specific)
eta_outcome <- matrix(NA, nrow=n_samps, ncol=max(d$outcome_id))

d_outcome_sort <- d_outcome %>% 
  arrange(id) %>% 
  mutate(outcome = str_replace_all(outcome, "_", " "))

for (s in 1:nrow(d_outcome_sort)) {
  eta_outcome[,s] = log( 1 + exp(post$a[,4] + post$resource_v[,d_outcome_sort$resource[s],4] + post$outcome_v[,d_outcome_sort$id[s],4]))
}

eta_outcome <- as.data.frame(eta_outcome)
names(eta_outcome) <- d_outcome_sort$id
eta_outcome$samp <- 1:n_samps

eta_outcome_long <- eta_outcome %>% 
  pivot_longer(-samp, names_to = "id") %>% 
  mutate(id = as.numeric(id))

# match up with resource data
eta_outcome_long <- left_join(eta_outcome_long, d_outcome_sort)

eta_outcome_summary <- eta_outcome_long %>%
  group_by(id) %>% 
  summarise(
    median_eta = median(value), resource=unique(resource),outcome = unique(outcome)) %>% 
  mutate(
    id = fct_reorder(as.factor(id), median_eta),
    resource = factor(resource, labels = c("Game", "Fish/Shellfish", "Mixed/Other", "Fruit", "USOs")))

ggplot(eta_outcome_summary, aes(x = median_eta, y = id)) +
  geom_point(aes(color=resource)) + 
  scale_color_manual(values = c(resource_cols[c(2,1)], "black", resource_cols[c(3,4)])) +
  geom_vline(aes(xintercept=1), alpha=0.8, lty="dotted") +
  scale_y_discrete(breaks=1:nrow(eta_outcome_summary), labels=eta_outcome_summary$outcome) +
  ylab("") +
  xlab(expression(eta)) +
  theme_minimal(base_size = 12) +
  theme(legend.title = element_blank()) 

ggsave("eta_outcome.pdf", dpi=600, height=11, width=8.5, units="in")

##########################################
#### Hurdle parameters (outcome specific)
d$obs_id <- 1:nrow(d)
d$pred_return <- NA

prob_return <- matrix(NA, nrow=n_samps, ncol=nrow(d))

for (n in 1:nrow(d)) {
  sex = ifelse(is.na(d$sex[n]), 0.5, d$sex[n])
  prob_return[,n] = (2*(inv_logit(post$mu_p[,n,1]) - 0.5))*(1-sex) + (2*(inv_logit(post$mu_p[,n,2]) - 0.5))*(sex)
}

prob_return <- as.data.frame(prob_return)
names(prob_return) <- d$obs_id
prob_return$samp <- 1:n_samps

prob_return_long <- prob_return %>% 
  pivot_longer(-samp, names_to = "obs_id") %>% 
  mutate(obs_id = as.integer(obs_id))

prob_return_long <- left_join(prob_return_long, select(d, c(obs_id, outcome_id)))

mean_prob_return <- prob_return_long %>% group_by(samp, outcome_id) %>% 
  summarise(pred_prop_nonzero = mean(value))

# get median and credible intervals
mean_prob_return_summary <- mean_prob_return %>% 
  group_by(outcome_id) %>% 
  summarise(
    med = median(pred_prop_nonzero),
    lower = PI(pred_prop_nonzero, prob=0.9)[1],
    upper = PI(pred_prop_nonzero, prob=0.9)[2])

d_outcome <- d %>% 
  group_by(outcome) %>% 
  summarise(outcome_id = unique(outcome_id),
            resource_label = unique(resource),
            resource = unique(resource_id),
            prop_nonzero = mean(scaled_return > 0)
  ) %>% 
  mutate(short_name = str_extract(outcome, "[^_]+"),
         resource2 = factor(resource, labels = c("Game", "Fish/Shellfish", "Mixed/Other", "Fruit", "USOs")))

mean_prob_return_summary <- left_join(mean_prob_return_summary, d_outcome)

# Plot hurdle model predicted vs observed
ggplot(filter(mean_prob_return_summary, prop_nonzero < 1)) + 
  geom_errorbarh(aes(xmin=lower, xmax=upper, y=as.character(outcome_id), color=resource2), height=0) +
  geom_point(aes(x=med, y=as.character(outcome_id), color=resource2), size=2) + 
  geom_point(aes(x=prop_nonzero, y=as.character(outcome_id), color=resource2), shape=1, size=2) + 
  scale_color_manual(values = c(resource_cols[c(2,1)], "black", resource_cols[c(3,4)])) +
  scale_y_discrete(breaks=1:nrow(mean_prob_return_summary), labels=mean_prob_return_summary$outcome) +
  ylab("") +
  xlab("Proportion Non-Zero Returns") +
  theme_minimal(base_size = 14) +
  theme(legend.title = element_blank()) 

ggsave("hurdle_outcome.pdf", dpi=600, height=11, width=8.5, units="in")


#####################################################
#### Plot all raw data ##############################
# If age given as sd
age_sd <- ifelse(is.na(d$age_sd), -99, d$age_sd / 20 )

# If age given as intervals
age_range <- (d$age_upper - d$age_lower) / 20
age_sd <- ifelse(!is.na(age_range), age_range/2, age_sd)

d_raw <- d %>% 
  mutate(age = ifelse(is.na(age), (age_upper + age_lower)/2, age),
         age_range = age_upper - age_lower) %>% 
  mutate(age_sd = ifelse(is.na(age_sd), age_range/2, age_sd)) %>% 
  mutate(resource = ifelse(resource == "fruits", "fruit", resource)) %>% 
  mutate(outcome = str_replace_all(outcome, "_", " ")) %>% 
  mutate(outcome_label = paste(outcome, resource),
         forager_sex = case_when(
           is.na(sex) ~ "both",
           sex > 0 & sex < 1 ~ "both",
           sex == 0 ~ "female",
           sex == 1 ~ "male"
         ))


marine_plots <- filter(d_raw, resource_cat == "fish_shellfish") %>% 
  ggplot(aes(x = age, y = scaled_return)) +
  geom_errorbarh(aes(xmin = age - age_sd, xmax=age + age_sd, y=scaled_return),color=resource_cols[1],alpha=0.5) + 
  geom_errorbar(aes(ymin=scaled_return - scaled_se, ymax=scaled_return + scaled_se, x=age),color=resource_cols[1],alpha=0.5) +
  facet_wrap(~outcome_label, labeller = label_wrap_gen(width=17)) + 
  geom_point(color=resource_cols[1],alpha=0.5, aes(shape=forager_sex)) +
  scale_shape_manual(values=c(15,19,17)) +
  theme_minimal(base_size=9) +
  theme(
    axis.text.y = element_blank(),
    panel.spacing = unit(2, "lines")) +
  ylab("Return (scaled)") +
  xlab("Age") + 
  ggtitle("Fish/Shellfish Foraging Returns")

ggsave("fishshellfish_data.pdf", plot=marine_plots, dpi=600, height=11, width=8.5, units="in")


game_plots <- filter(d_raw, resource_cat == "game") %>% 
  ggplot(aes(x = age, y = scaled_return)) +
  geom_errorbarh(aes(xmin = age - age_sd, xmax=age + age_sd, y=scaled_return),color=resource_cols[2],alpha=0.5) + 
  geom_errorbar(aes(ymin=scaled_return - scaled_se, ymax=scaled_return + scaled_se, x=age),color=resource_cols[2],alpha=0.5) +
  facet_wrap(~outcome_label, labeller = label_wrap_gen(width=17)) + 
  geom_point(color=resource_cols[2],alpha=0.5, aes(shape=forager_sex)) +
  scale_shape_manual(values=c(15,19,17)) +
  theme_minimal(base_size=9) +
  theme(
    axis.text.y = element_blank(),
    panel.spacing = unit(2, "lines")) +
  ylab("Return (scaled)") +
  xlab("Age") + 
  ggtitle("Game/Mixed Foraging Returns")

ggsave("game_data.pdf", plot=game_plots, dpi=600, height=11, width=8.5, units="in")



fruit_plots <- filter(d_raw, resource_cat == "fruit") %>% 
  ggplot(aes(x = age, y = scaled_return)) +
  geom_errorbarh(aes(xmin = age - age_sd, xmax=age + age_sd, y=scaled_return),color=resource_cols[3],alpha=0.5) + 
  geom_errorbar(aes(ymin=scaled_return - scaled_se, ymax=scaled_return + scaled_se, x=age),color=resource_cols[3],alpha=0.5) +
  facet_wrap(~outcome_label, labeller = label_wrap_gen(width=17)) + 
  geom_point(color=resource_cols[3],alpha=0.5, aes(shape=forager_sex)) +
  scale_shape_manual(values=c(15,19,17)) +
  theme_minimal(base_size=9) +
  theme(
    axis.text.y = element_blank(),
    panel.spacing = unit(2, "lines")) +
  ylab("Return (scaled)") +
  xlab("Age") + 
  ggtitle("Fruit Foraging Returns")

ggsave("fruit_data.pdf", plot=fruit_plots, dpi=600, height=11, width=8.5, units="in")


USO_plots <- filter(d_raw, resource_cat == "USOs") %>% 
  ggplot(aes(x = age, y = scaled_return)) +
  geom_errorbarh(aes(xmin = age - age_sd, xmax=age + age_sd, y=scaled_return),color=resource_cols[4],alpha=0.7) + 
  geom_errorbar(aes(ymin=scaled_return - scaled_se, ymax=scaled_return + scaled_se, x=age),color=resource_cols[4],alpha=0.7) +
  facet_wrap(~outcome_label, labeller = label_wrap_gen(width=17)) + 
  geom_point(color=resource_cols[4],alpha=0.5, aes(shape=forager_sex)) +
  scale_shape_manual(values=c(15,19,17)) +
  theme_minimal(base_size=9) +
  theme(
    axis.text.y = element_blank(),
    panel.spacing = unit(2, "lines")) +
  ylab("Return (scaled)") +
  xlab("Age") + 
  ggtitle("USO Foraging Returns")

ggsave("USO_data.pdf", plot=USO_plots, dpi=600, height=11, width=8.5, units="in")


other_plots <- filter(d_raw, resource_cat == "z") %>% 
  ggplot(aes(x = age, y = scaled_return)) +
  geom_errorbarh(aes(xmin = age - age_sd, xmax=age + age_sd, y=scaled_return),color="black",alpha=0.7) + 
  geom_errorbar(aes(ymin=scaled_return - scaled_se, ymax=scaled_return + scaled_se, x=age),color="black",alpha=0.7) +
  facet_wrap(~outcome_label, labeller = label_wrap_gen(width=17)) + 
  geom_point(color="black",alpha=0.5, aes(shape=forager_sex)) +
  scale_shape_manual(values=c(15,19,17)) +
  theme_minimal(base_size=9) +
  theme(
    axis.text.y = element_blank(),
    panel.spacing = unit(2, "lines")) +
  ylab("Return (scaled)") +
  xlab("Age") + 
  ggtitle("Mixed/Other Foraging Returns")

ggsave("other_data.pdf", plot=other_plots, dpi=600, height=11, width=8.5, units="in")







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

k_marine <- log( 1 + exp(post$a[,1] + post$resource_v[,1,1]))
k_game <- log( 1 + exp(post$a[,1] + post$resource_v[,2,1]))
k_fruit <- log( 1 + exp(post$a[,1] + post$resource_v[,3,1]))
k_USO <- log( 1 + exp(post$a[,1] + post$resource_v[,4,1]))

b_marine <- log(1 + exp(post$a[,2] + post$resource_v[,1,2]))
b_game <- log( 1 + exp(post$a[,2] + post$resource_v[,2,2]))
b_fruit <- log( 1 + exp(post$a[,2] + post$resource_v[,3,2]))
b_USO <- log( 1 + exp(post$a[,2] + post$resource_v[,4,2]))

eta_marine <- log( 1 + exp(post$a[,4] + post$resource_v[,1,4]))
eta_game <- log( 1 + exp(post$a[,4] + post$resource_v[,2,4]))
eta_fruit <- log( 1 + exp(post$a[,4] + post$resource_v[,3,4]))
eta_USO <- log( 1 + exp(post$a[,4] + post$resource_v[,4,4]))
