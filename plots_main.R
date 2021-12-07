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

#####################################################
##### Figure 4 ######################################
## 4a: overall returns ~ age
pdf(file = "return_avg.pdf", width = 6, height = 8)
par(pty='s',
    oma=c(0,0,0,0),
    mai = c(0.5,0.5,0.5,0.5),
    cex=1.3
)

# Get model predictions across ages
preds_both <- cfr_pred(age=age_seq, resp="nodim_returns")

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
pdf(file = "resource_return.pdf", width = 8, height= 8)
par(mfrow=c(2,2),
    pty='s',
    oma=c(0,0,0,0),
    mai = c(0.5,0.5,0.5,0.5),
    cex=1.3
)

for (r in 1:4) {
  
  d_outcome_temp <- filter(d_outcome, resource == resource_seq[r])
  
  #### Average curve ################
  preds_both <- cfr_pred(age=age_seq, resp="nodim_returns", resource = resource_seq[r])
  
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
    
    preds_both <- cfr_pred(age=age_seq, resp="nodim_returns", resource = resource_seq[r], outcome = d_outcome_temp$id[s])
    
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

preds <- cfr_pred(age=age_seq, resp="nodim_returns")

for (i in 1:nrow(preds)) preds[i,] <- preds[i,] / max(preds[i,])
age_5[,1] = preds[,6]
age_10[,1] = preds[,11] - preds[,6]
age_20[,1] = preds[,21] - preds[,11]

for (r in 1:4) {
  preds <- cfr_pred(age=age_seq, resp="nodim_returns", resource=resource_seq[r])
  
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
  xlab("% Increase in Foraging Returns") +
  ylab("")

ggsave("returns_age_plot.pdf", width=11, height=6, dpi=600)

#####################################################
##### Figure 5 ######################################
eta <- cbind( exp(post$a[,4]), exp(post$a[,4] + post$resource_v[,,4]) )
eta <- as.data.frame(eta)
names(eta) <- c("Average", "Game", "Fish/Shellfish", "Mixed", "Fruit", "USOs")
eta$samp <- 1:nrow(eta)

eta_long <- eta %>% pivot_longer(-samp) %>% filter(!(name %in% c("Mixed")))

eta_long$name <- fct_reorder(eta_long$name,eta_long$value)

eta_resource <- ggplot(eta_long, aes(x = value, y = name)) + 
  geom_hline(yintercept=seq(1:5), alpha=0.6) +
  geom_density_ridges2(aes(color=name, fill=name), alpha=0.8,lwd=0.8,scale=0.9, rel_min_height=0.001) +
  annotate("blank", x = 0, y= 5.7 ) +
  scale_x_continuous(expand = c(0.00, 0.05), limits=c(0,15)) + 
  scale_y_discrete(expand = c(0.00, 0)) + 
  scale_color_manual(values=c(resource_cols[c(3,1)],"black",resource_cols[c(4,2)])) +
  scale_fill_manual(values=c(resource_cols[c(3,1)],"black",resource_cols[c(4,2)])) +
  theme_minimal(base_size=16) + 
  theme(legend.position = "none",panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  ylab("") + 
  xlab(expression(paste(eta, " (elasticity of returns on skill)"))) 

eta <- eta %>% mutate(diff = Game - Fruit)

PP <- mean(eta$Game > eta$Fruit) 

eta_contrast <- ggplot(eta, aes(x=diff)) +
  geom_density(fill="gray80", color="gray80", alpha=0.8) +
  geom_vline(xintercept = 0, linetype="dashed") +
  theme_minimal(base_size=16) +
  scale_x_continuous(expand = c(0.00, 0.05), limits=c(-4,15)) +
  scale_y_continuous(expand = c(0.00, 0)) +
  theme( axis.text.y = element_blank(),
         axis.ticks.y = element_blank(),
         panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  ylab("") +
  xlab(expression(Delta(eta[Game]-eta[Fruit])))

eta_resource + eta_contrast

ggsave("eta_plot.pdf", width=10, height=6, dpi=600)






