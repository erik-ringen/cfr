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

### Read in fit stan model
fit <- readRDS("fit2.rds")

# Get posterior samples
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
  theme_bw(base_size = 12) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),strip.background = element_blank(),
        legend.title = element_blank(),
        panel.spacing = unit(2, "lines")) +
  ylab("Number of Studies") +
  xlab("Age")

###########################################
##### Differences by method ###############
d_method <- d %>% 
  group_by(outcome) %>% 
  summarise(units = unique(units),
            id = unique(outcome_id)) %>% 
  mutate(method = ifelse(
    units %in% c("g/h","item/h","kcal/h", "net kcal/h", "net kcal/hr", "net kcal/h", "kcal/day"), "Rate", "Quantity"),
    short_name = str_extract(outcome, "[^_]+"))

###########################################
##### Orchard plots for key parameters ####

### First females, then males #####

### k #####
### Females ##
avg_k <- exp( post$a_k[,1] )

outcome_k <- matrix(NA, nrow = n_samps, ncol = N_outcomes)

for (j in 1:N_outcomes) {
  outcome_k[,j] <- exp( post$a_k[,1] + post$outcome_v[,j,1] + post$resource_v[,d_outcome$resource[match(j, d_outcome$id)],1] )
}

precision_k <- 1/apply(outcome_k, 2, sd)
median_k <- apply(outcome_k, 2, mean)

k_df <- data.frame(median = median_k,
                   precision = precision_k,
                   resource = d_outcome$resource[match(1:N_outcomes, d_outcome$id)],
                   sex = rep("F", length(median_k))
)

avg_k_F <- data.frame(
  median = mean(avg_k),
  sd = sd(avg_k),
  lower = HPDI(avg_k, 0.95)[1],
  upper = HPDI(avg_k, 0.95)[2],
  sex = "F"
)

### Males ##
avg_k <- exp( post$a_k[,1] + post$a_k[,2] )

outcome_k <- matrix(NA, nrow = n_samps, ncol = N_outcomes)

for (j in 1:N_outcomes) {
  outcome_k[,j] <- exp( post$a_k[,1] + post$a_k[,2] + post$outcome_v[,j,1] + post$resource_v[,d_outcome$resource[match(j, d_outcome$id)],1] + post$outcome_v[,j,8] + post$resource_v[,d_outcome$resource[match(j, d_outcome$id)],8] )
}

precision_k <- 1/apply(outcome_k, 2, sd)
median_k <- apply(outcome_k, 2, mean)

k_df <- bind_rows( k_df,
                   
                   data.frame(median = median_k,
                   precision = precision_k,
                   resource = d_outcome$resource[match(1:N_outcomes, d_outcome$id)],
                   sex = rep("M", length(median_k))
)

)

avg_k <- rbind( avg_k_F,
  
  data.frame(
  median = mean(avg_k),
  sd = sd(avg_k),
  lower = HPDI(avg_k, 0.95)[1],
  upper = HPDI(avg_k, 0.95)[2],
  sex = "M"
)

)


k_df$resource <- factor(k_df$resource, labels = d_r$resource[match(1:max(d_r$id),d_r$id )])
###########

### b #####
### Females ##
avg_b <- exp( post$a_b[,1] )

outcome_b <- matrix(NA, nrow = n_samps, ncol = N_outcomes)

for (j in 1:N_outcomes) {
  outcome_b[,j] <- exp( post$a_b[,1] + post$outcome_v[,j,2] + post$resource_v[,d_outcome$resource[match(j, d_outcome$id)],2] )
}

precision_b <- 1/apply(outcome_b, 2, sd)
median_b <- apply(outcome_b, 2, mean)

b_df <- data.frame(median = median_b,
                   precision = precision_b,
                   resource = d_outcome$resource[match(1:N_outcomes, d_outcome$id)],
                   sex = rep("F", length(median_b))
)

avg_b_F <- data.frame(
  median = mean(avg_b),
  sd = sd(avg_b),
  lower = HPDI(avg_b, 0.95)[1],
  upper = HPDI(avg_b, 0.95)[2],
  sex = "F"
)

### Males ##
avg_b <- exp( post$a_b[,1] + post$a_b[,2] )

outcome_b <- matrix(NA, nrow = n_samps, ncol = N_outcomes)

for (j in 1:N_outcomes) {
  outcome_b[,j] <- exp( post$a_b[,1] + post$a_b[,2] + post$outcome_v[,j,2] + post$resource_v[,d_outcome$resource[match(j, d_outcome$id)],2] + post$outcome_v[,j,9] + post$resource_v[,d_outcome$resource[match(j, d_outcome$id)],9] )
}

precision_b <- 1/apply(outcome_b, 2, sd)
median_b <- apply(outcome_b, 2, mean)

b_df <- bind_rows( b_df,
                   
                   data.frame(median = median_b,
                              precision = precision_b,
                              resource = d_outcome$resource[match(1:N_outcomes, d_outcome$id)],
                              sex = rep("M", length(median_b))
                   )
                   
)

avg_b <- rbind( avg_b_F,
                
                data.frame(
                  median = mean(avg_b),
                  sd = sd(avg_b),
                  lower = HPDI(avg_b, 0.95)[1],
                  upper = HPDI(avg_b, 0.95)[2],
                  sex = "M"
                )
                
)


b_df$resource <- factor(b_df$resource, labels = d_r$resource[match(1:max(d_r$id),d_r$id )])
###########

### eta_p #####
### Females ##
avg_eta_p <- exp( post$a_eta[,1,1] )

outcome_eta_p <- matrix(NA, nrow = n_samps, ncol = N_outcomes)

for (j in 1:N_outcomes) {
  outcome_eta_p[,j] <- exp(  post$a_eta[,1,1] + post$outcome_v[,j,3] + post$resource_v[,d_outcome$resource[match(j, d_outcome$id)],3] )
}

precision_eta_p <- 1/apply(outcome_eta_p, 2, sd)
median_eta_p <- apply(outcome_eta_p, 2, mean)

eta_p_df <- data.frame(median = median_eta_p,
                   precision = precision_eta_p,
                   resource = d_outcome$resource[match(1:N_outcomes, d_outcome$id)],
                   sex = rep("F", length(median_eta_p))
)

avg_eta_p_F <- data.frame(
  median = mean(avg_eta_p),
  sd = sd(avg_eta_p),
  lower = HPDI(avg_eta_p, 0.95)[1],
  upper = HPDI(avg_eta_p, 0.95)[2],
  sex = "F"
)

### Males ##
avg_eta_p <- exp( post$a_eta[,1,1] + post$a_eta[,2,1] )

outcome_eta_p <- matrix(NA, nrow = n_samps, ncol = N_outcomes)

for (j in 1:N_outcomes) {
  outcome_eta_p[,j] <- exp( post$a_eta[,1,1] + post$a_eta[,2,1] + post$outcome_v[,j,3] + post$resource_v[,d_outcome$resource[match(j, d_outcome$id)],3] + post$outcome_v[,j,10] + post$resource_v[,d_outcome$resource[match(j, d_outcome$id)],10] )
}

precision_eta_p <- 1/apply(outcome_eta_p, 2, sd)
median_eta_p <- apply(outcome_eta_p, 2, mean)

eta_p_df <- bind_rows( eta_p_df,
                   
                   data.frame(median = median_eta_p,
                              precision = precision_eta_p,
                              resource = d_outcome$resource[match(1:N_outcomes, d_outcome$id)],
                              sex = rep("M", length(median_eta_p))
                   )
                   
)

avg_eta_p <- rbind( avg_eta_p_F,
                
                data.frame(
                  median = mean(avg_eta_p),
                  sd = sd(avg_eta_p),
                  lower = HPDI(avg_eta_p, 0.95)[1],
                  upper = HPDI(avg_eta_p, 0.95)[2],
                  sex = "M"
                )
                
)


eta_p_df$resource <- factor(eta_p_df$resource, labels = d_r$resource[match(1:max(d_r$id),d_r$id )])

### eta_r #####
### Females ##
avg_eta_r <- exp( post$a_eta[,1,2] )

outcome_eta_r <- matrix(NA, nrow = n_samps, ncol = N_outcomes)

for (j in 1:N_outcomes) {
  outcome_eta_r[,j] <- exp(  post$a_eta[,1,2] + post$outcome_v[,j,4] + post$resource_v[,d_outcome$resource[match(j, d_outcome$id)],4] )
}

precision_eta_r <- 1/apply(outcome_eta_r, 2, sd)
median_eta_r <- apply(outcome_eta_r, 2, mean)

eta_r_df <- data.frame(median = median_eta_r,
                       precision = precision_eta_r,
                       resource = d_outcome$resource[match(1:N_outcomes, d_outcome$id)],
                       sex = rep("F", length(median_eta_r))
)

avg_eta_r_F <- data.frame(
  median = mean(avg_eta_r),
  sd = sd(avg_eta_r),
  lower = HPDI(avg_eta_r, 0.95)[1],
  upper = HPDI(avg_eta_r, 0.95)[2],
  sex = "F"
)

### Males ##
avg_eta_r <- exp( post$a_eta[,1,2] + post$a_eta[,2,2] )

outcome_eta_r <- matrix(NA, nrow = n_samps, ncol = N_outcomes)

for (j in 1:N_outcomes) {
  outcome_eta_r[,j] <- exp( post$a_eta[,1,2] + post$a_eta[,2,2] + post$outcome_v[,j,4] + post$resource_v[,d_outcome$resource[match(j, d_outcome$id)],4] + post$outcome_v[,j,11] + post$resource_v[,d_outcome$resource[match(j, d_outcome$id)],11] )
}

precision_eta_r <- 1/apply(outcome_eta_r, 2, sd)
median_eta_r <- apply(outcome_eta_r, 2, mean)

eta_r_df <- bind_rows( eta_r_df,
                       
                       data.frame(median = median_eta_r,
                                  precision = precision_eta_r,
                                  resource = d_outcome$resource[match(1:N_outcomes, d_outcome$id)],
                                  sex = rep("M", length(median_eta_r))
                       )
)

avg_eta_r <- rbind( avg_eta_r_F,
                    
                    data.frame(
                      median = mean(avg_eta_r),
                      sd = sd(avg_eta_r),
                      lower = HPDI(avg_eta_r, 0.95)[1],
                      upper = HPDI(avg_eta_r, 0.95)[2],
                      sex = "M"
                    )
)


eta_r_df$resource <- factor(eta_r_df$resource, labels = d_r$resource[match(1:max(d_r$id),d_r$id )])
######

######

### Compose orchard plots 

resource_cols <- c("#046C9A", "#CB2313", "#0C775E", "#EBCC2A")

k_plot <- ggplot(k_df, aes(x = median, y = sex)) + 
  geom_jitter(aes(color = resource, size = precision), alpha=0.6, width=0, height=0.2) + 
  geom_errorbarh(data = avg_k, aes(xmin = lower, xmax = upper, y = sex), lwd=1, height=0, color="darkgrey") + 
  geom_errorbarh(data = avg_k, aes(xmin = median - sd, xmax = median + sd, y = sex), height=0, lwd=2, color="black") +
  geom_point(data = avg_k, aes(x = median, y = sex), size=4, color="seashell1") +
  geom_point(data = avg_k, aes(x = median, y = sex), size=4, color="black", shape=1) +
  scale_size(name = "Precision") +
  theme_bw(base_size = 12) + 
  scale_color_manual(values = resource_cols, guide=F) + 
  xlab("k") +
  ylab("")  + 
  theme(legend.position = c(0.7, 0.075),
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.title = element_text(size=8),
        legend.text = element_text(size=8),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.key.size = unit(0.5, 'lines')) + 
  guides(size = guide_legend(nrow = 1,title.position = "left"))



b_plot <- ggplot(b_df, aes(x = median, y = sex)) + 
  geom_jitter(aes(color = resource, size = precision), alpha=0.6, width=0, height=0.2) + 
  geom_errorbarh(data = avg_b, aes(xmin = lower, xmax = upper, y = sex), lwd=1, height=0, color="darkgrey") + 
  geom_errorbarh(data = avg_b, aes(xmin = median - sd, xmax = median + sd, y = sex), height=0, lwd=2, color="black") +
  geom_point(data = avg_b, aes(x = median, y = sex), size=4, color="seashell1") +
  geom_point(data = avg_b, aes(x = median, y = sex), size=4, color="black", shape=1) +
  geom_vline(xintercept = 1, lty="dashed") +
  scale_size(name = "Precision") +
  theme_bw(base_size = 12) + 
  scale_color_manual(values = resource_cols, guide=F) + 
  xlab("b") +
  ylab("")  + 
  theme(legend.position = c(0.7, 0.075),
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.title = element_text(size=8),
        legend.text = element_text(size=8),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.key.size = unit(0.5, 'lines')) + 
  guides(size = guide_legend(nrow = 1,title.position = "left"))


eta_p_plot <- ggplot(eta_p_df, aes(x = median, y = sex)) + 
  geom_jitter(aes(color = resource, size = precision), alpha=0.6, width=0, height=0.2) + 
  geom_errorbarh(data = avg_eta_p, aes(xmin = lower, xmax = upper, y = sex), lwd=1, height=0, color="darkgrey") + 
  geom_errorbarh(data = avg_eta_p, aes(xmin = ifelse(median - sd < 0, 0, median - sd), xmax = median + sd, y = sex), height=0, lwd=2, color="black") +
  geom_point(data = avg_eta_p, aes(x = median, y = sex), size=4, color="seashell1") +
  geom_point(data = avg_eta_p, aes(x = median, y = sex), size=4, color="black", shape=1) +
  geom_vline(xintercept = 1, lty="dashed") +
  scale_size(name = "Precision") +
  theme_bw(base_size = 12) + 
  scale_color_manual(values = resource_cols, guide=F) + 
  xlab(expression(eta["p"])) +
  ylab("")  + 
  theme(legend.position = c(0.7, 0.075),
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.title = element_text(size=8),
        legend.text = element_text(size=8),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.key.size = unit(0.5, 'lines')) + 
  guides(size = guide_legend(nrow = 1,title.position = "left"))


eta_r_plot <- ggplot(eta_r_df, aes(x = median, y = sex)) + 
  geom_jitter(aes(color = resource, size = precision), alpha=0.6, width=0, height=0.2) + 
  geom_errorbarh(data = avg_eta_r, aes(xmin = lower, xmax = upper, y = sex), lwd=1, height=0, color="darkgrey") + 
  geom_errorbarh(data = avg_eta_r, aes(xmin = ifelse(median - sd < 0, 0, median - sd), xmax = median + sd, y = sex), height=0, lwd=2, color="black") +
  geom_point(data = avg_eta_r, aes(x = median, y = sex), size=4, color="seashell1") +
  geom_point(data = avg_eta_r, aes(x = median, y = sex), size=4, color="black", shape=1) +
  geom_vline(xintercept = 1, lty="dashed") +
  scale_size(name = "Precision") +
  theme_bw(base_size = 12) + 
  scale_color_manual(values = resource_cols, guide=F) + 
  xlab(expression(eta["r"])) +
  ylab("")  + 
  theme(legend.position = c(0.7, 0.075),
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.title = element_text(size=8),
        legend.text = element_text(size=8),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.key.size = unit(0.5, 'lines')) + 
  guides(size = guide_legend(nrow = 1,title.position = "left"))

## Put em all together
orchard <- k_plot / b_plot / eta_p_plot / eta_r_plot

ggsave("orchard.pdf", plot = orchard, width=4, height=8, dpi=600)

#################################################################
##### Do same but for method differences ########################
k_df$id <- rep(1:N_outcomes, 2)

k_df_method <- left_join(k_df, d_method)

k_method_plot <- ggplot(k_df_method, aes(x = median, y = method)) +
  geom_jitter(aes(color = resource, size = precision, shape = sex), alpha=0.6,width=0, height=0.2) +
  scale_size(name = "Precision") +
  theme_bw(base_size = 12) + 
  scale_shape_manual(values = c("circle","triangle"), guide=F) +
  scale_color_manual(values = resource_cols, guide=F) + 
  xlab("k") +
  ylab("")  + 
  theme(legend.position = c(0.5, 0.07),
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.title = element_text(size=8),
        legend.text = element_text(size=8),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.key.size = unit(0.5, 'lines')) + 
  guides(size = guide_legend(nrow = 1,title.position = "left"))


b_df$id <- rep(1:N_outcomes, 2)

b_df_method <- left_join(b_df, d_method)

b_method_plot <- ggplot(b_df_method, aes(x = median, y = method)) +
  geom_jitter(aes(color = resource, size = precision, shape = sex), alpha=0.6,width=0, height=0.2) +
  scale_size(name = "Precision") +
  theme_bw(base_size = 12) + 
  scale_shape_manual(values = c("circle","triangle"), guide=F) +
  scale_color_manual(values = resource_cols, guide=F) + 
  xlab("b") +
  ylab("")  + 
  theme(legend.position = c(0.5, 0.07),
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.title = element_text(size=8),
        legend.text = element_text(size=8),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.key.size = unit(0.5, 'lines')) + 
  guides(size = guide_legend(nrow = 1,title.position = "left"))


eta_p_df$id <- rep(1:N_outcomes, 2)

eta_p_df_method <- left_join(eta_p_df, d_method)

eta_p_method_plot <- ggplot(eta_p_df_method, aes(x = median, y = method)) +
  geom_jitter(aes(color = resource, size = precision, shape = sex), alpha=0.6,width=0, height=0.2) +
  scale_size(name = "Precision") +
  theme_bw(base_size = 12) + 
  scale_shape_manual(values = c("circle","triangle"), guide=F) +
  scale_color_manual(values = resource_cols, guide=F) + 
  xlab(expression(eta["p"])) +
  ylab("")  + 
  theme(legend.position = c(0.5, 0.07),
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.title = element_text(size=8),
        legend.text = element_text(size=8),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.key.size = unit(0.5, 'lines')) + 
  guides(size = guide_legend(nrow = 1,title.position = "left"))



eta_r_df$id <- rep(1:N_outcomes, 2)

eta_r_df_method <- left_join(eta_r_df, d_method)

eta_r_method_plot <- ggplot(eta_r_df_method, aes(x = median, y = method)) +
  geom_jitter(aes(color = resource, size = precision, shape = sex), alpha=0.6,width=0, height=0.2) +
  scale_size(name = "Precision") +
  theme_bw(base_size = 12) + 
  scale_shape_manual(values = c("circle","triangle"), guide=F) +
  scale_color_manual(values = resource_cols, guide=F) + 
  xlab(expression(eta["r"])) +
  ylab("")  + 
  theme(legend.position = c(0.5, 0.07),
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.title = element_text(size=8),
        legend.text = element_text(size=8),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.key.size = unit(0.5, 'lines')) + 
  guides(size = guide_legend(nrow = 1,title.position = "left"))


## Put em all together
orchard <- k_method_plot / b_method_plot / eta_p_method_plot / eta_r_method_plot

ggsave("orchard_method.pdf", plot = orchard, width=4, height=8, dpi=600)



