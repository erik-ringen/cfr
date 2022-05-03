library(tidyverse)
library(cchunts)
library(rethinking)
library(data.table)

# Get all extracted data files in the subdirectories
paper_data <- list.files(pattern = "^data_", recursive = TRUE)

round1 <- paper_data[substr(paper_data, 12, 17) != "round2"]
round2 <- paper_data[substr(paper_data, 12, 17) == "round2"]

# Combine each round of data into a single dataframe
d_round1 <- round1 %>% 
  purrr::map_df(~{
    read_csv(.x) %>% 
  mutate(sex = as.character(sex))
  }) 

d_round2 <- round2 %>% 
  purrr::map_df(~{
    read_csv(.x) %>% 
      mutate(sex = as.character(sex))
  }) 


#Remove Gurven_2006 and Walker_2002 data as present in cchunts
d_round1 <- d_round1[-which(d_round1$study == "Walker_2002" | d_round1$study == "Gurven_2006"),]
d_round2 <- d_round2[-which(d_round2$study == "Walker_2002" | d_round2$study == "Gurven_2006"),]


# Are the same studies in each round of data entry?
sum(unique(d_round1$outcome) %in% unique(d_round2$outcome)) / length(unique(d_round1$outcome) )

# Look at the empirical cumulative distributions between each pair
d_both_rounds <- bind_rows(d_round1, d_round2)
d_both_rounds$round <- c(rep("round1", nrow(d_round1)), rep("round2", nrow(d_round2)))

ggplot(d_both_rounds, aes(raw_return, color=round)) + 
  facet_wrap(~study, scales="free_x") +
  stat_ecdf(alpha=0.6,lwd=0.8) + 
  theme_minimal() +
  ylab("ECDF")

# Looks good. We'll use round2 for analysis.

#####################################################
#### Deal with negative values in BliegeBird_2002a ##
d_round2 %>% 
  group_by(outcome) %>% 
  summarise(min_return = min(raw_return)) %>% 
  filter(min_return < 0)

## For now, count net negative values as zero-return? 
d_round2$raw_return <- ifelse( d_round2$raw_return < 0, 0, d_round2$raw_return )

#####################################################
#### Deal with resource type in Froehle 2018
d_round2 %>% 
  filter(is.na(resource))

d_round2$resource <- ifelse(is.na(d_round2$resource), "mixed", d_round2$resource)

#####################################################
#### correct mispelling of fruits in Hawkes 1995
d_round2$resource <- str_replace(d_round2$resource, "fruits", "fruit")


#####################################################
#### Bring in data from cchunts package #############
cchunts_dat <- make_joint( cchunts_data_sets )

#14 data points in the Bird_Bird_Codding dataset *might* be repeated in the data from Bird_2005
#cchunts_dat <- cchunts_dat[-which ( cchunts_dat$society == "Bird_Bird_Codding" & cchunts_dat$trip_year <= "2002" & cchunts_dat$age_dist_1 <= 14), ]

# First, get summary of adult returns
cchunts_dat_adult <- cchunts_dat %>% 
  mutate(study = paste0(society, "_cchunts")) %>% 
  filter(age_dist_1 >= 20) %>% 
  group_by(study, sex) %>% 
  summarise(
    adult_return = mean(harvest),
    adult_sd = sd(harvest),
    adult_se = sd(harvest)/sqrt(n())
  )

# Then get, individual-level data from foragers under 20
cchunts_dat_child <- cchunts_dat %>% 
  filter(age_dist_1 < 20) %>%
  select(society, forager_id, sex, harvest, age_type, age_dist_1, age_dist_2, age_type) %>% 
  mutate(study = paste0(society, "_cchunts"),
         outcome = paste0(society, "_cchunts"),
         id = paste0(society, forager_id),
         age = age_dist_1,
         age_error = fct_recode(age_type, "none" = "Exact", "distribution" = "Uncertain", "interval" = "Uniform"),
         age_sd = ifelse(age_type == "Uncertain", age_dist_2, NA),
         age_lower = ifelse(age_type == "Uniform", age_dist_1, NA),
         age_upper = ifelse(age_type == "Uniform", age_dist_2, NA),
         resource = "game",
         units = "kg",
         raw_return = harvest,
         raw_sd = NA
         ) %>% 
  select(names(d_both_rounds[1:13]))
  
# Match child with average adult values
cchunts_dat_child2 <- left_join(cchunts_dat_child, cchunts_dat_adult)
cchunts_dat_child2$sex <- ifelse(cchunts_dat_child2$sex == "M", 1, 0)

for (i in 1:nrow(d_round2)) {
  if (d_round2$sex[i] == "female") d_round2$sex[i] <- 0
  else if (d_round2$sex[i] == "male") d_round2$sex[i] <- 1
  else if (d_round2$sex[i] == "both") d_round2$sex[i] <- 0.5
  else d_round2$sex[i] <- as.numeric(d_round2$sex[i])
}

d_round2$sex <- as.numeric(d_round2$sex)

d_combined <- bind_rows(d_round2, cchunts_dat_child2)

d_combined <- d_combined %>% mutate(resource_cat = as.character(fct_recode(resource, game = "small_game", fish_shellfish = "shellfish", fish_shellfish = "fish", USOs = "tubers", USOs = "roots", fruit = "fruit", fruit = "fruits", z = "eggs", z = "honey", z = "mixed")))

# Adjust age error variables
d_combined$age_lower <- ifelse( d_combined$age_lower == d_combined$age_upper, d_combined$age_lower - 0.5, d_combined$age_lower )
d_combined$age_upper <- ifelse( d_combined$age_lower == d_combined$age_upper, d_combined$age_upper - 0.5, d_combined$age_upper )

d_combined$age_sd <- ifelse( d_combined$age_sd == 0, NA, d_combined$age_sd )

########################################################
#### Drop problematic/duplicated obs ##################
d_combined <- d_combined %>% 
  filter( !(outcome %in% c("Hawkes_1995_Table_4_Tafabe_stashing_rates_g.h",
                           "Bird_2002b_table2_Trid. gigas", 
                           "Hawkes_1995_Table_4_Ondishibe_stashing_rates_g.h", 
                           "Hawkes_1995_Table_6_Consumption_rate_Cal.h", 
                           "Hawkes_1995_Table_6_stashing_rate_Cal.h", 
                           "Hawkes_1995_Table_5_Ondishibe", 
                           "Hawkes_1995_Table_5_Tafabe", 
                           "Crittenden_2013_Fig_3",
                           "BliegeBird_2002a_fig2",
                           "BliegeBird_2002a_fig5c",
                           "BliegeBird_2002a_fig6",
                           "Froehle_2018_fig6b",
                           "Hawkes_1995_Table_6_tin_measured_rates_Cal.h")))

########################################################
#### Drop outcomes where we only have one observation ##
d_out <- d_combined %>% 
  group_by(outcome) %>% 
  summarise(n = n())

d_combined <- left_join(d_combined, d_out) %>% 
  filter(n > 1) %>% 
  select(-n)

#### Make sure there's no one over 20 ##############
d_combined <- d_combined %>% 
  filter(age_upper <= 20 | is.na(age_upper))

#########################################################
#### Scale returns data by maximum in each outcome ######
d_combined <- d_combined %>% 
  group_by(outcome) %>% 
  mutate(scaled_return = raw_return /  max(raw_return, na.rm=T),
         scaled_se = raw_se / max(raw_return, na.rm=T),
  ) %>% 
  ungroup() %>% 
  filter( !(is.na(scaled_return)) & ( is.na(raw_se) | raw_se > 0) )

#### Export combined dataset #######################
write_csv(d_combined, "data.csv")
write_csv(d_combined, "text/data/d_all_data.csv")
