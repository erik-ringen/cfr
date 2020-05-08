usePackage <- function(p) {
  if (!is.element(p, installed.packages()[,1]))
    install.packages(p, dep = TRUE)
  require(p, character.only = TRUE)
}

usePackage("tidyverse")

##################################
home <- getwd() # remember home directory to return to
temp_dir <- "paper_data/Bird_2002b/1" # temporarily set directory
setwd(temp_dir)

paper_name <- strsplit(temp_dir, split="/", fixed=T)[[1]][2]
paper_section <- strsplit(temp_dir, split="/", fixed=T)[[1]][3]

d <- read_csv("Table3.csv")

#### Step 1: Wrangle data ########
d_wide <- d %>%
  group_by(`Prey Type`) %>% 
  pivot_wider(names_from = age, values_from = c(`Mean e/h`, SE, `n (loads processed)`))

# drop prey where there isn't data for both adults and children, no comparison to be made
d_wide <- d_wide[complete.cases(d_wide),]
##################################
#### Step 2: Calculate standardized effect sizes 
# calculate log returns ratio
lRR_fun <- function( m1, m2, sd1, sd2, n1, n2, value="mean" ) {
  
  lRR_mean <- log( m1/m2 )
  lRR_sd <- sqrt(  (sd1^2 / (n1*m1^2)) + (sd2^2 / (n2*m2^2)) )
  
  if  (value == "mean") return( lRR_mean )
  if  (value == "sd") return( lRR_sd )
}

# Calculate log return ratio for each foraging age difference
d_wide$lRR_mean <- lRR_fun(
  m1 = d_wide$`Mean e/h_adult`,
  m2 = d_wide$`Mean e/h_child`, 
  sd1 = sqrt(d_wide$SE_adult),
  sd2 = sqrt(d_wide$SE_child),
  n1 = d_wide$`n (loads processed)_adult`,
  n2 = d_wide$`n (loads processed)_child`,
  value = "mean"
  )

d_wide$lRR_sd <- lRR_fun(
  m1 = d_wide$`Mean e/h_adult`,
  m2 = d_wide$`Mean e/h_child`, 
  sd1 = sqrt(d_wide$SE_adult),
  sd2 = sqrt(d_wide$SE_child),
  n1 = d_wide$`n (loads processed)_adult`,
  n2 = d_wide$`n (loads processed)_child`,
  value = "sd"
)

d_fin <- d_wide
##################################
#### Step 3: Add meta-data and additional covariate information
d_fin$study <- paper_name # paper id
d_fin$outcome <- paste(d_fin$study, 1:7, sep="_") # 7 outcomes in total, different shellfish
d_fin$id <- NA # study *  outcome * individual, if data are individual rather than group-level
d_fin$sex <- "both" # "female", "male", or "both"
d_fin$age_error <- "interval" # information on distribution of ages (sd), or just a range (interval)? 
d_fin$age_sd <- NA  # only if sd of ages is given
d_fin$age_lower <- 5 # only if interval ages given
d_fin$age_upper <- 15 # only if interval ages given
d_fin$resource <- "shellfish" # what type of foraging resource
d_fin$timescale <- "hr" # whether the rate is per hour (hr), per day, or other

##################################
#### Step 4: Export outcome csv for further processing 
d_export <- d_fin %>% ungroup %>% select(study, outcome, id, sex, age_error, age_sd, age_lower, age_upper, resource, timescale, lRR_mean, lRR_sd)

write_csv(d_export, paste0( paste("data", paper_name, sep="_"), ".csv" ))

setwd(home)
#################################
