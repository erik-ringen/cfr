usePackage <- function(p) {
  if (!is.element(p, installed.packages()[,1]))
    install.packages(p, dep = TRUE)
  require(p, character.only = TRUE)
}

usePackage("tidyverse")

##################################
home <- getwd() # remember home directory to return to
temp_dir <- "paper_data/Bird_2002b/table2" # temporarily set directory
setwd(temp_dir)

paper_name <- strsplit(temp_dir, split="/", fixed=T)[[1]][2]
paper_section <- strsplit(temp_dir, split="/", fixed=T)[[1]][3]

d <- read_csv("Table 2. Collecting Rates, Encounter Rates, and the Selectivity Quotient.csv")

#### Step 1: Wrangle data ########
d <- d %>% 
  rename( SE = LambdaCplusminus ) ## ATTENTION: value reported as plusminus in the table are interpreted as SE, given that is the measure used in the other tables, although this is not stated

d_select <- d %>% 
  select( `Prey Type`, LambdaC, n_follows_LambdaC, SE, age)

d_wide <- d_select %>%
  group_by(`Prey Type`) %>% 
  pivot_wider(names_from = age, values_from = c(`LambdaC`,  n_follows_LambdaC, SE))

# drop prey where there isn't data for both adults and children, no comparison to be made
d_wide <- d_wide[complete.cases(d_wide),]
##################################

d_fin <- d_wide
##################################
#### Add meta-data and additional covariate information
d_fin$study <- paper_name # paper id
d_fin$outcome <- paste(d_fin$study, d_fin$`Prey Type`, sep="_") # 7 outcomes in total, different shellfish
d_fin$id <- NA # study *  outcome * individual, if data are individual rather than group-level
d_fin$sex <- "both" # "female", "male", or "both"
d_fin$age <- NA # no mean age given
d_fin$age_error <- "interval" # information on distribution of ages (sd), or just a range (interval)? 
d_fin$age_sd <- NA  # only if sd of ages is given
d_fin$age_lower <- 5 # only if interval ages given
d_fin$age_upper <- 15 # only if interval ages given
d_fin$resource <- "shellfish" # what type of foraging resource
d_fin$units <- "item/hr" # whether the rate is per hour (hr), per day, or other
d_fin$raw_return <- d_wide$`LambdaC_children`
d_fin$raw_sd <- d_wide$SE_children * sqrt(d_wide$`n_follows_LambdaC_children`)
d_fin$adult_return <- d_wide$`LambdaC_adults`
d_fin$adult_sd <- d_wide$SE_adults * sqrt(d_wide$`n_follows_LambdaC_adults`)
d_fin$adult_se <- d_wide$SE_adults

##################################
#### Step 4: Export outcome csv for further processing 
d_export <- d_fin %>% ungroup %>% select(study, outcome, id, sex, age, age_error, age_sd, age_lower, age_upper, resource, units, raw_return, raw_sd, adult_return, adult_sd, adult_se)

write_csv(d_export, paste0( paste(paste("data", paper_name, sep="_"),paper_section, sep="_"), ".csv" ))

setwd(home)
#################################
