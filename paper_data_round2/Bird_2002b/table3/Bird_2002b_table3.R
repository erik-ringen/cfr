usePackage <- function(p) {
  if (!is.element(p, installed.packages()[,1]))
    install.packages(p, dep = TRUE)
  require(p, character.only = TRUE)
}

usePackage("tidyverse")

##################################
home <- getwd() # remember home directory to return to
temp_dir <- "paper_data_round2/Bird_2002b/table3" # temporarily set directory
setwd(temp_dir)

paper_name <- strsplit(temp_dir, split="/", fixed=T)[[1]][2]
paper_section <- strsplit(temp_dir, split="/", fixed=T)[[1]][3]

d <- read_csv("table3. Reef-flat collecting efficiency.csv")

#### Step 1: Wrangle data ########
d_wide <- d %>%
  group_by(`Prey Type`) %>% 
  pivot_wider(names_from = age, values_from = c(`Mean e/h`, SE, `n (loads processed)`))

# drop prey where there isn't data for both adults and children, no comparison to be made
d_wide <- d_wide[complete.cases(d_wide),]
##################################

d_fin <- d_wide
##################################
#### Add meta-data and additional covariate information
d_fin$study <- paper_name # paper id
d_fin$outcome <- paste(d_fin$study, paper_section, d_fin$`Prey Type`, sep="_") # 7 outcomes in total, different shellfish
d_fin$id <- NA # 
d_fin$sex <- 16/(16+19) # 16 males, 19 females
d_fin$age <- NA # no mean age given
d_fin$age_error <- "interval" # 
d_fin$age_sd <- NA  # 
d_fin$age_lower <- 5 # 
d_fin$age_upper <- 15 # 
d_fin$resource <- "shellfish" # 
d_fin$units <- "kcal/h" # 
d_fin$raw_return <- d_wide$`Mean e/h_child`
d_fin$raw_sd <- d_wide$SE_child * sqrt(d_wide$`n (loads processed)_child`)
d_fin$raw_se <- d_wide$SE_child
d_fin$adult_return <- d_wide$`Mean e/h_adult`
d_fin$adult_sd <- d_wide$SE_adult * sqrt(d_wide$`n (loads processed)_adult`)
d_fin$adult_se <- d_wide$SE_adult

##################################
#### Step 4: Export outcome csv for further processing 
d_export <- d_fin %>% ungroup %>% select(study, outcome, id, sex, age, age_error, age_sd, age_lower, age_upper, resource, units, raw_return, raw_sd, raw_se, adult_return, adult_sd, adult_se)

write_csv(d_export, paste0( paste(paste("data", paper_name, sep="_"),paper_section, sep="_"), ".csv" ))

setwd(home)
#################################
