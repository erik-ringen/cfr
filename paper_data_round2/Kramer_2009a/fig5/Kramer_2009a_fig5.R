usePackage <- function(p) {
  if (!is.element(p, installed.packages()[,1]))
    install.packages(p, dep = TRUE)
  require(p, character.only = TRUE)
}

usePackage("tidyverse")
usePackage("metaDigitise")

##################################
home <- getwd() # remember home directory to return to
temp_dir <- "paper_data_round2/Kramer_2009a/fig5" # temporarily set directory

### Pre-lim: digitize figure data
#metaDigitise(temp_dir)

# workflow: extract group*resource one at a time. Because sample sizes not given for specific ages, divide resource-specific sample sizes by number of age groups (n_group = 5)
# sr = 15, lr = 25, m = 29.8

#saveRDS(metaDigitise(temp_dir, summary=T), paste0(temp_dir, "/fig5.rds"))

#################################
setwd(temp_dir)

paper_name <- strsplit(temp_dir, split="/", fixed=T)[[1]][2]
paper_section <- strsplit(temp_dir, split="/", fixed=T)[[1]][3]

d_list <- readRDS(paste0(paper_section, ".rds"))

#### Step 1: Wrangle data ########
d <- d_list %>% select(group_id, mean, sd, se)
d$resource <- sapply( strsplit( as.character(d$group_id), "_" ), "[", 1 )
d$age_upper <- as.numeric( sapply( strsplit( as.character(d$group_id), "_" ), "[", 2 ) )
d$age_lower <- rep( c(6, 15, 25, 35, 45), each=3 )

# All are female
d$sex <- "female"

#transform kg into grams for comparability
d[ ,2:4] <- d[ ,2:4]*1000

# Get average returns of adults for each resource. Need to estimate pooled SD
adult_avg <- d %>% 
  filter(age_lower > 20) %>% 
  group_by(resource) %>%
    summarise( mean_adult=mean(mean), sd_adult=sqrt( sum(sd^2)/3 ) )

adult_avg$n_adult <- c( (125/5)*3, (149/5)*3, (75/5)*3 )
adult_avg$se_adult <- adult_avg$sd_adult / sqrt(adult_avg$n_adult)

# bring in age to main df
d <- left_join(d, adult_avg)

# filter out individuals above age 20
d <- filter(d, age_upper <= 20)

##################################

d_fin <- d
##################################
#### Step 3: Add meta-data and additional covariate information
d_fin$study <- paper_name # paper id
d_fin$outcome <- paste(d_fin$study, paper_section, d_fin$resource, sep="_")
d_fin$id <- NA #
d_fin$sex <- d$sex                                  # all female
d_fin$age <- NA
d_fin$age_error <- "interval" # 
d_fin$age_sd <- NA  # 
d_fin$age_lower <- d_fin$age_lower # 
d_fin$age_upper <- d_fin$age_upper # 
d_fin$resource <- c("roots", "roots", "fruit") # 
d_fin$units <- "g/h"                                #transformed from kg
d_fin$raw_return <- d_fin$mean
d_fin$raw_sd <- d_fin$sd
d_fin$raw_se <- d_fin$sd / sqrt( c( (125/5), (149/5), (75/5) ))
d_fin$adult_return <- d$mean_adult                  #adult values include age class with upper limits >20, hence with lower limit <20. to check for consistency with other tables
d_fin$adult_sd <- d$sd_adult
d_fin$adult_se <- d_fin$se_adult

##################################
#### Step 4: Export outcome csv for further processing 
d_export <- d_fin %>% ungroup %>% select(study, outcome, id, sex, age, age_error, age_sd, age_lower, age_upper, resource, units, raw_return, raw_sd, raw_se, adult_return, adult_sd, adult_se)

write_csv(d_export, paste0( paste(paste("data", paper_name, sep="_"),paper_section, sep="_"), ".csv" ))

setwd(home)
#################################
