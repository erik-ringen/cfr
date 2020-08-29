usePackage <- function(p) {
  if (!is.element(p, installed.packages()[,1]))
    install.packages(p, dep = TRUE)
  require(p, character.only = TRUE)
}

usePackage("tidyverse")
usePackage("metaDigitise")

##################################
home <- getwd() # remember home directory to return to
temp_dir <- "paper_data/BliegeBird_2002a/fig2" # temporarily set directory

### Pre-lim: digitize figure data
#metaDigitise(temp_dir)

# workflow: get points from one half of the scatterlpot (F/M) at a time, with a different group for every unique ID on the y axis. Starting top of y axis to bottom. Then do again with the male data (rght side).

#saveRDS(metaDigitise(temp_dir, summary=F), paste0(temp_dir, "/fig2.rds"))

#################################
setwd(temp_dir)

paper_name <- strsplit(temp_dir, split="/", fixed=T)[[1]][2]
paper_section <- strsplit(temp_dir, split="/", fixed=T)[[1]][3]

d_list <- readRDS("fig2.rds")

#### Step 1: Wrangle data ########
d <- bind_rows( d_list$`fig2. beach fishing efficiency across age categories.png`  ) %>% select(id, mean, error, n)

colnames(d)[1] <- "group"

# Get sex labels
d$sex <- NA

#create ids
d$id <- NA

#add age bounds
d$age_lower <- c (5, 10, 14, 30, 51) # only if interval ages given
d$age_upper <- c (9, 13, 29, 50, 65) # only if interval ages given

##################################

#calculate mean adult values
d_adults <- d [ which(d$age_lower >= 20), ]

d_adults$adult_return <- weighted.mean (d_adults$mean, d_adults$n)

##################################
#select age groups

d_select <- d[ 1:3, ]

##################################

##################################
#### Step 3: Add meta-data and additional covariate information
d_fin <- data.frame(study = rep( paper_name, nrow(d_select)))
d_fin$outcome <- paste(d_fin$study, 1, sep="_") # total kcal/hr outcome, 1997 data
d_fin$id <- NA # study *  outcome * individual, if data are individual rather than group-level
d_fin$sex <- d_select$sex # "female", "male", or "both"
d_fin$age <- NA 
d_fin$age_error <- NA # information on distribution of ages (sd), or just a range (interval)? 
d_fin$age_sd <- NA  # only if sd of ages is given
d_fin$age_lower <- d_select$age_lower # only if interval ages given
d_fin$age_upper <- d_select$age_upper # only if interval ages given
d_fin$resource <- "shellfish" # what type of foraging resource
d_fin$units <- "net kcal/hr" # whether the rate is per hour (hr), per day, or other
d_fin$raw_return <- d_select$mean
d_fin$raw_sd <- NA #error presented as CI98, and given the small sample sizes, calculate the SD from this is not advisable. Especially since the SD calculated from the raw data is available in the accompaining table
d_fin$adult_return <- d_adults$adult_return[1]
d_fin$adult_sd <- NA
d_fin$adult_se <- NA

##################################
#### Step 4: Export outcome csv for further processing 
d_export <- d_fin %>% ungroup %>% select(study, outcome, id, sex, age, age_error, age_sd, age_lower, age_upper, resource, units, raw_return, raw_sd, adult_return, adult_sd, adult_se)

write_csv(d_export, paste0( paste(paste("data", paper_name, sep="_"),paper_section, sep="_"), ".csv" ))

setwd(home)
#################################
