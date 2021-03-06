usePackage <- function(p) {
  if (!is.element(p, installed.packages()[,1]))
    install.packages(p, dep = TRUE)
  require(p, character.only = TRUE)
}

usePackage("tidyverse")
usePackage("metaDigitise")

##################################
home <- getwd() # remember home directory to return to
temp_dir <- "paper_data_round2/Froehle_2018/fig6b" # temporarily set directory

### Pre-lim: digitize figure data
#metaDigitise(temp_dir)

# workflow: get points from one half of the scatterlpot (F/M) at a time, with a different group for every unique ID on the y axis. Starting top of y axis to bottom. Then do again with the male data (rght side).

#saveRDS(metaDigitise(temp_dir, summary=F), paste0(temp_dir, "/fig6b.rds"))

#################################
setwd(temp_dir)

paper_name <- strsplit(temp_dir, split="/", fixed=T)[[1]][2]
paper_section <- strsplit(temp_dir, split="/", fixed=T)[[1]][3]

d_list <- readRDS("fig6b.rds")

##################################
#### Step 1: Wrangle data
d <- bind_rows( d_list$'fig6b.png'  ) %>% select(  id,  mean, error, n)

#give age limits from text
d$age_lower <- c( 10, 5)
d$age_upper <- c( 14, 10)


##################################
#### Step 3: Add meta-data and additional covariate information
d_fin <- data.frame(study = rep( paper_name, nrow(d)))
d_fin$outcome <- paste(d_fin$study, paper_section, sep="_") # total kcal/hr outcome, 1997 data
d_fin$id <-NA # study *  outcome * individual, if data are individual rather than group-level
d_fin$sex <- d$id # "female", "male", or "both"
d_fin$age <- NA 
d_fin$age_error <- NA # information on distribution of ages (sd), or just a range (interval)? 
d_fin$age_sd <- NA  # only if sd of ages is given
d_fin$age_lower <- d$age_lower # only if interval ages given
d_fin$age_upper <- d$age_upper # only if interval ages given
d_fin$resource <- NA # what type of foraging resource
d_fin$units <- "kcal/h" # whether the rate is per hour (hr), per day, or other
d_fin$raw_return <- d$mean
d_fin$raw_sd <- d$error
d_fin$raw_se <- d$error / sqrt(d$n)
d_fin$adult_return <- NA
d_fin$adult_sd <- NA
d_fin$adult_se <- NA

##################################
#### Step 4: Export outcome csv for further processing 
d_export <- d_fin %>% ungroup %>% select(study, outcome, id, sex, age, age_error, age_sd, age_lower, age_upper, resource, units, raw_return, raw_sd, raw_se, adult_return, adult_sd, adult_se)

write_csv(d_export, paste0( paste(paste("data", paper_name, sep="_"),paper_section, sep="_"), ".csv" ))

setwd(home)
#################################
