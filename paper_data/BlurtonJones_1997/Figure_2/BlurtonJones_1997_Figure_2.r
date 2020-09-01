usePackage <- function(p) {
  if (!is.element(p, installed.packages()[,1]))
    install.packages(p, dep = TRUE)
  require(p, character.only = TRUE)
}

usePackage("tidyverse")
usePackage("metaDigitise")

##################################
home <- getwd() # remember home directory to return to
temp_dir <- "paper_data/BlurtonJones_1997/Figure_2" # temporarily set directory

### Pre-lim: digitize figure data
#metaDigitise(temp_dir)

# workflow: get points from one half of the scatterlpot (F/M) at a time, with a different group for every unique ID on the y axis. Starting top of y axis to bottom. Then do again with the male data (rght side).

#saveRDS(metaDigitise(temp_dir, summary=F), paste0(temp_dir, "/Figure_2.rds"))

#################################
setwd(temp_dir)

paper_name <- strsplit(temp_dir, split="/", fixed=T)[[1]][2]
paper_section <- strsplit(temp_dir, split="/", fixed=T)[[1]][3]

d_list <- readRDS("Figure_2.rds")

#### Step 1: Wrangle data ########
d <- bind_rows( d_list$'Figure_2 Returns (kcal acquired per hour of searching and digging) for ekwa roots by age of forager..png'  ) %>% select(id, x, y)

colnames(d)[1:2] <- c ("sample", "age")

#negative and very low value as zero
d$y <- ifelse(abs(d$y - 0) < 21, 0, d$y)

#IDs
d$id <- 1:nrow(d)



##################################
#### Step 3: Add meta-data and additional covariate information
d_fin <- data.frame(study = rep( paper_name, nrow(d)))
d_fin$outcome <- paste(d_fin$study, 2, sep="_") # total kcal/hr outcome, 1997 data
d_fin$id <- paste(d_fin$outcome, d$sample, d$id, sep="_") # study *  outcome * individual, if data are individual rather than group-level
d_fin$sex <- "both" # "female", "male", or "both"
d_fin$age <- d$age 
d_fin$age_error <- NA # information on distribution of ages (sd), or just a range (interval)? 
d_fin$age_sd <- NA  # only if sd of ages is given
d_fin$age_lower <- NA # only if interval ages given
d_fin$age_upper <- NA # only if interval ages given
d_fin$resource <- "tubers" # what type of foraging resource
d_fin$units <- "kcal/hr" # whether the rate is per hour (hr), per day, or other
d_fin$raw_return <- d$y
d_fin$raw_sd <- NA
d_fin$adult_return <- NA
d_fin$adult_sd <- NA
d_fin$adult_se <- NA

##################################
#### Step 4: Export outcome csv for further processing 
d_export <- d_fin %>% ungroup %>% select(study, outcome, id, sex, age, age_error, age_sd, age_lower, age_upper, resource, units, raw_return, raw_sd, adult_return, adult_sd, adult_se)

write_csv(d_export, paste0( paste(paste("data", paper_name, sep="_"),paper_section, sep="_"), ".csv" ))

setwd(home)
#################################