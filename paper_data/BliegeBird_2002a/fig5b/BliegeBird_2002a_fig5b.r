usePackage <- function(p) {
  if (!is.element(p, installed.packages()[,1]))
    install.packages(p, dep = TRUE)
  require(p, character.only = TRUE)
}

usePackage("tidyverse")
usePackage("metaDigitise")

##################################
home <- getwd() # remember home directory to return to
temp_dir <- "paper_data/BliegeBird_2002a/fig5b" # temporarily set directory

### Pre-lim: digitize figure data
#metaDigitise(temp_dir)

# workflow: get points from one half of the scatterlpot (F/M) at a time, with a different group for every unique ID on the y axis. Starting top of y axis to bottom. Then do again with the male data (rght side).

#saveRDS(metaDigitise(temp_dir, summary=F), paste0(temp_dir, "/fig5b.rds"))

#################################
setwd(temp_dir)

paper_name <- strsplit(temp_dir, split="/", fixed=T)[[1]][2]
paper_section <- strsplit(temp_dir, split="/", fixed=T)[[1]][3]

d_list <- readRDS("fig5b.rds")

#### Step 1: Wrangle data ########
d <- bind_rows( d_list$`Fig5b. Age effects on shellfish collecting efficiency. Middle (men and boys).png`  ) %>% select(x, y)

colnames(d)[1] <- "age"


#create ids
d$id <- 1:nrow(d)

# Get average returns of adults, sex-specific
adult_avg <- d %>% 
  filter(age >= 20) %>% 
  summarise(mean_adult=mean(y), sd_adult=sd(y), n_adult=n())

# bring in adult average to main df
d [, 4:6] <- adult_avg

# filter out individuals above age 20
d <- filter(d, age <= 20)


##################################
#### Step 3: Add meta-data and additional covariate information
d_fin <- data.frame(study = rep( paper_name, nrow(d)))
d_fin$outcome <- paste(d_fin$study, "5b", sep="_") # total kcal/hr outcome, 1997 data
d_fin$id <- paste(d_fin$outcome, d$id, sep="_") # study *  outcome * individual, if data are individual rather than group-level
d_fin$sex <- "male" # "female", "male", or "both"
d_fin$age <- d$age 
d_fin$age_error <- NA # information on distribution of ages (sd), or just a range (interval)? 
d_fin$age_sd <- NA  # only if sd of ages is given
d_fin$age_lower <- NA # only if interval ages given
d_fin$age_upper <- NA # only if interval ages given
d_fin$resource <- "shellfish" # what type of foraging resource
d_fin$units <- "net kcal/hr" # whether the rate is per hour (hr), per day, or other
d_fin$raw_return <- d$y
d_fin$raw_sd <- NA
d_fin$adult_return <- d$mean_adult
d_fin$adult_sd <- d$sd_adult
d_fin$adult_se <- d$sd_adult / sqrt(d$n_adult)

##################################
#### Step 4: Export outcome csv for further processing 
d_export <- d_fin %>% ungroup %>% select(study, outcome, id, sex, age, age_error, age_sd, age_lower, age_upper, resource, units, raw_return, raw_sd, adult_return, adult_sd, adult_se)

write_csv(d_export, paste0( paste(paste("data", paper_name, sep="_"),paper_section, sep="_"), ".csv" ))

setwd(home)
#################################
