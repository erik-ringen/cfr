usePackage <- function(p) {
  if (!is.element(p, installed.packages()[,1]))
    install.packages(p, dep = TRUE)
  require(p, character.only = TRUE)
}

usePackage("tidyverse")
usePackage("metaDigitise")

##################################
home <- getwd() # remember home directory to return to
temp_dir <- "paper_data/Crittenden_2013/Fig_2/" # temporarily set directory

### Pre-lim: digitize figure data
#metaDigitise(temp_dir)

# workflow: get points from one half of the scatterlpot (F/M) at a time, with a different group for every unique ID on the y axis. Starting top of y axis to bottom. Then do again with the male data (rght side).

#saveRDS(metaDigitise(temp_dir, summary=F), paste0(temp_dir, "Fig_2.rds"))

#################################
setwd(temp_dir)

paper_name <- strsplit(temp_dir, split="/", fixed=T)[[1]][2]
paper_section <- strsplit(temp_dir, split="/", fixed=T)[[1]][3]

d_list <- readRDS("Fig_2.rds")

#### Step 1: Wrangle data ########
d <- select( d_list$scatterplot$`Fig_2 Kilocalories collected during focal follow foragign trips (amount consumed plus amount collected) (n_1 = 6 males, n_2 = 7 females).png`, x, y)

#add IDs 
d$id <- paste("b", 1:nrow(d), sep = '')

# Sex of individual unknown, although 6 males and 7 females
d$sex <- 6/(6+7)

# Round off age as it is presented as integer
d$age <- round(d$x)

##################################

d_fin <- d 
##################################
#### Step 3: Add meta-data and additional covariate information
d_fin$study <- paper_name # paper id
d_fin$outcome <- paste(d_fin$study, paper_section, sep="_") # total kcal/hr outcome, 1997 data
d_fin$id <- paste(d_fin$outcome, d$id, sep="_") # study *  outcome * individual, if data are individual rather than group-level
d_fin$sex <- d$sex # "female", "male", or "both"
d_fin$age_error <- NA # information on distribution of ages (sd), or just a range (interval)? 
d_fin$age_sd <- NA  # only if sd of ages is given
d_fin$age_lower <- NA # only if interval ages given
d_fin$age_upper <- NA # only if interval ages given
d_fin$resource <- "mixed" # fruit;birds;tubers;honey;small_game;vegetables
d_fin$units <- "kcal/day" # total kcal per day
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
