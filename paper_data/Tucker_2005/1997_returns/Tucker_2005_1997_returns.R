usePackage <- function(p) {
  if (!is.element(p, installed.packages()[,1]))
    install.packages(p, dep = TRUE)
  require(p, character.only = TRUE)
}

usePackage("tidyverse")
usePackage("metaDigitise")

##################################
home <- getwd() # remember home directory to return to
temp_dir <- "paper_data/Tucker_2005/1997_returns" # temporarily set directory

### Pre-lim: digitize figure data
# metaDigitise(temp_dir)

# workflow: get points from one half of the scatterlpot (F/M) at a time, with a different group for every unique ID on the y axis. Starting top of y axis to bottom. Then do again with the male data (rght side).

# saveRDS(metaDigitise(temp_dir, summary=F), paste0(temp_dir, "/1997_returns.rds"))

#################################
setwd(temp_dir)

paper_name <- strsplit(temp_dir, split="/", fixed=T)[[1]][2]
paper_section <- strsplit(temp_dir, split="/", fixed=T)[[1]][3]

d_list <- readRDS("1997_returns.rds")

#### Step 1: Wrangle data ########
d <- bind_rows( d_list$scatterplot$`1997_returns.png`, d_list$scatterplot$`1997_returns - male.png`  ) %>% select(id, x, y)

# A couple of obs ~0 were registered as negative, drop zero returns?
d$x <- ifelse(abs(d$x - 0) < 10, 0, d$x)

# Get sex labels
d$sex <- ifelse(substr(d$id, 1, 1) == "f", "female", "male")

# Average over the very small within-id variance in age due to extraction error
age_avg <- d %>%
  group_by(id) %>%
  summarise(age = mean(y))

# Get average returns of adults, sex-specific
adult_avg <- d %>% 
  filter(y >= 20) %>% 
  group_by(sex) %>%
    summarise(mean_adult=mean(x))

# bring in age to main df
d <- left_join(d, age_avg)

# filter out individuals above age 20
d <- filter(d, age <= 20)

##################################
#### Step 2: Calculate standardized effect sizes 
# calculate log returns ratio
lRR_fun <- function( m1, m2, sd1, sd2, n1, n2, value="mean" ) {
  
  lRR_mean <- log( m1/m2 )
  lRR_sd <- sqrt(  (sd1^2 / (n1*m1^2)) + (sd2^2 / (n2*m2^2)) )
  
  if  (value == "mean") return( lRR_mean )
  if  (value == "sd") return( lRR_sd )
}

d$lRR_mean <- ifelse(   d$sex == "female",
  lRR_fun( m2 = adult_avg$mean_adult[adult_avg$sex == "female"], m1 = d$x, sd1=NA, sd2=NA, n1=NA, n2=NA  ),
  lRR_fun( m2 = adult_avg$mean_adult[adult_avg$sex == "male"], m1 = d$x, sd1=NA, sd2=NA, n1=NA, n2=NA  )
)

d$lRR_sd <- NA

d_fin <- d
##################################
#### Step 3: Add meta-data and additional covariate information
d_fin$study <- paper_name # paper id
d_fin$outcome <- paste(d_fin$study, 1, sep="_") # total kcal/hr outcome, 1997 data
d_fin$id <- paste(d_fin$outcome, d$id, sep="_") # study *  outcome * individual, if data are individual rather than group-level
d_fin$sex <- d$sex # "female", "male", or "both"
d_fin$age_error <- NA # information on distribution of ages (sd), or just a range (interval)? 
d_fin$age_sd <- NA  # only if sd of ages is given
d_fin$age_lower <- NA # only if interval ages given
d_fin$age_upper <- NA # only if interval ages given
d_fin$resource <- "tubers;small_game;marine" # what type of foraging resource
d_fin$units <- "kcal/hr" # whether the rate is per hour (hr), per day, or other

##################################
#### Step 4: Export outcome csv for further processing 
d_export <- d_fin %>% ungroup %>% select(study, outcome, id, sex, age, age_error, age_sd, age_lower, age_upper, resource, units, lRR_mean, lRR_sd)

write_csv(d_export, paste0( paste(paste("data", paper_name, sep="_"),paper_section, sep="_"), ".csv" ))

setwd(home)
#################################
