usePackage <- function(p) {
  if (!is.element(p, installed.packages()[,1]))
    install.packages(p, dep = TRUE)
  require(p, character.only = TRUE)
}

usePackage("tidyverse")
usePackage("metaDigitise")

##################################
home <- getwd() # remember home directory to return to
temp_dir <- "paper_data/BlurtonJones_1989/table2" # temporarily set directory

### Pre-lim: digitize figure data
#metaDigitise(temp_dir)

# workflow: get points from one half of the scatterlpot (F/M) at a time, with a different group for every unique ID on the y axis. Starting top of y axis to bottom. Then do again with the male data (rght side).

#saveRDS(metaDigitise(temp_dir, summary=F), paste0(temp_dir, "/fig2.rds"))

#################################
setwd(temp_dir)

paper_name <- strsplit(temp_dir, split="/", fixed=T)[[1]][2]
paper_section <- strsplit(temp_dir, split="/", fixed=T)[[1]][3]

d <- read.csv("table2. Children and their foraging returns (in grams per hour).csv")

#### Step 1: Wrangle data ########

colnames(d)[c (1, 6:8)] <- c ("ID", "Ekwa", "honey", "fruit")

d$id <- 1:nrow(d)

#make column Age numeric and give age >20 to women
d[nrow(d), "Age"] <- 100 
d$Age <- as.numeric(d$Age)

#make column baobab numeric and give 0 to children who tried
d[1:2, "fruit"] <- 0 
d$fruit <- as.numeric(d$fruit)

#create a tubers column that combines Makalita and Ekwa
d$tubers <- rowSums (d [, c ("Makalita", "Ekwa" )], na.rm = TRUE)
d$tubers[d$tubers == 0] <- NA

d <- d [ , -c ( 5, 6 )]

#make data long
d_long <- d %>% 
  pivot_longer (cols = c ("tubers", "honey", "fruit"), 
                names_to = "resources",
                values_to = "raw_returns")

##################################
d_long$adults <-  rep (as.numeric (d [nrow (d), c (8, 5, 6) ] ), nrow(d))


##################################

# filter out individuals above age 20
d_long <- filter(d_long, Age <= 20)

##################################

##################################
#### Step 3: Add meta-data and additional covariate information
d_fin <- data.frame(study = rep( paper_name, nrow(d_long)))
d_fin$outcome <- paste(d_fin$study, 1, sep="_") #
d_fin$id <- paste(d_fin$outcome, d_long$id, sep="_") # study *  outcome * individual, if data are individual rather than group-level
d_fin$sex <- NA # "female", "male", or "both"
d_fin$age <- d_long$Age
d_fin$age_error <- NA # information on distribution of ages (sd), or just a range (interval)? 
d_fin$age_sd <- NA  # only if sd of ages is given
d_fin$age_lower <- NA # only if interval ages given
d_fin$age_upper <- NA # only if interval ages given
d_fin$resource <- d_long$resources # what type of foraging resource
d_fin$units <- "g/h" # whether the rate is per hour (hr), per day, or other
d_fin$raw_return <- d_long$raw_returns
d_fin$raw_sd <- NA
d_fin$adult_return <- d_long$adults
d_fin$adult_sd <- NA
d_fin$adult_se <- NA

##################################
#### Step 4: Export outcome csv for further processing 
d_export <- d_fin %>% ungroup %>% select(study, outcome, id, sex, age, age_error, age_sd, age_lower, age_upper, resource, units, raw_return, raw_sd, adult_return, adult_sd, adult_se)

write_csv(d_export, paste0( paste(paste("data", paper_name, sep="_"),paper_section, sep="_"), ".csv" ))

setwd(home)
#################################
