usePackage <- function(p) {
  if (!is.element(p, installed.packages()[,1]))
    install.packages(p, dep = TRUE)
  require(p, character.only = TRUE)
}

usePackage("tidyverse")
usePackage("metaDigitise")

##################################
home <- getwd() # remember home directory to return to
temp_dir <- "paper_data/Gurven_2006/fig6a" # temporarily set directory

### Pre-lim: digitize figure data
# metaDigitise(temp_dir)

# workflow: no groups needed, simply select all points

# saveRDS(metaDigitise(temp_dir, summary=F), paste0(temp_dir, "/fig6a.rds"))

#################################
setwd(temp_dir)

paper_name <- strsplit(temp_dir, split="/", fixed=T)[[1]][2]
paper_section <- strsplit(temp_dir, split="/", fixed=T)[[1]][3]

d_list <- readRDS(paste0(paper_section, ".rds"))

#### Step 1: Wrangle data ########
d <- bind_rows(d_list$scatterplot) %>% select(x,y)
colnames(d)[1] <- "age"

# Returns that are within small margin of 0 are actually zero-return
d$y <- ifelse(abs(d$y - 0) < 10, 0, d$y)

# All are male
d$sex <- "male"

# Get average returns of adults, sex-specific
adult_avg <- d %>% 
  filter(age > 20) %>% 
  group_by(sex) %>%
    summarise(mean_adult=mean(y))

# bring in age to main df
d <- left_join(d, adult_avg)

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

d$lRR_mean <- lRR_fun(m1=d$y, m2=d$mean_adult, sd1=NA, sd2=NA, n1=NA, n2=NA)
d$lRR_sd <- NA

d_fin <- d
##################################
#### Step 3: Add meta-data and additional covariate information
d_fin$study <- paper_name # paper id
d_fin$outcome <- paste(d_fin$study, paper_section, sep="_")
d_fin$id <- NA # study *  outcome * individual, if data are individual rather than group-level
d_fin$sex <- d$sex # "female", "male", or "both"
d_fin$age_error <- "none" # information on distribution of ages (sd), or just a range (interval)? 
d_fin$age_sd <- NA  # only if sd of ages is given
d_fin$age_lower <- NA # only if interval ages given
d_fin$age_upper <- NA # only if interval ages given
d_fin$resource <- "meat" # what type of foraging resource
d_fin$timescale <- "hr" # whether the rate is per hour (hr), per day, or other

##################################
#### Step 4: Export outcome csv for further processing 
d_export <- d_fin %>% ungroup %>% select(study, outcome, id, sex, age, age_error, age_sd, age_lower, age_upper, resource, timescale, lRR_mean, lRR_sd)

write_csv(d_export, paste0( paste(paste("data", paper_name, sep="_"),paper_section, sep="_"), ".csv" ))

setwd(home)
#################################
