usePackage <- function(p) {
  if (!is.element(p, installed.packages()[,1]))
    install.packages(p, dep = TRUE)
  require(p, character.only = TRUE)
}

usePackage("tidyverse")
usePackage("metaDigitise")

##################################
home <- getwd() # remember home directory to return to
temp_dir <- "paper_data/Gurven_2006/fig6b" # temporarily set directory

### Pre-lim: digitize figure data
#metaDigitise(temp_dir)

#saveRDS(metaDigitise(temp_dir, summary=F), paste0(temp_dir, "/fig6b.rds"))

#################################
setwd(temp_dir)

paper_name <- strsplit(temp_dir, split="/", fixed=T)[[1]][2]
paper_section <- strsplit(temp_dir, split="/", fixed=T)[[1]][3]

d_list <- readRDS("fig6b.rds")

#### Step 1: Wrangle data ########
d <- bind_rows( d_list$scatterplot$`fig6b` ) %>% select(id, x, y)

# A couple of obs ~0 were registered as negative
d$y <- ifelse(abs(d$y - 0) < 12, 0, d$y)
d$y <- ifelse(d$y < 0, 0, d$y)

# Get sex labels
d$sex <-  "male"

# Average over the very small within-id variance in age due to extraction error
age_avg <- d %>%
  group_by(id) %>%
  summarise(age = mean(x))

# Get average returns of adults, sex-specific
adult_avg <- d %>% 
  filter(x >= 20) %>% 
    summarise(mean_adult=mean(y), sd_adult=sd(y), n_adult=n())

# bring in age and adult values to main df
d <- left_join(d, age_avg)

d [ ,6:8] <- adult_avg

# filter out individuals above age 20
d <- filter(d, age <= 20)

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
d_fin$resource <- "game" # what type of foraging resource
d_fin$units <- "kcal/h" # written in data as cal/h but assumed to be a mistake
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
