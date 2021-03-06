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

#rename columns
colnames(d)[1] <- "age"

#create ids
d$id <- 1:nrow(d)

# Returns that are within small margin of 0 are actually zero-return
d$y <- ifelse(abs(d$y - 0) < 10, 0, d$y)

# All are male
d$sex <- "male"

# Get average returns of adults, sex-specific
adult_avg <- d %>% 
  filter(age > 20) %>% 
  group_by(sex) %>%
    summarise(mean_adult=mean(y), sd_adult=sd(y), n_adult=n())

# bring in adult values to main df
d <- left_join(d, adult_avg)

# filter out individuals above age 20
d <- filter(d, age <= 20)

##################################


##################################
#### Step 3: Add meta-data and additional covariate information
#d_fin$study <- paper_name # paper id
d_fin <- data.frame(study = rep( paper_name, nrow(d)))
d_fin$outcome <- paste(d_fin$study, paper_section, sep="_")
d_fin$id <- paste(d_fin$outcome, d$id, sep="_") # 
d_fin$sex <- d$sex # "all male
d_fin$age <- d$age 
d_fin$age_error <- NA # 
d_fin$age_sd <- NA  # 
d_fin$age_lower <- NA # 
d_fin$age_upper <- NA # 
d_fin$resource <- "game" # 
d_fin$units <- "kcal/h"     #reported as cal/h but seems on the kcal scale.
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
