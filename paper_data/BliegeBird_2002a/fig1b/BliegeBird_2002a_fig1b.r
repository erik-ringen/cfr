usePackage <- function(p) {
  if (!is.element(p, installed.packages()[,1]))
    install.packages(p, dep = TRUE)
  require(p, character.only = TRUE)
}

usePackage("tidyverse")
usePackage("metaDigitise")

##################################
home <- getwd() # remember home directory to return to
temp_dir <- "paper_data/BliegeBird_2002a/fig1b" # temporarily set directory

### Pre-lim: digitize figure data
#metaDigitise(temp_dir)

# workflow: get points from one half of the scatterlpot (F/M) at a time, with a different group for every unique ID on the y axis. Starting top of y axis to bottom. Then do again with the male data (rght side).

#saveRDS(metaDigitise(temp_dir, summary=F), paste0(temp_dir, "/fig1b.rds"))

#################################
setwd(temp_dir)

paper_name <- strsplit(temp_dir, split="/", fixed=T)[[1]][2]
paper_section <- strsplit(temp_dir, split="/", fixed=T)[[1]][3]

d_list <- readRDS("fig1b.rds")

#### Step 1: Wrangle data ########
d <- bind_rows( d_list$`Fig1b. age effects on large-hook beach fishing efficiency.png`  ) %>% select(x, y)

#rename column
colnames(d)[1] <- "age"

# Get sex labels
d$sex <- "both"

#create ids
d$id <- 1:nrow(d)

#round age
d$age <- round(d$age)

##################################
###ADD ADULT AVERAGE FROM OTHER HALF OF FIGURE

dd<- read.csv("../fig1a/data_BliegeBird_2002a_fig1a.csv")

d$mean_adult <- dd$adult_return 
d$sd_adult   <- dd$adult_sd 
d$adult_se   <- dd$adult_se

##################################
#### Step 3: Add meta-data and additional covariate information
d_fin <- data.frame(study = rep( paper_name, nrow(d)))
d_fin$outcome <- paste(d_fin$study, paper_section, sep="_") # 
d_fin$id <- paste(d_fin$outcome, d$id, sep="_") # 
d_fin$sex <- d$sex # 
d_fin$age <- d$age 
d_fin$age_error <- NA # 
d_fin$age_sd <- NA  # 
d_fin$age_lower <- NA # 
d_fin$age_upper <- NA # 
d_fin$resource <- "fish" # 
d_fin$units <- "net kcal/hr" # 
d_fin$raw_return <- d$y
d_fin$raw_sd <- NA
d_fin$adult_return <- d$mean_adult
d_fin$adult_sd <- d$sd_adult
d_fin$adult_se <- d$adult_se

##################################
#### Step 4: Export outcome csv for further processing 
d_export <- d_fin %>% ungroup %>% select(study, outcome, id, sex, age, age_error, age_sd, age_lower, age_upper, resource, units, raw_return, raw_sd, adult_return, adult_sd, adult_se)

write_csv(d_export, paste0( paste(paste("data", paper_name, sep="_"),paper_section, sep="_"), ".csv" ))

setwd(home)
#################################
