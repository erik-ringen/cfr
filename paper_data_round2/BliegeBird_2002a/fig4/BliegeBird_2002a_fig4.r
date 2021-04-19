usePackage <- function(p) {
  if (!is.element(p, installed.packages()[,1]))
    install.packages(p, dep = TRUE)
  require(p, character.only = TRUE)
}

usePackage("tidyverse")
usePackage("metaDigitise")

##################################
home <- getwd() # remember home directory to return to
temp_dir <- "paper_data_round2/BliegeBird_2002a/fig4" # temporarily set directory

### Pre-lim: digitize figure data
#metaDigitise(temp_dir)

# workflow: get points from one half of the scatterlpot (F/M) at a time, with a different group for every unique ID on the y axis. Starting top of y axis to bottom. Then do again with the male data (rght side).

#saveRDS(metaDigitise(temp_dir, summary=F), paste0(temp_dir, "/fig4.rds"))

#################################
setwd(temp_dir)

paper_name <- strsplit(temp_dir, split="/", fixed=T)[[1]][2]
paper_section <- strsplit(temp_dir, split="/", fixed=T)[[1]][3]

d_list <- readRDS("fig4.rds")

#### Step 1: Wrangle data ########
d <- bind_rows( d_list$`Figure_4 Spearfishing efficiency across ageexperience.png`  ) %>% select(id, mean, error, n)

#rename column
colnames(d)[1] <- "group"

#remove best man and keep only other men and children
d <- d[2:3,]

# Get sex labels
d$sex <- NA

#create ids
d$id <- NA

#add age bounds
d$age_lower <- c (5, 16) # only if interval ages given
d$age_upper <- c (15, 65) # only if interval ages given

##################################
#calculate SD from confidence intervals
#sd was calculated from error, presented as CI95, with formula SD= ???N X (upper limit - lower limit)/ 2*students' t value (for appropriate sample size)

d$sd <- sqrt(d[,"n"]) * ( d[,"error"]/ c(5.552, 4.612))

##################################
#### Step 3: Add meta-data and additional covariate information
d_fin <- data.frame(study =  paper_name)
d_fin$outcome <- paste(d_fin$study, paper_section, sep="_") # 
d_fin$id <- NA # 
d_fin$sex <- "both" # 
d_fin$age <- NA 
d_fin$age_error <- NA # 
d_fin$age_sd <- NA  # 
d_fin$age_lower <- d$age_lower[1] # 
d_fin$age_upper <- d$age_upper[1] # 
d_fin$resource <- "fish" # 
d_fin$units <- "net kcal/h" # all data in paper defined as "efficiency", hence net
d_fin$raw_return <- d$mean[1]
d_fin$raw_sd <- d$sd[1] 
d_fin$raw_se <- d$sd[1] / sqrt(d$n[1]) 
d_fin$adult_return <- d$mean[2]
d_fin$adult_sd <- d$sd[2]
d_fin$adult_se <- d$sd[2] / sqrt(d$n[2])

##################################
#### Step 4: Export outcome csv for further processing 
d_export <- d_fin %>% ungroup %>% select(study, outcome, id, sex, age, age_error, age_sd, age_lower, age_upper, resource, units, raw_return, raw_sd,raw_se, adult_return, adult_sd, adult_se)

write_csv(d_export, paste0( paste(paste("data", paper_name, sep="_"),paper_section, sep="_"), ".csv" ))

setwd(home)
#################################
