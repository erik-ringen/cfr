usePackage <- function(p) {
  if (!is.element(p, installed.packages()[,1]))
    install.packages(p, dep = TRUE)
  require(p, character.only = TRUE)
}

usePackage("tidyverse")

##################################
home <- getwd() # remember home directory to return to
temp_dir <- "paper_data_round2/Froehle_2018/table4" # temporarily set directory

#################################
setwd(temp_dir)

paper_name <- strsplit(temp_dir, split="/", fixed=T)[[1]][2]
paper_section <- strsplit(temp_dir, split="/", fixed=T)[[1]][3]

d <- read.csv("table 4. Focal follow energy expenditure data.csv")

#### Step 1: Wrangle data ########

#extract column names from first row and remove first row
colnames(d) <-  d[1,]
d <- d[-1,]

#calculate returns as food brought to camp/ time spent in trip
d$trip_returns <- as.numeric(d$`Food returns to camp (kcal)`)/ as.numeric(d$`Duration (houe)`)

##################################
#### Step 3: Add meta-data and additional covariate information
d_fin <- data.frame(study = rep( paper_name, nrow(d)))
d_fin$outcome <- paste(d_fin$study, paper_section, sep="_") # 
d_fin$id <- paste(d_fin$outcome, d$ID, sep="_") # 
d_fin$sex <- d$Sex # 
d_fin$age <- as.numeric(d$`Age (year)`) 
d_fin$age_error <- NA # 
d_fin$age_sd <- NA  # 
d_fin$age_lower <- NA # 
d_fin$age_upper <- NA # 
d_fin$resource <- NA # non specified. COnsider wether to input mixed
d_fin$units <- "kcal/h" # 
d_fin$raw_return <- d$trip_returns
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
