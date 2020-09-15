usePackage <- function(p) {
  if (!is.element(p, installed.packages()[,1]))
    install.packages(p, dep = TRUE)
  require(p, character.only = TRUE)
}

usePackage("tidyverse")
usePackage("metaDigitise")

##################################
home <- getwd() # remember home directory to return to
temp_dir <- "paper_data/Crittenden_2013/Table_1/" # temporarily set directory

setwd(temp_dir)

paper_name <- strsplit(temp_dir, split="/", fixed=T)[[1]][2]
paper_section <- strsplit(temp_dir, split="/", fixed=T)[[1]][3]

d_raw <- read.csv("Table_1 Average daily returns for individual foragers.csv")

#### Step 1: Wrangle data ########
d <- select(d_raw, Forager)

#extract data from table
d$age <- d_raw$Age
d$sex <- ifelse (d_raw$Sex..1...male..2...female == 1, "male", "female")
d$return <- d_raw$Average.kcal.collected.per.foraging.day

#make ID
d$id <- ifelse (d$sex == "female", 
                paste( "f", d$Forager, sep = ""),
                paste( "m", d$Forager, sep = ""))

##################################
#rearrange columns
d_fin <- d [, c(5, 2, 3, 4)]

##################################
#### Step 3: Add meta-data and additional covariate information
d_fin$study <- paper_name # paper id
d_fin$outcome <- paste(d_fin$study, paper_section, sep="_") #
d_fin$id <- paste(d_fin$outcome, d$id, sep="_") # 
d_fin$sex <- d$sex # "
d_fin$age_error <- NA #  
d_fin$age_sd <- NA  # 
d_fin$age_lower <- NA #
d_fin$age_upper <- NA # 
d_fin$resource <- "mixed" # fruit;birds;tubers;honey;small_game;vegetables
d_fin$units <- "kcal/day" # total kcal day
d_fin$raw_return <- d$return
d_fin$raw_sd <- NA
d_fin$adult_return <- NA
d_fin$adult_sd <- NA
d_fin$adult_se <- NA

##################################
#### Step 4: Export outcome csv for further processing 
d_export <- d_fin %>% ungroup %>% select(study, outcome, id, sex, age, age_error, age_sd, age_lower, age_upper, resource, units, raw_return, raw_sd, adult_return, adult_sd, adult_se)

write_csv(d_export, paste0( paste(paste("data", paper_name, sep="_"),paper_section, sep="_"), ".csv" ))

setwd(home)
