usePackage <- function(p) {
  if (!is.element(p, installed.packages()[,1]))
    install.packages(p, dep = TRUE)
  require(p, character.only = TRUE)
}

usePackage("tidyverse")
usePackage("metaDigitise")

##################################
home <- getwd() # remember home directory to return to
temp_dir <- "paper_data_round2/Crittenden_2013/Fig_2/" # temporarily set directory

### Pre-lim: digitize figure data
#metaDigitise(temp_dir)

# workflow: get all points, no groups. y axis is on the natural log scale (we think)

#saveRDS(metaDigitise(temp_dir, summary=F), paste0(temp_dir, "Fig_2.rds"))

#################################
setwd(temp_dir)

paper_name <- strsplit(temp_dir, split="/", fixed=T)[[1]][2]
paper_section <- strsplit(temp_dir, split="/", fixed=T)[[1]][3]

d_list <- readRDS("Fig_2.rds")

#### Step 1: Wrangle data ########
d <- bind_rows(d_list$scatterplot) %>% select(x,y)

#add IDs 
d$id <- NA

# Sex of individual unknown, although 6 males and 7 females
d$sex <- 6/(6+7)

# Round off age as it is presented as integer
d$age <- round(d$x)

##################################

d_fin <- d 
##################################
#### Step 3: Add meta-data and additional covariate information
d_fin$study <- paper_name # paper id
d_fin$outcome <- paste(d_fin$study, paper_section, sep="_") # 
d_fin$id <- paste(d_fin$outcome, d$id, sep="_") # 
d_fin$sex <- d$sex            # 6 males and 7 females
d_fin$age_error <- NA # 
d_fin$age_sd <- NA  # 
d_fin$age_lower <- NA # 
d_fin$age_upper <- NA # 
d_fin$resource <- "mixed"     # fruit;birds;tubers;honey;small_game;vegetables
d_fin$units <- "kcal/day"     # total kcal per day
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
