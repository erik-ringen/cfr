usePackage <- function(p) {
  if (!is.element(p, installed.packages()[,1]))
    install.packages(p, dep = TRUE)
  require(p, character.only = TRUE)
}

usePackage("tidyverse")
usePackage("metaDigitise")

##################################
home <- getwd() # remember home directory to return to
temp_dir <- "paper_data_round2/Bock_2005/fig5.2" # temporarily set directory

### Pre-lim: digitize figure data
#metaDigitise(temp_dir)

# workflow: extract all points, no groups

#saveRDS(metaDigitise(temp_dir, summary=F), paste0(temp_dir, "/fig5.2.rds"))

#################################
setwd(temp_dir)

paper_name <- strsplit(temp_dir, split="/", fixed=T)[[1]][2]
paper_section <- strsplit(temp_dir, split="/", fixed=T)[[1]][3]

d_list <- readRDS("fig5.2.rds")

#### Step 1: Wrangle data ########
d <- bind_rows( d_list$scatterplot ) %>% select( x, y)

#rename columns
colnames(d)[1] <-  c ( "age")

#IDs
d$id <- 1:nrow(d)

#negative and very low value as zero
d$y <- ifelse(abs(d$y - 0) < 5, 0, d$y)

#round ages (as ages are all close to round numbers, I assume variation comes from errors in clicking on the dots)
d$age <- round(d$age)

##################################
#### Step 3: Add meta-data and additional covariate information
d_fin <- data.frame(study = rep( paper_name, nrow(d)))
d_fin$outcome <- paste(d_fin$study, paper_section, sep="_") # 
d_fin$id <- paste(d_fin$outcome, d$id, sep="_") # 
d_fin$sex <- "female" # all female
d_fin$age <- d$age 
d_fin$age_error <- NA # 
d_fin$age_sd <- NA  # 
d_fin$age_lower <- NA # 
d_fin$age_upper <- NA # 
d_fin$resource <- "fish" # 
d_fin$units <- "kcal/h" # 
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
