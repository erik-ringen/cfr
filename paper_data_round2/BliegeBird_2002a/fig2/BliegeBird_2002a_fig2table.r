usePackage <- function(p) {
  if (!is.element(p, installed.packages()[,1]))
    install.packages(p, dep = TRUE)
  require(p, character.only = TRUE)
}

usePackage("tidyverse")
usePackage("metaDigitise")

##################################
home <- getwd() # remember home directory to return to
temp_dir <- "paper_data/BliegeBird_2002a/fig2" # temporarily set directory

### Pre-lim: digitize figure data
#metaDigitise(temp_dir)

# workflow: get points from one half of the scatterlpot (F/M) at a time, with a different group for every unique ID on the y axis. Starting top of y axis to bottom. Then do again with the male data (rght side).

#saveRDS(metaDigitise(temp_dir, summary=F), paste0(temp_dir, "/fig2.rds"))

#################################
setwd(temp_dir)

paper_name <- strsplit(temp_dir, split="/", fixed=T)[[1]][2]
paper_section <- strsplit(temp_dir, split="/", fixed=T)[[1]][3]

d <- read.csv("fig2table.csv")

#### Step 1: Wrangle data ########
#rename columns
colnames(d)[1:3] <- c ("age_group", "n", "mean_kcal/h")

#add age bounds
d$age_lower <- c (5, 10, 14, 30, 51) # only if interval ages given
d$age_upper <- c (9, 13, 29, 50, 65) # only if interval ages given

##################################
#calculate average and sd for adult values
d_a <- d [ which(d$age_lower >= 20), ] #select adult groups

#construct functions to calculate mean and sd of adult samples
grand.mean <- function(M, N) {weighted.mean(M, N)} 
grand.sd   <- function(S, M, N) {sqrt(weighted.mean(S^2 + M^2, N) -
                                      weighted.mean(M, N)^2)}
#calculate mean and sd of adult sample
d_a$adult_return <- grand.mean (d_a$`mean_kcal/h`, d_a$n)
d_a$adult_sd <- grand.sd (d_a$s.d. , d_a$`mean_kcal/h`, d_a$n)



##################################
#select age groups

d_select <- d[ 1:3, ]

##################################

##################################
#### Step 3: Add meta-data and additional covariate information
d_fin <- data.frame(study = rep( paper_name, nrow(d_select)))
d_fin$outcome <- paste(d_fin$study, paper_section, sep="_") # 
d_fin$id <- NA # 
d_fin$sex <- "both" # 
d_fin$age <- NA 
d_fin$age_error <- NA #  
d_fin$age_sd <- NA  # 
d_fin$age_lower <- d_select$age_lower # 
d_fin$age_upper <- d_select$age_upper # o
d_fin$resource <- "fish" # 
d_fin$units <- "net kcal/hr" # 
d_fin$raw_return <- d_select$mean
d_fin$raw_sd <- d_select$s.d. 
d_fin$raw_se <- d_select$s.d. / sqrt(sum(d_select$n))
d_fin$adult_return <- d_a$adult_return[1]
d_fin$adult_sd <- d_a$adult_sd[1]
d_fin$adult_se <- d_a$adult_sd[1] / sqrt (sum (d_a$n))

##################################
#### Step 4: Export outcome csv for further processing 
d_export <- d_fin %>% ungroup %>% select(study, outcome, id, sex, age, age_error, age_sd, age_lower, age_upper, resource, units, raw_return, raw_sd, raw_se, adult_return, adult_sd, adult_se)

write_csv(d_export, paste0( paste(paste("data", paper_name, sep="_"),paper_section, sep="_"), ".csv" ))

setwd(home)
#################################
