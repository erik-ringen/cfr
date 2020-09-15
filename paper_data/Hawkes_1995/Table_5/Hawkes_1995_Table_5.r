usePackage <- function(p) {
  if (!is.element(p, installed.packages()[,1]))
    install.packages(p, dep = TRUE)
  require(p, character.only = TRUE)
}

usePackage("tidyverse")

##################################
home <- getwd() # remember home directory to return to
temp_dir <- "paper_data/Hawkes_1995/Table_5" # temporarily set directory
setwd(temp_dir)

paper_name <- strsplit(temp_dir, split="/", fixed=T)[[1]][2]
paper_section <- strsplit(temp_dir, split="/", fixed=T)[[1]][3]

d <- read.csv("Table_5 Acquisition rates for Tafabe and Ondishibe berries by sex and age.csv")
dd <- read.csv("../Table_2/Table_2 Time allocation to food acqiusition and processing in or near camp by focal child.csv")

#### Step 1: Wrangle data ########


#turn around dataframe
d_long <- pivot_longer(d, cols = -food, names_to = "who", values_to = "value")
d_long$what <- str_extract(d_long$who, "[^_]+")
d_long$who <- sub("^[^_]*_", "",d_long$who)

d_wide <- pivot_wider(d_long , names_from = "what", values_from = "value")

##############################
#reshape table to have adult values on the side
for(i in 1: (nrow(d_wide) -1)) {
  d_wide[ i, 6:8 ] <- d_wide[ (i +1) ,3 : 5]
}

#change names
colnames(d_wide)[6:8] <- c("adult_mean", "adult_S.E.", "adult_n") 

#remove adult values
d_all <- d_wide %>% 
  filter(row_number() %% 2 == 1)

#add sex
d_all$sex <- rep( c ("female", "male", "both") , 2)

#add age info
d_all$age_mean <- rep ( 
  c( mean(dd[ which(dd$Sex == "f"),"Estimated_age"], na.rm = TRUE),
     mean(dd[ which(dd$Sex == "m"),"Estimated_age"], na.rm = TRUE),
     mean(dd[ ,"Estimated_age"], na.rm = TRUE)), 2)

d_all$age_sd <- rep ( 
  c( sd(dd[ which(dd$Sex == "f"),"Estimated_age"], na.rm = TRUE),
     sd(dd[ which(dd$Sex == "m"),"Estimated_age"], na.rm = TRUE),
     sd(dd[ ,"Estimated_age"], na.rm = TRUE)), 2)


d_all$age_upper <- rep ( 
             c( max(dd[ which(dd$Sex == "f"),"Estimated_age"], na.rm = TRUE),
                max(dd[ which(dd$Sex == "m"),"Estimated_age"], na.rm = TRUE),
                max(dd[ ,"Estimated_age"], na.rm = TRUE)), 2)

d_all$age_lower <- rep ( 
  c( min(dd[ which(dd$Sex == "f"),"Estimated_age"], na.rm = TRUE),
     min(dd[ which(dd$Sex == "m"),"Estimated_age"], na.rm = TRUE),
     min(dd[ ,"Estimated_age"], na.rm = TRUE)), 2)


##################################
#### Add meta-data and additional covariate information
d_fin <- data.frame(study = rep( paper_name, nrow(d_all)))
d_fin$outcome <- paste(d_fin$study,  paper_section, d_all$food, sep="_") # 
d_fin$id <-  NA # study *  outcome * individual, if data are individual rather than group-level
d_fin$sex <- d_all$sex # "female", "male", or "both"
d_fin$age <- d_all$age_mean # no mean age given
d_fin$age_error <- NA # information on distribution of ages (sd), or just a range (interval)? Ages as presented in table 2 same paper
d_fin$age_sd <- d_all$age_sd  # only if sd of ages is given
d_fin$age_lower <- d_all$age_lower # only if interval ages given
d_fin$age_upper <- d_all$age_upper # only if interval ages given; 
d_fin$resource <- "fruit" # what type of foraging resource
d_fin$units <- "g/h" # whether the rate is per hour (hr), per day, or other
d_fin$raw_return <- d_all$mean
d_fin$raw_sd <- d_all$S.E. * sqrt(d_all$n)
d_fin$adult_return <- d_all$adult_mean
d_fin$adult_sd <- d_all$adult_S.E. * sqrt(d_all$adult_n)
d_fin$adult_se <- d_all$adult_S.E. 

##################################
#### Step 4: Export outcome csv for further processing 
d_export <- d_fin %>% ungroup %>% select(study, outcome, id, sex, age, age_error, age_sd, age_lower, age_upper, resource, units, raw_return, raw_sd, adult_return, adult_sd, adult_se)

write_csv(d_export, paste0( paste(paste("data", paper_name, sep="_"),paper_section, sep="_"), ".csv" ))

setwd(home)
#################################
