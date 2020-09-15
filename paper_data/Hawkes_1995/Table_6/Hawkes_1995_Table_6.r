usePackage <- function(p) {
  if (!is.element(p, installed.packages()[,1]))
    install.packages(p, dep = TRUE)
  require(p, character.only = TRUE)
}

usePackage("tidyverse")

##################################
home <- getwd() # remember home directory to return to
temp_dir <- "paper_data/Hawkes_1995/Table_6" # temporarily set directory
setwd(temp_dir)

paper_name <- strsplit(temp_dir, split="/", fixed=T)[[1]][2]
paper_section <- strsplit(temp_dir, split="/", fixed=T)[[1]][3]

d <- read.csv("Table_6 Tin-Measured acquisition, stashign and estimated consumption rates and time spent picking by sex and age.csv")
dd <- read.csv("../Table_2/Table_2 Time allocation to food acqiusition and processing in or near camp by focal child.csv")

#### Step 1: Wrangle data ########

#turn around dataframe
d_long <- pivot_longer(d, cols = -Who, names_to = "what", values_to = "value")
d_long$resource <- sub("^[^_]*_", "",d_long$what)
d_long$what <- str_extract(d_long$what, "[^_]+")
d_wide <- pivot_wider(d_long , names_from = "what", values_from = "value")

#reshape table to have adult values on the side

d_all <- d_wide[ 9:20,  ]
d_all[ 1:8 , 7:10] <- d_wide [1:8, 3:6]

#change names
colnames(d_all)[7:10] <- c("adult_mean", "adult_S.E.", "adult_n", "adult_estimate") 

#add units
d_all$unit <- ifelse( is.na(d_all$estimated), "kcal/h", "%")

############################################
#add info from table 2
#add sex
d_all$sex <- rep( c ("female", "male", "both") , each = 4)

#add age info
d_all$age_mean <- rep ( 
  c( mean(dd[ which(dd$Sex == "f"),"Estimated_age"], na.rm = TRUE),
     mean(dd[ which(dd$Sex == "m"),"Estimated_age"], na.rm = TRUE),
     mean(dd[ ,"Estimated_age"], na.rm = TRUE)), each = 4)

d_all$age_sd <- rep ( 
  c( sd(dd[ which(dd$Sex == "f"),"Estimated_age"], na.rm = TRUE),
     sd(dd[ which(dd$Sex == "m"),"Estimated_age"], na.rm = TRUE),
     sd(dd[ ,"Estimated_age"], na.rm = TRUE)), each = 4)


d_all$age_upper <- rep ( 
  c( max(dd[ which(dd$Sex == "f"),"Estimated_age"], na.rm = TRUE),
     max(dd[ which(dd$Sex == "m"),"Estimated_age"], na.rm = TRUE),
     max(dd[ ,"Estimated_age"], na.rm = TRUE)), each = 4)

d_all$age_lower <- rep ( 
  c( min(dd[ which(dd$Sex == "f"),"Estimated_age"], na.rm = TRUE),
     min(dd[ which(dd$Sex == "m"),"Estimated_age"], na.rm = TRUE),
     min(dd[ ,"Estimated_age"], na.rm = TRUE)), each = 4)


##########################################
#combine adult measures for the two sexes to bring in the adult value to compare children
#combine means
d_all$adult_mean <- as.numeric(d_all$adult_mean)
for(i in 9:12) {
d_all[ i , "adult_mean"] <- weighted.mean (d_all$adult_mean[i - c(4, 8) ], d_all$adult_n[i - c(4, 8) ])
}

#combine standard errors
#function to combine standard errors
comb_se <- function(se1, se2, n1, n2) {
  var1 <- se1^2 * n1 #calculate variances
  var2 <- se2^2 * n2
  var <- ( var1 * (n1-1) + var2 * (n2 - 1) ) / (n2 + n1 - 2) #combine variances
  se <- sqrt(var / n1+n2) #calculate standard error
  return(se)
}

d_all$adult_S.E. <- as.numeric(d_all$adult_S.E.)
for(i in 9:12) {
  d_all[ i , "adult_S.E."] <-comb_se( d_all$adult_S.E.[i - 4 ], d_all$adult_S.E.[i - 8 ], d_all$adult_n[i - 4 ], d_all$adult_n[i -  8 ])

}

#combine n
for(i in 9:12) {
  d_all[ i , "adult_n"] <- sum (d_all$adult_n[i - c(4, 8) ])
}


#########################################
#deal with different type of data in the table
#bring the estimated picked/stashed rate under the mean, to bring in the d_fin if decided so
d_all$mean[!is.na(d_all$estimated)] <- d_all$estimated[!is.na(d_all$estimated)]
#OR
#select only parts of the dataframe
d_all <- d_all[ which(is.na(d_all$estimated)), ]#removes estimated proportion of picked which are stashed - no sd, no 


##################################
#### Add meta-data and additional covariate information
d_fin <- data.frame(study = rep( paper_name, nrow(d_all)))
d_fin$outcome <- paste(d_fin$study,  paper_section, d_all$resource, sep="_") # 
d_fin$id <-  NA # study *  outcome * individual, if data are individual rather than group-level
d_fin$sex <- d_all$sex # "female", "male", or "both"
d_fin$age <- d_all$age_mean # no mean age given
d_fin$age_error <- "distribution" # information on distribution of ages (sd), or just a range (interval)? Ages as presented in table 2 same paper
d_fin$age_sd <- d_all$age_sd  # only if sd of ages is given
d_fin$age_lower <- d_all$age_lower # only if interval ages given
d_fin$age_upper <- d_all$age_upper # only if interval ages given; 
d_fin$resource <- "fruit" # what type of foraging resource
d_fin$units <- d_all$unit # whether the rate is per hour (hr), per day, or other
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
