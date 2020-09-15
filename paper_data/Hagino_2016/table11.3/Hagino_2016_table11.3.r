usePackage <- function(p) {
  if (!is.element(p, installed.packages()[,1]))
    install.packages(p, dep = TRUE)
  require(p, character.only = TRUE)
}

usePackage("tidyverse")

##################################
home <- getwd() # remember home directory to return to
temp_dir <- "paper_data/Hagino_2016/table11.3" # temporarily set directory
setwd(temp_dir)

paper_name <- strsplit(temp_dir, split="/", fixed=T)[[1]][2]
paper_section <- strsplit(temp_dir, split="/", fixed=T)[[1]][3]

d <- read.csv("table11.3. Total weights of foods brought back to the camp by children and adults in 6 days.csv")

#### Step 1: Wrangle data ########
#rename columns
colnames(d)[1] <- "resource"

#transform hyphens in NAs
d[which(d$Children == "-"), 2:4] <- NA

#make columns numeric
d[,2] <-as.numeric(d[,2])
d[,3] <-as.numeric(d[,3])
d[,4] <-as.numeric(d[,4])

# drop prey where there isn't data for both adults and children, no comparison to be made
d <- d[complete.cases(d),]



##################################
#### Add meta-data and additional covariate information
d_fin <- data.frame(study = rep( paper_name, nrow(d)))
d_fin$outcome <- paste(d_fin$study,  d$resource, sep="_") #
d_fin$id <- NA # 
d_fin$sex <- "both" # 
d_fin$age <- NA # 
d_fin$age_error <- "interval" # 
d_fin$age_sd <- NA  # 
d_fin$age_lower <- 10 # 
d_fin$age_upper <- 16           # from text: "During the 8-day trip, direct observations of a total of six children (three children for each sex) were conducted. The ages of the three boys were estimated at 12, 14, and 16 years old. The three girls were estimated to be 10, 13, and 15 years old."
d_fin$resource <- "small_game"  # 
d_fin$units <- "g/6days"        # data given as total returns over a 6 days trip
d_fin$raw_return <- d$Children
d_fin$raw_sd <- NA
d_fin$adult_return <- d$Adults
d_fin$adult_sd <- NA
d_fin$adult_se <- NA

##################################
#### Step 4: Export outcome csv for further processing 
d_export <- d_fin %>% ungroup %>% select(study, outcome, id, sex, age, age_error, age_sd, age_lower, age_upper, resource, units, raw_return, raw_sd, adult_return, adult_sd, adult_se)

write_csv(d_export, paste0( paste(paste("data", paper_name, sep="_"),paper_section, sep="_"), ".csv" ))

setwd(home)
#################################
