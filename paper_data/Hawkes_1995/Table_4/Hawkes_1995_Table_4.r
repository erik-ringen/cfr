usePackage <- function(p) {
  if (!is.element(p, installed.packages()[,1]))
    install.packages(p, dep = TRUE)
  require(p, character.only = TRUE)
}

usePackage("tidyverse")

##################################
home <- getwd() # remember home directory to return to
temp_dir <- "paper_data/Hawkes_1995/Table_4" # temporarily set directory
setwd(temp_dir)

paper_name <- strsplit(temp_dir, split="/", fixed=T)[[1]][2]
paper_section <- strsplit(temp_dir, split="/", fixed=T)[[1]][3]

#load tables
d <- read.csv("Table_4 Children's acquisition and stashing rates.csv")
dd <- read.csv("../Table_2/Table_2 Time allocation to food acqiusition and processing in or near camp by focal child.csv")
d_a <- read.csv("../Table_5/data_Hawkes_1995_Table_5.csv")


#### Step 1: Wrangle data ########
#collect info from table 2 (age and sex of individuals)
dd <- dd[ 1:nrow(dd)-1,]
dd$Child <- as.numeric(dd$Child)
dd$Sex <- ifelse(dd$Sex == "f", "female", "male")

#turn around dataframe
d_long <- pivot_longer(d, cols = -Child, names_to = "resource", values_to = "value")
d_long$what <- str_extract(d_long$resource, "[^_]+")
d_long$resource <- sub("^[^_]*_", "",d_long$resource)
d_wide <- pivot_wider(d_long , names_from = "what", values_from = "value")

#join info on children
d_all <- left_join(d_wide, dd[ , 1:3])

#add resource type
d_all$resource_type <- ifelse(grepl( "Makalita" ,d_all$resource),'tubers',
                              ifelse(grepl( "ekwa" ,d_all$resource),'tubers', "fruits"))

#remove empty rows
d_all <- d_all[!is.na(d_all$mean),]


#####################################
#add adult values for tafabe and ondishibe from table 5 (NB after processing)
d_all[ which (d_all$resource == "tin_measured_Ondishibe_rates_g.h" & d_all$Sex == "female"), 9:11] <- d_a[ 1, 14:16]
d_all[ which (d_all$resource == "tin_measured_Ondishibe_rates_g.h" & d_all$Sex == "male"), 9:11] <- d_a[ 2, 14:16]
d_all[ which (d_all$resource == "tin_measured_Tafabe_rates_g.h" & d_all$Sex == "female"), 9:11] <- d_a[ 4, 14:16]
d_all[ which (d_all$resource == "tin_measured_Tafabe_rates_g.h" & d_all$Sex == "male"), 9:11] <- d_a[ 5, 14:16]


##################################
#### Add meta-data and additional covariate information
d_fin <- data.frame(study = rep( paper_name, nrow(d_all)))
d_fin$outcome <- paste(d_fin$study, paper_section, d_all$resource, sep="_") # 
d_fin$id <-  paste(d_fin$study, d_all$Child, sep="_") # 
d_fin$sex <- d_all$Sex                      # from table 2
d_fin$age <- d_all$Estimated_age            # from table 2
d_fin$age_error <- NA # 
d_fin$age_sd <- NA  # 
d_fin$age_lower <- NA # 
d_fin$age_upper <- NA # 
d_fin$resource <- d_all$resource_type # 
d_fin$units <- "g/h" #
d_fin$raw_return <- d_all$mean
d_fin$raw_sd <- d_all$S.E. * sqrt(d_all$n)
d_fin$adult_return <- d_all$adult_return    #adult values mostly missing, those present obtained from table 5, after processing
d_fin$adult_sd <- d_all$adult_sd
d_fin$adult_se <- d_all$adult_se

##################################
#### Step 4: Export outcome csv for further processing 
d_export <- d_fin %>% ungroup %>% select(study, outcome, id, sex, age, age_error, age_sd, age_lower, age_upper, resource, units, raw_return, raw_sd, adult_return, adult_sd, adult_se)

write_csv(d_export, paste0( paste(paste("data", paper_name, sep="_"),paper_section, sep="_"), ".csv" ))

setwd(home)
#################################
