usePackage <- function(p) {
  if (!is.element(p, installed.packages()[,1]))
    install.packages(p, dep = TRUE)
  require(p, character.only = TRUE)
}

usePackage("tidyverse")
##################################
home <- getwd() # remember home directory to return to
temp_dir <- "paper_data_round2/BlurtonJones_1989/table2" # temporarily set directory

#################################
setwd(temp_dir)

paper_name <- strsplit(temp_dir, split="/", fixed=T)[[1]][2]
paper_section <- strsplit(temp_dir, split="/", fixed=T)[[1]][3]

d <- read.csv("table2. Children and their foraging returns (in grams per hour).csv")

#### Step 1: Wrangle data ########
#rename columns
colnames(d)[c (1, 6:8)] <- c ("ID", "Ekwa", "honey", "fruit")

#create id
d$id <- 1:nrow(d)

#make column Age numeric and give age >20 to women
d$Age <- as.character(d$Age)
d[nrow(d), "Age"] <- 100 
d$Age <- as.numeric(d$Age)

#make column baobab numeric and give 0 to children who tried and failed
d$fruit <- as.character(d$fruit)
d[1:2, "fruit"] <- 0 
d$fruit <- as.numeric(d$fruit)

#create a tubers column that combines Makalita and Ekwa
d$tubers <- rowSums (d [, c ("Makalita", "Ekwa" )], na.rm = TRUE)
d$tubers[d$tubers == 0] <- NA

#remove makalita and ekwa columns
d <- d [ , -c ( 5, 6 )]

#make data long
d_long <- d %>% 
  pivot_longer (cols = c ("tubers", "honey", "fruit"), 
                names_to = "resources",
                values_to = "raw_returns")

##################################
#add adult values for baobab
d_long$adults <-  rep (as.numeric (d [nrow (d), c (8, 5, 6) ] ), nrow(d))

#load adult values for tubers from table 4 (NB after processing)
dd <- read.csv("../table4/data_BlurtonJones_1989_table4.csv")
adult_tubers <- dd[1, c("adult_return", "adult_sd",  "adult_se")]
##################################

# filter out individuals above age 20
d_long <- filter(d_long, Age <= 20)

#remove empty rows
d_long <- d_long[!is.na(d_long$raw_returns),]

##################################

##################################
#### Step 3: Add meta-data and additional covariate information
d_fin <- data.frame(study = rep( paper_name, nrow(d_long)))
d_fin$outcome <- paste(d_fin$study, paper_section, d_long$resources, sep="_") #
d_fin$id <- paste(d_fin$study, paper_section, d_long$id, sep="_") # 
d_fin$sex <- "both"                 # maybe we can ask nbj?
d_fin$age <- d_long$Age
d_fin$age_error <- NA # 
d_fin$age_sd <- NA  # 
d_fin$age_lower <- NA # 
d_fin$age_upper <- NA # 
d_fin$resource <- d_long$resources # makalita and ekwa grouped into tubers. This way they can be compared to the adult values from table 4
d_fin$units <- "g/h" # 
d_fin$raw_return <- d_long$raw_returns
d_fin$raw_sd <- NA
d_fin$adult_return <- ifelse( d_long$resources == "tubers", adult_tubers[,1], d_long$adults) # adult values for tubers from table 4 after processing
d_fin$adult_sd <- ifelse( d_long$resources == "tubers", adult_tubers[,2], NA) # adult values for tubers from table 4
d_fin$adult_se <- ifelse( d_long$resources == "tubers", adult_tubers[,3], NA) # adult values for tubers from table 4

##################################
#### Step 4: Export outcome csv for further processing 
d_export <- d_fin %>% ungroup %>% select(study, outcome, id, sex, age, age_error, age_sd, age_lower, age_upper, resource, units, raw_return, raw_sd, adult_return, adult_sd, adult_se)

write_csv(d_export, paste0( paste(paste("data", paper_name, sep="_"),paper_section, sep="_"), ".csv" ))

setwd(home)
#################################
