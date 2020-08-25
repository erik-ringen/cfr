usePackage <- function(p) {
  if (!is.element(p, installed.packages()[,1]))
    install.packages(p, dep = TRUE)
  require(p, character.only = TRUE)
}

usePackage("tidyverse")

##################################
home <- getwd() # remember home directory to return to
temp_dir <- "paper_data/BliegeBird_1995/table3" # temporarily set directory
setwd(temp_dir)

paper_name <- strsplit(temp_dir, split="/", fixed=T)[[1]][2]
paper_section <- strsplit(temp_dir, split="/", fixed=T)[[1]][3]

d <- read_csv("table3. summary of total foraging returns by individual.csv")

#### Step 1: Wrangle data ########

#ATTENTION: Notes on time from Caption: Foraging time includes travel, search and handling (harvesting and field processing)
#Egg time does not include travel time to the patch, which was shared with travel to the intertidal patch
#since egg collecting occurred within an intertidal foraging episode. To obtain total egg time, add 161 minutes to boy 1 and 2,
#103 to boy 5, 58 to girl 8.

#add travel time to have total egg time, as indicated in caption
add_times <- c(161, 161, NA, NA, 103, NA, NA, 58, NA, NA, NA, NA)
d_recal [, 6] <- d_recal [, 6] + add_times

#recalculate weights per hour
d_recal <- d
d_recal [, 4:6] <- d_recal [, 4:6] / 60 #duration of trips in hours
d_recal <- d_recal %>% 
  rename( `intertidal foraging time (h)` = `intertidal foraging time (min)`,
          `fruit foraging time (h)` = `fruit foraging time (min)`,
          `eggs foraging time (h)` = `eggs foraging time (min)`) 

d_recal [, 8:10] <- d_recal [, 8:10] / d_recal [, 4:6] #duration of trips in hours
d_recal <- d_recal %>% 
  rename( `intertidal total weight (g/h)` = `intertidal total weight (g)`,
          `fruit total weight (g/h)` = `fruit total weight (g)`,
          `eggs total weight (g/h)` = `eggs total weight (g)`) 

d_recal <- d_recal %>% 
  select( "ID" ,
          "Sex",                                   
          "Age",
          "intertidal total weight (g/h)",   
          "fruit total weight (g/h)",
          "eggs total weight (g/h)",         
          "average returns (kcal/observation day)" )

#long data frame
d_long <- d_recal %>%
  select( "ID" ,
          "Sex",                                   
          "Age",
          "intertidal total weight (g/h)",   
          "fruit total weight (g/h)",
          "eggs total weight (g/h)",         
          "average returns (kcal/observation day)" 
          ) %>%
  pivot_longer( c("intertidal total weight (g/h)",   
                  "fruit total weight (g/h)",
                  "eggs total weight (g/h)",         
                  "average returns (kcal/observation day)" ), 
                names_to = 'data_type', values_to = 'raw_returns' )

#extract units
d_long$units <- str_extract(string = d_long$data_type,
                            pattern = "(?<=\\().*(?=\\))")
#extract resource type
d_long$resource <- word(d_long$data_type, 1)
d_long$resource <- gsub("intertidal", "shellfish", d_long$resource )
d_long$resource <- gsub("average", "mixed", d_long$resource )

# drop NAs (no measure)
d_long <- d_long[complete.cases(d_long),]

d_fin <- data.frame(study = rep( paper_name, nrow(d_long)))
##################################
#### Add meta-data and additional covariate information
d_fin$study <- paper_name # paper id
d_fin$outcome <- paste(d_fin$study, d_long$`data_type`, sep="_") # 7 outcomes in total, different shellfish
d_fin$id <- paste(d_fin$outcome, d_long$ID, sep="_") # study *  outcome * individual, if data are individual rather than group-level
d_fin$sex <- d_long$Sex # "female", "male", or "both"
d_fin$age <- d_long$Age # no mean age given
d_fin$age_error <- NA # information on distribution of ages (sd), or just a range (interval)? 
d_fin$age_sd <- NA  # only if sd of ages is given
d_fin$age_lower <- NA # only if interval ages given
d_fin$age_upper <- NA # only if interval ages given
d_fin$resource <- d_long$resource # what type of foraging resource
d_fin$units <- d_long$units # whether the rate is per hour (hr), per day, or other
d_fin$raw_return <- d_long$raw_returns
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
