usePackage <- function(p) {
  if (!is.element(p, installed.packages()[,1]))
    install.packages(p, dep = TRUE)
  require(p, character.only = TRUE)
}

usePackage("tidyverse")
usePackage("metaDigitise")

##################################
home <- getwd() # remember home directory to return to
temp_dir <- "paper_data_round2/Tucker_2005/1997_returns" # temporarily set directory

### Pre-lim: digitize figure data
#metaDigitise(temp_dir)

# workflow: get points from one half of the scatterlpot (F/M) at a time, with a different group for every unique ID on the y axis. Starting top of y axis to bottom. Then do again with the male data (rght side).

# saveRDS(metaDigitise(temp_dir, summary=F), paste0(temp_dir, "/1997_returns.rds"))

#################################
setwd(temp_dir)

paper_name <- strsplit(temp_dir, split="/", fixed=T)[[1]][2]
paper_section <- strsplit(temp_dir, split="/", fixed=T)[[1]][3]

d_list <- readRDS("1997_returns.rds")

#### Step 1: Wrangle data ########
d <- bind_rows( d_list$scatterplot$`1997_returns.png`, d_list$scatterplot$`1997_returns - male.png`  ) %>% select(id, x, y)

# A couple of obs ~0 were registered as negative
d$x <- ifelse(abs(d$x - 0) < 10, 0, d$x)
d$x <- ifelse(d$x < 0, 0, d$x)

# Get sex labels
d$sex <- ifelse(substr(d$id, 1, 1) == "f", "female", "male")


#######Unclear whether the data are presented as Age rank or ages. 
#Wrangling with interpretation as ages below - change the section that is commented out

###Process with Age rank
#######
# round y values to obtain age rank 
d$rank <- round(d$y)
d$rank[1] <- 1 #rank extracted incorrectly

#give age group
d$age_group <- ifelse( d$rank <= 13, "child",
                       ifelse(d$rank <= 21, "adolescent",
                              "adult"))

# Get average returns of adults, sex-specific
adult_avg <- d %>% 
  filter(age_group == "adult") %>% 
  group_by(sex) %>%
    summarise(mean_adult=mean(x), sd_adult=sd(x), n_adult=n())

# bring in age to main df

d <- left_join(d, adult_avg)

# filter out adults
d <- filter(d, !age_group == "adult")

#give age group range
d$age_upper <- ifelse (d$age_group == "child", 11, 19 ) #age group from bram's contribution to TBHG data 
d$age_lower <- ifelse (d$age_group == "child", 6, 12 ) #Children (olo kely): Children who are weaned and prepubescent, are
                                                          #mobile enough to leave camp and travel/work with others in the environs
                                                          #of the camp, but are not old enough to travel alone.
                                                        #Adolescents (olo be-be; kidabo lahy): Young people nearing, experiencing,
                                                          #or just past puberty, unmarried, with complete independent personal
                                                          #mobility, who can travel to the well or to the market alone. They
                                                          #frequently talk about marriage and sex. Some are sexually active.
##################################
#### Step 3: Add meta-data and additional covariate information
d_fin <- data.frame(study = rep( paper_name, nrow(d)))
d_fin$outcome <- paste(d_fin$study, paper_section, sep="_") #
d_fin$id <- paste(d_fin$outcome, d$id, sep="_") # study *  outcome * individual, if data are individual rather than group-level
d_fin$sex <- d$sex # 
d_fin$age <- NA #
d_fin$age_error <- NA # 
d_fin$age_sd <- NA  # 
d_fin$age_lower <- d$age_lower # age interval from definitions in TBHG data
d_fin$age_upper <- d$age_upper # age interval from definitions in TBHG data
d_fin$resource <- "mixed" # tubers;small_game;marine
d_fin$units <- "net kcal/h" # 
d_fin$raw_return <- d$x
d_fin$raw_sd <- NA
d_fin$adult_return <- d$mean_adult
d_fin$adult_sd <- d$sd_adult
d_fin$adult_se <- d$sd_adult / sqrt(d$n_adult)



# ###Process with Age
######
# # Average over the very small within-id variance in age due to extraction error
# age_avg <- d %>%
#   group_by(id) %>%
#   summarise(age = mean(y))
# 
# # Get average returns of adults, sex-specific
# adult_avg <- d %>% 
#   filter(y >= 20) %>% 
#   group_by(sex) %>%
#     summarise(mean_adult=mean(x), sd_adult=sd(x), n_adult=n())
# 
# # bring in age and adult values to main df
# d <- left_join(d, age_avg)
# 
# d <- left_join(d, adult_avg)
# 
# # filter out individuals above age 20
# d <- filter(d, age <= 20)

##################################

# d_fin <- d
# ##################################
# #### Step 3: Add meta-data and additional covariate information
# d_fin$study <- paper_name # paper id
# d_fin$outcome <- paste(d_fin$study, paper_section, sep="_") # total kcal/hr outcome, 1997 data
# d_fin$id <- paste(d_fin$outcome, d$id, sep="_") # 
# d_fin$sex <- d$sex # 
# d_fin$age_error <- NA # 
# d_fin$age_sd <- NA  # 
# d_fin$age_lower <- NA # to check ages, as age is given as rank and not actual age
# d_fin$age_upper <- NA # 
# d_fin$resource <- "mixed" # tubers;small_game;marine
# d_fin$units <- "net kcal/h" # 
# d_fin$raw_return <- d$x
# d_fin$raw_sd <- NA
# d_fin$adult_return <- d$mean_adult
# d_fin$adult_sd <- d$sd_adult
# d_fin$adult_se <- d$sd_adult / sqrt(d$n_adult)
# 
##################################
#### Step 4: Export outcome csv for further processing 
d_export <- d_fin %>% ungroup %>% select(study, outcome, id, sex, age, age_error, age_sd, age_lower, age_upper, resource, units, raw_return, raw_sd, adult_return, adult_sd, adult_se)

write_csv(d_export, paste0( paste(paste("data", paper_name, sep="_"),paper_section, sep="_"), ".csv" ))

setwd(home)
#################################
