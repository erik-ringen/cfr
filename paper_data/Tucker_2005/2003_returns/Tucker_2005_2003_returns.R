usePackage <- function(p) {
  if (!is.element(p, installed.packages()[,1]))
    install.packages(p, dep = TRUE)
  require(p, character.only = TRUE)
}

usePackage("tidyverse")
usePackage("metaDigitise")

##################################
home <- getwd() # remember home directory to return to
temp_dir <- "paper_data/Tucker_2005/2003_returns" # temporarily set directory

### Pre-lim: digitize figure data
# metaDigitise(temp_dir)

# workflow: get points from one half of the scatterlpot (F/M) at a time, with a different group for every unique ID on the y axis. Starting top of y axis to bottom. Then do again with the male data (rght side).

#saveRDS(metaDigitise(temp_dir, summary=F), paste0(temp_dir, "/2003_returns.rds"))

#################################
setwd(temp_dir)

paper_name <- strsplit(temp_dir, split="/", fixed=T)[[1]][2]
paper_section <- strsplit(temp_dir, split="/", fixed=T)[[1]][3]

d_list <- readRDS("2003_returns.rds")

#### Step 1: Wrangle data ########
d <- bind_rows( d_list$scatterplot$`2003_returns_male.png`, d_list$scatterplot$`2003_returns_female.png`  ) %>% select(id, x, y)


# Get sex labels
d$sex <- ifelse(substr(d$id, 1, 1) == "f", "female", "male")

# round y values to obtain age rank 
d$rank <- round(d$y)

#give age group
d$age_group <- ifelse( d$rank <= 9, "child",
                       ifelse(d$rank <= 15, "adolescent",
                              "adult"))

# Get average returns of adults, sex-specific
adult_avg <- d %>% 
  filter(age_group == "adult") %>% 
  group_by(sex) %>%
    summarise(mean_adult=mean(x), sd_adult=sd(x), n_adult=n())

# bring in age to main df

d <- left_join(d, adult_avg)

# filter out individuals above age 20
d <- filter(d, !age_group == "adult")

d$age_upper <- ifelse (d$age_group == "child", 12, 19 ) #age group limits to be checked
d$age_lower <- ifelse (d$age_group == "child", 4, 13 ) #Children (olo kely): Children who are weaned and prepubescent, are
                                                          #mobile enough to leave camp and travel/work with others in the environs
                                                          #of the camp, but are not old enough to travel alone.
                                                        #Adolescents (olo be-be; kidabo lahy): Young people nearing, experiencing,
                                                          #or just past puberty, unmarried, with complete independent personal
                                                          #mobility, who can travel to the well or to the market alone. They
                                                          #frequently talk about marriage and sex. Some are sexually active.



##################################

##################################
#### Step 3: Add meta-data and additional covariate information
d_fin <- data.frame(study = rep( paper_name, nrow(d)))
d_fin$outcome <- paste(d_fin$study, paper_section, sep="_") # total kcal/hr outcome, 1997 data
d_fin$id <- paste(d_fin$outcome, d$id, sep="_") # study *  outcome * individual, if data are individual rather than group-level
d_fin$sex <- d$sex # "female", "male", or "both"
d_fin$age <- NA #info is only age rank
d_fin$age_error <- NA # information on distribution of ages (sd), or just a range (interval)? 
d_fin$age_sd <- NA  # only if sd of ages is given
d_fin$age_lower <- d$age_lower # only if interval ages given
d_fin$age_upper <- d$age_upper # only if interval ages given
d_fin$resource <- "mixed" # tubers;small_game;marine
d_fin$units <- "net kcal/h" # whether the rate is per hour (hr), per day, or other
d_fin$raw_return <- d$x
d_fin$raw_sd <- NA
d_fin$adult_return <- d$mean_adult
d_fin$adult_sd <- d$sd_adult
d_fin$adult_se <- d$sd_adult / sqrt(d$n_adult)

##################################
#### Step 4: Export outcome csv for further processing 
d_export <- d_fin %>% ungroup %>% select(study, outcome, id, sex, age, age_error, age_sd, age_lower, age_upper, resource, units, raw_return, raw_sd, adult_return, adult_sd, adult_se)

write_csv(d_export, paste0( paste(paste("data", paper_name, sep="_"),paper_section, sep="_"), ".csv" ))

setwd(home)
#################################
