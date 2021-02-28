usePackage <- function(p) {
  if (!is.element(p, installed.packages()[,1]))
    install.packages(p, dep = TRUE)
  require(p, character.only = TRUE)
}

usePackage("tidyverse")
usePackage("metaDigitise")

##################################
home <- getwd() # remember home directory to return to
temp_dir <- "paper_data_round2/Walker_2002/Fig_2b" # temporarily set directory

### Pre-lim: digitize figure data
 metaDigitise(temp_dir)

# workflow: get points from one half of the scatterlpot (F/M) at a time, with a different group for every unique ID on the y axis. Starting top of y axis to bottom. Then do again with the male data (rght side).

saveRDS(metaDigitise(temp_dir, summary=F), paste0(temp_dir, "/Fig_2b.rds"))

#################################
setwd(temp_dir)

paper_name <- strsplit(temp_dir, split="/", fixed=T)[[1]][2]
paper_section <- strsplit(temp_dir, split="/", fixed=T)[[1]][3]

d_list <- readRDS("Fig_2b.rds")

#### Step 1: Wrangle data ########
d <- bind_rows( d_list$scatterplot$`Fig_2b Graph same as a except hunters that were sampled in both decades have their data points connected with a line.png`) %>% select(id, x, y)

#bring kg to g
d$y <- d$y *1000

# A couple of obs ~0 were registered as negative
d$y <- ifelse(abs(d$y - 0) < 20, 0, d$y)

#rename column
colnames(d)[2] <-  "age"


# Get average returns of adults
adult_avg <- d %>% 
  filter(age >= 20) %>% 
    summarise(mean_adult=mean(y), sd_adult=sd(y), n_adult=n())

# bring in adult values
d[4:6] <- adult_avg

# filter out individuals above age 20
d <- filter(d, age <= 20)


##################################
#### Step 3: Add meta-data and additional covariate information
d_fin <- data.frame(study = rep( paper_name, nrow(d)))
d_fin$outcome <- paste(d_fin$study, paper_section, sep="_") #
d_fin$id <- paste(d_fin$outcome,  d$id, sep="_") # 
d_fin$sex <- "male" # male returns only
d_fin$age <- d$age #
d_fin$age_error <- NA # 
d_fin$age_sd <- NA  # 
d_fin$age_lower <- NA # 
d_fin$age_upper <- NA # 
d_fin$resource <- "game" # 
d_fin$units <- "g/h" # 
d_fin$raw_return <- d$y
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
