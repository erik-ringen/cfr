usePackage <- function(p) {
  if (!is.element(p, installed.packages()[,1]))
    install.packages(p, dep = TRUE)
  require(p, character.only = TRUE)
}

usePackage("tidyverse")
usePackage("metaDigitise")

##################################
home <- getwd() # remember home directory to return to
temp_dir <- "paper_data_round2/BlurtonJones_2002/Figure_4" # temporarily set directory

### Pre-lim: digitize figure data
#metaDigitise(temp_dir)

# workflow: group by sex on scatterplot (males triangles, females squares)

#saveRDS(metaDigitise(temp_dir, summary=F), paste0(temp_dir, "/Figure_4.rds"))

#################################
setwd(temp_dir)

paper_name <- strsplit(temp_dir, split="/", fixed=T)[[1]][2]
paper_section <- strsplit(temp_dir, split="/", fixed=T)[[1]][3]

d_list <- readRDS("Figure_4.rds")

#### Step 1: Wrangle data ########
d <- bind_rows( d_list$scatterplot  ) %>% select(  id,  x, y)

colnames(d)[1:2] <-  c ("sex", "age")


#IDs
d$id <- 1:nrow(d)

#transform returns in grams
d$y <- d$y *1000

#negative and very low value as zero
d$y <- ifelse(abs(d$y - 0) < 21, 0, d$y)

# Get average returns of adults, sex-specific
adult_avg <- d %>% 
  filter(age >= 20) %>% 
  group_by(sex) %>%
  summarise(mean_adult=mean(y), sd_adult=sd(y), n_adult=n())

# bring in adult values to main df
d <- left_join(d, adult_avg)

# filter out individuals above age 20
d <- filter(d, age <= 20)

##################################
#### Step 3: Add meta-data and additional covariate information
d_fin <- data.frame(study = rep( paper_name, nrow(d)))
d_fin$outcome <- paste(d_fin$study, paper_section, sep="_") # 
d_fin$id <- paste(d_fin$outcome, d$id, sep="_") # 
d_fin$sex <- d$sex # 
d_fin$age <- d$age 
d_fin$age_error <- NA # 
d_fin$age_sd <- NA  # 
d_fin$age_lower <- NA # 
d_fin$age_upper <- NA # 
d_fin$resource <- "tubers" # 
d_fin$units <- "g/h"          # original in kg
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
