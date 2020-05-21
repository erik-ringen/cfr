usePackage <- function(p) {
  if (!is.element(p, installed.packages()[,1]))
    install.packages(p, dep = TRUE)
  require(p, character.only = TRUE)
}

usePackage("tidyverse")
usePackage("metaDigitise")

##################################
home <- getwd() # remember home directory to return to
temp_dir <- "paper_data/Bird_2005/fig6.2" # temporarily set directory

### Pre-lim: digitize figure data
# metaDigitise(temp_dir)

# workflow: no groups needed, simply select all points

# saveRDS(metaDigitise(temp_dir, summary=F), paste0(temp_dir, "/fig6.2.rds"))

#################################
setwd(temp_dir)

paper_name <- strsplit(temp_dir, split="/", fixed=T)[[1]][2]
paper_section <- strsplit(temp_dir, split="/", fixed=T)[[1]][3]

d_list <- readRDS(paste0(paper_section, ".rds"))

#### Step 1: Wrangle data ########
d <- bind_rows(d_list$scatterplot) %>% select(x,y)
colnames(d)[1] <- "age"

# Returns that are within small margin of 0 are actually zero-return
d$y <- ifelse(abs(d$y - 0) < 10, 0, d$y)

# Round ages to get rid of clicking-variance
d$age <- round(d$age)

##################################

d_fin <- d
##################################
#### Step 3: Add meta-data and additional covariate information
d_fin$study <- paper_name # paper id
d_fin$outcome <- paste(d_fin$study, paper_section, sep="_")
d_fin$id <- NA # study *  outcome * individual, if data are individual rather than group-level
d_fin$sex <- 12/26 # 12/26 kids were male
d_fin$age_error <- "none" # information on distribution of ages (sd), or just a range (interval)? 
d_fin$age_sd <- NA  # only if sd of ages is given
d_fin$age_lower <- NA # only if interval ages given
d_fin$age_upper <- NA # only if interval ages given
d_fin$resource <- "lizards" # what type of foraging resource
d_fin$units <- "kcal/hr"
d_fin$raw_return <- d$y
d_fin$raw_sd <- NA
d_fin$adult_return <- 1102.1 # adult avg 1102.1 +- 276.5
d_fin$adult_sd <- sqrt(131)*276.5 # 131 adults in sample
d_fin$adult_se <- 276.5

##################################
#### Step 4: Export outcome csv for further processing 
d_export <- d_fin %>% ungroup %>% select(study, outcome, id, sex, age, age_error, age_sd, age_lower, age_upper, resource, units, raw_return, raw_sd, adult_return, adult_sd, adult_se)

write_csv(d_export, paste0( paste(paste("data", paper_name, sep="_"),paper_section, sep="_"), ".csv" ))

setwd(home)
#################################
