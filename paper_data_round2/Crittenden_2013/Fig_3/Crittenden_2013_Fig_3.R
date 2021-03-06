usePackage <- function(p) {
  if (!is.element(p, installed.packages()[,1]))
    install.packages(p, dep = TRUE)
  require(p, character.only = TRUE)
}

usePackage("tidyverse")
usePackage("metaDigitise")

##################################
home <- getwd() # remember home directory to return to
temp_dir <- "paper_data_round2/Crittenden_2013/Fig_3/" # temporarily set directory

### Pre-lim: digitize figure data
# metaDigitise(temp_dir)

# workflow: make groups for each sex

# saveRDS(metaDigitise(temp_dir, summary=F), paste0(temp_dir, "/Fig_3.rds"))

#################################
setwd(temp_dir)

paper_name <- strsplit(temp_dir, split="/", fixed=T)[[1]][2]
paper_section <- strsplit(temp_dir, split="/", fixed=T)[[1]][3]

d_list <- readRDS("Fig_3.rds")

#### Step 1: Wrangle data ########
d <- bind_rows(d_list$scatterplot) %>% select(id,x,y)

# Round off age as it is presented as integer
d$age <- round(d$x)

#sex
d$sex <- ifelse( d$id == "female", "female", "male")

#make ID
d$id <- ifelse (d$sex == "female", 
                paste( "f", 1:sum(d$sex == "female"), sep = ""),
                paste( "m", 1:sum(d$sex == "male"), sep = ""))


##################################

d_fin <- d
##################################
#### Step 3: Add meta-data and additional covariate information
d_fin$study <- paper_name # paper id
d_fin$outcome <- paste(d_fin$study, paper_section, sep="_") # 
d_fin$id <- paste(d_fin$outcome, d$id, sep="_") #
d_fin$sex <- d$sex # 
d_fin$age_error <- NA       # nb age rounded off as it's clearly presented as integer in the graph
d_fin$age_sd <- NA  # 
d_fin$age_lower <- NA # 
d_fin$age_upper <- NA # 
d_fin$resource <-  "mixed" # fruit;birds;tubers;honey;small_game;vegetables 
d_fin$units <- "kcal/day" # 
d_fin$raw_return <- d$y
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
