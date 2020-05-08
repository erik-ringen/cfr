library(tidyverse)

# Get all extracted data files in the subdirectories
paper_data <- list.files(pattern = "^data_", recursive = TRUE)

# Combine them into a single dataframe
d <- paper_data %>%
  map(read_csv) %>%
    reduce(rbind)

# Prep data for Stan
N <- nrow(d)
N_studies <- length(unique(d$study))
N_outcomes <- length(unique(d$id))

# Some quick descriptives
par(mfrow=c(1,2))
hist(d$lRR_mean, main="ln(Return Ratio)",  xlab="")

