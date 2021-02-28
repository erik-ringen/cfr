#Load packages
library(tidyverse)
library(ggmap)
library(maps)
library(rjson)
library(ggrepel) #to create distance between dots and numbers

#setwd wd to "cfr/text/images"
setwd("~/Nextcloud/Foraging returns review/cfr/text/images/")

#load data
metadata <- read.csv("../data/papers_metadata.csv")
world <- map_data("world")

#set environment
theme_set(theme_classic())


###############
###world map###
###############
populations <- unique(metadata[,c("Population",
                          "N_studies_per_population",
                          "Long", "Lat")])

world_pop <- ggplot() +
  geom_polygon(data = world, aes(x = long, y = lat, group = group), fill = rgb(0, 103/255 , 91/255, 0.8))+
  geom_point(data = populations, aes (x = Long, y = Lat, size = N_studies_per_population), colour = "darkred") +
  geom_text_repel (data = populations, aes (x = Long, y = Lat, label = Population), colour = "grey10", size = 3)+
  theme_nothing()+
  coord_fixed()

png("world_pop.png", width = 8, height = 3, units = 'in', res = 800 )#opens png file to print map in. Currently set to a folder map inside directory, folder needs to be created or deleted
world_pop
dev.off()


