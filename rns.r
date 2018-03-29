#install.packages("knitr")
#install.packages("plotly")
library(tidyverse)
library(gridExtra)
library(knitr)
library(plotly)

#not sure if we need this part, but at the moment i put it here first
#downloading data
download.file("https://ndownloader.figshare.com/files/2292169", "data/portal_data_joined.csv")

#read_csv = tibble, diff from read.csv, it treat stringasfactor = FALSE by default, load faster
surveys <- read_csv("data/portal_data_joined.csv")
str(surveys)

summary(surveys)

#weight according to species
#compare with weight according to genus
surveys_weight <- surveys %>%
    filter(!is.na(year), !is.na(weight), !species_id == "") %>%
    group_by(year, species_id) %>% 
    mutate(average_weight = mean(weight)) 

#weight according to genus
surveys_weight <- surveys %>%
  filter(!is.na(year), !is.na(weight), !genus == "") %>%
  group_by(year, genus) %>% 
  mutate(average_weight = mean(weight)) 

#just to check
#to see distribution of species according to year
#to find out only include only those have 1977-2002 data by Swan
species_year <- surveys %>%
  filter(!is.na(year), !is.na(weight), !genus == "") %>%
  group_by(year, genus, weight) %>% select(year, genus, weight) %>% tally()
