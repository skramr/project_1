#Project 1 script

# LIBRARIES ----
library(tidymodels)
library(vip)
library(tidyverse)
library(readr)

#load data

beetles <- read_csv("~/Downloads/data_science_2/hgen_612_temp2/data/Data_1993.csv")



#Infest_sever1- Infestation severity nearest to response 

#overview of data


beetles %>% ggplot(aes(x=IndDBHIn, y=NeighBSA9115)) + geom_point()
