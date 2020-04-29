###########################################
# Histogram of Shootings per year
# Author: Rachel Weber
# Created: April 2, 2020
#########################################


library(anytime)
library(tidyverse)
library(magrittr)
library(pander)
library(ggplot2)
library(MASS)

options(scipen=999)

# read in data
shoot <- read.csv(file = "C:/Users/rache/Documents/Capstone/Data/Historic Data 2006_2019.csv", 
                  sep = ",", na.strings=c("","NA"))

shoot$PRECINCT <- as.factor(shoot$PRECINCT)
shoot$OCCUR_DATE <- anytime(shoot$OCCUR_DATE)
shoot$month <- as.numeric(format(shoot$OCCUR_DATE, "%m"))
shoot$day <- as.numeric(format(shoot$OCCUR_DATE, "%d"))
shoot %<>% 
  mutate(year = lubridate::year(OCCUR_DATE))

all_dates <- shoot %>% 
  mutate(OCCUR_DATE = as.Date(OCCUR_DATE)) %>% 
  complete(OCCUR_DATE = seq.Date(min(OCCUR_DATE), max(OCCUR_DATE), by = "day"))


all_dates$month <- as.numeric(format(all_dates$OCCUR_DATE, "%m"))
all_dates$day <- as.numeric(format(all_dates$OCCUR_DATE, "%d"))
all_dates %<>% 
  mutate(year = lubridate::year(OCCUR_DATE))

sum_shoot_year <- all_dates %>% 
  group_by(year) %>%
  summarise(number = n()) 

ggplot(all_dates, aes(x = as.factor(year))) + geom_bar() + 
  geom_text(stat='count', aes(label=..count..), vjust=-1) +
  ggtitle("Number of Shootings by Year") + theme_minimal() +
  xlab("Year") + ylab("N shootings") + ylim(0,2500)
