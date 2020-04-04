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

shoot$OCCUR_DATE <- anytime(shoot$OCCUR_DATE)
shoot %<>% 
  mutate(OCCUR_DATE = as.Date(OCCUR_DATE))
shoot$month <- as.numeric(format(shoot$OCCUR_DATE, "%m"))

shoot %<>% 
  mutate(year = lubridate::year(OCCUR_DATE))

shoot_count <- shoot %>% 
                group_by(year, month) %>% 
                summarise(number = n()) 

shoot_count$month_count <-1:nrow(shoot_count)

ggplot(shoot_count, aes(x = month_count, y = number)) + geom_line() +
  theme_minimal() +
  scale_x_continuous(breaks = c(0,24,48,72,94,118,142,164,188,212)) +
  xlab("Months Since Jan. 2006") +
  ylab("Total Shootings Per Month") +
  ggtitle("Monthly Shooting Counts January 2006-December 2019")
