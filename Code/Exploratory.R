########################################
# NYC shooting data project
# Author: Rachel Weber
# Data provided by NYC Open Data
########################################

# the objective of this study is to determine the odds of a shooting-free weekend in NYC
# this was initiated following the first shooting-free weekend in September 2018 NYC has had in a decade
# data were requested and granted from the NYC Open Data resource
# data was received on 11/1/18

library(anytime)
library(CIDAtools)
library(tidyverse)
library(magrittr)
library(pander)
library(ggplot2)

# read in data
shoot <- read.csv(file = "C:/Users/weberra/Documents/NYC Shootings Project/Data/NYPD_Shooting_Incident_Data__Historic_.csv", sep = ",")

shoot$PRECINCT <- as.factor(shoot$PRECINCT)
shoot$OCCUR_DATE <- anytime(shoot$OCCUR_DATE)

# There are dates where no shooting happened
# to know how many and when they occured, I'll fill in any missing dates as empty rows

all_dates <- shoot %>% 
  mutate(OCCUR_DATE = as.Date(OCCUR_DATE)) %>% 
  complete(OCCUR_DATE = seq.Date(min(OCCUR_DATE), max(OCCUR_DATE), by="day"))

# we gained 158 days. There were 158 days in 5 years where no shootings happened in NYC
# assuming 1 leap year in the 5 years of data, there were 1826 days in the dataset
# 8% of days between 2013 and 2017 had no shootings


# let's get a column of just year for shootings
# we know rates have been going down and may have to accomodate that in our analysis
all_dates %<>% 
  mutate(year = lubridate::year(OCCUR_DATE))

# a table 1 to summarize shooting demographic info
table1 <- Table1(c("BORO", "PERP_AGE_GROUP", "PERP_SEX", "PERP_RACE", "VIC_AGE_GROUP", "VIC_SEX", "VIC_RACE"), NULL,
                 data = shoot, incl_missing = F)
set.alignment("center", row.names="left")
pander(table1, caption = "")

# a graph showing number of shootings over the 5 years
ggplot(all_dates, aes(x = factor(year))) + geom_bar() + 
  ggtitle("Number of Shootings by Year") + theme_minimal() +
  xlab("Year") + ylab("N shootings")

# shootings are trending down. Do we need to account for that?
# Since this was the first shooting-free weekend in a decade, why not include all data,
# even from years where rates were higher than the present year?

  