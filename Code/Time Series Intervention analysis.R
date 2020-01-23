#############################
# Change Point Analysis
# Rachel Weber
# Created: 10/23/2019
#############################

library(anytime)
library(tidyverse)
library(magrittr)
library(pander)

# read in data
shoot <- read.csv(file = "C:/Users/rache/Documents/Capstone/Data/Historic Data 2006_2018.csv", sep = ",", na.strings=c("","NA"))

shoot$PRECINCT <- as.factor(shoot$PRECINCT)
shoot$OCCUR_DATE <- anytime(shoot$OCCUR_DATE)
shoot$month <- as.numeric(format(shoot$OCCUR_DATE, "%m"))
shoot$day <- as.numeric(format(shoot$OCCUR_DATE, "%d"))
shoot %<>% 
  mutate(year = lubridate::year(OCCUR_DATE))

all_dates <- shoot %>% 
  mutate(OCCUR_DATE = as.Date(OCCUR_DATE)) %>% 
  complete(OCCUR_DATE = seq.Date(min(OCCUR_DATE), max(OCCUR_DATE), by="day"))

all_dates$weekday <- weekdays(all_dates$OCCUR_DATE)

getSeason <- function(DATES) {
  WS <- as.Date("2012-12-15", format = "%Y-%m-%d") # Winter Solstice
  SE <- as.Date("2012-3-15",  format = "%Y-%m-%d") # Spring Equinox
  SS <- as.Date("2012-6-15",  format = "%Y-%m-%d") # Summer Solstice
  FE <- as.Date("2012-9-15",  format = "%Y-%m-%d") # Fall Equinox
  
  d <- as.Date(strftime(DATES, format="2012-%m-%d"))
  
  ifelse (d >= WS | d < SE, "Winter",
          ifelse (d >= SE & d < SS, "Spring",
                  ifelse (d >= SS & d < FE, "Summer", "Fall")))
}

all_dates$season <- as.factor(getSeason(all_dates$OCCUR_DATE))
all_dates$month <- as.numeric(format(all_dates$OCCUR_DATE, "%m"))
all_dates$day <- as.numeric(format(all_dates$OCCUR_DATE, "%d"))
all_dates %<>% 
  mutate(year = lubridate::year(OCCUR_DATE))

sum_shoot <- all_dates %>% 
  group_by(year, month, day) %>%
  summarise(number = n()) 


# join other columns
test <- left_join(sum_shoot, all_dates[,c(5,19:23)])
sum_shoot <- test[!duplicated(test),]

# make zero days zero
sum_shoot[is.na(sum_shoot$PRECINCT),]$number <- 0

# remove precinct then remove duplicates
sum_shoot <- sum_shoot[-c(5)]
sum_shoot <- sum_shoot[!duplicated(sum_shoot),]

# add number for each month and day
sum_shoot <- sum_shoot %>% 
  mutate(md = row_number())

sum_shoot$md <-1:nrow(sum_shoot)

# January 15th 2013 is row 2572
monthday <- (sum_shoot$md > 2572) * 1
m11513 <- sum_shoot$md - 2572
datatsi <- cbind(sum_shoot, monthday, m11513)
lm.a <- lm(number ~ season + weekday + year + monthday : m11513, data = datatsi)