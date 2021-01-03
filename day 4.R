library(car) # for avplots
library(tidyverse)

ehresp_2014 <- read_csv("C:/study/Data Science Study/eating_health/ehresp_2014.csv") %>%
  filter(erbmi > 0) #remove rows where reported BMI is NA or less than 0
nyc_census_tracts <- read_csv("C:/study/Data Science Study/New_york_city/nyc_census_tracts.csv")

# fit model
model <- glm(erbmi ~ euexfreq + euwgt + euhgt + ertpreat,
             data = ehresp_2014, family = ('gaussian'))

par(mfrow = c(2, 2))
plot(model)

summary(model)
avPlots(model)

# let's work with second datasets

model1 <- glm(Employed ~ White + Black + Native + Asian, data = nyc_census_tracts, family = ('poisson'))

par(mfrow = c(2, 2))
plot(model1)

# Based on graphs; that are good criteria to measure the employed

summary(model1)
# All variables has strong relationship to output

avPlots(model1)

# That is surprised, so when we have more black people we have less employed.


