# Task: predict whether or not a recipe is a dessert based on
# how many calories it has
library(readr)
library(tidyverse) #usually using for working with many packeges
recipes <- read_csv("C:/study/Data Science Study/epi/epi_r.csv")
bikes <- read_csv("C:/study/Data Science Study/bicycle/nyc-east-river-bicycle-counts.csv")
weather <- read_csv("C:/study/Data Science Study/weather/weatherHistory.csv")

# data cleaning for recipes, delete all recipes with > 10k kal, and missing values
recipes <- recipes %>% 
  filter(calories < 10000) %>%
  na.omit()

# Let try to check type of variables:
is.numeric(recipes$rating)
# check if all the values are integers (whole numbers)
all.equal(recipes$rating, as.integer(recipes$rating)) == T

# let's plot and fit our model

ggplot(recipes, aes(x = calories, y = dessert))+
  geom_point() +
  geom_smooth(method = 'glm', method.args = list(family = 'binomial'))

# That blue line we've plotted is actually a regression model! Looking at the regression line,
# we can tell that as the number of calories in a recipe increases, it's more likely that that 
# recipe is a dessert. 
# (We can tell this because the line moves towards 1, which means "is a dessert" and away from 0, which means "isn't a dessert")

# now let's try to work with weather datasets. 
# I'll try to predict temperature based on wind speed
is.numeric(weather$temper)
all.equal(weather$temper, as.integer(weather$temper)) == T

#temperature is Continuous variable, so the model of regression is Gaussian
names(weather)[names(weather) == 'Temperature (C)'] <- 'temper'
names(weather)[names(weather) == 'Wind Speed (km/h)'] <- 'wind'

geom_smooth(method = 'glm', method.args = list (family = 'gaussian'))

ggplot(weather, aes (y = wind, x = temper)) +
  geom_point() +
  geom_smooth(method = 'glm', method.args = list (family = 'gaussian'))

# Unfortunately, as we can see the wind don't have any effect on temperature
# and we can predict temperature based on wind

# Now let's start working with bicycle 
# Question, can we predict amount of rider depend on temperature

is.numeric(bikes$Total)
all.equal(bikes$Total, as.integer(bikes$Total)) == T

#ok, total is poisson and count
ggplot(bikes, aes(y = bikes$Total, x = bikes$`Low Temp (°F)`)) + 
  geom_point() + 
  geom_smooth(method = 'glm', method.args = list (family = 'poisson'))

# As we can see, then temperature is increasing the total is increasing too.

