library(tidyverse)
library(glmnet) # fit glm with elastic net
library(car) # for avplots
coders_2016 <- read_csv("C:/study/Data Science Study/new_coder/2016-FCC-New-Coders-Survey-Data.csv")
coder_2017 <- read_csv("C:/study/Data Science Study/free_code_pack/2017-fCC-New-Coders-Survey-Data.csv")

# Create a subset of the data with only interested variables

dataSubset <- coders_2016 %>%
  mutate(IsWoman = (Gender == 'female')) %>% # convert gender to a boolean variables
  select(Age, CommuteTime, HasChildren, AttendedBootcamp, IsWoman,
         HoursLearning, MonthsProgramming, HasDebt, Income) %>%
  na.omit()
# conver our input variables to a matrix
input <- dataSubset %>%
  select(-Income) %>% # dont include the variable we're predicting!
  as.matrix()
output <- dataSubset$Income

print('Mean of our predicted value: ')
mean(output)

# use 10-fold cross-validation to fit a bunch of models using elastic net
cv_fit <- cv.glmnet(input, output, family = 'gaussian')

# get coefficients for the best model
coef(cv_fit, s = "lambda.min")

# get a (non-sparse) matrix of the coefficents for the best model
coef_matrix <- coef(cv_fit, s = 'lambda.min') %>%
  as.matrix()
# get the variables with a coefficent that's not 0 
variables <- row.names(coef_matrix)[coef_matrix != 0] %>% # get variables w/ non-zero intercepts
  setdiff("(Intercept)") #remove the intercept (if it's included)

# this variable has just our selected features. 
# print the first few
head(variables)

# turn our list of formulas into a variable
variables_selected <- paste(variables, collapse="+")
formula <- paste("Income ~ ",variables_selected,sep = "") %>%
  as.formula()

# fit a glm model
model <- glm(formula, # formula
             data = dataSubset, # dataset
             family = ("gaussian")) # fit a poisson model

par(mfrow = c(2, 2))
plot(model)

avPlots(model)


# Start work with next datasets
# does the evant effect to income

data2017Subset <- coder_2017 %>%
  select(CodeEventConferences, CodeEventDjangoGirls, CodeEventFCC, CodeEventGameJam,
         CodeEventGirlDev, CodeEventHackathons, CodeEventMeetup, CodeEventNodeSchool, 
         CodeEventNone, CodeEventOther, CodeEventRailsBridge, CodeEventRailsGirls, 
         CodeEventStartUpWknd, CodeEventWkdBootcamps, CodeEventWomenCode, CodeEventWorkshops, Income)
# Let's change NA to 0, since that 1 is TRUE for Events
data2017Subset <- data2017Subset %>% replace(is.na(.), 0)

input2017 <- data2017Subset %>% 
  select(-Income) %>%
  as.matrix()

output2017 <- data2017Subset$Income

print('Mean of our predicted value: ')
mean(output2017)

# let's make cross-validation with n = 10

cv_fit2017 <- cv.glmnet(input2017, output2017, family = 'gaussian')

# get coefficients for the best model
coef(cv_fit2017, s = "lambda.min")

# get a (non-sparse) matrix of the coefficents for the best model
coef_matrix2017 <- coef(cv_fit2017, s = 'lambda.min') %>%
  as.matrix()
# get the variables with a coefficent that's not 0 
variables2017 <- row.names(coef_matrix2017)[coef_matrix2017 != 0] %>% # get variables w/ non-zero intercepts
  setdiff("(Intercept)") #remove the intercept (if it's included)

# this variable has just our selected features. 
# print the first few
head(variables2017)

# turn our list of formulas into a variable
variables_selected2017 <- paste(variables2017, collapse="+")
formula2017 <- paste("Income ~ ",variables_selected2017,sep = "") %>%
  as.formula()

# fit a glm model
model2017 <- glm(formula2017, # formula
             data = data2017Subset, # dataset
             family = ("gaussian")) # fit a poisson model

par(mfrow = c(2, 2))
plot(model2017)

avPlots(model2017)

# Almost no effect from event and income 
