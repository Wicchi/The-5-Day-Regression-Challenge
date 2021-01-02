library(tidyverse)
library(boot) #for diagnostic plots

kaggle <- read.csv("C:/study/Data Science Study/2017kaggleSurvey/multipleChoiceResponses.csv")
stackOverflow <- read.csv("C:/study/Data Science Study/StackOverflowDevSurvey/survey_results_public.csv")

# look at rows where we have people who have reported having compensation of more than 0 units of currency.
has_compensation <- kaggle %>% 
  filter(CompensationAmount > 0) %>% 
  mutate(CleanedCompensationAmount = str_replace_all(CompensationAmount, "[[:punct:]]", "")) %>%
  mutate(CleanedCompensationAmount = as.numeric(CleanedCompensationAmount))

# the last two lines remove punctuation (some of the salaries has commas in them)
# and make sure that salary is numeric     

model <- glm(CleanedCompensationAmount ~ Age, data = has_compensation, family = poisson)
glm.diag.plots(model)

has_compensation <- has_compensation %>%
  filter(CleanedCompensationAmount < 150000)
model1 <- glm(CleanedCompensationAmount ~ Age, data = has_compensation, family = poisson)
glm.diag.plots(model1)
ggplot(has_compensation, aes(x = Age, y = CleanedCompensationAmount)) + # draw a 
  geom_point() + # add points
  geom_smooth(method = "glm", # plot a regression...
              method.args = list(family = "poisson"))

# As we can see here, more 'older' data scientist has more salary



# here my work on stackoverflow dataset. I want to know could we predict the jobsatisfaction based on type of HoursPerWeek
# First I will do some filtering

stackOverflow <- stackOverflow[!is.na(stackOverflow$JobSatisfaction), ]
stackOverflow <- stackOverflow[!is.na(stackOverflow$HoursPerWeek), ]

model3 <- glm(JobSatisfaction ~ HoursPerWeek, data = stackOverflow, family = poisson)
glm.diag.plots(model3)
# Graphs are not so bad that model is good
ggplot(stackOverflow, aes(x = JobSatisfaction, y = HoursPerWeek)) + # draw a 
  geom_point() + # add points
  geom_smooth(method = "glm", # plot a regression...
              method.args = list(family = "poisson"))







# Can we Predict the Salary of the data scientists based on their gendeR?

#Salary of Data Scientist in United States
Kag_salary<-kaggle  %>%
  filter(Country == 'United States') %>%
  filter(CompensationAmount > 0) %>%
  mutate(CleanAmount = str_replace_all(CompensationAmount, "[[:punct:]]", "")) %>%
  mutate(CleanAmount = as.numeric(CleanAmount))


## we are predicting a count value, hence the Poisson 
kag_model<-glm(CleanAmount ~ GenderSelect, data = Kag_salary, family = gaussian)

#diagonostic plots from boot library
glm.diag.plots(kag_model)

# remove compensation values above 200K
Kag_salary <- Kag_salary %>%
  filter(CleanAmount < 200000)

# model to predict salary by age with poisson
kag_model1 <- glm(CleanAmount ~ GenderSelect, data = Kag_salary, family = gaussian)

# diagnostic plots
glm.diag.plots(kag_model1)

## Quick Plot to show the Regression Line.
## Not getting any regression line on the graph. why?
ggplot(Kag_salary, aes(x = GenderSelect, y = CleanAmount)) +  
  geom_point() + 
  geom_smooth(method = "glm", 
              method.args = list(family = "gaussian")) 

