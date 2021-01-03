library(tidyverse)
library(boot)
camera_dataset <- read_csv("C:/study/Data Science Study/camera_data_set/camera_dataset.csv")
harddrive <- read_csv("C:/study/Data Science Study/harddriver_data_set/harddrive.csv", n_max = 100000)

# I will work with failure in harddrive datasets
# Because of the fact the is a categorical variable with two values
# I am going to use the binomial family to fit the model

model <- glm(failure ~ smart_1_normalized, data = harddrive, family = 'binomial')

# I can use diagnostic plots, but not now, fast analysis - summary function

summary(model)
ggplot(harddrive, aes(x = smart_1_normalized, y = failure)) + 
  geom_point() +
  geom_smooth(method = 'glm', method.args = list(family = 'binomial'))


# let's start to work with second dataset
# how the price related to max resolution
is.numeric(camera_dataset$Price)
all.equal(camera_dataset$Price, as.integer(camera_dataset$Price)) == T

camera_dataset <- camera_dataset %>% rename('Max_resolution' = `Max_resolutin`)

new_model <- glm(Price ~ Max_resolution, data = camera_dataset, family = 'poisson')
glm.diag.plots(new_model)
# based on linear predictors our variable is not so good as it can. 
# based on ordered deviance residual our variable is not so good too
# based on two lower graphics we have some points which ruined model
# let's check this conclusions using summary

summary(new_model)
# Deviance Residuals showed that our distribution is bad, 3Q > 1Q 
# Estimate of intercept is more than 0, also std. error don't cover that difference
# Null deviance and Residual is high, also there is big difference between them
# AIC, is high mb I should use other criterion AIC = 597817

ggplot(camera_dataset, aes(x = Max_resolution, y = Price)) + 
  geom_point() +
  geom_smooth(method = 'glm', method.args = list(family = 'poisson'))

#based on graph we can make the conclusion that when max resolution increase price increase too


# let's try to use effective pixels
summary(camera_dataset) # to look at do the effective_pix variable have NA
is.numeric(camera_dataset$Effective_pix)
all.equal(camera_dataset$Effective_pix, as.integer(camera_dataset$Effective_pix)) == T # check which one of variable we have, we have count variable. 

camera_dataset <- camera_dataset %>% rename('Effective_pix' = `Effective pixels`)
new_model_1 <- glm (Price ~ Effective_pix, data = camera_dataset, family = 'poisson')
glm.diag.plots(new_model_1)

# Based on graphs that variable is not so good too
# let's check summary to understand is better or not previous variable
summary(new_model_1)

# I don't focus on previous coefficients, let look at AIC = 597803
# based on AIC effective pixels has more effect on price than max_resolution

ggplot(camera_dataset, aes(x = Effective_pix, y = Price)) + 
  geom_point() +
  geom_smooth(method = 'glm', method.args = list(family = 'poisson'))

# Same conclusion as previous one

# For camera datasets better to use random forest (small datasets to use decision tree)
# First what we need to do is make some data clearing
summary(camera_dataset)
# We have NA values in Macro focus range(1), Storage included(2), Dimensions(2), Weight (inc. batteries)(2)
# In sum it give as 7 values, lets delete the row with that NA, since we can't use median to put here

camera_dataset <- camera_dataset[!is.na(camera_dataset$'Macro focus range'), ]
camera_dataset <- camera_dataset[!is.na(camera_dataset$'Storage included'), ]
camera_dataset <- camera_dataset[!is.na(camera_dataset$'Dimensions'), ]
camera_dataset <- camera_dataset[!is.na(camera_dataset$'Weight (inc. batteries)'), ]

# Also we need change the name of column because to fit model we cannot use quotes
camera_dataset <- camera_dataset %>% rename('Release_date' = `Release date`)
camera_dataset <- camera_dataset %>% rename('Low_resolution' = `Low resolution`)
camera_dataset <- camera_dataset %>% rename('Zoom_wide' = `Zoom wide (W)`)
camera_dataset <- camera_dataset %>% rename('Zoom_tele' = `Zoom tele (T)`)
camera_dataset <- camera_dataset %>% rename('Normal_focus_range' = `Normal focus range`)
camera_dataset <- camera_dataset %>% rename('Macro_focus_range' = `Macro focus range`)
camera_dataset <- camera_dataset %>% rename('Storage_included' = `Storage included`)
camera_dataset <- camera_dataset %>% rename('Weight_inc.batteries' = `Weight (inc. batteries)`)

# Let's check again
summary(camera_dataset)
# Ok, now no NA values 

library(randomForest)

# First of all we need to divide our datasets to training data and test data
index <- sort(sample(nrow(camera_dataset),round(0.25*nrow(camera_dataset))))
camera_dataset_train <- camera_dataset[-index, ]
camera_dataset_test <- camera_dataset[index, ]



fitRandomForest <- randomForest(Price ~ Release_date + Max_resolution + Low_resolution +
                                  Effective_pix + Zoom_wide + Zoom_tele + 
                                  Normal_focus_range + Macro_focus_range + Storage_included + Weight_inc.batteries + Dimensions
                                  , data = camera_dataset_train, ntree = 500)

# Also that good to calculate mae for model
library(modelr)
mae_randomforest <- mae(model = fitRandomForest, data = camera_dataset_test)
mae_randomforest
# so our mae is small, that good
summary(fitRandomForest)

# Let's try to clarify which variable has most weight
library(caret)
varImpPlot(fitRandomForest) # let's save the varImp object

# So we can see that weight_inc.batteries is biggest weight variable
# Let's try to use regression with weight_inc.batteries variable
is.numeric(camera_dataset$Weight_inc.batteries)
all.equal(camera_dataset$Weight_inc.batteries, as.integer(camera_dataset$Weight_inc.batteries))

new_model_2 <- glm (Price ~ Weight_inc.batteries, data = camera_dataset, family = 'poisson')
glm.diag.plots(new_model_2)

# So if we compare 3 sets of graphics, this one (new_model_2) is looks like more good

summary(new_model_2)

#AIC  = 465078 

#If we compare 3 AIC from three different models we can see that last one
# has smaller AIC. That means better to predict price is use weight_inc.batteries
# By honestly, all variable have effect to price. 




