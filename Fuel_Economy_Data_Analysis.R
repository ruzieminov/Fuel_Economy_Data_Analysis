#1. Add ggplot2::mpg dataset. 
library(tidyverse) 
library(data.table)
library(rstudioapi)
library(skimr)
library(inspectdf)
library(mice)
library(plotly)
library(highcharter)
library(recipes) 
library(caret) 
library(purrr) 
library(graphics) 
library(Hmisc) 
library(glue)
library(h2o)
data <- mpg
#2. Make data ready for analysis doing preprocessing techniques.

skim(data)
df.num <- data %>%
  select_if(is.numeric) %>%
  select(cty,everything())

df.chr <- data %>%
  select_if(is.character)

df.chr <- dummyVars(" ~ .", data = df.chr) %>% 
  predict(newdata = df.chr) %>% 
  as.data.frame()

data <- cbind(df.chr,df.num) %>% 
  select(cty,everything())

data[,-1] <- data[,-1] %>% scale() %>% as.data.frame()

names(data) <- names(data) %>% 
  str_replace_all(" ","_") %>%
  str_replace_all("-","_") %>%
  str_replace_all("\\(","") %>% 
  str_replace_all("\\)","") %>% 
  str_replace_all("\\'","")

target <- 'cty'
features <- data %>% select(-cty) %>% names()
f <- as.formula(paste(target,paste(features,collapse = "+"), sep ="~"))
glm <- glm(f,data=data)

coef_na <- attributes(alias(glm)$Complete)$dimnames[[1]]
features <- features[!features %in% coef_na]

f <- as.formula(paste(target, paste(features, collapse = " + "), sep = " ~ "))
glm <- glm(f, data = data)

glm %>% summary()

while(glm %>% faraway::vif() %>% sort(decreasing = T) %>% .[1] >= 1.5){
  afterVIF <- glm %>% faraway::vif() %>% sort(decreasing = T) %>% .[-1] %>% names()
  f <- as.formula(paste(target, paste(afterVIF, collapse = " + "), sep = " ~ "))
  glm <- glm(f, data = data)
}

glm %>% faraway::vif() %>% sort(decreasing = T) %>% names() -> features 

h2o.init()
h2o_data <- data %>% as.h2o()
h2o_data <- h2o_data %>% h2o.splitFrame(ratios = 0.8, seed = 123)
train <- h2o_data[[1]]
test <- h2o_data[[2]]

#3. Fit Generalized Linear Model using H2O in R. 

model <- h2o.glm(
  x = features, y = target,
  training_frame = train,
  validation_frame = test,
  nfolds = 10, seed = 123,
  lambda = 0, compute_p_values = T)

#4. Run GLM using following modelling structure. cty ~ year + cyl + displ.

glm <- glm(cty~year+cyl+displ,data=data)

#5. Print coefficients table and give interpretation of results. 

model@model$coefficients_table 


#6. Name your final homework Script as “Fuel_Economy_Data_Analysis”.
#   Complete your homework with following tasks:
#1. Create a new folder and add your Script to this folder, then add and make commits of your 
#   changes. 
#2. Create repository named “Fuel_Economy_Data” in your Github account. 
#3. Push your homework Script to this repository. 
#4. Fork other users’ repositories, make changes as needed and make pull requests.
