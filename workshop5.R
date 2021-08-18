#install.packages("MASS")
#install.packages("ISLR")

library(tidymodels)
library(MASS, ISLR)

set.seed(123)

lm_spec<-linear_reg()%>%
  set_mode("regression")%>%
  set_engine("lm")

lm_spec

data(Boston)

lm_fit<-lm_spec%>%
  fit(data=Boston,medv~lstat)
lm_fit

lm_fit%>%pluck("fit")%>%
  summary()
tidy(lm_fit)

predict(lm_fit, new_data = Boston)

final_model<-augment(lm_fit, new_data = Boston)

rm(list=ls())

data(Boston)

#1
model_spec<-linear_reg()%>%
  set_mode("regression")%>%
  set_engine("lm")

#2
model_fit<-model_spec%>%fit(data= Boston, medv~age+crim+rm)

#3
#model_predicted<-predict(model_fit, new_data=Boston)

model_predicted_augment<-augment(model_fit, new_data = Boston)
view(model_predicted_augment)


rm(list = ls())

data(Boston)

#1
model_lm_spec<-linear_reg()%>%
  set_mode("regression")%>%
  set_engine("lm")

model_rf_spec<-rand_forest()%>%
  set_mode("regression")%>%
  set_engine("ranger")

install.packages("ranger")
library(ranger)

#2
model_lm_fit<-model_lm_spec%>%
  fit(data=Boston, medv~.)

#3
model_lm_augm<-augment(model_lm_fit, new_data = Boston)
