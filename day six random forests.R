library(rsample)
library(tidymodels)
library(dplyr)
diamonds2<-diamonds %>% 
  sample_n(1000)

data_split <- diamonds2 %>%
  initial_split(prop = .8)

training_data <- training(data_split)
validation_data <- testing(data_split)


rf_cls_spec <- 
  rand_forest(trees = 200, min_n = 5) %>% 
  # This model can be used for classification or regression, so set mode
  set_mode("classification") %>% 
  set_engine("randomForest")
rf_cls_spec

set.seed(97331)
#we have no reason to believe that this will produce any meaningful results
rf_cls_fit <- rf_cls_spec %>% fit(color ~ clarity + cut, data = training_data)
rf_cls_fit

predicted<-bind_cols(
  predict(rf_cls_fit, validation_data),
  predict(rf_cls_fit, validation_data, type = "prob")
)

result<-data.frame(validation_data,predicted)

#and to avoid chaos
colnames(result)[3]<-"diamond_color"

library(ggplot2)

result %>%
  mutate("correct" = if_else(as.character(.pred_class)==as.character(diamond_color), "yes", "no")) %>% 
  ggplot(aes(.pred_class, diamond_color, colour=correct))+geom_jitter()


diamonds %>% 
  ggplot(aes(carat, clarity, colour=color))+geom_jitter()

#lets try a regression then...
#using the same data

rf_reg_spec <- 
  rand_forest(trees = 200, min_n = 5) %>% 
  # This model can be used for classification or regression, so set mode
  set_mode("regression") %>% 
  set_engine("randomForest")
rf_reg_spec

set.seed(97331 )
rf_reg_fit <- rf_reg_spec %>% fit(price ~ color + clarity + depth, data = training_data)
rf_reg_fit

result<-predict(rf_reg_fit, validation_data)

diamond_errors<-data.frame(validation_data, result)
diamond_errors %>% 
  ggplot(aes(price, .pred, colour=color, size=carat))+geom_point()


diamonds %>% 
  ggplot(aes(price, carat, color=color))+geom_jitter()+facet_grid(~clarity)+theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

#lets get some analysis going here...

#a small cultural data set
TV3<-TV %>% 
  filter(Year > 1978 & Year <1989)

library(rsample)
data_split<-initial_split(TV3)
training_data <- training(data_split)

validation_data <- testing(data_split)


rf_reg_spec <- 
  rand_forest(trees = 200, min_n = 5) %>% 
  # This model can be used for classification or regression, so set mode
  set_mode("regression") %>% 
  set_engine("randomForest")
rf_reg_spec

set.seed(97331)
rf_reg_fit <- rf_reg_spec %>% fit(Rating ~ as.factor(Network) + as.factor(Type), data = training_data)
rf_reg_fit

result<-predict(rf_reg_fit, validation_data)
ratings<-data.frame(result, validation_data)
ratingsb<-ratings %>% 
  mutate("difference" = Rating-.pred)

ratingsb %>% 
  ggplot(aes(Rating, .pred, colour=difference))+geom_point()

#lets explore s
#where was the model good? Where was it bad?
View(ratingsb)





