
library(tidyverse)
library(caret)
library(skimr)
library(RANN)
library(rpart)
library(rpart.plot)
library(randomForest)


fitControl <- trainControl(
  method = "repeatedcv",
  number = 10,
  repeats = 10,
  allowParallel = TRUE,
  search = "random"
)

model <-train(rating_count_tot ~ size_mb + price + prime_genre + cont_rating + is_free, data=app_store_df, method='ridge', trainControl=fitControl, preProcess = c('scale', 'center'), na.action = na.omit)

saveRDS(model, "./ridge_model.rds")

