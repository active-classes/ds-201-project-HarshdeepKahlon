library(tidyverse)
library(caret)
library(randomForest)
library(MASS)
app_store_df <- read.csv("../Data/AppleStore.csv")
app_store_df <- mutate(app_store_df, size_mb = size_bytes/1000000)
app_store_df <- mutate(app_store_df, is_free = price == 0)
app_store_df <- mutate(app_store_df, user_rating_string = as.character(user_rating))

sapply(app_store_df, class)

index <- createDataPartition(app_store_df$rating_count_tot, p=0.75, list=FALSE)

train_ids <- createDataPartition(app_store_df$rating_count_tot,p=0.75,list = F)
iris_train <- app_store_df[train_ids, ]
iris_test <- app_store_df[-train_ids, ]

rf_mod <- randomForest(Species ~., data=iris_train,mtry=4)

rf_mod$confusion

rf_mod$importance

train_results <- data.frame(pred=rf_mod$predicted,obs=rf_mod$y)

defaultSummary(train_results)

mod_preds <- predict(rf_mod,iris_test)

confusionMatrix(mod_preds,iris_test$rating_count_tot)$
  
print(confusionMatrix(mod_preds, iris_test$rating_count_tot))

(train_acc <- 1 - 6/(35 + 35 + 38 + 3 + 3))
(test_acc <- 1 - 2/(12 + 12 + 10 + 2))

train_ids2 <- createDataPartition(Boston$medv,p=0.8,list=F)
boston_train <- Boston[train_ids2, ]
boston_test <- Boston[-train_ids2, ]

boston_rfmod <- randomForest(medv ~., data=boston_train,mtry=13)

(test_rmse_est <- RMSE(boston_rfmod$predicted,boston_train$medv))

pred_vals <- predict(boston_rfmod,boston_test) 

(test_rmse <- RMSE(pred_vals,boston_test$medv))






