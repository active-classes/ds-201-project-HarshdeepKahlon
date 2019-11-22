
library(tidyverse)
library(caret)
library(skimr)
library(RANN)
library(rpart)
library(rpart.plot)
library(randomForest)

app_store_df <- read.csv("../Data/AppleStore.csv")
app_store_df <- mutate(app_store_df, size_mb = size_bytes/1000000)
app_store_df <- mutate(app_store_df, is_free = price == 0)
app_store_df <- mutate(app_store_df, user_rating_string = as.character(user_rating))

sapply(app_store_df, class)


index <- createDataPartition(app_store_df$rating_count_tot, p=0.75, list=FALSE)
trainSet <- app_store_df[ index,]
testSet <- app_store_df[-index,]

control <- rfeControl(functions = rfFuncs,
                   method = "repeatedcv",
                   repeats = 3,
                   verbose = FALSE)
outcomeName<-'rating_count_tot'

predictors<-names(trainSet)[!names(trainSet) %in% outcomeName]

predictors<-c("price", "prime_genre", "cont_rating", "size_mb")

fitControl <- trainControl(
  method = "repeatedcv",
  number = 5,
  repeats = 5,
  allowParallel = TRUE)

modelLookup(model='gbm')

grid <- expand.grid(n.trees=c(10,20,50,100,500,1000),shrinkage=c(0.01,0.05,0.1,0.5),n.minobsinnode = c(3,5,10),interaction.depth=c(1,5,10))

model_gbm<-train(trainSet[,predictors],trainSet[,outcomeName],method='gbm',trControl=fitControl,tuneGrid=grid)

saveRDS(model_gbm, "./final_model.rds")

