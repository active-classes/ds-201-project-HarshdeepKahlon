library(tidyverse)
library(caret)
library(skimr)
library(RANN)
library(rpart)
library(rpart.plot)
library(randomForest)
library(party)

app_store_df <- read.csv("../Data/AppleStore.csv")
app_store_df <- mutate(app_store_df, size_mb = size_bytes/1000000)
app_store_df <- mutate(app_store_df, is_free = price == 0)
app_store_df <- mutate(app_store_df, user_rating_string = as.character(user_rating))

sapply(app_store_df, class)


index <- createDataPartition(app_store_df$rating_count_tot, p=0.75, list=FALSE)
trainSet <- app_store_df[ index,]
testSet <- app_store_df[-index,]
print(trainSet)
print(testSet)

outcomeName<-'rating_count_tot'

predictors<-names(trainSet)[!names(trainSet) %in% outcomeName]

print(predictors)

predictors<-c("price", "prime_genre", "cont_rating", "size_mb")

fitControl <- trainControl(
  method = "repeatedcv",
  number = 5,
  repeats = 5,
  allowParallel = TRUE
)

model_cforest<-train(trainSet[,predictors],trainSet[,outcomeName],method='cforest')
model_rf<-train(trainSet[,predictors],trainSet[,outcomeName],method='rf')
model_nnet<-train(trainSet[,predictors],trainSet[,outcomeName],method='nnet')
model_glm<-train(trainSet[,predictors],trainSet[,outcomeName],method='glm')

saveRDS(model_rf, "../rf_model.rds")
saveRDS(model_nnet, "../nnet_model.rds")
saveRDS(model_glm, "../glm_model.rds")
saveRDS(model_cforest, "../cforest_model.rds")

