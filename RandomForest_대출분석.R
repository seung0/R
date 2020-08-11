library(tidyverse)
getwd()
setwd(dir = "/Users/useonsong/Nano Degree/R머신러닝")
list.files()
load(file = "Bank_DataSet.RDA")


install.packages('randomForest')
library(randomForest)

set.seed(seed = 1234)
fitC <- randomForest(x = trainSet[, -8],
                     y = trainSet[, 8],
                     xtest = testSet[, -8],
                     ytest = testSet[, 8],
                     ntree = 1000,
                     mtry = 3,
                     importance = TRUE,
                     do.trace = 50,
                     keep.forest = TRUE)

# OOB 오차 추정
fitC$err.rate
plot(x = fitC$err.rate[, 1], type = 'l')

# 분류모형
plot(fitC, lwd = 2)

# 변수의 중요도
importance(fitC)
varImpPlot(fitC)

# 개별 나무 모형의 끝마디 수
fitC %>% treesize(terminal = TRUE) %>% 
  hist(main = 'Number of Terminal Nodes')

#fitC$test$predicted
pred1 <- predict(object = fitC, newdata = testSet, type = 'response')
real <- testSet$PersonalLoan


library(caret)
confusionMatrix(data = pred1, reference = real, positive = '1')

library(MLmetrics)
F1_Score(y_true = real, y_pred = pred1, positive = '1')

library(pROC)
prob1 <- predict(object = fitC, newdata = testSet, type = 'vote')[, 2]
print(prob1)
roc(response = real, predictor = prob1) %>% 
  plot(main = 'ROC curve', col = 'red', lwd = 2)
auc(response = real, predictor = prob1)

list.files()
fitDT <- readRDS(file = "DecisionTree.RDS")

pred0 <- predict(object = fitDT, newdata = testSet, type = 'class')

confusionMatrix(data = pred0, reference = real, positive = '1')
F1_Score(y_true =  real, y_pred = pred0, positive = '1')
prob0 <- predict(object = fitDT, newdata = testSet, type = 'prob')[,2]
prob0

roc(response = real, predictor = prob0) %>% 
  plot(add = TRUE, col = 'blue', lwd = 2, lty = 2)

# 변수에 따른 
plot(formula = Income ~ PersonalLoan, data = bank)
plot(formula = Family ~ PersonalLoan, data = bank)
library(gmodels)
CrossTable(x = bank$PersonalLoan, y = bank$Education)


# 튜닝(Tuning)

grid <- expand.grid(ntree = c(300, 500, 700, 1000),
                    mtry = c(3, 4, 5, 6, 7))
print(grid)
grid$error <- NA

for (i in 1:nrow(grid)) {
  set.seed(seed = 1234)
  #cat(i, '행 실행 중 [ntree: ', grid[i, 'ntree'], ', mtry:', grid[i, 'mtry']\n\n)
  cat(str_glue('{i} 행 실행 중 [ntree: {grid$ntree[i]}, mtry: {grid$mtry[i]}]'), '\n\n')
  
  fit <- randomForest(x = trainSet[, -8],
                      y = trainSet[, 8],
                      ntree = grid$ntree[i],
                      mtry = grid$mtry[i],
                      do.trace = 50)
  grid$error[i] <- fit$err.rate[, 1] %>% tail(n = 1)
  }

# best 모형 찾기
plot(x = grid$error,
     ttype = 'b',
     main = 'Grid Search Result')
abline(h = min(grid$error),
       col = 'red',
       lty = 2)

loc <- which.min(grid$error)
loc
bestPara <- grid[loc, ]
print(bestPara)


# best 모형 만들기

set.seed(seed = 1234)
bestC <- randomForest(x = trainSet[, -8],
                      y = trainSet[, 8],
                      ntree = bestPara$ntree,
                      mtry = bestPara$mtry,
                      do.trace = 50)
predB <- predict(object = bestC, newdata = testSet, type = 'response')

confusionMatrix(data = predB, reference = real, positive = '1')
F1_Score(y_true = real, y_pred = predB, positive = '1')

probB <- predict(object = bestC, newdata = testSet, type = 'vote')[,2]

# roc 비교

roc(response = real, predictor = prob1) %>% 
  plot(main = 'ROC curve', col = 'red', lwd = 2)
roc(response = real, predictor = prob0) %>% 
  plot(add = TRUE, col = 'blue', lwd = 2, lty = 2)
roc(response = real, predictor = probB) %>% 
  plot(add = TRUE, col = 'black', lwd = 2, lty = 2)


auc(response = real, predictor = probB)

saveRDS(object = bestC, file = 'RandomForestClassification.RDS')












