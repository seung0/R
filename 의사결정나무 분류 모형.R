library(tidyverse)

bank <- read.csv(file = 'https://bit.ly/universal_bank')
glimpse(bank)
summary(bank)

#데이터 전처리
bank <- bank %>% select(-ID, -ZIP.Code) %>% filter(Experience > 0)
bank[, c(6, 8:12)] <- map_df(bank[, c(6, 8:12)], as.factor)
summary(bank)


#목표변수 파악
bank$PersonalLoan %>% table() %>% prop.table()

#분석 데이터 분할
set.seed(seed = 1234)
n <- nrow(bank)
index <- sample(n, n * 0.7, replace = FALSE)
trainSet <- bank %>% slice(index)
testSet <- bank %>% slice(-index)

#비중 확인
trainSet$PersonalLoan %>% table() %>% prop.table()
testSet$PersonalLoan %>% table() %>% prop.table()



#RDA파일로 저장
getwd()
setwd(dir = '/Users/useonsong/Nano Degree/R머신러닝')
save(list = c('bank', 'trainSet', 'testSet'),
     file = 'Bank_DataSet.RDA')


#의사결정나무 분류 모형 만들기
library(rpart)
set.seed(seed = 1234)
fitC <- rpart(formula = PersonalLoan ~ .,
              data = trainSet,
              control = rpart.control(minsplit = 20,
                                      cp = 0.01,
                                      maxdepth = 30))

summary(fitC)

#나무모형 시각화
install.packages('rpart.plot')
library(rpart.plot)

rpart.plot(x = fitC,
           type = 2,
           extra = 101,
           fallen.leaves = FALSE)


printcp(fitC)
plotcp(fitC)

#정지규칙 변경
set.seed(seed = 1234)
fitC <- rpart(formula = PersonalLoan ~ .,
              data = trainSet,
              control = rpart.control(minsplit = 10,
                                      cp = 0.001,
                                      maxdepth = 30))
printcp(fitC)
plotcp(fitC)
fitP <- prune.rpart(tree = fitC, cp = 0.009)




#성능검사

real <- testSet$PersonalLoan
predC <- predict(fitC, testSet, 'class')
predP <- predict(fitP, testSet, 'class')

print(real)
print(predC)
print(predP)

library(caret)
#혼동행렬
confusionMatrix(data = predC, reference = real, positive = '1')

#f1
library(MLmetrics)
F1_Score(y_true = real, y_pred = predC, positive = '1')


confusionMatrix(data = predP, reference = real, positive = '1')
F1_Score(y_true = real, y_pred = predP, positive = '1')

#ROC curve
probC <- predict(object = fitC, newdata = testSet, type = 'prob')
probC <- probC[,2]

library(pROC)
roc(response = real, predictor = probC) %>% 
  plot(main = 'ROC 곡선')

#가지치기모형
probP <- predict(object = fitP, newdata = testSet, type = 'prob')
probP <- probP[,2]

roc(response = real, predictor = probP) %>% 
  plot(col = 'red', lwd = 2, add = TRUE)

#AUC
auc(response = real, predictor = probC)
auc(response = real, predictor = probP)


#가지치기 모형 저장
getwd()
saveRDS(object = fitP, file = 'DecisionTree.RDS')











