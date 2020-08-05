install.packages('reghelper')
library(reghelper)

#회귀계수를 표준화해서 어떤 변수가 더 영향력 있는지 비교할 수 있다.
stdBeta <- beta(fit2)
round(stdBeta$coefficients[, 1], digits = 4)

#와인 데이터 실습


library(tidyverse)
wine <- read.csv(file = 'https://bit.ly/white_wine_quality', sep = ';')
glimpse(wine)

wine$quality %>% table() %>% 
  prop.table() %>% 
  cumsum() %>% 
  round(digits = 4L) * 100

tbl <- table(wine$quality)
bp <- barplot(height = tbl,
              ylim = c(0, 2400),
              col = c(rep('gray70', 4), rep('red', 3)),
              xlab = 'Quality Score',
              main = 'White Wine Quality')

text(x = bp,
     y = tbl,
     labels = tbl,
     pos = 3,
     font =2)


#pallette 사용법
install.packages('RColorBrewer')
library(RColorBrewer)
display.brewer.all()
display.brewer.pal(n = 8, name = 'Accent')
myPal1 <- brewer.pal(n = 7, name = 'Accent')
myPal1


#1)
bp <- barplot(height = tbl,
              ylim = c(0, 2400),
              col = myPal1,
              xlab = 'Quality Score',
              main = 'White Wine Quality')


#2)
myPal2 <- gray(level = seq(from = 0.2, to = 0.8, length = 7))
myPal2


#3)
myPal3 <- colorRampPalette(colors = c('red', 'yellow', 'purple'))(7)



#목표변수 탐색하기
wine$grade <- ifelse(wine$quality <= 6,
                     yes = 'good',
                     no = 'best')
wine$grade <- as.factor(wine$grade)
table(wine$grade, wine$quality)
wine$quality <- NULL


#상관성 분석
boxplot(formula = alcohol ~ grade,
        data = wine)


#데이터 분할하기
set.seed(1234)
n <- nrow(wine)
index <- sample(n, n*0.7, replace = FALSE)
trainSet <- wine %>% slice(index)
testSet <- wine %>% slice(-index)

#비중 확인
table(trainSet$grade) %>% prop.table()
table(testSet$grade) %>% prop.table()

#kkn모형 집합
install.packages('kknn')
library(kknn)

k <- trainSet %>% nrow() %>% sqrt() %>% ceiling()
k
#59개

fitN <- kknn(formula = grade ~.,
             train = trainSet,
             test = testSet,
             k = k,
             kernel = 'rectangular')
str(fitN)

#추정값 확인
predN <- fitN$fitted.values
real <- testSet$grade
table(predN, real)

#혼동행렬
install.packages('caret')
library(caret)
confusionMatrix(data = predN, reference = real, positive = 'best')

install.packages('e1071')



#f1점수
install.packages('MLmetrics')
library(MLmetrics)
F1_Score(y_true = real, y_pred = predN, positive = 'best')


#ROC curve(추정 확률로)
library(pROC)
library(tidyverse)
probN <- fitN$prob[, 1]
roc(response = real, predictor =  probN) %>% plot()
auc(response = real, predictor =  probN)



#SMOTE함수

install.packages('DMwR')
library(DMwR)
set.seed(1234)
trainBal <- SMOTE(form = grade ~ .,
                  data = trainSet,
                  perc.over = 200,
                  k = 10,
                  perc.under = 150)

table(trainSet$grade) %>% prop.table()
table(trainBal$grade) %>% prop.table()
#5:5로 맞춰짐

levels(trainSet$grade)
levels(trainBal$grade)

fitB <- kknn(formula = grade ~.,
             train = trainSet,
             test = testSet,
             k = k,
             kernel = 'rectangular')
predB <- fitB$fitted.values
confusionMatrix(data = predB, reference = real, positive = 'best')

F1_Score(y_true = real, y_pred = predB, positive = 'best')

probB <- fitB$prob[, 1]
roc(response = real, predictor = probB) %>% plot(col = 'blue', add = TRUE)
roc(response = real, predictor = probN) %>% plot(main = 'ROC곡선', col = 'red')

auc(response = real, predictor = probN)
auc(response = real, predictor = probB)


#가중치 있는 knn 모형

fitW <- kknn(formula = grade ~ .,
             train = trainSet,
             test = testSet,
             k = k,
             kernel = 'triangular')
predW <- fitW$fitted.values
confusionMatrix(predW, real, 'best')
F1_Score(y_true =real, y_pred = predW, positive = 'best')

probW <- fitW$prob[, 1]
roc(response = real, predictor = probW) %>% 
  plot(col = 'black',
       lwd = 2,
       add = TRUE)

auc(response = real, predictor = probW)



fitWB <- kknn(formula = grade ~ .,
             train = trainBal,
             test = testSet,
             k = k,
             kernel = 'triangular') 
predWB <- fitWB$fitted.values
confusionMatrix(predWB, real, 'best')
F1_Score(real, predWB, 'best')

probWB <- fitWB$prob[, 1]
roc(response = real, predictor =  probWB) %>% plot(col = 'orange',
                           lwd = 3,
                           lty = 2,
                           add = TRUE)

auc(real, probWB)


#최적의 k를 찾기 위한 교차검증
kvec <- seq(from = 3, to = 159, by = 2)
kvec
result <- c()

for (k in kvec) {
  cat('현재', k, '로 작업 중!\n', sep = '')
  fit <- kknn(formula = grade ~ .,
                       train = trainBal,
                       test = testSet,
                       k = k,
                       kernel = 'triangular') 
  real <- testSet$grade
  pred <- fit$fitted.values
  f1 <- F1_Score(y_true = real, y_pred =  pred, positive = 'best')
  result <- c(result, f1)
}

print(result)
plot(x = kvec, y = result, type = 'b', col = 'red')
max(result)
which(result == max(result))
abline(h = max(result),
       col = 'black',
       lty = 3)
abline(v = 7,
       col = 'black',
       lty = 3)

text(x = 7, y = 0.5, labels = str_c('k는',  kvec[3]),
     font = 2,
     col = 'red')


fitW <- kknn(formula = grade ~ .,
             train = trainSet,
             test = testSet,
             k = 7,
             kernel = 'triangular')
predW <- fitW$fitted.values
confusionMatrix(predW, real, 'best')
F1_Score(y_true =real, y_pred = predW, positive = 'best')

probW <- fitW$prob[, 1]
roc(response = real, predictor = probW) %>% 
  plot(col = 'black',
       lwd = 2)

auc(response = real, predictor = probW)


