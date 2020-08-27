library(tidyverse)

wine <- read.csv(file = 'https://bit.ly/white_wine_quality', sep = ';')
glimpse(wine)

wine$quality %>% 
  table() %>% 
  prop.table() %>% 
  cumsum() %>% 
  round(4L) * 100

tbl <- table(wine$quality)
tbl

bp <- barplot(height = tbl,
              ylim = c(0, 2400),
              col = c(rep('gray70', times = 4),
                      rep('red', times = 3)),
              xlab = 'Quality Score',
              main = 'White wine quality')
text(x = bp,
     y = tbl,
     labels = tbl,
     pos = 3,
     font = 2)

install.packages('RColorBrewer')
library(RColorBrewer)
display.brewer.all()
display.brewer.pal(n = 11, name = 'Spectral')
myPal <- brewer.pal(n = 7, name = 'Spectral')

bp <- barplot(height = tbl,
              ylim = c(0, 2400),
              col = myPal,
              xlab = 'Quality Score',
              main = 'White Wine Quality')

wine$grade <- ifelse(test = wine$quality <= 6,
                     yes = 'good',
                     no = 'best')
wine$grade <- as.factor(wine$grade)
table(wine$grade, wine$quality)

wine$quality <- NULL

boxplot(formula = alcohol ~ grade,
        data = wine)

set.seed(seed = 1234)
n <- nrow(wine)
index <- sample(x = n, size = n * 0.7, replace = FALSE)
trainSet <- wine %>% slice(index)
testSet <- wine %>% slice(-index)

table(trainSet$grade) %>% prop.table()
table(testSet$grade) %>% prop.table()

library(kknn)
k <- trainSet %>% nrow() %>% sqrt() %>% ceiling()
k

fitN <- kknn(formula = grade ~.,
             train = trainSet,
             test = testSet,
             k = k,
             kernel = 'rectangular')
str(fitN)

real <- testSet$grade
predN <- fitN$fitted.values

library(caret)
confusionMatrix(data = predN,
                reference = real,
                positive = 'best')
library(MLmetrics)
F1_Score(y_true = real,
         y_pred = predN,
         positive = 'best')
library(pROC)
probN <- fitN$prob[,1]
roc(response = real, predictor = probN) %>% plot()
auc(response = real, predictor = probN)


library(DMwR)
trainBal <- SMOTE(form = grade ~.,
                  data = trainSet,
                  perc.over = 200,
                  k = 10,
                  perc.under = 150)
table(trainSet$grade) %>% prop.table()
table(trainBal$grade) %>% prop.table()

levels(trainSet$grade)
levels(trainBal$grade)

fitB <- kknn(formula = grade ~.,
             train = trainBal,
             test = testSet,
             k = k,
             kernel = 'rectangular')
predB <- fitB$fitted.values

confusionMatrix(data = predB,
                reference = real,
                positive = 'best')
F1_Score(y_true = real,
         y_pred = predB,
         positive = 'best')
probB <- fitB$prob[,1]
roc(response = real, predictor = probB) %>% plot(col = 'blue', add = TRUE)
auc(response = real, predictor = probB)

fitW <- kknn(formula = grade ~.,
             train = trainSet,
             test = testSet,
             k = k,
             kernel = 'triangular')
predW <- fitW$fitted.values

confusionMatrix(data = predW, reference = real, positive = 'best')
F1_Score(y_true = real, y_pred = predW, positive = 'best')

probW <- fitW$prob[,1]
roc(response = real, predictor = probW) %>% plot(col = 'green', lwd = 2, add = TRUE)
auc(response = real, predictor = probW)

fitWB <- kknn(formula = grade ~.,
              train = trainBal,
              test = testSet,
              k = k,
              kernel = 'triangular')
predWB <- fitWB$fitted.values

confusionMatrix(data = predWB, reference = real, positive = 'best')
F1_Score(y_true = real, y_pred = predWB, positive = 'best')

probWB <- fitWB$prob[,1]
roc(response = real, predictor = probWB) %>% plot(col = 'orange', lwd = 3, add = TRUE)
auc(response = real, predictor = probWB)


kvec <- seq(from = 3, to = 159, by = 2)
kvec

library(kknn)
library(MLmetrics)

result <- c()

for (k in kvec) {
  cat('현재', k, '로 작업 중!\n', sep = '')
  
  fit <- kknn(formula = grade ~.,
              train = trainSet,
              test = testSet,
              k = k,
              kernel = 'triangular')
  real <- testSet$grade
  pred <- fit$fitted.values
  
  f1 <- F1_Score(y_true = real, y_pred = pred, positive = 'best')
  result <- c(result, f1)
}

result
plot(x = kvec, y = result, type = 'b', col = 'red')
max(result)
which(result == max(result))

abline(h = max(result), col = 'orange', lty = 3)
abline(v = 7, col = 'orange', lty = 3)
text(x = 7, y = 0.6, labels = str_c('k=', kvec[3]), font = 2, col = 'orange')

fitW <- kknn(formula = grade ~.,
             train = trainSet,
             test = testSet,
             k = 7,
             kernel = 'triangular')
predW <- fitW$fitted.values

confusionMatrix(data = predW, reference = real, positive = 'best')
F1_Score(y_true = real, y_pred = predW, positive = 'best')
probW <- fitW$prob[,1]
roc(response = real, predictor = probW) %>% plot(main = 'ROC curve', col = 'red')
auc(response = real, predictor = probW)
roc(response = real, predictor = probN) %>% plot(color = 'black', add = TRUE)



