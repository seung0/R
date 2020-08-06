#중고차 가격 분석
library(tidyverse)
getwd()
setwd(dir = "/Users/useonsong/Nano Degree/DATA")
list.files(pattern = 'RDS')
df <- readRDS(file = 'Toyota.RDS')

set.seed(seed = 1234)
n <- nrow(df)
index <- sample(n, n*0.7, replace = FALSE)
trainSet <- df %>% slice(index)
testSet <- df %>% slice(-index)

set.seed(seed = 1234)
fitR <- rpart(formula = Price ~.,
              data = trainSet,
              control = rpart.control(minsplit = 10,
                                       cp = 0.001,
                                       maxdepth =30))
printcp(fitR)

fitR$cptable[, 4]
plotcp(fitR)

rpart.plot(x = fitR, type = 2, extra = 101, fallen.leaves = FALSE)

real <- testSet$Price
predR <- predict(object = fitR, newdata = testSet, type = 'vector')
errorR <- real - predR



regMeasure <- function(real, pred) {
  error <- abs(x = real - pred)
  result <- data.frame(rmse = error^2 %>% mean() %>% sqrt(),
                       mape = (error/real) %>% abs() %>% mean(),
                       mse = error^2 %>% mean(),
                       mae = error %>% abs() %>% mean())
  return(result)
}

regMeasure(real = real, pred = predR)


#유의성 검정
full <- lm(formula = Price ~ .,
           data = trainSet)
fitL <- step(object = full, direction = 'both')
summary(fitL)


#잔차과정
par(mfrow = c(2, 2))
plot(fitL)
par(mfrow = c(1, 1))

#정규분포 하는지
shapiro.test(fitL$residuals)

#등분산 하는지
ncvTest(fitL)

#독립성 검정
durbinWatsonTest(fitL)

#선형성 확인
crPlots(fitL)

#이상치
influencePlot(fitL)

predL <- predict(object = fitL, newdata = testSet, type = 'response')

#성능비교
regMeasure(real = real, pred = predR)
regMeasure(real = real, pred = predL)
#회귀나무모형의 성능이 더 좋음!!!!
