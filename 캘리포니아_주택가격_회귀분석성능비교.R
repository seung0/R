#주택 가격을 추정하는 회귀모형을 회귀나무 알고리즘을 이용하여 적합합니다.
library(tidyverse)
house <- read.csv(file = 'https://bit.ly/median_house_value')
str(house)
summary(house)
glimpse(house)

#목표변수 히스토그램 그리기
hist(house$MedianHouseValue)

#500000건 이상인 데이터 삭제
house <- house %>% filter(MedianHouseValue <= 500000)


#분석 데이터의 분할(30% 훈련용, 70% 시험용)
set.seed(seed = 1234)
n <- nrow(house)
index <- sample(n, n * 0.3, replace = FALSE)
trainSet <- house %>% slice(index)
testSet <- house %>% slice(-index)


trainSet$MedianHouseValue %>% mean()
testSet$MedianHouseValue %>% mean()

getwd()
save(list = c('trainSet', 'testSet', 'house'), file = 'House_Dataset.RDA')

#의사결정나무 회귀모형 함수(rpart)
library(rpart)
set.seed(seed = 1234)
fitR <- rpart(formula = MedianHouseValue ~ .,
      data = trainSet,
      control = rpart.control(minsplit = 20,
                              cp = 0.01,
                              maxdepth = 10))
summary(fitR)
#xerror의 마지막이 가장 작으므로 가지치기 필요없음

#나무모형 시각화
library(rpart.plot)
rpart.plot(x = fitR, type = 2, extra = 101, fallen.leaves = FALSE)

#가지치기 여부 판단:비용복잡도 테이블
printcp(fitR)
plotcp(fitR)

#회귀모형의 재적합(정지규칙 변경)
set.seed(seed = 1234)
fitR <- rpart(formula = MedianHouseValue ~ .,
              data = trainSet,
              control = rpart.control(minsplit = 10,
                                      cp = 0.001,
                                      maxdepth = 30))
printcp(fitR)
plotcp(fitR)

#가지치기
fitP <- prune.rpart(tree = fitR, cp = 0.001)
rpart.plot(fitR, type = 2, extra = 101, fallen.leaves = FALSE)


#회귀모형의 성능 평가


real <- testSet$MedianHouseValue
predR <- predict(object = fitR, newdata = testSet, type = 'vector')
predP <- predict(object = fitP, newdata = testSet, type = 'vector')

errorR <- real - predR
errorR^2 %>% mean() %>% sqrt()
errorR^2 %>% abs() %>% mean()
(errorR/real) %>% abs() %>% mean()

errorP <- real - predP
errorP^2 %>% mean() %>% sqrt()
errorP^2 %>% abs() %>% mean()
(errorP/real) %>% abs() %>% mean()

#선형 회귀모형과 비교
full <- lm(formula = MedianHouseValue ~., data = trainSet)
fitL <- step(object = full, direction = 'both')

null <- lm(formula = MedianHouseValue ~ 1, data = trainSet)
fit2 <- step(object = null,
             scope = list(lower = null, upper = full),
             direction = 'both')
summary(fitL)

#잔차과정
par(mfrow = c(2,2))
plot(fitL)

shapiro.test(x = fitL$residuals) #0.05보다 커야함

#5000개 넘어가면 앤더슨 달리 테스트
install.packages('nortest')
library(car)
library(nortest)

ad.test(x = fitL$residuals) #0.05보다 커야하지만 작으므로 정규분포 안함
ncvTest(fitL)
durbinWatsonTest(fitL)
crPlots(fitL)
influencePlot(fitL) #5006번 이상치


#추정값 만들기

predL <- predict(object = fitL, newdata = testSet, type = 'response')


errorL <- real - predL
errorL^2 %>% mean() %>% sqrt()
errorL^2 %>% abs() %>% mean()
(errorL/real) %>% abs() %>% mean()
#나무모형보다 성능 떨어짐(항상 선형모형이 좋은 것이 아니다.)

saveRDS(object = fitR, file = 'RegressionTree.RDS')


#회귀나무 알고리즘은 지역모형
install.packages('tree')
library(tree)

fitT <- tree(formula = MedianHouseValue ~ Longitude + Latitude,
             data = house)
decile <- quantile(x = house$MedianHouseValue,
                   probs = seq(from = 0, to = 1, by = 0.1))
decile

#grade <- cut(x = house$MedianHouseValue,
#             breaks = decile)
grade <- cut(x = house$MedianHouseValue,
             breaks = decile,
             include.lowest = TRUE,
             right = TRUE)
grade

#NA 확인
table(grade, useNA = 'ifany')
table(grade, useNA = 'always')


#지역모형 그리기
par(mfrow = c(1,1),
    family = 'AppleGothic')
cols <- grey(level = seq(from = 1, to = 0, by = -0.1))[grade]

plot(x = house$Longitude,
     y = house$Latitude,
     col = cols,
     pch = 20,
     xlab = '경도',
     ylab = '위도')


partition.tree(tree = fitT,
               ordvars = c('Longitude', 'Latitude'),
               col = 'red',
               cex = 0.8,
               font = 2,
               add = TRUE)







