library(tidyverse)

install.packages('mnormt')
library(psych)

getwd()
setwd(dir ="/Users/useonsong/Nano Degree/DATA")
list.files(pattern = 'RDS')
df <- readRDS("Toyota.RDS")

#column명과 인덱스를 간략하게 보여준다. (dplyr에 있음-structure와 같은 기능)
glimpse(df)

#기술통계량을 한꺼번에 보여준다.
describe(df)
#n은 결측값이 없는 갯수를 나타낸다.
#trimmed는 절사 평균(양 극단의 10%를 잘라낸 평균)
#se(standard error) : 표본평균의 표준오차


set.seed(1234)
index <- sample(nrow(df),
                nrow(df)*0.7,
                FALSE)
index

trainSet <- df %>% slice(index)
testSet <- df %>% slice(-index)
#fancy indexing 사용

#평균값 거의 비슷
trainSet$Price %>% mean()
testSet$Price %>% mean()


#모형만들기
fit1 <- lm(formula = Price ~ Age,
           data = trainSet)
summary(fit1)

#창 분할해줘야 한꺼번에 볼 수 있음(2행 2열로)
par(mfrow = c(2,2))
plot(fit1)

par(mfrow = c(1,1))
hist(fit1$residuals, freq = FALSE)
lines(density(fit1$residuals), 
      col = 'red', 
      lwd = 2)
shapiro.test(fit1$residuals)
#p-value가 0.05보다 작으므로 귀무가설 기각 -> 정규분포 하지 않음

install.packages('car')
library(car)

ncvTest(model = fit1)
durbinWatsonTest(fit1)
crPlots(fit1)
influencePlot(fit1)




#선형성 검증에서 분홍색 실선이 'U'자형 곡선으로 그려지면 2차항을 추가(비추)
#I는 asis함수
fit2 <- lm(formula = Price ~ Age
           + I(Age^2),
           data = trainSet)
summary(fit2)
#결과를 보면 다중공선성 문제가 발생한다.

crPlots(fit2)
#직선으로 변한 것을 볼 수 있음

#분산팽창지수(다중공선성)를 확인
vif(mod = fit2)
#두 입력변수 모두 10을 초과하므로 다중공선성 문제가 있는 것이다.
#따라서 기술적으로 끼워맞출 수는 있어도 분석적으로 문제가 있는 모형이다.


#2차항을 편차제곱으로 바꾸면 다중공선성 문제도 해결된다.(이것도 비추)
avg <- mean(trainSet$Age)
fit3 <- lm(formula = Price ~ Age
           + I((Age-avg)^2),
           data = trainSet)
summary(fit3)
crPlots(fit3)
vif(fit3)
#분산팽창지수가 10을 넘는 변수가 없다.
#그냥 다른 입력 변수를 추가하는 것이 최선의 방법!!

#불필요한 객체 삭제
rm(fit2, fit3)


#추정값 산출
real <- testSet$Price
pred1 <- predict(fit1,
                 newdata = testSet,
                 type = 'response')
error1 <- real - pred1


#회귀모형 성능 측정
#MSE
error1^2 %>% mean()

#RMSE
error1^2 %>% mean() %>% sqrt()

#MAE
error1 %>% abs() %>% mean()

#MAPE
(error1/real) %>% abs() %>% mean()


x <- 'Age'
y <- 'Price'
str_glue('{y}-{x}')
parse(text = str_glue('{y}-{x}'))
eval()

#사용자 정의 함수 만들기
crossvalidation <- function(x, y, data, k = 5, seed = 1234) {
  set.seed(seed)
  index <- sample(1:k, nrow(data), TRUE)
  formula <- str_glue('{y}-{x}') %>% parse(text = .) %>% eval()
  errors <- c()
  for (i in 1:k) {
    train <- data %>% filter(index != i)
    valid <- data %>% filter(index == i)
    fit <- lm(formula = formula, data = train)
    real <- str_glue('valid${y}') %>% parse(text = .) %>% eval()
    pred <- predict(object = fit, newdata = valid, type = 'response')
    (real - pred)^2 %>% mean() %>% sqrt()
  }
  return(mean(errors))
}
crossvalidation(x = 'Age', y = 'Price', data = trainSet)
crossvalidation(x = 'KM', y = 'Price', data = trainSet)


#다중선형회귀분석
full <- lm(formula = Price ~., data = trainSet)
null <- lm(formula = Price ~ 1, data = trainSet)

fit2 <- step(object = null,
             scope = list(lower = null, upper = full),
             direction = 'both')

summary(fit2)

#잔차검정
par(mfrow = c(2, 2))
plot(fit2)
par(mfrow = c(1, 1))

shapiro.test(x = fit2$residuals) #정규성
ncvTest(fit2) #등분산
durbinWatsonTest(fit2) #독립성
crPlots(fit2) #선형성
influencePlot(fit2) #이상치









