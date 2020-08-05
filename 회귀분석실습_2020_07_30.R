library(tidyverse)

univ <- read.csv(file = 'http://bit.ly/university_admit')
glimpse(univ)
summary(univ)

#admit, rank를 명목형으로 바꿔줍니다.
f <- c('admit', 'rank')
univ[, f] <- map_df(univ[, f], as.factor)
glimpse(univ)
summary(univ)

#목표변수의 비율 확인
univ$admit %>% table() %>% prop.table()  #불균형 데이터


#입력변수와의 상관성 탐색
boxplot(formula = gre ~ admit,
        data = univ)
abline(h = mean(univ$gre),
       col = 'red',
       lty = 2)

boxplot(formula = gpa ~ admit,
        data = univ)
abline(h = mean(univ$gpa),
       col = 'red',
       lty = 2)                 #그래프 상으로만 봤을 때는 어느 정도 차이가 있어보임

#등급에 따른 합격 비율(등급이 합격률에 영향을 준다.)
table(univ$admit, univ$rank) %>% prop.table(margin = 2)


#데이터 분할하기
set.seed(1234)
n <- nrow(univ)
index <- sample(n, n * 0.7, FALSE)
trainSet <- univ %>% slice(index)
testSet <- univ %>% slice(-index)

table(trainSet$admit) %>% prop.table()
table(testSet$admit) %>% prop.table()



#이항 로지스틱 회귀분석 함수 
fitC <- glm(formula = admit ~ .,
            data = trainSet,
            family = binomial(link = 'logit'))
summary(fitC)


#회귀모형의 유의성 검정(카이제곱 검정-단측검정)
pchisq(q = fitC$null.deviance - fitC$deviance,
       df = fitC$df.null - fitC$df.residual,
       lower.tail = FALSE)

result <- summary(object = fitC)
coefs <- result$coefficients
colnames(coefs) <- c('coef', 'se', 'z-stats', 'p-value')
coefs

#회귀모형의 유의성 검정을 통과 못했다고 변수를 버리지는 않습니다.


#각 입력변수의 오즈비를 출력합니다.
fitC$coefficients %>% exp()

#표준화 회귀계수 확인
library(reghelper)
beta(fitC)


#목표변수의 추정확률 생성
probC <- predict(fitC, testSet, type = 'response')
probC

predC <- ifelse(probC >= 0.5, yes = '1', no = '0')
predC <- as.factor(predC)
predC

real <- testSet$admit
real

library(caret)
confusionMatrix(data = predC, reference = real, positive = '1')

library(MLmetrics)
F1_Score(real, predC, positive = '1')

library(pROC)
roc(real, probC) %>% 
  plot(main = 'ROC 곡선')

auc(real, probC)




#분리기준점(cut-off)의 설정
install.packages('mccr')
library(mccr)
mccr(act = real, pred = predC)

#mcc가 최대가 되는 cut-off를 찾아라
cuts <- seq(from = 0.01, to = 1.00, by = 0.01)

mccs <- c()
for (cut in cuts) {
  pred <- ifelse(probC >= cut, yes = '1', no = '0')
  pred <- factor(x = pred, levels = c(0, 1)) 
  mcc <- mccr(act = real, pred = pred)
  mccs <- c(mccs, mcc)
}

mccs


plot(x = cuts, y = mccs, type = '1')
abline(h = max(mccs), col = 'red', lwd = 2)
locs <- which(x = mccs == max(mccs))
cuts[locs]



#불균형된 데이터로 추정한 결과
boxplot(formula = probC ~ real,
        main = 'Box Plot')
abline(h = 0.5,
       col = 'red',
       lty = 2)
rate <- table(real) %>% prop.table()
rate[2]





