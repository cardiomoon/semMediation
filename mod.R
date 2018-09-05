
# 사용 패키지
library(car)
library(jtools)
library(lavaan)
library(QuantPsyc)
library(lavaanPlot)

# setwd("E:/mod")
rawData<-read.csv("moddata1.csv")
names(rawData)
newData <- rawData

# 평균중심화
newData$centered.x <- scale(rawData$x, center =T, scale = T)
newData$centered.mod <- scale(rawData$mod, center =T, scale = T)
newData$centered.xmod <- newData$centered.x*newData$centered.mod

# 평균중심화 하지 않은 조절효과
mod1 <- lm(y ~ x + mod + x:mod, newData)

ggiraphExtra::ggPredict(mod1)
# 평균중심화 한 조절효과
mod2 <- lm(y ~ centered.x + centered.mod + centered.x:centered.mod, newData)
ggiraphExtra::ggPredict(mod2)
# 두개의 모형 비교
anova(mod1)
anova(mod2)

# 평균중심화로 다중공선성 변화 확인
vif(mod1)
vif(mod2)

# QuantPsyc 패키지를 이용한 조절효과 분석
lm.mod1 <- moderate.lm(x, mod, y, newData, mc = T)
summary(lm.mod1)


# jtools 패키지를 이용한 조절효과 분석 및 그래프
summ(mod1)
summ(mod1, center = T)
interact_plot(mod1, pred = "x", modx = "mod")

# lavaan에서의 조절효과(조절변수가 연속형인 경우)
sem.mod1 <- '
y ~ x + mod + centered.xmod
'
fit <- sem(sem.mod1, newData)
summary(fit, std=T, fit=T)

# lavaanPlot
labels <- list(x = "교육기간", mod = "학급인원수", centeredxmod = "교육기간X학급인원수", y = "월수입")

cat(makeDiagram(fit))
lavaanPlot(model=fit)
semDiagram(fit)
lavaanPlot(model=fit, labels = labels, coef =T, stand = T, stars = "regress",cov=T)
semDiagram(fit,labels = labels,nodeOptions=list(width=2),whatLabels="std")
