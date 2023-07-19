#package
library(dplyr)
library(lubridate)
library(broom)

install.packages("Rcpp")
library(Rcpp)

# problem sets
# 5.1
ice_path <- here::here("data/data/data/chap04/icecream.csv")
icedata <- read.csv(ice_path)
View(icedata)
ice_reg <- lm(icedata$icecream~icedata$income+icedata$u15,data=icedata)
summary(ice_reg)

betahat <- summary(ice_reg)$coefficients[, 1]
sigma <- summary(ice_reg)$coefficients[, 2]

lower <- betahat - 1.96 * sigma
upper <- betahat + 1.96 * sigma
cbind(lower, upper)

ice_reg %>% confint(level = 0.95)

#5.2
temp_path <- here::here("data/data/data/chap04/temperature_aug.csv")
tempdata <- read.csv(temp_path)

View(tempdata)
tempdata <- tempdata%>%
  mutate( morning = 1*(time>=6)&
            (time<=12),
          afternoon = 1*(time>=13)&
            (time<=18),
          saturday = 1*((date=="2014/8/2")|
                          (date=="2014/8/9")|
                          (date=="2014/8/16")|
                          (date=="2014/8/23")|
                          (date=="2014/8/30"))
  )


tempdata$daytime <- 
  (tempdata$time >= 9) & (tempdata$time <= 18)

lm(elec ~ temp + daytime ,
   data = tempdata)%>%
  summary()
elec_reg<-lm(elec ~ temp + morning +afternoon + saturday,
   data = tempdata)


betahat <- summary(elec_reg)$coefficients[, 1]
sigma <- summary(elec_reg)$coefficients[, 2]

lower <- betahat - 1.96 * sigma
upper <- betahat + 1.96 * sigma
cbind(lower, upper)

elec_reg %>% confint(level = 0.95)


#5.3
x <- rnorm(1000, 0, 1)
y <- 1+5*x+rnorm(1000, 0, 1)
reg <- lm(y~x)
summary_reg <- summary(reg)
beta_hat <- summary_reg$coefficients[2, 1]
sigma <- summary_reg$coefficients[2, 2]
bool_CI <- (5>=beta_hat-1.96*sigma)&(5<=beta_hat+1.96*sigma)

bool_CI <- vector()
for(i in 1:1000){
  x <- rnorm(1000, 0, 1)
  y <- 1+5*x+rnorm(1000, 0, 1)
  reg <- lm(y~x)
  summary_reg <- summary(reg)
  beta_hat <- summary_reg$coefficients[2, 1]
  sigma <- summary_reg$coefficients[2, 2]
  bool_CI[i] <- (5>=beta_hat-1.96*sigma)&(5<=beta_hat+1.96*sigma)
}
sum(bool_CI)/10000


#2つの仮説の下での分布の重なり
p40 <- pbinom(q = 40, size = 100, prob = 0.5)
p60 <- pbinom(q = 60, size = 100, prob = 0.5)
p60 - p40

#平均値の検定
simdata <- readr::read_csv("distributions.csv")
simdata %>% 
  summarise(mean_A = mean(distA),
            var_A  = var(distA))

#t valueの計算
simdata %>% 
  summarise(mean_A  = mean(distA),
            var_A   = var(distA),
            t.value = sqrt(100) / sqrt(var_A) * mean_A)

#P値　すなわち、棄却域の確率
1 - pnorm(1.250113) + pnorm(-1.250113)

#beta係数のシミュレーション
set.seed(2022)
X <- rnorm(1000, 0, 1)
Y <- 1 + 5 * X + rnorm(1000, 0, 1)
beta1 <- lm(Y ~ X)$coefficients

names(lm(Y ~ X))

#10000回　β係数の計算を繰り返す
S <- 10000
beta1 <- numeric(S) # 結果の保存
for(i in 1:S){      # 繰り返し開始
  
  x <- rnorm(1000, 0, 1)
  y <- 1 + 5 * x + rnorm(1000, 0, 1)
  beta1[i] <- lm(y ~ x)$coefficients[2]
  
}                   # 繰り返し終了

summary(beta1)      # 結果の要約
sd(beta1)           # 標準偏差
hist(beta1)         # 結果の描画


wagedata <- readr::read_csv("wage.csv")

wagedata %>% 
  lm(log(wage) ~ educ + exper,
     data = .) %>% 
  summary()

#confidence interval
result <- wagedata %>% 
  lm(log(wage) ~ educ + exper,
     data = .)

summary(result)$coefficients
#結果の行列式から値を取ってくる
summary(result)$coefficients[, 1:2]

betahat <- summary(result)$coefficients[, 1]
sigma <- summary(result)$coefficients[, 2]

lower <- betahat - 1.96 * sigma
upper <- betahat + 1.96 * sigma
cbind(lower, upper)

result %>% confint(level = 0.99)

