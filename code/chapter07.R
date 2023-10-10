library(dplyr)


head(ToothGrowth)

library(broom)
ToothGrowth%>%
  lm(len~dose, data=.)%>%
  tidy()

set.seed(2022)

n <- 200 # サンプルサイズ
b0 <- 1  # 切片の真の値
b1 <- 2  # 係数の真の値

#xが誤差eに相関するシミュレータ
e <- rnorm(n)                    # 誤差項
X <- (1 + 0.5 * e) * runif(n) # 説明変数
Y <- b0 + b1 * X + e             # 被説明変数
lm(Y ~ X)$coefficient



n <- 200 # サンプルサイズ
b0 <- 1  # 切片の真の値
b1 <- 2  # 係数の真の値
e <-rnorm(n)
x <-rnorm(n)
u <-runif(n, -1, 1)
w<-x+u #測定誤差
Y<-b0+b1*x+e
lm(Y~x)
lm(Y~w)
