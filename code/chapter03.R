#practice set
ice_path <- here::here("data/data/data/chap03/icecream.csv")
icedata <- read.csv(ice_path)

tmp_path <- here::here("data/data/data/chap03/temperature.csv")
tmpdata <- read.csv(tmp_path)


#3.2
mean_all_tmp <- mean(tmpdata$temp)

head_100_tmp <- head(tmpdata,100)
mean_head_tmp <- mean(head_100_tmp$temp)

#無作為抽出は復元？被復元？
random_tmp <- sample(tmpdata,size=100,replace=TRUE)
random_tmp
mean_random <- mean(random_tmp$temp)
#頭から抜きだした場合、日付順にソートされている為、日にちと温度の相関が存在し、似た温度が抽出される

#3.3
max_index <- which.max(icedata$icecream)
icedata$city[max_index]

plot(icedata$icecream,icedata$income)
cor(icedata$icecream,icedata$income)


plot(icedata$icecream,icedata$u15)
cor(icedata$icecream,icedata$u15)
#icecreamの消費とincomeの方が相関が強い。しかし、相関は因果と異なるため、交絡因子を確認する必要がある
#子供の多い地方は、都市部でなく地方である。地方には、あまりアイスが流通していない可能性もある。(日本はそんなことないが)
#3.4

x <-rnorm(1000,50,10)
y <-rnorm(1000,50,10)

#10は-3σ以上だから、実現確率は1パーセント未満
#P(X>10)=ΣI{x>10}P(x>10)
mean(x>10)

mean(x>-10)-mean(x>10)

mean(x>y**2)

#3.5

pop_mean <-50
s2 <- function(x){ var(x)*(length(x)-1)/length(x) }
cnt <- 0
S <-10000
for(i in 1:S){
  n<-1000
  x <-rnorm(n,50,10)
  bar_x <-mean(x)
  sample_var<- s2(x)
  sample_deviation <- sqrt(sample_var)
  CI_lower <-mean(x)-1.65*sample_deviation/sqrt(n)
  CI_upper <- mean(x)+1.65*sample_deviation/sqrt(n)
  if(CI_upper>50 && CI_lower<50){cnt <- cnt+1}
}
cnt


set.seed(2022)
coin <- c("Head", "Tail")
sample(coin, 100, replace = TRUE)

factorial(100) /
  (factorial(50) ^ 2) / 
  (2 ^ 100)

coin <- c(1, 0)
z <- sample(coin, 100,
            replace = TRUE)
sum(z)


a=100
a
S <- 100000
rec <- numeric(S)
coin <- c(1, 0)

#100回コイン投げる」を10000回行う
for(i in 1:S){
  z <- sample(coin, 100, replace = TRUE)
  rec[i] <- sum(z)
  
}
#10000回の標本の結果を表示する
hist(rec)

summary(rec)

2 > 1
2 > 1000
200 == 100 * 2
TRUE + TRUE
#recのうち50/100で表が出ているものをカウントする
count <- (rec == 50)
head(count)
#0,1なので、E(x)=p
mean(count)

# 独立性の確認
#ZはX,Yによる決定的関数
S <- 100000 # シミュレーション回数
X <- rnorm(S, 0, 10) # Xを抽出
Y <- rnorm(S, 0, 10) # Yを抽出
Z <- X + Y # Zを構成

# Pr(X > 5) * Pr(Z > 1)
#ここは中身が論理演算氏なのでインディケーター関数の期待値E(I(x>5))を求めているこれは、確率密度に一致する
mean(X > 5) * mean(Z > 10)

# Pr(X > 5 かつ Z > 10)
mean((X > 5) * (Z > 10))

1.41**2+(-0.82)**2+0.58**2


X <- rnorm(100000, 50, 10)
Y <- rnorm(100000, 50, 10)
cor(X, Y)
#ZはXの決定的関数なので、非独立だが、
#相関はしていない(線形的関係はないので、無相関)
#このときCov(x,y)がないので回帰係数がでてこない
Z <- - ((X - 50) ^ 2) / 10
cor(X, Z)
plot(X, Z)


curve(pnorm(x, 50, 10), 0, 100)
pnorm(60, 50, 10) - pnorm(40, 50, 10)


#3.3.7
library(here)
wage_path <- here::here("data/data/data/chap03/wage.csv")
malesdata <- read.csv(wage_path)
head(malesdata)
summary(malesdata)
plot(malesdata)

sch12 <- malesdata[malesdata$school == 12, ] # 高校卒業者の抜き出し
sch16 <- malesdata[malesdata$school == 16, ] # 学部卒業者の抜き出し


# 教科書と異なり指数変換している、この変換によって結果がNaNになっている
exp(mean(sch12$wage)) # 高校卒業者の平均賃金
exp(mean(sch16$wage)) # 学部卒業者の平均賃金

sch11 <- malesdata[malesdata$school <= 11, ] # 高卒未満の抜き出し
sch12 <- malesdata[malesdata$school >= 12, ] # 高卒以上の抜き出し

exp(mean(sch11$wage)) # 高卒未満の平均賃金
exp(mean(sch12$wage)) # 高卒以上の平均賃金


#CLT
S <- 10000
n <- 10000
Zn <- numeric(S)

for(i in 1:S){
  X <- rnorm(n, 50, 10)
  Xbar <- mean(X)
  Sn <- var(X)
  Zn[i] <- sqrt(n) * (Xbar  - 50) / sqrt(Sn)
  
}
hist(Zn)


# 信頼区間シミュレーション

S <- 10000         # シミュレーション回数
n <- 10000         # 標本の大きさ
rec <- numeric(S)  # 結果記録用のベクトル

for(i in 1:S){     # 繰り返し開始
  X <- rnorm(n, 50, 10)  # N(50,10^2) から標本抽出
  Xbar <- mean(X)
  Sn <- var(X)
  rec[i] <- (Xbar - 1.96 * sqrt(Sn / n) < 50) * (50 < Xbar + 1.96 * sqrt(Sn / n))
  
}
mean(rec) # 確率を計算

pnorm(69.6,50,10) - pnorm(30.4,50,10)

