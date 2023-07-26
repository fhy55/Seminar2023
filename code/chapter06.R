library(dplyr)
library(ggplot2)


video_path <- here::here("data/data/data/chap06/video_game.csv")
videodata <- readr::read_csv(video_path)


videodata %>% 
  ggplot(aes(x = hours, y = grade)) +
  geom_point()
videodata %>% 
  summarise(cor = cor(grade, hours))  
crimedata <- readr::read_csv("police_crime.csv")

crime_path <- here::here("data/data/data/chap06/police_crime.csv")
crimedata <- readr::read_csv(crime_path)

crimedata %>% 
  ggplot(aes(x = police, y = crime)) +
  geom_point()
crimedata %>% 
  summarise(cor = cor(police, crime))

#6.2
set.seed(2022)

n  <- 400
D  <- rbinom(n, 1, 0.6)  # 確率 0.6 で 1，確率 0.4 で 0 をとるトリートメント変数
TE <- 2                  # トリートメント効果 = 2 で個人間の差は無いと想定
#トリートメントダミーの切片無回帰が、トリートメント効果
Y  <- TE * D + rnorm(n)  # 結果変数

EY1 <- sum(D * Y) / sum(D)
EY0 <- sum((1 - D) * Y) / sum(1 - D)
EY1 - EY0
#重要:一様分布に従ってランダムにトリートメント変数D=0 or 1を割り当てる
Z <- runif(n)        # D と Y に共通して影響を与える変数
D <- rbinom(n, 1, Z) # Z が大きいほど 1 をとりやすいトリートメント変数
Y <- TE * D + (2 * Z - 1) + rnorm(n)

EY1 <- sum(D * Y) / sum(D)
EY0 <- sum((1 - D) * Y) / sum(1 - D)
EY1 - EY0
