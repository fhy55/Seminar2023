library(readr)
library(ggplot2)
library(dplyr)
library(lubridate)


#problem set

#4.1
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
lm(elec ~ temp + morning +afternoon + saturday,
   data = tempdata)


#4.2
ice_path <- here::here("data/data/data/chap04/icecream.csv")
icedata <- read.csv(ice_path)
View(icedata)

result_lm <-lm(icecream~income,icedata)
result_lm

ggplot(icedata,aes(income,icecream))+
  geom_point()+
  xlab("annual_income")+
  ylab("icecream_consumption")+
  geom_abline(intercept = result_lm$coefficients[1],
              slope = result_lm$coefficients[2])

lm(income~icecream,icedata)%>%
  summary()

lm(u15~icecream,icedata)



#4.3
wage_path <- here::here("data/data/data/chap04/wage.csv")
wagedata <- read.csv(wage_path)

result_lm <- lm(wage~educ,wagedata)
lm(wage~educ,wagedata)%>%
  summary()
result_lm
ggplot(wagedata,aes(educ,wage))+
  geom_point()+
  xlab("education")+
  ylab("wage")+
  geom_abline(intercept = result_lm$coefficients[1],
              slope = result_lm$coefficients[2])
lm(wage~educ+exper,wagedata)%>%
  summary()
#教育年数の効果は上昇している。これは、経験年数を一定にした時には、教育の効果が大きいことを示す
#一方で、経験年数を一定にしないときに、教育年数の賃金への効果は小さいことが考えられる。
#教育年数の分だけバイアスが働いている。脱落変数バイアス

#4.1
tempdata <- read_csv("temperature_aug.csv")

ggplot(tempdata, aes(x = time, y = temp)) +
  geom_point() +
  xlab("時刻") +
  ylab("気温") +
  theme_gray(base_family = "HiraKakuPro-W3")

tempdata


tempdata %>% 
  summarise(mean_temp = mean(temp))

tempdata %>% 
  summarise(mean_temp = mean(temp),
            sd_temp   = sd(temp),
            max_temp  = max(temp),
            min_temp  = min(temp))



tempdata %>%
  group_by(time) %>% 
  summarise(mean_temp = mean(temp))

tempdata %>%
  group_by(time) %>%
  summarise(mean_temp = mean(temp)) %>%
  ggplot(aes(x = time, y = mean_temp)) +
  geom_line() +
  xlab("時間") +
  ylab("気温") + 
  theme_gray(base_family = "HiraKakuPro-W3")

tempdata %>% 
  ggplot(aes(x = time, y = temp)) +
  stat_summary(geom = "line", fun = "mean") +
  xlab("時間") + 
  ylab("気温") +
  theme_gray(base_family = "HiraKakuPro-W3")

tempdata %>% 
  ggplot(aes(x = time, y = temp)) +
  stat_summary(geom = "line", fun = "mean") +
  xlab("時間") + 
  ylab("気温") +
  theme_gray(base_family = "HiraKakuPro-W3")


tempdata %>% 
  ggplot(aes(x = time, y = temp)) +
  geom_point() +
  stat_summary(geom = "line", fun = "mean") +
  xlab("時間") + 
  ylab("気温") +
  coord_cartesian(ylim = c(20, 30)) +
  theme_gray(base_family = "HiraKakuPro-W3")

#4.2
with(tempdata, cor(temp, elec))

tempdata %>% 
  summarise(cor(temp, elec))

tempdata %>% 
  with(cor(temp, elec))

tempdata %>% 
  ggplot(aes(x = temp, y = elec)) +
  stat_summary(geom = "line", fun = "mean") + 
  xlab("電気使用量の回帰曲線") +
  ylab("電気使用量（万kw）") +
  theme_gray() +
  theme_gray(base_family = "HiraKakuPro-W3")

lm(elec ~ temp,
   data = tempdata)

result <- lm(elec ~ temp,
             data = tempdata)

result$coefficients

tempdata %>% 
  ggplot(aes(x = temp, y = elec)) +
  geom_point() +
  geom_abline(intercept = result$coefficients[1],
              slope     = result$coefficients[2]) +
  xlab("気温") +
  ylab("電気使用量（万kw）") +
  theme_gray(base_family = "HiraKakuPro-W3")

tempdata %>% 
  ggplot(aes(x = temp, y = elec)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  xlab("気温") +
  ylab("電気使用量（万kw）") +
  theme_gray(base_family = "HiraKakuPro-W3")

#4.3
tempdata %>% 
  ggplot(aes(x = time, y = elec)) +
  geom_point() +
  xlab("時刻") +
  ylab("電気使用量（万kw）") +
  theme_gray(base_family = "HiraKakuPro-W3")

tempdata$daytime <- 
  (tempdata$time >= 9) & (tempdata$time <= 18)

tempdata <- tempdata %>% 
  mutate(daytime = 1 * (9 <= time & time <= 18),
         elec100 = elec / 100,
         time12  = time %% 12,
         ampm    = ifelse(time < 12, "a.m.", "p.m."))

lm(elec ~ temp + daytime,
   data = tempdata)

lm(elec ~ temp + daytime + prec,
   data = tempdata)

tempdata %>%
  mutate(sunday = 1 * 
           (date == "2014/8/3")  |
           (date == "2014/8/10") |
           (date == "2014/8/17") |
           (date == "2014/8/24") |
           (date == "2014/8/31"))

tempdata <- tempdata %>% 
  mutate(date = ymd(date))
tempdata

wday(tempdata$date, label = TRUE, locale = "ja_JP")

tempdata <- tempdata %>% 
  mutate(dow    = wday(date, label = TRUE, locale = "ja_JP"),
         sunday = 1 * (dow == "日"))

tempdata <- tempdata %>% 
  mutate(recess = 1 * ("2014-08-11" <= date & 
                         date <= "2014-08-16"))

lm(elec ~ temp + daytime + prec + sunday + recess,
   data = tempdata)

179.07 + 113.48 * 28 + 563.53 * 1 + 14.27 * 0 - 448.39 * 0 - 438.23 * 0

result <- lm(elec ~ temp + daytime + prec + sunday + recess,
             data = tempdata)
sum(result$coefficients * c(1, 28, 1, 0, 0, 0))

#4.4
lm(elec ~ temp + daytime + prec + sunday + recess,
   data = tempdata) %>% 
  summary()

lm(elec ~ temp,
   data = tempdata) %>% 
  summary()

#4.4.3
set.seed(2022)

a <- sample(1:6, 15, replace = TRUE)
b <- sample(1:6, 15, replace = TRUE)
c <- sample(1:6, 15, replace = TRUE)
d <- sample(1:6, 15, replace = TRUE)
e <- sample(1:6, 15, replace = TRUE)
f <- sample(1:6, 15, replace = TRUE)
g <- sample(1:6, 15, replace = TRUE)

summary(lm(a ~ b + c + d + e + f + g))

h <- sample(1:6, 15, replace = TRUE)

lm(a ~ b + c + d + e + f + g + h) %>% summary()

#補足
tempdata <- readr::read_csv("temperature_aug.csv")

result <- lm(elec ~ temp,
             data = tempdata)

ehat <- result$residuals

sum(ehat)

sum(tempdata$temp * ehat)




