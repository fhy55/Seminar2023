library(readr)
library(ggplot2)
library(dplyr)
library(lubridate)

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




