#chapter01

#package呼び出し
.libPaths()
#packageをDLするpathを設定する
.libPaths("C:/06-R/seminar_R/library")
install.packages("dplyr")
install.packages("tidyr")
install.packages("ggplot2")
install.packages("readxl")
install.packages("tidyverse")
install.packages("here")
library(dplyr)
library(tidyr)
library(ggplot2)
library(readxl)
library(tidyverse)
library(here)
#Figure 1.1
output_path <- here::here("output/ch01")

women %>% 
  ggplot(aes(x = weight, 
             y = height)) +
  geom_point()
ggsave("Figure_1.1.png",path = output_path)


#Figure 1.2
View(USPersonalExpenditure)

US_long <- USPersonalExpenditure %>% 
  as_tibble(rownames = "item") %>% 
  pivot_longer(cols = `1940`:`1960`, 
               names_to  = "year",  
               values_to = "expenditure") 
View(US_long)

USPersonalExpenditure %>% 
  as_tibble(rownames = "item") %>% 
  pivot_longer(`1940`:`1960`, 
               names_to  = "year",  
               values_to = "expenditure") %>% 
  pivot_wider(names_from  = item, 
              values_from = expenditure) %>% 
  ggplot(aes(x = `Food and Tobacco`,
             y = `Medical and Health`)) +
  geom_point() +
  geom_line() +
  xlab("食料品およびタバコへの支出額") +
  ylab("医療および健康への支出総額") +
  theme_gray(base_family = "HiraKakuPro-W3")
ggsave("Figure_1.2.png",path = output_path)

#table 1.1
install.packages("kableExtra")
library(kableExtra)
USPersonalExpenditure %>% 
  kableExtra::kbl() %>% 
  kableExtra::kable_classic_2()

#Figure 1.3
usp <- USPersonalExpenditure %>% 
  as_tibble(rownames = "item") %>% 
  pivot_longer(`1940`:`1960`, 
               names_to  = "year",  
               values_to = "expenditure") %>% 
  pivot_wider(names_from  = item, 
              values_from = expenditure)

g1 <- usp %>% 
  ggplot(aes(x = `Food and Tobacco`,
             y = `Household Operation`)) +
  geom_point() +
  geom_line()

g2 <- usp %>% 
  ggplot(aes(x = `Medical and Health`,
             y = `Household Operation`)) +
  geom_point() +
  geom_line()

g3 <- usp %>% 
  ggplot(aes(x = `Private Education`,
             y = `Household Operation`)) +
  geom_point() +
  geom_line()

g4 <- usp %>% 
  ggplot(aes(x = `Food and Tobacco`,
             y = `Personal Care`)) +
  geom_point() +
  geom_line()

g5 <- usp %>% 
  ggplot(aes(x = `Medical and Health`,
             y = `Personal Care`)) +
  geom_point() +
  geom_line()

g6 <- usp %>% 
  ggplot(aes(x = `Private Education`,
             y = `Personal Care`)) +
  geom_point() +
  geom_line()

install.packages("patchwork")
library(patchwork)

g1 + g2 + g3 + 
  g4 + g5 + g6 + 
  plot_layout(ncol = 3)
ggsave("Figure_1.3.png",path = output_path)

#Figure 1.5
women %>%
  ggplot(aes(x = weight,
             y = height)) +
  geom_smooth(method = "lm", se = FALSE) +
  geom_point()
ggsave("Figure_1.5.png",path = output_path)
