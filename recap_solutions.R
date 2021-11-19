library(tidyverse)

# Exercise I --------------------------------------------------------------

# 1  
measurements <- read_csv("https://education.rstudio.com/blog/2020/08/more-example-exams/measurements.csv")

# 2
cleaned <- measurements %>% drop_na

# 3
cleaned %>% 
  group_by(quantity) %>%
  count

# 4
cleaned %>% 
  group_by(quantity) %>%
  summarise(min = min(reading),
            max = max(reading))

#5

corrected1 <- cleaned %>%
  mutate(reading = case_when(quantity == "sal" & reading > 1 ~ reading / 100,
                             !(quantity == "sal" & reading > 1) ~ reading))

corrected2 <- cleaned %>%
  mutate(reading = ifelse(quantity == "sal" & reading > 1, reading / 100, reading))

