# Concatenates the four input CSVs into a single dataset

library(tidyverse)
library(lubridate)

dat_raw <- bind_rows(
  read_csv("1. ALL cannabis.csv") %>%
    mutate(topic = "cannabis",
           retweets = TRUE,
           Date = as.Date(Date, format = "%m/%d/%Y")),
  
  read_csv("2. Cannabis - Unique.csv") %>%
    mutate(topic = "cannabis",
           retweets = FALSE,
           Date = as.Date(Date, format = "%m/%d/%Y")),

  read_csv("3. ALL Referendum.csv") %>%
    mutate(topic = "referendum",
           retweets = TRUE,
           Date = as.Date(Date, format = "%m/%d/%Y")),

  read_csv("4. Unique Referendum.csv") %>%
    mutate(topic = "referendum",
           retweets = FALSE,
           Date = as.Date(Date, format = "%m/%d/%Y"))
  ) %>%
  rename(date = Date) %>%
  gather(valence, count, 2:4) 

bind_rows(
  dat_raw %>%
    mutate(
      rollup = "year",
      date = floor_date(date, "year")
      ) %>%
    group_by(rollup, date, topic, retweets, valence) %>%
    summarise(count = sum(count)),
  dat_raw %>%
    mutate(
      rollup = "month",
      date = floor_date(date, "month")
    ) %>%
    group_by(rollup, date, topic, retweets, valence) %>%
    summarise(count = sum(count)),
  dat_raw %>%
    mutate(
      rollup = "day"
    ) %>%
    group_by(rollup, date, topic, retweets, valence) %>%
    summarise(count = sum(count))
  ) %>%
  group_by(rollup, date, topic, retweets) %>%
  mutate(total = sum(count),
         proportion = ifelse(total > 0, count / total, 0)) %>%
  select(-total) %>%
  gather(measure, value, 6:7) %>%
  write_csv("cannabis_sentiment/cannabis_data.csv")
