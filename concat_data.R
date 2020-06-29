# Concatenates the four input CSVs into a single dataset

library(tidyverse)

bind_rows(
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
           retweets = FALSE)
  ) %>%
  gather(valence, count, 2:4) %>%
  write_csv("cannabis_sentiment/cannabis_data.csv")
