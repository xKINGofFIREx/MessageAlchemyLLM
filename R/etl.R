library(jsonlite)
library(dplyr)
library(stringr)
library(lubridate)

data <- fromJSON("data/telegram_messages.json")

data_clean <- data %>%
  mutate(
    text = as.character(text),
    text = str_replace_all(text, "<.*?>", ""),
    text = str_replace_all(text, "[^\x20-\x7Fа-яА-ЯёЁ0-9 .,!?-]", ""),
    text = str_squish(text),
    date_parsed = ymd_hms(date)
  ) %>%
  filter(text != "")

saveRDS(data_clean, file = "data/messages.rds")