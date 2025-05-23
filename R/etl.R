library(jsonlite)
library(dplyr)
library(stringr)
library(lubridate)

# Найдем все файлы telegram_messages в папке data
json_files <- list.files("data", pattern = "telegram_messages_.*\\.json$", full.names = TRUE)

# Если нет файлов, выходим с сообщением об ошибке
if (length(json_files) == 0) {
  stop("Не найдены файлы telegram_messages_*.json в папке data")
}

# Функция для обработки одного файла
process_file <- function(file_path) {
  # Извлекаем имя канала из имени файла
  channel_name <- str_match(file_path, "telegram_messages_(.+)\\.json$")[1,2]
  
  # Загружаем данные из JSON
  data <- fromJSON(file_path)
  
  # Обрабатываем данные и добавляем столбец channel
  data_clean <- data %>%
    mutate(
      text = as.character(text),
      text = str_replace_all(text, "<.*?>", ""),
      text = str_replace_all(text, "[^\x20-\x7Fа-яА-ЯёЁ0-9 .,!?-]", ""),
      text = str_squish(text),
      date_parsed = ymd_hms(date),
      channel = channel_name
    ) %>%
    filter(text != "")
  
  return(data_clean)
}

# Обрабатываем все файлы и объединяем результаты
all_data <- lapply(json_files, process_file) %>%
  bind_rows()

# Сохраняем результат
saveRDS(all_data, file = "data/messages.rds")

# Вывод информации
message(sprintf("Обработано %d файлов", length(json_files)))
message(sprintf("Всего сообщений: %d", nrow(all_data)))
message(sprintf("Каналы: %s", paste(unique(all_data$channel), collapse = ", ")))