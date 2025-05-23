library(httr)
library(dplyr)
library(lubridate)
library(stringr)

# Загружаем данные
data <- readRDS("data/messages.rds")

# Функция для разделения текста на чанки (не более 1500 слов)
split_text_into_chunks <- function(text, max_words = 1500) {
  if (is.null(text) || text == "") return(list())
  
  words <- strsplit(text, "\\s+")[[1]]
  n_words <- length(words)
  
  if (n_words <= max_words) {
    return(list(text))
  }
  
  chunks <- list()
  current_chunk <- 1
  
  for (i in seq(1, n_words, max_words)) {
    end_idx <- min(i + max_words - 1, n_words)
    chunk_text <- paste(words[i:end_idx], collapse = " ")
    chunks[[current_chunk]] <- chunk_text
    current_chunk <- current_chunk + 1
  }
  
  return(chunks)
}

# Функция для проверки на китайский текст
contains_chinese <- function(text) {
  if (is.null(text) || text == "") return(FALSE)
  return(str_detect(text, "[\u4e00-\u9fff]"))
}

# Функция для суммаризации одного чанка
summarize_chunk <- function(text, max_attempts = 3) {
  if (is.null(text) || text == "") return("")
  
  for (attempt in 1:max_attempts) {
    # Базовый промпт
    if (attempt == 1) {
      prompt <- paste0(
        "<system>\n",
        "Ты — русскоязычный журналист. Пиши только по-русски.\n",
        "</system>\n",
        "<user>\n",
        "Сделай краткую сводку (3–4 предложения) текста ниже. Ответь строго на русском.\n\n",
        "<<<\n",
        text,
        "\n>>>\n",
        "</user>"
      )
    } else {
      # Более жесткий промпт для повторных попыток
      prompt <- paste0(
        "<system>\n",
        "Ты — русскоязычный журналист. Категорически запрещено использовать любые языки кроме русского. Даже одно китайское слово недопустимо.\n",
        "</system>\n",
        "<user>\n",
        "Сделай краткую сводку (3–4 предложения) текста ниже. Ответь СТРОГО НА РУССКОМ ЯЗЫКЕ, без единого иностранного слова.\n\n",
        "<<<\n",
        text,
        "\n>>>\n",
        "</user>\n",
        "Ты ранее вернул китайский текст. Перепиши ответ полностью по-русски."
      )
    }
    
    res <- POST(
      url = "http://localhost:11434/api/generate",
      body = list(
        model = "qwen2.5:7b-instruct",
        prompt = prompt,
        stream = FALSE,
        temperature = 0.3,
        top_p = 0.9,
        max_tokens = 256,
        presence_penalty = 0,
        frequency_penalty = 0
      ),
      encode = "json"
    )
    
    out <- content(res)
    
    if (is.null(out$response)) {
      warning("Ошибка при обработке чанка текста")
      return("")
    }
    
    response_text <- trimws(out$response)
    
    # Проверка на китайский текст
    if (!contains_chinese(response_text)) {
      return(response_text) # Возвращаем ответ, если в нем нет китайских иероглифов
    }
    
    # Если это последняя попытка и все равно китайский текст
    if (attempt == max_attempts) {
      warning("После нескольких попыток все еще получаем китайский текст. Возвращаем пустую строку.")
      return("Резюме недоступно из-за языковых ограничений.")
    }
    
    # Иначе продолжаем попытки
    warning(paste("Попытка", attempt, "вернула китайский текст. Повторяем запрос."))
  }
}

# Функция для создания финального дайджеста по всем сводкам за день
create_daily_digest <- function(summaries, date, max_attempts = 3) {
  if (length(summaries) == 0) return("")
  
  # Объединяем все сводки
  all_summaries <- paste(summaries, collapse = "\n")
  
  # Форматируем дату
  formatted_date <- format(date, "%d.%m.%Y")
  
  for (attempt in 1:max_attempts) {
    # Базовый промпт
    if (attempt == 1) {
      prompt <- paste0(
        "<system>\n",
        "Ты — русскоязычный журналист. Пиши только по-русски.\n",
        "</system>\n",
        "<user>\n",
        "Ниже краткие сводки постов канала за ", formatted_date, ". Напиши итоговый дайджест в 3–5 предложениях, строго на русском языке.\n\n",
        "<<<\n",
        all_summaries,
        "\n>>>\n",
        "</user>"
      )
    } else {
      # Более жесткий промпт для повторных попыток
      prompt <- paste0(
        "<system>\n",
        "Ты — русскоязычный журналист. Категорически запрещено использовать любые языки кроме русского. Даже одно китайское слово недопустимо.\n",
        "</system>\n",
        "<user>\n",
        "Ниже краткие сводки постов канала за ", formatted_date, ". Напиши итоговый дайджест в 3–5 предложениях, СТРОГО НА РУССКОМ ЯЗЫКЕ, без единого иностранного слова.\n\n",
        "<<<\n",
        all_summaries,
        "\n>>>\n",
        "</user>\n",
        "Ты ранее вернул китайский текст. Перепиши ответ полностью по-русски."
      )
    }
    
    res <- POST(
      url = "http://localhost:11434/api/generate",
      body = list(
        model = "qwen2.5:7b-instruct",
        prompt = prompt,
        stream = FALSE,
        temperature = 0.3,
        top_p = 0.9,
        max_tokens = 256,
        presence_penalty = 0,
        frequency_penalty = 0
      ),
      encode = "json"
    )
    
    out <- content(res)
    
    if (is.null(out$response)) {
      warning(paste("Ошибка при создании дайджеста за", formatted_date))
      return("")
    }
    
    response_text <- trimws(out$response)
    
    # Проверка на китайский текст
    if (!contains_chinese(response_text)) {
      return(response_text) # Возвращаем ответ, если в нем нет китайских иероглифов
    }
    
    # Если это последняя попытка и все равно китайский текст
    if (attempt == max_attempts) {
      warning(paste("После нескольких попыток все еще получаем китайский текст для дайджеста за", formatted_date))
      return("Дайджест недоступен из-за языковых ограничений.")
    }
    
    # Иначе продолжаем попытки
    warning(paste("Попытка", attempt, "для дайджеста за", formatted_date, "вернула китайский текст. Повторяем запрос."))
  }
}

# Создаем структуру для хранения результатов суммаризации
summarization_results <- list()

# Группируем сообщения по каналу и дате
data <- data %>% 
  mutate(date_only = as.Date(date))

# Получаем уникальные комбинации канал-дата
unique_combinations <- data %>%
  select(channel, date_only) %>%
  distinct()

# Добавляем вывод прогресса обработки
cat("Начинаем суммаризацию сообщений...\n")
total_combinations <- nrow(unique_combinations)
pb <- txtProgressBar(min = 0, max = total_combinations, style = 3)

# Обрабатываем каждую комбинацию канал-дата
for (i in 1:total_combinations) {
  channel_name <- unique_combinations$channel[i]
  current_date <- unique_combinations$date_only[i]
  
  # Получаем все сообщения для текущего канала и даты
  channel_date_messages <- data %>%
    filter(channel == channel_name, date_only == current_date)
  
  # Объединяем все сообщения в один текст
  combined_text <- paste(channel_date_messages$text, collapse = "\n\n")
  
  # Разбиваем текст на чанки
  chunks <- split_text_into_chunks(combined_text)
  
  # Суммаризируем каждый чанк
  chunk_summaries <- list()
  for (j in 1:length(chunks)) {
    cat(sprintf("\rОбработка канала '%s', дата %s: чанк %d из %d", 
               channel_name, current_date, j, length(chunks)))
    chunk_summaries[[j]] <- summarize_chunk(chunks[[j]])
  }
  
  # Создаем финальный дайджест для текущего дня
  daily_digest <- create_daily_digest(unlist(chunk_summaries), current_date)
  
  # Сохраняем результат
  if (!channel_name %in% names(summarization_results)) {
    summarization_results[[channel_name]] <- list()
  }
  summarization_results[[channel_name]][[as.character(current_date)]] <- list(
    chunks = chunks,
    chunk_summaries = chunk_summaries,
    daily_digest = daily_digest
  )
  
  setTxtProgressBar(pb, i)
}

close(pb)
cat("\nСуммаризация завершена.\n")

# Добавляем результаты в датафрейм
data <- data %>%
  group_by(channel, date_only) %>%
  mutate(daily_digest = sapply(date_only, function(d) {
    chan <- channel[1]
    date_str <- as.character(d)
    if (chan %in% names(summarization_results) && 
        date_str %in% names(summarization_results[[chan]])) {
      return(summarization_results[[chan]][[date_str]]$daily_digest)
    } else {
      return(NA)
    }
  }))

# Сохраняем результаты
saveRDS(data, file = "data/messages.rds")
saveRDS(summarization_results, file = "data/summaries.rds")

cat("Результаты сохранены в data/messages.rds и data/summaries.rds\n")
