library(httr)

data <- readRDS("data/messages.rds")

analyze_sentiment <- function(text) {
  prompt <- paste0(
    "Проанализируй тональность текста. В ответе укажи только одно слово из этих 3: POSITIVE, NEUTRAL, NEGATIVE.\n\n",
    "Текст: ", text, "\nОтвет:"
  )
  
  res <- POST(
    url = "http://localhost:11434/api/generate",
    body = list(
      model = "qwen2.5:7b-instruct",
      prompt = prompt,
      stream = FALSE
    ),
    encode = "json"
  )
  
  out <- content(res)
  result <- toupper(trimws(out$response))
  
  if (!result %in% c("POSITIVE", "NEUTRAL", "NEGATIVE")) {
    return("NEUTRAL")  # fallback
  }
  
  return(result)
}

# Добавляем вывод прогресса обработки
cat("Начинаем анализ тональности сообщений...\n")
total <- nrow(data)
pb <- txtProgressBar(min = 0, max = total, style = 3)

for(i in 1:total) {
  data$sentiment[i] <- analyze_sentiment(data$text[i])
  cat(sprintf("\rОбработано сообщение %d из %d. Тональность: %s", i, total, data$sentiment[i]))
  setTxtProgressBar(pb, i)
}

close(pb)
cat("\nАнализ тональности завершен.\n")

saveRDS(data, file = "data/messages.rds")
