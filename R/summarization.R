library(httr)

data <- readRDS("data/messages.rds")

summarize_text <- function(text) {
  if (is.null(text) || text == "") return("")
  
  prompt <- paste0(
    "Напиши краткое резюме следующего текста в одно предложение:\n\n",
    text
  )
  
  res <- POST(
    url = "http://localhost:11434/api/generate",
    body = list(
      model = "qwen2.5:7b",
      prompt = prompt,
      stream = FALSE
    ),
    encode = "json"
  )
  
  out <- content(res)
  
  if (is.null(out$response)) {
    warning(paste("Ошибка при обработке текста:", text))
    return("")
  }
  
  return(trimws(out$response))
}

# Добавляем вывод прогресса обработки
cat("Начинаем создание резюме сообщений...\n")
total <- nrow(data)
pb <- txtProgressBar(min = 0, max = total, style = 3)

for(i in 1:total) {
  data$summary[i] <- summarize_text(data$text[i])
  cat(sprintf("\rОбработано сообщение %d из %d", i, total))
  setTxtProgressBar(pb, i)
}

close(pb)
cat("\nСоздание резюме завершено.\n")

saveRDS(data, file = "data/messages.rds")
