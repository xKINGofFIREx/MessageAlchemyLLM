library(httr)

token <- Sys.getenv("HF_TOKEN")
headers <- add_headers(Authorization = paste("Bearer", token))
url <- "https://api-inference.huggingface.co/models/cointegrated/rubert-tiny-sentiment-balanced"

data <- readRDS("data/messages.rds")

summarize_text <- function(text) {
  if (is.null(text) || text == "") return("")
  res <- POST(url, headers, body = list(inputs = text), encode = "json")
  out <- content(res)
  if (!is.null(out$error)) {
    warning(paste("Ошибка при обработке текста:", text))
    return("")
  }
  if (!is.null(out[[1]]$summary_text)) {
    return(out[[1]]$summary_text)
  } else {
    return("")
  }
}

data$summary <- sapply(data$text, summarize_text)

saveRDS(data, file = "data/messages.rds")
