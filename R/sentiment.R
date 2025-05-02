library(httr)

token <- Sys.getenv("HF_TOKEN")
headers <- add_headers(Authorization = paste("Bearer", token))
url <- "https://api-inference.huggingface.co/models/blanchefort/rubert-base-cased-sentiment"

data <- readRDS("data/messages.rds")

analyze_sentiment <- function(text) {
  res <- POST(url, headers, body = list(inputs = text), encode = "json")
  out <- content(res)
  if (!is.null(out$error)) return("NEUTRAL")
  best <- which.max(sapply(out, function(x) x$score))
  out[[best]]$label
}

data$sentiment <- sapply(data$text, analyze_sentiment)
saveRDS(data, file = "data/messages.rds")