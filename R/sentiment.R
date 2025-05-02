library(httr)

token <- Sys.getenv("HF_TOKEN")
headers <- add_headers(Authorization = paste("Bearer", token))
url <- "https://api-inference.huggingface.co/models/cointegrated/rubert-tiny-sentiment-balanced"

data <- readRDS("data/messages.rds")

analyze_sentiment <- function(text) {
  res <- POST(url, headers, body = list(inputs = text), encode = "json")
  out <- content(res)
  if (!is.null(out$error)) return("NEUTRAL")
  if (length(out) > 0) {
    scores <- sapply(out[[1]], function(x) x$score)
    best <- which.max(scores)
    return(out[[1]][[best]]$label)
  } else {
    return("NEUTRAL")
  }
}

data$sentiment <- sapply(data$text, analyze_sentiment)
saveRDS(data, file = "data/messages.rds")
