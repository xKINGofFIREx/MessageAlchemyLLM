library(httr)

token <- Sys.getenv("HF_TOKEN")
headers <- add_headers(Authorization = paste("Bearer", token))
url <- "https://api-inference.huggingface.co/models/facebook/bart-large-cnn"

data <- readRDS("data/messages.rds")

summarize_text <- function(text) {
  res <- POST(url, headers, body = list(inputs = text), encode = "json")
  out <- content(res)
  if (!is.null(out$error)) return("")
  out[[1]]$summary_text
}

data$summary <- sapply(data$text, summarize_text)
saveRDS(data, file = "data/messages.rds")