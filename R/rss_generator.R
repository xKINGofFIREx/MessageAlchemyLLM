library(xml2)

data <- readRDS("data/messages.rds")

rss <- xml_new_root("rss", version = "2.0")
channel <- xml_add_child(rss, "channel")
xml_add_child(channel, "title", "Telegram Summaries")
xml_add_child(channel, "link", "http://example.com/")
xml_add_child(channel, "description", "Сводки и тональность")

for (i in 1:nrow(data)) {
  item <- xml_add_child(channel, "item")
  xml_add_child(item, "title", paste("Сообщение", data$id[i]))
  xml_add_child(item, "link", paste0("http://example.com/msg/", data$id[i]))
  xml_add_child(item, "description", paste(data$summary[i], "(Тональность:", data$sentiment[i], ")"))
  xml_add_child(item, "pubDate", format(data$date_parsed[i], "%a, %d %b %Y %H:%M:%S %z"))
}

write_xml(rss, "data/feed.xml")