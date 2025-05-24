library(xml2)

summaries <- readRDS("data/summaries.rds")

rss <- xml_new_root("rss", version = "2.0")
channel <- xml_add_child(rss, "channel")
xml_add_child(channel, "title", "Telegram Daily Summaries")
xml_add_child(channel, "link", "http://t.me/")
xml_add_child(channel, "description", "Сводки Telegram-каналов по дням (автоматически сгенерировано)")

for (ch in names(summaries)) {
  for (date_str in names(summaries[[ch]])) {
    digest <- summaries[[ch]][[date_str]]$daily_digest
    pubdate <- as.POSIXct(date_str, tz = "Europe/Moscow")
    item <- xml_add_child(channel, "item")
    xml_add_child(item, "title", paste0("Дайджест ", ch, " за ", date_str))
    xml_add_child(item, "link", paste0("http://t.me/", ch, "/", date_str))
    xml_add_child(item, "description", digest)
    xml_add_child(item, "pubDate", format(pubdate, "%a, %d %b %Y 00:00:00 +0300"))
  }
}

write_xml(rss, "data/feed.xml")
cat("RSS-файл сохранён в data/feed.xml\n")
