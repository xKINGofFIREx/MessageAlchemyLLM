server_part2 <- function(input, output, session, filtered_data) {
  
  # ---- wordcloud --------------------------------------------------------
  output$wordcloud <- renderPlot({
    sel <- input$channel
    if (sel == "Все" || !(sel %in% names(json_files))) return(NULL)
    json_path <- json_files[[sel]]
    if (!file.exists(json_path)) return(NULL)
    
    messages <- fromJSON(json_path, flatten = TRUE)
    messages$date_only <- as.Date(substr(messages$date, 1, 10))
    messages <- messages[
      messages$date_only >= as.Date(input$dateRange[1]) &
        messages$date_only <= as.Date(input$dateRange[2]), ]
    messages <- messages[!is.na(messages$text) & messages$text != "", ]
    if (nrow(messages) == 0) return(NULL)
    
    txt <- paste(messages$text, collapse = " ")
    corp <- Corpus(VectorSource(txt)) |>
      tm_map(content_transformer(tolower)) |>
      tm_map(removePunctuation) |>
      tm_map(removeNumbers)
    stop_custom <- c(
      stopwords("russian"), stopwords("english"),
      "bankieconomy", "banki_economy", "bbbreaking", "economylive",
      "это","также","который","год","где","будет","может","свой","они","все")
    corp <- tm_map(corp, removeWords, stop_custom) |>
      tm_map(stripWhitespace)
    
    dtm <- TermDocumentMatrix(corp)
    m   <- as.matrix(dtm)
    freqs <- sort(rowSums(m), decreasing = TRUE)
    df <- data.frame(word = names(freqs), freq = freqs)
    df <- df[nchar(df$word) > 2, ]
    if (nrow(df) == 0) return(NULL)
    
    wordcloud(df$word, df$freq,
              min.freq = 2, max.words = 100,
              scale = c(4, 0.9),
              random.order = FALSE, rot.per = 0.3,
              colors = brewer.pal(8, "Dark2"))
  })
  
  # ---- value-box'ы ------------------------------------------------------
  output$box_total <- renderValueBox({
    df <- filtered_data()
    valueBox(nrow(df), "Сообщений за период",
             icon = icon("envelope"), color = "blue")
  })
  
  output$box_unique <- renderValueBox({
    df <- filtered_data()
    words <- unlist(strsplit(paste(df$text, collapse = " "), "\\s+"))
    valueBox(length(unique(words)), "Уникальных слов",
             icon = icon("font"), color = "green")
  })
  
  output$box_avgwords <- renderValueBox({
    df <- filtered_data()
    avg <- round(mean(sapply(strsplit(df$text, "\\s+"), length)), 1)
    valueBox(avg, "Среднее кол-во слов в сообщении",
             icon = icon("calculator"), color = "orange")
  })
  
  output$table <- DT::renderDataTable({
    df <- filtered_data()
    if (nrow(df) == 0) return(NULL)
    datatable(df[, c("date_parsed", "channel", "text", "sentiment")],
              escape = FALSE, rownames = FALSE,
              options = list(pageLength = 10)) |>
      formatStyle(
        "sentiment", target = "cell",
        backgroundColor = styleEqual(
          c("POSITIVE","NEGATIVE","NEUTRAL"),
          c("#d4f7d4","#ffe3e3","#ececec")),
        color = styleEqual(
          c("POSITIVE","NEGATIVE","NEUTRAL"),
          c("#219150","#c0392b","#7f8c8d")),
        fontWeight = "bold")
  })
  
  # ---- pie --------------------------------------------------------------
  output$pie_sentiment <- renderPlot({
    df <- filtered_data()
    if (nrow(df) == 0) return(NULL)
    pie_data <- df |> count(sentiment) |> mutate(frac = n / sum(n))
    ggplot(pie_data, aes("", frac, fill = sentiment)) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar("y") + theme_void() +
      scale_fill_manual(values = c(
        POSITIVE="#27ae60", NEGATIVE="#e74c3c", NEUTRAL="#95a5a6"),
        name = "Тональность")
  })
  
  # ---- топ-слова таблицей ----------------------------------------------
  output$top_words <- renderTable({
    sel <- input$channel
    if (sel == "Все" || !(sel %in% names(json_files))) return(NULL)
    json_path <- json_files[[sel]]
    if (!file.exists(json_path)) return(NULL)
    
    messages <- fromJSON(json_path, flatten = TRUE)
    messages$date_only <- as.Date(substr(messages$date, 1, 10))
    messages <- messages[
      messages$date_only >= as.Date(input$dateRange[1]) &
        messages$date_only <= as.Date(input$dateRange[2]), ]
    messages <- messages[!is.na(messages$text) & messages$text != "", ]
    if (nrow(messages) == 0) return(NULL)
    
    txt <- paste(messages$text, collapse = " ")
    corp <- Corpus(VectorSource(txt)) |>
      tm_map(content_transformer(tolower)) |>
      tm_map(removePunctuation) |>
      tm_map(removeNumbers) |>
      tm_map(removeWords, stop_custom) |>
      tm_map(stripWhitespace)
    
    dtm <- TermDocumentMatrix(corp)
    freqs <- sort(rowSums(as.matrix(dtm)), decreasing = TRUE)
    df <- data.frame(word = names(freqs), freq = freqs)
    df <- df[nchar(df$word) > 2, ]
    head(df, 10)
  })
  
  # ---- waffle -----------------------------------------------------------
  output$waffle_unique <- renderPlot({
    req(input$channel == "Все")
    word_counts <- data |>
      group_by(channel) |>
      summarise(unique_words = length(
        unique(unlist(strsplit(paste(text, collapse = " "), "\\s+"))))) |>
      arrange(desc(unique_words))
    
    vals <- word_counts$unique_words
    names(vals) <- word_counts$channel
    waffle(round(vals / 100),
           rows = 5,
           title = "Уникальные слова по каналам (1 блок = 100 слов)",
           colors = c("#3778C2", "#E84A5F", "#43A047"),
           xlab = "1 блок = 100 уникальных слов")
  })
}

# -------- основной server + запуск -------------------------------------
server <- function(input, output, session) {
  
  # общий reactive, доступный обеим частям
  filtered_data <- reactive({
    res <- data
    if (input$channel != "Все")
      res <- res[res$channel == input$channel, ]
    res <- res[
      res$date_only >= as.Date(input$dateRange[1]) &
        res$date_only <= as.Date(input$dateRange[2]), ]
    res
  })
  
  server_part1(input, output, session, filtered_data)
  server_part2(input, output, session, filtered_data)
}
