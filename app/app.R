library(shiny)
library(ggplot2)
library(dplyr)
library(lubridate)
library(jsonlite)
library(wordcloud)
library(tm)
library(RColorBrewer)
library(shinydashboard)
library(shinyWidgets)
library(DT)
library(waffle)


# Абсолютный путь к корню проекта
root <- normalizePath("..")

# Загружаем данные
data <- readRDS(file.path(root, "data", "messages.rds"))

# JSON-файлы по каналам
json_files <- list(
  "banki_economy" = file.path(root, "data", "telegram_messages_banki_economy.json"),
  "bbbreaking"    = file.path(root, "data", "telegram_messages_bbbreaking.json"),
  "economylive"   = file.path(root, "data", "telegram_messages_economylive.json")
)

# Загружаем сводки, если они есть
summaries_file <- file.path(root, "data", "summaries.rds")
if (file.exists(summaries_file)) {
  summaries <- readRDS(summaries_file)
} else {
  summaries <- list()
}

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      body.dark-mode, body.dark-mode .container-fluid, body.dark-mode .main-panel, body.dark-mode .tab-content, body.dark-mode .well, body.dark-mode .panel, body.dark-mode .form-control, body.dark-mode .selectize-input, body.dark-mode .table, body.dark-mode .tab-pane {
        background-color: #1a1c1f !important; color: #eee !important; border-color: #333 !important;
      }
      body.dark-mode .panel-heading, body.dark-mode .panel-title, body.dark-mode .value-box, body.dark-mode .shiny-output-error, body.dark-mode label, body.dark-mode .control-label, body.dark-mode .nav-tabs>li>a {
        color: #eee !important;
      }
      body.dark-mode .well, body.dark-mode .panel, body.dark-mode .box, body.dark-mode .form-group {
        background-color: #23262b !important; color: #eee !important;
      }
      body.dark-mode .form-control, body.dark-mode .selectize-input {
        background-color: #333 !important; color: #eee !important; border-color: #444 !important;
      }
      body.dark-mode .btn, body.dark-mode .btn-default, body.dark-mode .btn-primary {
        background-color: #2c2f34 !important; color: #eee !important; border-color: #444 !important;
      }
      body.dark-mode .table>tbody>tr>td, body.dark-mode .table>thead>tr>th {
        background-color: #222 !important; color: #eee !important;
      }
      body.dark-mode .nav-tabs>li.active>a, body.dark-mode .nav-tabs>li.active>a:focus, body.dark-mode .nav-tabs>li.active>a:hover {
        background-color: #23262b !important; color: #eee !important; border-color: #444 !important;
      }
      body.dark-mode .value-box {
        background-color: #2c2f34 !important; color: #eee !important; border-color: #222 !important;
      }
      body.dark-mode .sidebar {
        background-color: #18191a !important;
      }
    ")),
    tags$script(HTML("
      Shiny.addCustomMessageHandler('setDarkMode', function(enabled) {
        if (enabled) {
          document.body.classList.add('dark-mode');
        } else {
          document.body.classList.remove('dark-mode');
        }
      });
    "))
  ),
  titlePanel("Анализ Telegram-сообщений"),
  sidebarLayout(
    sidebarPanel(
      prettySwitch(
        inputId = "dark_mode", 
        label = "Тёмная тема", 
        status = "primary", 
        fill = TRUE, 
        bigger = TRUE,      # делает крупнее
        inline = FALSE
      ),
      selectInput("channel", "Выберите канал:",
                  choices = c("Все", unique(data$channel)),
                  selected = "Все"),
      dateRangeInput("dateRange", "Выберите период:",
                     start = min(as.Date(data$date)), 
                     end = max(as.Date(data$date))),
      downloadButton("rss", "Скачать RSS")
    ),
    mainPanel(
      fluidRow(
        valueBoxOutput("box_total"),
        valueBoxOutput("box_unique"),
        valueBoxOutput("box_avgwords")
      ),
      tabsetPanel(
        tabPanel("Дайджесты", uiOutput("digests_ui")),
        tabPanel("Сообщения", DT::dataTableOutput("table")),
        tabPanel("Активность", plotOutput("activity")),
        tabPanel("Тональность", plotOutput("tone")),
        tabPanel("Облако слов",
                 fluidRow(
                   column(8, plotOutput("wordcloud")),
                   column(4, tableOutput("top_words"))
                 )
        ),
        tabPanel("Визуализация",
                 plotOutput("pie_sentiment"),
                 plotOutput("waffle_unique")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  # Тёмная тема
  observe({
    session$sendCustomMessage("setDarkMode", input$dark_mode)
  })
  
  # Фильтр по каналу и датам
  filtered_data <- reactive({
    result <- data
    if (input$channel != "Все") {
      result <- result[result$channel == input$channel, ]
    }
    result <- result[
      result$date_only >= as.Date(input$dateRange[1]) &
        result$date_only <= as.Date(input$dateRange[2]),
    ]
    result
  })
  
  output$table <- renderTable({
    df <- filtered_data()
    if (nrow(df) == 0) return(NULL)
    head(df[, c("date_parsed", "channel", "text", "sentiment")], 20)
  })
  
  output$activity <- renderPlot({
    df <- filtered_data()
    if (nrow(df) == 0) return(NULL)
    ggplot(df, aes(date_only)) +
      geom_histogram(binwidth = 1, fill = "steelblue") +
      labs(x = "Дата", y = "Число сообщений")
  })
  
  output$tone <- renderPlot({
    df <- filtered_data()
    if (nrow(df) == 0) return(NULL)
    df %>%
      count(date = date_only, sentiment) %>%
      group_by(date) %>%
      mutate(frac = n / sum(n)) %>%
      ggplot(aes(date, frac, color = sentiment)) +
      geom_line() +
      labs(x = "Дата", y = "Доля")
  })
  
  output$digests_ui <- renderUI({
    filtered <- filtered_data()
    selected_channel <- input$channel
    if (nrow(filtered) == 0) {
      return(h3("Нет данных для выбранного периода"))
    }
    if (selected_channel == "Все") {
      return(h3("Пожалуйста, выберите конкретный канал для просмотра дайджестов"))
    }
    if (!(selected_channel %in% names(summaries))) {
      return(h3("Дайджесты для выбранного канала отсутствуют. Запустите скрипт summarization.R"))
    }
    channel_data <- filtered %>%
      filter(channel == selected_channel) %>%
      arrange(desc(date_only))
    unique_dates <- unique(channel_data$date_only)
    daily_panels <- lapply(unique_dates, function(date) {
      date_str <- as.character(date)
      formatted_date <- format(date, "%d.%m.%Y")
      if (date_str %in% names(summaries[[selected_channel]])) {
        digest <- summaries[[selected_channel]][[date_str]]$daily_digest
        div(
          style = "margin-bottom: 20px; padding: 15px; border: 1px solid #ddd; border-radius: 5px;",
          h3(paste0("Дайджест за ", formatted_date)),
          p(digest)
        )
      } else {
        div(
          style = "margin-bottom: 20px; padding: 15px; border: 1px solid #ddd; border-radius: 5px;",
          h3(paste0("Дата: ", formatted_date)),
          p("Дайджест отсутствует")
        )
      }
    })
    do.call(tagList, daily_panels)
  })
  
  output$rss <- downloadHandler(
    filename = function() { "feed.xml" },
    content = function(file) {
      file.copy(file.path(root, "data", "feed.xml"), file)
    }
  )
  
  output$wordcloud <- renderPlot({
    selected_channel <- input$channel
    if (selected_channel == "Все" || !(selected_channel %in% names(json_files))) {
      return(NULL)
    }
    json_path <- json_files[[selected_channel]]
    if (!file.exists(json_path)) return(NULL)
    messages <- fromJSON(json_path, flatten = TRUE)
    messages$date_only <- as.Date(substr(messages$date, 1, 10))
    messages <- messages[
      messages$date_only >= as.Date(input$dateRange[1]) &
        messages$date_only <= as.Date(input$dateRange[2]),
    ]
    messages <- messages[!is.na(messages$text) & messages$text != "", ]
    if (nrow(messages) == 0) return(NULL)
    text_all <- paste(messages$text, collapse = " ")
    corpus <- Corpus(VectorSource(text_all))
    corpus <- tm_map(corpus, content_transformer(tolower))
    corpus <- tm_map(corpus, removePunctuation)
    corpus <- tm_map(corpus, removeNumbers)
    custom_stopwords <- c(
      stopwords("russian"),
      stopwords("english"),
      "bankieconomy", "banki_economy", "bbbreaking", "economylive",
      "это", "также", "который", "год", "где", "будет", "может", "свой", "они", "все"
    )
    corpus <- tm_map(corpus, removeWords, custom_stopwords)
    corpus <- tm_map(corpus, stripWhitespace)
    dtm <- TermDocumentMatrix(corpus)
    m <- as.matrix(dtm)
    word_freqs <- sort(rowSums(m), decreasing = TRUE)
    df <- data.frame(word = names(word_freqs), freq = word_freqs)
    df <- df[nchar(df$word) > 2, ]
    if (nrow(df) == 0) return(NULL)
    wordcloud(
      words = df$word, freq = df$freq,
      min.freq = 2, max.words = 100,
      scale = c(4, 0.9),
      random.order = FALSE, rot.per = 0.3,
      colors = brewer.pal(8, "Dark2")
    )
  })
  
  output$box_total <- renderValueBox({
    df <- filtered_data()
    valueBox(
      value = nrow(df),
      subtitle = "Сообщений за период",
      icon = icon("envelope"),
      color = "blue"
    )
  })
  
  output$box_unique <- renderValueBox({
    df <- filtered_data()
    words <- unlist(strsplit(paste(df$text, collapse=" "), "\\s+"))
    valueBox(
      value = length(unique(words)),
      subtitle = "Уникальных слов",
      icon = icon("font"),
      color = "green"
    )
  })
  
  output$box_avgwords <- renderValueBox({
    df <- filtered_data()
    avg_words <- round(mean(sapply(strsplit(df$text, "\\s+"), length)), 1)
    valueBox(
      value = avg_words,
      subtitle = "Среднее кол-во слов в сообщении",
      icon = icon("calculator"),
      color = "orange"
    )
  })
  output$table <- DT::renderDataTable({
    df <- filtered_data()
    if (nrow(df) == 0) return(NULL)
    datatable(
      df[, c("date_parsed", "channel", "text", "sentiment")],
      escape = FALSE,  # чтобы HTML работал
      rownames = FALSE,
      options = list(pageLength = 10)
    ) %>%
      formatStyle(
        'sentiment',
        target = 'cell',
        backgroundColor = styleEqual(
          c("POSITIVE", "NEGATIVE", "NEUTRAL"),
          c("#d4f7d4", "#ffe3e3", "#ececec")  # зелёный, красный, серый
        ),
        color = styleEqual(
          c("POSITIVE", "NEGATIVE", "NEUTRAL"),
          c("#219150", "#c0392b", "#7f8c8d")
        ),
        fontWeight = 'bold'
      )
  })
  output$pie_sentiment <- renderPlot({
    df <- filtered_data()
    if (nrow(df) == 0) return(NULL)
    pie_data <- df %>%
      count(sentiment) %>%
      mutate(frac = n / sum(n))
    ggplot(pie_data, aes(x = "", y = frac, fill = sentiment)) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar("y") +
      theme_void() +
      scale_fill_manual(
        values = c(POSITIVE="#27ae60", NEGATIVE="#e74c3c", NEUTRAL="#95a5a6"),
        name = "Тональность"
      )
  })
  output$top_words <- renderTable({
    selected_channel <- input$channel
    if (selected_channel == "Все" || !(selected_channel %in% names(json_files))) return(NULL)
    json_path <- json_files[[selected_channel]]
    if (!file.exists(json_path)) return(NULL)
    messages <- fromJSON(json_path, flatten = TRUE)
    messages$date_only <- as.Date(substr(messages$date, 1, 10))
    messages <- messages[
      messages$date_only >= as.Date(input$dateRange[1]) &
        messages$date_only <= as.Date(input$dateRange[2]),
    ]
    messages <- messages[!is.na(messages$text) & messages$text != "", ]
    if (nrow(messages) == 0) return(NULL)
    text_all <- paste(messages$text, collapse = " ")
    corpus <- Corpus(VectorSource(text_all))
    corpus <- tm_map(corpus, content_transformer(tolower))
    corpus <- tm_map(corpus, removePunctuation)
    corpus <- tm_map(corpus, removeNumbers)
    custom_stopwords <- c(
      stopwords("russian"), stopwords("english"),
      "bankieconomy", "banki_economy", "bbbreaking", "economylive",
      "это", "также", "который", "год", "где", "будет", "может", "свой", "они", "все"
    )
    corpus <- tm_map(corpus, removeWords, custom_stopwords)
    corpus <- tm_map(corpus, stripWhitespace)
    dtm <- TermDocumentMatrix(corpus)
    m <- as.matrix(dtm)
    word_freqs <- sort(rowSums(m), decreasing = TRUE)
    df <- data.frame(word = names(word_freqs), freq = word_freqs)
    df <- df[nchar(df$word) > 2, ]
    head(df, 10)
  })
  output$waffle_unique <- renderPlot({
    req(input$channel == "Все")  # Только если выбраны все каналы
    # Считаем уникальные слова по каждому каналу
    word_counts <- data %>%
      group_by(channel) %>%
      summarise(
        unique_words = length(unique(unlist(strsplit(paste(text, collapse = " "), "\\s+"))))
      ) %>%
      arrange(desc(unique_words))
    
    # Формируем named vector для waffle
    vals <- word_counts$unique_words
    names(vals) <- word_counts$channel
    
    # Вычисляем единицу размера (например, 1 блок = 100 слов, подбери под свой масштаб)
    size_unit <- 100
    vals_waffle <- round(vals / size_unit)
    
    waffle::waffle(
      vals_waffle,
      rows = 5,  # или auto
      title = "Уникальные слова по каналам (1 блок = 100 слов)",
      colors = c("#3778C2", "#E84A5F", "#43A047"),
      xlab = "1 блок = 100 уникальных слов"
    )
  })
}

shinyApp(ui, server)
