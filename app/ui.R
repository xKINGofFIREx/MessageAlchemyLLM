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

# --- данные -------------------------------------------------------------
root <- normalizePath("..")

data <- readRDS(file.path(root, "data", "messages.rds"))

json_files <- list(
  banki_economy = file.path(root, "data", "telegram_messages_banki_economy.json"),
  bbbreaking    = file.path(root, "data", "telegram_messages_bbbreaking.json"),
  economylive   = file.path(root, "data", "telegram_messages_economylive.json")
)

summaries_file <- file.path(root, "data", "summaries.rds")
summaries <- if (file.exists(summaries_file)) readRDS(summaries_file) else list()

stop_custom <- c(
  stopwords("russian"), stopwords("english"),
  "bankieconomy", "banki_economy", "bbbreaking", "economylive",
  "это", "также", "который", "год", "где", "будет", "может", "свой", "они", "все"
)
# --- UI -----------------------------------------------------------------
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
        background-color: #23262b !important; color: #eee !important; border-color: #333 !important;
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
      prettySwitch("dark_mode", "Тёмная тема", status = "primary",
                   fill = TRUE, bigger = TRUE),
      selectInput("channel", "Выберите канал:",
                  choices = c("Все", unique(data$channel)), selected = "Все"),
      dateRangeInput("dateRange", "Выберите период:",
                     start = min(as.Date(data$date)),
                     end   = max(as.Date(data$date))),
      downloadButton("rss", "Скачать RSS")
    ),
    mainPanel(
      fluidRow(
        valueBoxOutput("box_total"),
        valueBoxOutput("box_unique"),
        valueBoxOutput("box_avgwords")
      ),
      tabsetPanel(
        tabPanel("Дайджесты",  uiOutput("digests_ui")),
        tabPanel("Сообщения",  DT::dataTableOutput("table")),
        tabPanel("Активность", plotOutput("activity")),
        tabPanel("Тональность", plotOutput("tone")),
        tabPanel("Облако слов",
                 fluidRow(
                   column(8, plotOutput("wordcloud")),
                   column(4, tableOutput("top_words"))
                 )),
        tabPanel("Визуализация",
                 plotOutput("pie_sentiment"),
                 plotOutput("waffle_unique"))
      )
    )
  )
)

# ---- первая половина server как отдельная функция ----------------------
server_part1 <- function(input, output, session, filtered_data) {
  
  # переключатель тёмной темы
  observe({
    session$sendCustomMessage("setDarkMode", input$dark_mode)
  })

  output$table <- renderTable({
    df <- filtered_data()
    if (nrow(df) == 0) return(NULL)
    head(df[, c("date_parsed", "channel", "text", "sentiment")], 20)
  })
  
  # ---- активность ------------------------------------------------------
  output$activity <- renderPlot({
    df <- filtered_data()
    if (nrow(df) == 0) return(NULL)
    ggplot(df, aes(date_only)) +
      geom_histogram(binwidth = 1, fill = "steelblue") +
      labs(x = "Дата", y = "Число сообщений")
  })
  
  # ---- динамика тональности -------------------------------------------
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
  
  # ---- дайджесты --------------------------------------------------------
  output$digests_ui <- renderUI({
    filtered <- filtered_data()
    sel <- input$channel
    if (nrow(filtered) == 0)
      return(h3("Нет данных для выбранного периода"))
    if (sel == "Все")
      return(h3("Пожалуйста, выберите конкретный канал для просмотра дайджестов"))
    if (!(sel %in% names(summaries)))
      return(h3("Дайджесты для выбранного канала отсутствуют. Запустите скрипт summarization.R"))
    
    channel_data <- filtered %>%
      filter(channel == sel) %>%
      arrange(desc(date_only))
    
    unique_dates <- unique(channel_data$date_only)
    panels <- lapply(unique_dates, function(d) {
      d_str <- as.character(d)
      pretty <- format(d, "%d.%m.%Y")
      if (d_str %in% names(summaries[[sel]])) {
        digest <- summaries[[sel]][[d_str]]$daily_digest
        div(style = "margin-bottom:20px; padding:15px; border:1px solid #ddd; border-radius:5px;",
            h3(paste0("Дайджест за ", pretty)), p(digest))
      } else {
        div(style = "margin-bottom:20px; padding:15px; border:1px solid #ddd; border-radius:5px;",
            h3(paste0("Дата: ", pretty)), p("Дайджест отсутствует"))
      }
    })
    do.call(tagList, panels)
  })
  
  # ---- RSS --------------------------------------------------------------
  output$rss <- downloadHandler(
    filename = function() "feed.xml",
    content  = function(file) {
      file.copy(file.path(root, "data", "feed.xml"), file)
    }
  )
}
