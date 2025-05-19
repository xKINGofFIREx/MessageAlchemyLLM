library(shiny)
library(ggplot2)
library(dplyr)
library(lubridate)

# Загружаем данные
data <- readRDS("../data/messages.rds")

# Загружаем сводки, если они есть
summaries_file <- "../data/summaries.rds"
if (file.exists(summaries_file)) {
  summaries <- readRDS(summaries_file)
} else {
  summaries <- list()
}

ui <- fluidPage(
  titlePanel("Анализ Telegram-сообщений"),
  sidebarLayout(
    sidebarPanel(
      selectInput("channel", "Выберите канал:",
                  choices = c("Все", unique(data$channel)),
                  selected = "Все"),
      dateRangeInput("dateRange", "Выберите период:",
                  start = min(as.Date(data$date)), 
                  end = max(as.Date(data$date))),
      downloadButton("rss", "Скачать RSS")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Дайджесты", 
                 uiOutput("digests_ui")),
        tabPanel("Сообщения", 
                 tableOutput("table")),
        tabPanel("Активность", 
                 plotOutput("activity")),
        tabPanel("Тональность", 
                 plotOutput("tone"))
      )
    )
  )
)

server <- function(input, output) {
  
  # Реактивное выражение для фильтрации данных по каналу и датам
  filtered_data <- reactive({
    result <- data
    
    if(input$channel != "Все") {
      result <- result %>% filter(channel == input$channel)
    }
    
    result <- result %>% 
      filter(as.Date(date) >= input$dateRange[1] & 
             as.Date(date) <= input$dateRange[2])
    
    return(result)
  })
  
  # Отображение ежедневных дайджестов
  output$digests_ui <- renderUI({
    filtered <- filtered_data()
    
    if (nrow(filtered) == 0) {
      return(h3("Нет данных для выбранного периода"))
    }
    
    # Получаем канал
    selected_channel <- input$channel
    if (selected_channel == "Все") {
      return(h3("Пожалуйста, выберите конкретный канал для просмотра дайджестов"))
    }
    
    # Проверяем есть ли дайджесты для выбранного канала
    if (!(selected_channel %in% names(summaries))) {
      return(h3("Дайджесты для выбранного канала отсутствуют. Запустите скрипт summarization.R для их создания."))
    }
    
    # Получаем уникальные даты для выбранного периода
    channel_data <- filtered %>%
      filter(channel == selected_channel) %>%
      mutate(date_only = as.Date(date)) %>%
      arrange(desc(date_only))
    
    unique_dates <- unique(channel_data$date_only)
    
    # Создаем панели для каждого дня
    daily_panels <- lapply(unique_dates, function(date) {
      date_str <- as.character(date)
      
      # Проверяем, есть ли дайджест для этой даты
      if (date_str %in% names(summaries[[selected_channel]])) {
        digest <- summaries[[selected_channel]][[date_str]]$daily_digest
        
        # Форматируем текст дайджеста
        formatted_date <- format(date, "%d.%m.%Y")
        
        # Оборачиваем в карточку
        div(
          style = "margin-bottom: 20px; padding: 15px; border: 1px solid #ddd; border-radius: 5px;",
          h3(paste0("Дайджест за ", formatted_date)),
          p(digest)
        )
      } else {
        # Если нет дайджеста, показываем сообщение
        div(
          style = "margin-bottom: 20px; padding: 15px; border: 1px solid #ddd; border-radius: 5px;",
          h3(paste0("Дата: ", format(date, "%d.%m.%Y"))),
          p("Дайджест отсутствует")
        )
      }
    })
    
    # Объединяем все панели
    do.call(tagList, daily_panels)
  })
  
  # Таблица с сообщениями
  output$table <- renderTable({
    head(filtered_data()[, c("date_parsed", "channel", "text", "sentiment")], 20)
  })
  
  # График активности
  output$activity <- renderPlot({
    ggplot(filtered_data(), aes(as.Date(date_parsed))) +
      geom_histogram(binwidth = 1, fill = "steelblue") +
      labs(x = "Дата", y = "Число сообщений")
  })
  
  # График тональности
  output$tone <- renderPlot({
    filtered_data() %>%
      count(date = as.Date(date_parsed), sentiment) %>%
      group_by(date) %>%
      mutate(frac = n / sum(n)) %>%
      ggplot(aes(date, frac, color = sentiment)) +
      geom_line() +
      labs(x = "Дата", y = "Доля")
  })
  
  # Скачивание RSS
  output$rss <- downloadHandler(
    filename = function() { "feed.xml" },
    content = function(file) {
      file.copy("../data/feed.xml", file)
    }
  )
}

shinyApp(ui, server)
