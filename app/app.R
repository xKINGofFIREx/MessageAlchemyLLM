library(shiny)
library(ggplot2)
library(dplyr)

data <- readRDS("../data/messages.rds")

ui <- fluidPage(
  titlePanel("Анализ Telegram-сообщений"),
  sidebarLayout(
    sidebarPanel(
      downloadButton("rss", "Скачать RSS")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Сводки", tableOutput("table")),
        tabPanel("Активность", plotOutput("activity")),
        tabPanel("Тональность", plotOutput("tone"))
      )
    )
  )
)

server <- function(input, output) {
  output$table <- renderTable({
    head(data[, c("date_parsed", "summary", "sentiment")], 20)
  })
  
  output$activity <- renderPlot({
    ggplot(data, aes(as.Date(date_parsed))) +
      geom_histogram(binwidth = 1, fill = "steelblue") +
      labs(x = "Дата", y = "Число сообщений")
  })
  
  output$tone <- renderPlot({
    data %>%
      count(date = as.Date(date_parsed), sentiment) %>%
      group_by(date) %>%
      mutate(frac = n / sum(n)) %>%
      ggplot(aes(date, frac, color = sentiment)) +
      geom_line() +
      labs(x = "Дата", y = "Доля")
  })
  
  output$rss <- downloadHandler(
    filename = function() { "feed.xml" },
    content = function(file) {
      file.copy("data/feed.xml", file)
    }
  )
}

shinyApp(ui, server)
