library(tidyverse)
library(shinysense)
library(shiny)
library(fortunes)
epmc <- readr::read_csv("data_epmc.csv")

ui <- fixedPage(
  h1("Stats Quotes"),
  p("This is a simple demo of the R package shinyswipr. Swipe on the quote card below to store your rating. What each direction (up, down, left, right) mean is up to you. (We won't tell.)"),
  hr(),
  shinyswiprUI("quote_swiper",
                h4("Swipe Me!"),
                hr(),
                h4("Quote:"),
                textOutput("quote"),
                h4("Author(s):"),
                textOutput("quote_author")
  ),
  hr(),
  h4("Swipe History"),
  tableOutput("resultsTable")
)

server <- function(input, output, session) {
  card_swipe <- callModule(shinyswipr, "quote_swiper")
  
  appVals <- reactiveValues(
    quote = sample_n(epmc, 1),
    swipes = data.frame(quote = character(), author = character(), swipe = character())
  )
  
  our_quote <- isolate(appVals$quote)
  output$quote <- renderText({ our_quote$title })
  output$quote_author <- renderText({ our_quote$authorString })
  output$resultsTable <- renderDataTable({appVals$swipes})
  
  observeEvent(card_swipe(),{
    #Record our last swipe results.
    appVals$swipes <- rbind(
      data.frame(quote = appVals$quote$title,
                 author = appVals$quote$authorString,
                 swipe = card_swipe()
      ), appVals$swipes
    )
    #send results to the output.
    output$resultsTable <- renderTable({appVals$swipes})
    
    #update the quote
    appVals$quote <- sample_n(epmc, 1)
    
    #send update to the ui.
    output$quote <- renderText({ appVals$quote$title })
    
    output$quote_author <- renderText({ appVals$authorString })
  }) #close event observe.
}

shinyApp(ui, server)