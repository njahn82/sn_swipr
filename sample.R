library(tidyverse)
library(shinysense)
library(shiny)
library(fortunes)
epmc <- readr::read_csv("data_epmc.csv")

ui <- fixedPage(
   h1("Disease Dashboard"),
  p("This is a simple demo of the Disease Dashboard provided by SpringerNature Medizin.
     Search for a particular disease (e.g., hypertension), and then click, drag, and
     release the Searh Results box to either select a given paper as relevant (drag right) 
     or to discard a paper as irrelevant (drag left). Please drag the box slowly to make 
     sure the app can correctly identify the direction (right or left)."),
  hr(),
  shinyswiprUI( "quote_swiper",
                h4("Search Results"),
                hr(),
                h4(
                textOutput("quote")),
                h4("Author(s):"),
                textOutput("quote_author"),
                h4("Abstract:"),
                textOutput("abstract")
  ),
  hr(),
  h4("Swipe History"),
  tableOutput("resultsTable")
)

server <- function(input, output, session) {
  card_swipe <- callModule(shinyswipr, "quote_swiper")
  
  appVals <- reactiveValues(
    df = sample_n(epmc, 1),
    swipes = data.frame(quote = character(), author = character(), swipe = character())
  )
  
  our_quote <- isolate(appVals$df)
  output$quote <- renderText({ our_quote$title })
  output$abstract <- renderText({ our_quote$abstractText })
  output$quote_author <- renderText({ our_quote$authorString })
  output$resultsTable <- renderDataTable({appVals$swipes})
  
  observeEvent(card_swipe(),{
    #Record our last swipe results.
    appVals$swipes <- rbind(
      data.frame(quote = appVals$df$title,
                 author = appVals$df$authorString,
                 swipe = card_swipe()
      ), appVals$swipes
    )
    #send results to the output.
    output$resultsTable <- renderTable({appVals$swipes})
    
    #update the quote
    appVals$df <- sample_n(epmc, 1)
    
    #send update to the ui.
    output$quote <- renderText({ appVals$df$title })
    
    output$quote_author <- renderText({ appVals$df$authorString })
    
    output$abstract <- renderText({ appVals$df$abstractText })
  }) #close event observe.
}

shinyApp(ui, server)
