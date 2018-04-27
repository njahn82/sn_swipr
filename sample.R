library(shinysense)
library(shiny)
library(fortunes)

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
                h4("Title:"),
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
    quote = fortune(),
    swipes = data.frame(quote = character(), author = character(), swipe = character())
  )
  
  our_quote <- isolate(appVals$quote)
  output$quote <- renderText({ our_quote$quote })
  output$quote_author <- renderText({ paste0("-",our_quote$author) })
  output$resultsTable <- renderDataTable({appVals$swipes})
  
  observeEvent( card_swipe(),{
    #Record our last swipe results.
    appVals$swipes <- rbind(
      data.frame(quote = appVals$quote$quote,
                 author = appVals$quote$author,
                 swipe = card_swipe()
      ), appVals$swipes
    )
    #send results to the output.
    output$resultsTable <- renderTable({appVals$swipes})
    
    #update the quote
    appVals$quote <- fortune()
    
    #send update to the ui.
    output$quote <- renderText({ appVals$quote$quote })
    
    output$quote_author <- renderText({ paste0("-",appVals$quote$author) })
  }) #close event observe.
}

shinyApp(ui, server)
