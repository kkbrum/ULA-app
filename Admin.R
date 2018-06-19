library(shiny)
# devtools::install_github('ayayron/shinydnd')
library(shinyDND)
 
ui <- shinyUI(
  mainPanel(
    h2("DragUI"),
    dragUI("div6","bar"),
    dragUI("div5","foo"),
    dragUI("div4", "hello"),
    h2("Drop UI"),
    h3("S&DS 361"),
    dropUI("div3"),
    h3("S&DS 361"),
    dropUI("div2", "woo"), 
    dropUI("div1")
  )
)

# server with reactive for observing reactive drop event
server <- shinyServer(function(input, output,session) {
  observeEvent(input$div2,{
    output$foo = renderText(
      paste("The dropUI element currently contains:", input$div2))
  })
})

shinyApp(ui, server)