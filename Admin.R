library(shiny)
# devtools::install_github('ayayron/shinydnd')
library(shinyDND)
 
# Define UI for application that draws a histogram
ui <- shinyUI(
  mainPanel(
    h2("DragUI"),
    dragUI("div6","bar"),
    dragUI("div5","foo", style = "background-color:red", class = "dragelement"),
    dragUI("div4",tags$a("a",href = "foo")),
    h2("Drop UI"),
    dropUI("div3",vertical = TRUE, nrow = 4),
    dropUI("div2"),
    h2("DragSetUI"),
    dragSetUI("div1", textval = list("foo",tags$a("a",href = "bar"),"baz"))
  )
)

# server with reactive for observing reactive drop event
server = shinyServer(function(input, output,session) {
  observeEvent(input$div2,{
    output$foo = renderText(
      paste("The dropUI element currently contains:", input$div2))
  })
})