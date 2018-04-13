library(shiny)

ui <- fluidPage(
  
  tags$head(
    tags$style(
      HTML(".shiny-notification {
           position:fixed;
           top: calc(30%);
           left: calc(50%);
           width: calc(20%);
           }",
           
           "#loginbox {
            background-color: #99ccff;
            padding-left: 20px;
            padding-bottom: 20px;
            padding-top: 10px;
            }
           "
      )
    )
  ),
  
  titlePanel("Faculty View"), 
  
  mainPanel(width=12,
            uiOutput("tabs1")
  ),
  
  absolutePanel(id="loginbox", top="15%", right="8%", width="20%", draggable=TRUE,
                textInput("username", "Username", "", width="90%"),
                textInput("pin", "4-digit Pin", "", width="90%"),
                actionButton("login", "Log in")
  )
)

server <- function(session, input, output) {
  
  prof_courses <- NULL
  
  observeEvent(input$login, {
    mydata <- read.csv("Profs.csv", header=TRUE, as.is=TRUE)
    professor <- mydata$Prof[mydata$User==input$username & mydata$Pin==input$pin]
    courses <- read.csv("courses.csv", as.is=TRUE)
    prof_courses <- courses$course[courses$prof==professor]
    output$tabs1 <- renderUI({
      tabs <- list(NULL)
      if (!is.null(prof_courses)) {
        for (i in 1:length(prof_courses)) {
          tabs[[i]] <- tabPanel(prof_courses[i])
        }
        tabs[[length(prof_courses)+1]] <- tabPanel("Summary") 
      }
      do.call(tabsetPanel, tabs)
    })
  })
  
}

shinyApp(ui, server)

