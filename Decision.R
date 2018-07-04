library(shiny)
library(shinyjs)

ui <- fluidPage(
  
  useShinyjs(),
  
  # Formatting ----
  
  # HTML formatting of notification boxes and log in box
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
  
  # Panels ----
  
  titlePanel("Decision Page"), 
  shinyjs::hidden(
    mainPanel(width=12, id="main",
              htmlOutput("student_message"),
              selectInput("decision", "Do you wish to accept your ULA assignment?",
                          c("", "Yes", "No")),
              actionButton("finalize", "Submit")
    )
  ),
  # Log in box ----
  
  mainPanel(id="startPage", 
            fluidRow(
              column(6,
                     div(id="loginbox", 
                         textInput("username", "Username", "", width="90%"),
                         textInput("pin", "4-digit Pin", "", width="90%"),
                         actionButton("login", "Log in")
                     )
              )
            )
  )
)

server <- function(session, input, output) {
  
  # Data to be used throughout ----
  final_assignments <- read.csv("Final_assignments.csv", stringsAsFactors=FALSE)
  student_credentials <- read.csv("student_credentials.csv", stringsAsFactors=FALSE)
  student_info <- merge(final_assignments, student_credentials, by="student", all.y=TRUE)
  
  # Page structure ----
  
  # Switch pages after log in 
  rv <- reactiveValues(page = 1, loginSuccess=FALSE)
  
  observe({
    if(rv$page==2) {
      hide("startPage")
      show("main")
    }
  })
  
  navPage <- function(direction) {
    rv$page <- rv$page + direction
  }
  
  observeEvent(rv$loginSuccess, 
               if(rv$loginSuccess == TRUE) {navPage(1)}
  )
  
  observeEvent(input$login, {
    tryCatch({
      student_data <- student_info[input$username == student_info$netid & input$pin == student_info$pin, ]
      if (nrow(student_data) < 1) {stop()}
      rv$loginSuccess <- TRUE
    }, error=function(e) {showNotification("Username or pin incorrect", duration=5, type="error")})
    
    
  })  
  
  
  
  
}

shinyApp(ui, server)