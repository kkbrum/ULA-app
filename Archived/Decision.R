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
    mainPanel(width=12, id="main_assigned",
              htmlOutput("student_message_assigned"),
              selectInput("decision_assigned", "Do you wish to accept your assignment?",
                          c("", "Yes", "No")),
              shinyjs::hidden(selectInput("decision_rejected", "Do you wish to be considered for a different class if there is an opening?",
                                          c("", "Yes", "No"))),
              actionButton("finalize_assigned", "Submit")
    )
  ),
  
  shinyjs::hidden(
    mainPanel(width=12, id="main_unassigned",
              htmlOutput("student_message_unassigned"),
              selectInput("decision_unassigned", "Do you wish to be considered if there is an opening?",
                          c("", "Yes", "No")),
              actionButton("finalize_unassigned", "Submit")
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
  courses <- read.csv("courses.csv", stringsAsFactors=FALSE)
  
  # Page structure ----
  
  # Switch pages after log in 
  rv <- reactiveValues(page = 1, loginSuccess=FALSE)
  
  observe({
    if (rv$page==2) {
      hide("startPage")
      if (student_data$course != "unassigned") {
        show("main_assigned") 
      } else if (student_data$course == "unassigned") {
        show("main_unassigned")
      }
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
      student_data <<- student_info[input$username == student_info$netid & input$pin == student_info$pin, ]
      if (nrow(student_data) < 1) {stop()}
      rv$loginSuccess <- TRUE
    }, error=function(e) {showNotification("Incorrect username or pin", duration=5, type="error")})
  })  
  
  # Render appropriate UI depending on whether or not the student was assigned
  # to a class
  output$student_message_assigned <- renderUI({
    text <- character(3)
    text[1] <- paste0("You have been assigned to serve as a ULA for <strong>", student_data$course, 
                      "</strong> being taught by <strong>", courses$prof[courses$course == student_data$course], "</strong>!")
    
    expr <- HTML(paste(text, collapse="<br/>"))
  })
  
  output$student_message_unassigned <- renderUI({
    text <- character(3)
    text[1] <- "Unfortunately you have not been assigned a class to serve as a ULA for this semester."
    expr <- HTML(paste(text, collapse="<br/>"))
  })
  
  observeEvent(input$decision_assigned, {
    if( input$decision_assigned == "No") {
      shinyjs::show("decision_rejected")
    }
  })
  
  # Write decision information to a csv
  observeEvent(input$finalize_assigned, {
    if (input$decision_assigned== "Yes") {
      decision <- as.data.frame(cbind(student_data$course, input$decision_assigned))
    } else {
      decision <- as.data.frame(rbind(cbind(student_data$course, input$decision_assigned), 
                                      cbind("unassigned", input$decision_rejected)))
    }
    names(decision) <- c("course", "decision")
    file_name <- paste0(student_data$netid, "_decision.csv")
    write.csv(decision, file_name, row.names=FALSE)
    showNotification("Decision submitted!", duration=5, type="message")
  })
  
  observeEvent(input$finalize_unassigned, {
    decision <- as.data.frame(cbind(student_data$course, input$decision_unassigned))
    names(decision) <- c("course", "decision")
    file_name <- paste0(student_data$netid, "_decision.csv")
    write.csv(decision, file_name, row.names=FALSE)
    showNotification("Decision submitted!", duration=5, type="message")
  })
  
}

shinyApp(ui, server)