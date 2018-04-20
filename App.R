library(shiny)
#install.packages(DT)
library(DT)

courses <- read.csv("courses.csv", as.is = TRUE)
courses <- courses[,-1]
colnames(courses) <- c("Course Code", "Day", "Meeting Time", "Professor")

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
  
  titlePanel("Student View"), 
  
  mainPanel(width=12,
            tabsetPanel(type = "tabs",
                        tabPanel("My Info", 
                                 br(),
                                 textInput("netid", "NetID (New Users Only)", "", width="60%"),
                                 textInput("new_pin", "Create a 4-digit pin for future login", "", width="60%"),
                                 textInput("first_name", "First Name", "", width="60%"),
                                 textInput("last_name", "Last Name", "", width="60%"),
                                 selectInput("year", "Class Year", 
                                             c("Select a year", 2018, 2019, 2020, 2021), width="60%"),
                                 textInput("major", "Major", "", width="60%"),
                                 textAreaInput("why", 
                                               "Why do you want to serve as a ULA?", 
                                               "", width="60%", height="60%")
                        ),
                        tabPanel("Course Preferences", 
                                 
                                 br(),
                                 tableOutput("t_course"),
                                 
                                 hr(),
                                 
                                 fluidRow(
                                   selectizeInput(
                                     inputId = "choices",
                                     label = "Choose your courses",
                                     choices = courses[,1],
                                     multiple = TRUE
                                   ),
                                   
                                   br(),
                                   actionButton("select", "Select"),
                                   hr(),
                                   textOutput("length"),
                                   br(),
                                   conditionalPanel(condition = "output.numSelected > 1",
                                                    numericInput(
                                                      inputId = "courseSelect",
                                                      label = "courseSelect",
                                                      value = 1
                                                    ))
                                 )
                                 
                        ),
                        tabPanel("Summary", br(), htmlOutput("summarytext"), br(), actionButton("submit", "Submit"))
            )
  ),
  
  absolutePanel(id="loginbox", top="15%", right="8%", width="20%", draggable=TRUE,
                textInput("username", "Username", "", width="90%"),
                textInput("pin", "4-digit Pin", "", width="90%"),
                actionButton("login", "Log in")
  )
)

server <- function(session, input, output) {
  
  # My Info tab
  
  observeEvent(input$login, {
    tryCatch({
      mydata <- read.csv(paste0(input$username, "_", input$pin, ".csv"), header=TRUE)
      updateTextInput(session, 'netid', value = mydata$netid)
      updateTextInput(session, 'new_pin', value = mydata$new_pin)
      updateTextInput(session, 'first_name', value = mydata$first_name)
      updateTextInput(session, 'last_name', value = mydata$last_name)
      updateSelectInput(session, 'year', selected = mydata$year)
      updateTextInput(session, 'major', value = mydata$major)
      updateTextAreaInput(session, 'why', value = mydata$why)
    }, error= function(e) {showNotification('User and pin not found', duration=5, type="error")})
  })
  
  # Courses tab
  
  # Display course information
  output$t_course <- renderTable({
    courses <- read.csv("courses.csv", as.is = TRUE)
    courses <- courses[,-1]
    colnames(courses) <- c("Course Code", "Day", "Meeting Time", "Professor")
    return(courses)
  })
  
  # Student ranking
  maxRank <- eventReactive(input$select, 
                           {
                             length(input$choices)
                           })
  
  output.numSelected <- reactive({length(input$choices)})
  
  selectedNames <- eventReactive(input$select, {
    this <- rep(NA, length(input$choices))
    for (i in 1:length(input$choices)){
      this[i] <- grep(input$choices[i], courses[,1], fixed=TRUE)
    }
    return(this)
  })
  
  output$css <- renderText({
    courses[selectedNames(), ]
  })
  
  output$length <- renderText({
    paste0("Please rank your courses from 1 (first preference) to ",
           maxRank(), " (lowest preference).")
  })
  
  # Summary tab
  
  # Error checking
  r <- reactive({
    req(input$netid, input$new_pin, !is.na(as.numeric(input$new_pin)), nchar(input$new_pin) == 4, 
        input$first_name, input$last_name, input$year != "Select a year", input$major, input$why)
  })
  
  # Display information inputs
  output$summarytext <- renderUI({
    
    text <- character(6)
    
    ifelse(input$netid == "", 
           text[1] <- "<font color='red'>Please input your NetID</font>",
           text[1] <- paste0("<strong>You have entered your NetID as: </strong>", input$netid))
    ifelse(input$new_pin == "" | nchar(input$new_pin) != 4 | is.na(as.numeric(input$new_pin)),
           text[2] <- "<font color='red'>Please create a 4-digit pin</font>",
           text[2] <- "<strong>You have entered a pin</strong>")
    ifelse(input$first_name == "" | input$last_name == "",
           text[3] <- "<font color='red'>Please enter your full name</font>",
           text[3] <- paste0("<strong>You have entered your name as: </strong>", 
                             input$first_name, " ", input$last_name))
    ifelse(input$year == "Select a year",
           text[4] <- "<font color='red'>Please select a class year</font>",
           text[4] <- paste0("<strong>You have entered your class year as: </strong>", input$year))
    ifelse(input$major == "", 
           text[5] <- "<font color='red'>Please enter your major(s)</font>",
           text[5] <- paste0("<strong>You have entered your major(s) as: </strong>", input$major))
    ifelse(input$why == "", 
           text[6] <- "<font color='red'>Please explain why you would like to serve as a ULA</font>", 
           text[6] <- paste0("<strong>You have entered your reason for applying as: </strong>", input$why))       
    
    expr = HTML(paste(text, collapse="<br/>"))
    
  })
  
  # Write file upon application completion
  observeEvent(input$submit, {
    
    r()
    write.csv(as.data.frame(cbind("netid"=input$netid, 
                                  "new_pin"=input$new_pin, 
                                  "first_name"=input$first_name, 
                                  "last_name"=input$last_name,
                                  "year"=input$year, 
                                  "major"=input$major, 
                                  "why"=input$why)), 
              paste0(input$netid, "_", input$new_pin, ".csv"))
    showNotification("Application Successful!", duration=5, type="message")
    
  })
}

shinyApp(ui, server)