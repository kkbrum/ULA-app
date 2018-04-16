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
  
  titlePanel("Student View"), 
  
  mainPanel(width=12,
            tabsetPanel(type = "tabs",
                        tabPanel("My Info", 
                                 textInput("netid", "NetID (New Users Only)", "", width="60%"),
                                 textInput("new_pin", "Create a 4-digit pin for future login", "", width="60%"),
                                 textInput("first_name", "First Name", "", width="60%"),
                                 textInput("last_name", "Last Name", "", width="60%"),
                                 selectInput("year", "Class Year", 
                                             c(2018, 2019, 2020, 2021), width="60%"),
                                 textInput("major", "Major", "", width="60%"),
                                 textAreaInput("why", 
                                               "Why do you want to serve as a ULA?", 
                                               "", width="60%", height="60%"),
                                 actionButton("submit1", "Submit")
                        ),
                        tabPanel("Course Preferences",  tableOutput("courses")),
                        tabPanel("Summary", actionButton("submit", "Submit"))
            )
  ),
  
  absolutePanel(id="loginbox", top="15%", right="8%", width="20%", draggable=TRUE,
                textInput("username", "Username", "", width="90%"),
                textInput("pin", "4-digit Pin", "", width="90%"),
                actionButton("login", "Log in")
  )
)

server <- function(session, input, output) {
    
    r <- reactive({
      req(input$netid, !is.na(as.numeric(input$new_pin)), nchar(input$new_pin) == 4, 
          input$first_name, input$last_name, input$year, input$major, input$why)
    })
  
    observeEvent(input$submit1, {
      
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

  output$courses <- renderTable(read.csv("courses.csv"))
  
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
}

shinyApp(ui, server)