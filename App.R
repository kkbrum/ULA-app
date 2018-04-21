library(shiny)
#install.packages(DT)
library(DT)


courses <- read.csv("courses.csv", as.is = TRUE)
courses <- courses[,-1]
colnames(courses) <- c("Course Code", "Day", "Meeting Time", "Professor")


saveData <- function(data) {
  data <- as.data.frame(t(data))
  if (exists("responses")) {
    responses <<- rbind(responses, data)
  } else {
    responses <<- data
  }
}

loadData <- function() {
  if (exists("responses")) {
    responses
  }
}

fields <- c("course", "taken", "whentaken", "prof", "grade", "suit", "rank")

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
                                             c(2018, 2019, 2020, 2021), width="60%"),
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
                                   DT::dataTableOutput("testDT"),
                                   br(),
                                   actionButton("submit.table", "Submit"),
                                   
                                   
                                   # DT::dataTableOutput("responses")
                                   
                                   
                                   tableOutput("DFtest1")
                                   
                                   # tableOutput("omg")
                                   # rHandsontableOutput("hot")
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
  
  output.numSelected <- reactive({length(input$choices)})
  
  observeEvent(input$select, {
    this <- rep(NA, length(input$choices))
    for (i in 1:length(input$choices)){
      this[i] <- grep(input$choices[i], courses[,1], fixed=TRUE)
    }
    temp_df <- courses[this,1]
#    temp_df <- cbind(temp_df, rep(0, length(this)), rep(0, length(this)))
#    colnames(temp_df) <- c("courses_selected", "taken", "rank")
    write.csv(temp_df, paste0("temp_", input$netid, ".csv"), row.names = FALSE)
    
    DF <- read.csv(paste0("temp_", input$netid, ".csv"), as.is=TRUE)
    
    output$testDT <- DT::renderDataTable({
      #DF
      DF[,"Taken"] <<- ""
      sapply(1:nrow(DF), FUN = function(i) {
        DF$Taken[i] <<- as.character(selectizeInput(paste0("taken",i), "",
                                                    choices = c("", "Y", "N"), 
                                                    selected = NULL,
                                                    multiple = FALSE))
      })
      
      # If input$takeni == "Y", then have student fill in WhenTaken, Professor, Grade columns
      # If input$takeni == "N", have student fill in why they are suitable for this course
      
      DF[,"WhenTaken"] <- ""
      sapply(1:nrow(DF), FUN = function(i) {
        DF$WhenTaken[i] <<- as.character(numericInput(paste0("whentaken",i), "",
                                                      value=NA, min = 2010,
                                                      max=as.integer(format(Sys.Date(), "%Y")),
                                                      step = 1))
      })
      
      DF[,"Professor"] <- ""
      sapply(1:nrow(DF), FUN=function(i) {
        DF$Professor[i] <<- as.character(textInput(paste0("prof",i), ""))
      })
      
      DF[,"Grade"] <- ""
      sapply(1:nrow(DF), FUN = function(i) {
        DF$Grade[i] <<- as.character(selectizeInput(paste0("grade",i), "",
                                                  choices = c("", "A", "B", "C", "D", "F"), 
                                                  multiple = FALSE))
      })
      
      DF[,"Suitable"] <- ""
      sapply(1:nrow(DF), FUN=function(i) {
        DF$Suitable[i] <<- as.character(textInput(paste0("suitable",i), ""))
      })
      
      
      # Always have Rank as a Column
      DF[,"Rank"] <- ""
      sapply(1:nrow(DF), FUN = function(i) {
        DF$Rank[i] <<- as.character(numericInput(paste0("num",i), "",
                                                 value = NA, min = 1, 
                                                 max = nrow(DF), step = 1))
      })
      
      datatable(DF, colnames = c("Course Tilte", "Have you taken this course? (Y/N)",
                                 "What year did you take this course?",
                                 "Who was your professor?",
                                 "What was your grade in this course?",
                                 "Why are you suitable for this course?",
                                 "Rank your preference of ULAing this course"),
                filter = "none", selection = "none", escape = 2)
      
    
    })
    
    observeEvent(input$submit.table, {
      output$DFtest1 <- renderTable(DF)
    })
    
#    output$DFtest1 <- renderTable(DFupdate)
  
  })
  

  
#  # Whenever a field is filled, aggregate all form data
#  formData <- reactive({
#    data <- apply(fields, function(x) input[[x]])
#    data
#  })
#  
#  observeEvent(input$submit.table, {
#    saveData(formData())
#  })
#  # Show the previous responses
#  # (update with current response when Submit is clicked)
#  output$responses <- DT::renderDataTable({
#    input$submit.table
#    loadData()
#  })  
#
  
  # output$length <- renderText({
  #   paste0("Please rank your courses from 1 (first preference) to ",
  #          maxRank(), " (lowest preference).")
  # })
  
  r <- reactive({
    req(input$netid, !is.na(as.numeric(input$new_pin)), nchar(input$new_pin) == 4, 
        input$first_name, input$last_name, input$year, input$major, input$why)
  })
  
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
  
  output$t_course <- renderTable({
    courses <- read.csv("courses.csv", as.is = TRUE)
    courses <- courses[,-1]
    colnames(courses) <- c("Course Code", "Day", "Meeting Time", "Professor")
    return(courses)
  })
  
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
  
  output$summarytext <- renderUI({
    
    
    condition1 <- c(input$netid == "", input$new_pin == "", input$first_name == "",
                    input$last_name == "", input$year == "", input$major == "",
                    input$why == "")
    
    if(all(condition1)) { #All entries are blank
      expr = HTML("You haven't selected anything yet, silly!") 
      
    } else {
      text <- character(7)
      text[1] <- paste("You have input your netid as", input$netid)
      text[2] <- "You have input a pin"
      text[3] <- paste("You have entered your first name as", input$first_name)
      text[4] <- paste("You have entered your last name as", input$last_name)
      text[5] <- paste("You have input your class year as", input$year)
      text[6] <- paste("You have input your major as", input$major)
      text[7] <- paste("You have input why you want to be a ULA")
      text[8] <- ""
      if(!any(condition1)) { #No entries are blank
        text[8] = "Super duper! You've filled in all of the fields. You are ready to submit!" 
      }
      expr = HTML(paste(text[which(c(input$netid, input$new_pin, input$first_name, input$last_name, 
                                     input$year, input$major, input$why, text[8]) != "")], collapse = "<br/>"))
    }
    
    #outputOptions(output, "numSelected", suspendWhenHidden = TRUE)
    
  })
}

shinyApp(ui, server)