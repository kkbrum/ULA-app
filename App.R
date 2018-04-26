library(shiny)
#install.packages('DT')
library(DT)
#install.packages('shinyjs')
library(shinyjs)

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
  
  shinyjs::useShinyjs(),
  
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
                                   DT::dataTableOutput("testDT"),
                                   br(),
                                   verbatimTextOutput('printForm'),
                                   actionButton("submit.table", "Submit")
                                   
                                   # DT::dataTableOutput("responses")
                                   
                                   # tableOutput("DFtest1")
                                   
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
  
  # Error checking
  r <- reactive({
    req(input$netid, input$new_pin, !is.na(as.numeric(input$new_pin)), nchar(input$new_pin) == 4, 
        input$first_name, input$last_name, input$year != "Select a year", input$major, input$why)
  })
  
  
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
  
  # Select some classes
  
  output.numSelected <- reactive({length(input$choices)})
  
  # Instructions for the students 
  observeEvent(input$select, { output$length <- renderText({
    paste0("Please rank your preferences in the final column, from 1 (first choice) to ",
           length(input$choices), " (lowest preference)")
  })})
  
  # Enter more information for those selected classes
  
  observeEvent(input$select, {
    if (length(input$choices) > 0) {
      this <- rep(NA, length(input$choices))
      for (i in 1:length(input$choices)){
        this[i] <- grep(input$choices[i], courses[,1], fixed=TRUE)
      }
      temp_df <- courses[this,1]
      #    temp_df <- cbind(temp_df, rep(0, length(this)), rep(0, length(this)))
      #    colnames(temp_df) <- c("courses_selected", "taken", "rank")
      write.csv(temp_df, paste0("temp_", input$netid, ".csv"), row.names = FALSE)
      
      # create a character vector of shiny inputs 
      shinyInput = function(FUN, len, id, ...) { 
        inputs = character(len) 
        for (i in seq_len(len)) { 
          inputs[i] = as.character(FUN(paste0(id, i), label = NULL, ...)) 
        } 
        inputs 
      } 
      
      # obtain the values of inputs 
      shinyValue = function(id, len) { 
        unlist(lapply(seq_len(len), function(i) { 
          value = input[[paste0(id, i)]] 
          if (is.null(value)) NA else value 
        })) 
      } 
      
      
      DF <- read.csv(paste0("temp_", input$netid, ".csv"), as.is=TRUE)
      
      DF$Taken <- shinyInput(selectizeInput, nrow(DF), 'taken', choices = c("", "Y", "N"), 
                             selected = "",
                             multiple = FALSE, width="50%")
      DF$WhenTaken <- shinyInput(numericInput, nrow(DF), "whentaken",
                                 value=NA, min = 2010,
                                 max=as.integer(format(Sys.Date(), "%Y")),
                                 step = 1, width="60%")
      DF$Professor  <- shinyInput(textInput, nrow(DF), 'prof')
      DF$Grade  <- shinyInput(selectInput, nrow(DF), "grade",
                              choices = c("", "A", "B", "C", "D", "F"),
                              multiple = FALSE, selectize=FALSE, width="50%")
      DF$Suitable <- shinyInput(textInput, nrow(DF), "suitable")
      DF$Rank <- shinyInput(numericInput, nrow(DF), 'num',
                            value = NA, min = 1, 
                            max = nrow(DF), step = 1, width="60%")
      
      names(DF) <-  c("Course Title", "Have you taken this course? (Y/N)",
                      "What year did you take this course?",
                      "Who was your professor?",
                      "What was your grade in this course?",
                      "Why are you suitable for this course?",
                      "Rank your preference of ULAing this course")
      
      # render the table containing shiny inputs 
      output$testDT = DT::renderDataTable( 
        DF, server = FALSE, escape = 2, selection='none', options = list( 
          preDrawCallback = JS('function() { 
                             Shiny.unbindAll(this.api().table().node()); }'), 
          drawCallback = JS('function() { 
                          Shiny.bindAll(this.api().table().node()); } '),
          dom = 't'
        ) 
      )
      
      
      # Disabling/Enabling buttons      
      
      observeEvent(shinyValue("taken",nrow(DF)), {
        for (j in which(shinyValue("taken", nrow(DF)) =="")) {
          shinyjs::disable(paste0("whentaken",j))
          shinyjs::disable(paste0("prof",j))
          shinyjs::disable(paste0("grade",j))
          shinyjs::disable(paste0("suitable",j))
          shinyjs::disable(paste0("num", j))
        }
        
        for (j in which(shinyValue("taken", nrow(DF))=="N")) {
          shinyjs::disable(paste0("whentaken",j))
          shinyjs::disable(paste0("prof",j))
          shinyjs::disable(paste0("grade",j))
          shinyjs::enable(paste0("suitable",j))
          shinyjs::enable(paste0("num", j))
        }
        for (j in which(shinyValue("taken", nrow(DF))=="Y")) {
          shinyjs::enable(paste0("whentaken",j))
          shinyjs::enable(paste0("prof",j))
          shinyjs::enable(paste0("grade",j))
          shinyjs::enable(paste0("suitable",j))
          shinyjs::enable(paste0("num", j))
        }
      })
    }
    
    
    # Write csv upon submit
    observeEvent(input$submit.table, {
      
      r()
      
      # Collect student inputs
      preferences <- data.frame(Title= DF[,'Course Title'],
                                Taken = shinyValue('taken', nrow(DF)), 
                                WhenTaken = shinyValue('whentaken', nrow(DF)),
                                Prof = shinyValue('prof', nrow(DF)),
                                Grade = shinyValue('grade', nrow(DF)),
                                Suitable = shinyValue('suitable', nrow(DF)),
                                Rank = shinyValue('num', nrow(DF)))
      
      # Basic error checking
      input.correct <- rep(NA, nrow(preferences))
      
      if (length(unique(preferences$Rank)) != nrow(preferences)) {
        input.correct <- rep(FALSE, nrow(preferences))
        showNotification("Please pick unique ranks", duration=5, type="error")
        
      }
      
      for (i in 1:nrow(preferences)) {
        
        if (preferences$Taken[i] == "") {
          input.correct[i] <- FALSE
        } else if (preferences$Taken[i] == "Y" & 
                   (is.na(preferences$WhenTaken[i]) | preferences$Prof[i] == "" |
                    preferences$Grade[i] == "" | preferences$Suitable[i] == "" |
                    is.na(preferences$Rank[i]))) {
          input.correct[i] <- FALSE
        } else if (preferences$Taken[i] == "Y" & 
                   (!is.na(preferences$WhenTaken[i]) & preferences$Prof[i] != "" &
                    preferences$Grade[i] != "" & preferences$Suitable[i] != "" &
                    !is.na(preferences$Rank[i]))) {
          input.correct[i] <- TRUE
        } else if (preferences$Taken[i] == "N" & 
                   (preferences$Suitable[i] == "" | is.na(preferences$Rank[i]))) {
          input.correct[i] <- FALSE
        } else if (preferences$Taken[i] == "N" & 
                   (preferences$Suitable[i] != "" & !is.na(preferences$Rank[i]))) {
          input.correct[i] <- TRUE
        }
        
      }
      
      if (all(input.correct)) {
        showNotification("Courses correctly selected", duration=5, type="message")
        write.csv(preferences, paste0(input$netid, "_preferences.csv"), row.names = FALSE)
      } else {
        showNotification("Incorrect inputs", duration=5, type="error")
      }
      
    })
  })
  
  # Summary tab
  
  # Display information inputs
  
  
  #When the "submit" button in the "Course Preferences" tab is clicked, it checks to see whether
  #the table.condition is met. If so, the submitcourse$bool is changed to TRUE and the summary tab is updated.
  
  #table.condition can be any desired criteria that needs to be met before the user sees approval
  #in the "summary" tab. It could be that all of the tables values are populated with something, 
  #for example. I left it blank for now, set to TRUE by default.
  
  table.condition <- TRUE
  submitcourse <- reactiveValues(bool = FALSE)
  
  observeEvent(input$submit.table, if(table.condition) submitcourse$bool <- TRUE)
  
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
    ifelse(submitcourse$bool,
           text[7] <- "<strong>You have submitted a course selection. </strong>",
           text[7] <- "<font color='red'>Please submit your desired course information in the correct form.</font>")
    
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