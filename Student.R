library(shiny)
library(DT)
library(shinyjs)

ui <- fluidPage(
  
  shinyjs::useShinyjs(),
  
  # HTML formatting for notifications and log in box
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
  
  titlePanel("ULA application for S&DS courses"), 
  
  shinyjs::hidden(
    mainPanel(id="main", width=12,
              tabsetPanel(type = "tabs",
                          # Collect all student meta data
                          tabPanel("My Info", 
                                   br(),
                                   textInput("netid", "NetID (New Users Only)*", "", width="60%"),
                                   textInput("new_pin", "Create a 4-digit pin for future login*", "", width="60%"),
                                   textInput("first_name", "First Name*", "", width="60%"),
                                   textInput("last_name", "Last Name*", "", width="60%"),
                                   selectInput("year", "Class Year*", 
                                               c("Select a year", 2018, 2019, 2020, 2021), width="60%"),
                                   textInput("major", "Major*", "", width="60%"),
                                   textAreaInput("why", 
                                                 "Why do you want to serve as a ULA?*", 
                                                 "", width="60%", height="60%"),
                                   br(),
                                   "(*) Denotes a required field."
                          ),
                          # Collect student prefererences
                          tabPanel("Course Preferences", 
                                   br(),
                                   tableOutput("t_course"),
                                   hr(),
                                   fluidRow(
                                     br(),
                                     DT::dataTableOutput("rankDT")
                                   )
                          ),
                          tabPanel("Summary", 
                                   br(), 
                                   htmlOutput("summarytext"), 
                                   htmlOutput("summarytext2"), 
                                   htmlOutput("ranked"),
                                   br(), 
                                   actionButton("submit", "Submit")
                          )
                          
              )
              
              
    )
  ),
  
  #Log in box
  mainPanel(id="startPage", 
            fluidRow(
              column(6, 
                     "To start a new application, press 'Begin'",
                     br(),
                     br(),
                     actionButton("begin", "Begin")
              ),
              column(6,
                     "To edit an existing application, log in:",
                     br(),
                     br(),
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
  observeEvent(input$begin, navPage(1))
  
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
  
  
  # Load in course information
  courses <- read.csv("courses.csv", as.is = TRUE)
  courses <- courses[,-1]
  colnames(courses) <- c("Course Code", "Day", "Meeting Time", "Professor")
  
  # Update the course options based on course information
  
  DF <- data.frame(x = courses[,1])
  
  
  # Create fields to be inputted for each class in the table
  DF$Desire <- shinyInput(selectizeInput, nrow(DF), 'Desire', choices = c("", "Y", "N"), 
                          selected = "",
                          multiple = FALSE, width="60%")
  DF$Taken <- shinyInput(selectizeInput, nrow(DF), 'Taken', choices = c("", "Y", "N"), 
                         selected = "",
                         multiple = FALSE, width="60%")
  DF$WhenTaken <- shinyInput(numericInput, nrow(DF), "WhenTaken",
                             value=NA, min = 2010,
                             max=as.integer(format(Sys.Date(), "%Y")),
                             step = 1, width="70%")
  DF$Professor  <- shinyInput(textInput, nrow(DF), 'Professor', width="99%")
  DF$Grade  <- shinyInput(selectInput, nrow(DF), "Grade",
                          choices = c("", "A", "A-", "B+", "B", "B-", "C+", "C, C-, D+", "D, D-, F", "CR, P"),
                          multiple = FALSE, selectize=FALSE, width="60%")
  DF$Suitable <- shinyInput(textInput, nrow(DF), "Suitable")
  DF$Rank <- shinyInput(numericInput, nrow(DF), 'Rank',
                        value = NA, min = 1, 
                        max = nrow(DF), step = 1, width="60%")
  
  names(DF) <-  c("Course Title", "Would you ULA this course if offered?", "Have you Taken this course? (Y/N)",
                  "What year did you take this course?",
                  "Who was your professor?",
                  "What was your Grade in this course?",
                  "Why are you Suitable for this course?",
                  "Rank your preference of ULAing this course (1 is the most preferred)")
  
  
  # Error checking
  errCheck <- reactive({
    req(input$netid, input$new_pin, !is.na(as.numeric(input$new_pin)), nchar(input$new_pin) == 4, 
        input$first_name, input$last_name, input$year != "Select a year", input$major, input$why)
  })
  
  
  
  # Display course information
  output$t_course <- renderTable(courses)
  
  
  
  
  
  
  # Enter more information for those selected classes
  
  
  # render the table containing shiny inputs 
  output$rankDT = DT::renderDataTable( 
    DF, server = FALSE, escape = 2, selection='none', options = list( 
      preDrawCallback = JS('function() { 
                             Shiny.unbindAll(this.api().table().node()); }'), 
      drawCallback = JS('function() { 
                          Shiny.bindAll(this.api().table().node()); } '),
      dom = 't', rownames = FALSE
    ) 
  )
  
  # Disabling/Enabling buttons      
  observeEvent(shinyValue("Taken",nrow(DF)), {
    lapply(which(shinyValue("Taken", nrow(DF)) =="" | is.na(shinyValue("Taken", nrow(DF)))), function(j) {
      shinyjs::disable(paste0("WhenTaken",j))
      shinyjs::disable(paste0("Professor",j))
      shinyjs::disable(paste0("Grade",j))
      shinyjs::disable(paste0("Suitable",j))
      shinyjs::disable(paste0("Rank", j))
    })
    lapply(which(shinyValue("Taken", nrow(DF))=="N"), function(j) {
      shinyjs::disable(paste0("WhenTaken",j))
      shinyjs::disable(paste0("Professor",j))
      shinyjs::disable(paste0("Grade",j))
      shinyjs::enable(paste0("Suitable",j))
      shinyjs::enable(paste0("Rank", j))
    })
    lapply(which(shinyValue("Taken", nrow(DF))=="Y"), function(j) {
      shinyjs::enable(paste0("WhenTaken",j))
      shinyjs::enable(paste0("Professor",j))
      shinyjs::enable(paste0("Grade",j))
      shinyjs::enable(paste0("Suitable",j))
      shinyjs::enable(paste0("Rank", j))
    })
    
  })
  
  observeEvent(shinyValue("Desire",nrow(DF)), {
    lapply(which(shinyValue("Desire", nrow(DF)) ==""), function(j) {
      shinyjs::disable(paste0("Taken",j))
    })
    lapply(which(shinyValue("Desire", nrow(DF))=="N"), function(j) {
      shinyjs::disable(paste0("Taken",j))
    })
    lapply(which(shinyValue("Desire", nrow(DF))=="Y"), function(j) {
      shinyjs::enable(paste0("Taken",j))
    })
    
  })
  
  
  # Write preferences csv upon submit on preferences page
  observeEvent(input$submit, {
    # Collect student inputs
    ind <- which(shinyValue('Desire', nrow(DF))=="Y")
    preferences <- data.frame(Title= DF[ind,'Course Title'],
                              Taken = shinyValue('Taken', nrow(DF))[ind], 
                              WhenTaken = shinyValue('WhenTaken', nrow(DF))[ind],
                              Professor = shinyValue('Professor', nrow(DF))[ind],
                              Grade = shinyValue('Grade', nrow(DF))[ind],
                              Suitable = shinyValue('Suitable', nrow(DF))[ind],
                              Rank = shinyValue('Rank', nrow(DF))[ind])
    # Basic error checking of preferences
    input.correct <- rep(NA, nrow(preferences))
    if (any(sort(preferences$Rank) != 1:nrow(preferences))) {
      input.correct <- rep(FALSE, nrow(preferences))
      showNotification(paste0("Please pick unique ranks from 1 to ", nrow(preferences), " (the number of classes you chose)."), duration=5, type="error")
    }
    for (i in 1:nrow(preferences)) {
      if (preferences$Taken[i] == "") {
        input.correct[i] <- FALSE
      } else if (preferences$Taken[i] == "Y" & 
                 (is.na(preferences$WhenTaken[i]) | preferences$Professor[i] == "" |
                  preferences$Grade[i] == "" | preferences$Suitable[i] == "" |
                  is.na(preferences$Rank[i]))) {
        input.correct[i] <- FALSE
      } else if (preferences$Taken[i] == "Y" & 
                 (!is.na(preferences$WhenTaken[i]) & preferences$Professor[i] != "" &
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
      write.csv(preferences, paste0(input$netid, "_preferences.csv"), row.names = FALSE)
    } else {
      showNotification("Incorrect inputs", duration=5, type="error")
    }
    
  })
  
  observeEvent(c(shinyValue("Rank",nrow(DF)), rv$loginSuccess),
               if (sum(is.na(shinyValue("Rank",nrow(DF)))) < nrow(DF)) {
                 hide("summarytext2")
                 ind <- which(shinyValue("Desire", nrow(DF))=="Y")
                 if (any(sort(shinyValue("Rank",nrow(DF))[ind]) != 1:length(ind))) {
                   output$ranked <- renderUI(HTML(paste0("<font color='red'>Please select unique ranks of courses from 1 to ", length(ind), " (the number of classes you chose).</font>")))
                 } else {
                   output$ranked <- renderUI(HTML(paste0("<strong>You have selected (from highest to lowest preference): </strong> </br>", paste(DF[ind,'Course Title'][order(shinyValue('Rank', nrow(DF))[ind])], collapse=", "))))
                 }
               }
  )
  
  
  # Load information from login on the myinfo tab
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
      rv$loginSuccess <- TRUE
    }, error= function(e) {showNotification('User and pin not found', duration=5, type="error")})
    try({
      mypref <- read.csv(paste0(input$username, "_preferences.csv"), header=TRUE, as.is=TRUE)
      chosen <- which(DF[,1] %in% mypref$Title)
      row <- unlist(lapply(chosen, function(x) which(mypref$Title==DF[chosen,1])))
      DF[chosen, 2] <- unlist(lapply(chosen, function(x) as.character(selectizeInput(paste0('Desire', x), 
                                                                                     choices = c("", "Y", "N"), 
                                                                                     selected = "Y",
                                                                                     label=NULL,
                                                                                     multiple = FALSE, 
                                                                                     width="60%"))))
      DF[chosen,3] <- unlist(lapply(1:length(chosen), function(x) as.character(selectizeInput(paste0('Taken', chosen[x]), 
                                                                                              choices = c("", "Y", "N"), 
                                                                                              selected = mypref$Taken[row[x]],
                                                                                              multiple = FALSE, width="60%", label=NULL))))
      DF[chosen,4] <- unlist(lapply(1:length(chosen), function(x) as.character(numericInput(paste0( "WhenTaken", chosen[x]),
                                                                                            value=mypref$WhenTaken[row[x]], label=NULL, min = 2010,
                                                                                            max=as.integer(format(Sys.Date(), "%Y")),
                                                                                            step = 1, width="70%"))))
      DF[chosen,5] <- unlist(lapply(1:length(chosen), function(x) as.character(textInput(paste0('Professor', chosen[x]), width="99%", label=NULL,
                                                                                         value=mypref$Professor[row[x]]))))
      DF[chosen,6] <- unlist(lapply(1:length(chosen), function(x) as.character(selectInput(paste0("Grade", chosen[x]),
                                                                                           choices = c("", "A", "A-", "B+", "B", "B-", "C+", "C, C-, D+", "D, D-, F", "CR, P"),
                                                                                           multiple = FALSE, selectize=FALSE, width="60%",
                                                                                           selected=mypref$Grade[row[x]], label=NULL))))
      DF[chosen,7] <- unlist(lapply(1:length(chosen), function(x) as.character(textInput(paste0("Suitable", chosen[x]), label=NULL, value=mypref$Suitable[row[x]]))))
      DF[chosen,8] <- unlist(lapply(1:length(chosen), function(x) as.character(numericInput(paste0('Rank', chosen[x]),
                                                                                            value = mypref$Rank[row[x]], min = 1, 
                                                                                            max = nrow(DF), step = 1, width="60%",label=NULL))))
      output$rankDT = DT::renderDataTable( 
        DF, server = FALSE, escape = 2, selection='none', options = list( 
          preDrawCallback = JS('function() { 
                               Shiny.unbindAll(this.api().table().node()); }'), 
          drawCallback = JS('function() { 
                            Shiny.bindAll(this.api().table().node()); } '),
          dom = 't', rownames = FALSE
        ) 
      )
      
    })
  })
  
  # Summary tab
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
  
  output$summarytext2 <- renderUI(HTML("<font color='red'>Please select and rank courses to ULA.</font>"))
  
  # Write file upon meta data completion
  observeEvent(input$submit, {
    # Error checking
    errCheck()
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
