library(shiny)
library(DT)
library(shinyjs)

ui <- fluidPage(
  
  shinyjs::useShinyjs(),
  
  # Formatting ----
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
  
  # Panels ----
  titlePanel("ULA application for S&DS courses"), 
  shinyjs::hidden(
    mainPanel(id="main", width=12,
              tabsetPanel(type = "tabs", id= "inTabset",
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
                                   textAreaInput("why", "Why do you want to serve as a ULA?*", 
                                                 "", width="60%", height="60%"),
                                   br(),
                                   "(*) Denotes a required field.",
                                   br(),
                                   br(),
                                   actionButton("nextPage1", "Go to next page")
                          ),
                          # Collect student prefererences
                          tabPanel("Course Preferences", 
                                   value = "Tab 2",
                                   br(),
                                   htmlOutput("instructions_1"),
                                   tableOutput("t_course"),
                                   hr(),
                                   fluidRow(
                                     br(),
                                     htmlOutput("instructions_2"),
                                     DT::dataTableOutput("rankDT")
                                   ),
                                   br(),
                                   actionButton("nextPage2", "Go to next page")
                          ),
                          tabPanel("Summary", 
                                   value = "Tab 3",
                                   br(), 
                                   htmlOutput("summarytext"),
                                   br(), 
                                   fluidRow(
                                     column(3, actionButton("save", "Save and continue later")),
                                     column(3, actionButton("submit", "Submit application"))
                                   )
                          )
              )
    )
  ),
  
  #Log in box ----
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
  # Functions for later ----
  
  # Create a character vector of shiny inputs 
  shinyInput = function(FUN, len, id, ...) { 
    inputs = character(len) 
    for (i in seq_len(len)) { 
      inputs[i] = as.character(FUN(paste0(id, i), label = NULL, ...)) 
    } 
    inputs 
  } 
  
  # Obtain the values of inputs 
  shinyValue = function(id, len) { 
    unlist(lapply(seq_len(len), function(i) { 
      value = input[[paste0(id, i)]] 
      if (is.null(value)) NA else value 
    })) 
  } 
  
  # Info for later ----
  
  # Load in course information
  courses <- read.csv("courses.csv", as.is = TRUE)
  courses <- courses[,c(-1, -6)]
  colnames(courses) <- c("Course Code", "Day", "Meeting Time", "Professor")
  
  # Display course information
  output$t_course <- renderTable(courses)
  
  # Page structure ----
  
  # Switch pages after log in / start button
  rv <- reactiveValues(page = 1, loginSuccess=FALSE, errors = rep(FALSE, 8))
  
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
  
  observeEvent(input$nextPage1, updateTabsetPanel(session, "inTabset", selected = "Tab 2"))
  observeEvent(input$nextPage2, updateTabsetPanel(session, "inTabset", selected = "Tab 3"))
  
  
  # Instructions for preferences ----
  
  output$instructions_1 <- renderUI(HTML("Below are the courses that are looking for ULAs this semester. <br> Some professors may request that you attend class times, although this is not a requirement."))
  
  output$instructions_2 <- renderUI(HTML("Please fill out the relevant information for courses you would ULA if offered. <br> Please do not rank courses if you would not accept an offer to ULA that course. <br> Rankings must be unique, with 1 being your most preferred class."))
  
  # DF creation ----
  
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
  DF$Suitable <- shinyInput(textAreaInput, nrow(DF), "Suitable")
  DF$Available <- shinyInput(selectizeInput, nrow(DF), 'Available', choices = c("", "Y", "N"), 
                         selected = "",
                         multiple = FALSE, width="60%")
  DF$Rank <- shinyInput(numericInput, nrow(DF), 'Rank',
                        value = NA, min = 1, 
                        max = nrow(DF), step = 1, width="60%")
  
  names(DF) <-  c("Course Title", 
                  "Would you ULA this course if offered?", 
                  "Have you taken this course? (Y/N)",
                  "What year did you take this course?",
                  "Who was your professor?",
                  "What was your grade in this course?",
                  "Why are you suitable for this course?",
                  "Are you available during course meeting times?",
                  "Rank your preference of ULAing this course (1 is the most preferred)")
  
  # DF use ----
  
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
      shinyjs::disable(paste0("Available",j))
      shinyjs::disable(paste0("Rank", j))
    })
    lapply(which(shinyValue("Taken", nrow(DF))=="N"), function(j) {
      shinyjs::disable(paste0("WhenTaken",j))
      shinyjs::disable(paste0("Professor",j))
      shinyjs::disable(paste0("Grade",j))
      shinyjs::enable(paste0("Suitable",j))
      shinyjs::enable(paste0("Available",j))
      shinyjs::enable(paste0("Rank", j))
    })
    lapply(which(shinyValue("Taken", nrow(DF))=="Y"), function(j) {
      shinyjs::enable(paste0("WhenTaken",j))
      shinyjs::enable(paste0("Professor",j))
      shinyjs::enable(paste0("Grade",j))
      shinyjs::enable(paste0("Suitable",j))
      shinyjs::enable(paste0("Available",j))
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
  
  
  # Summary tab ----
  output$summarytext <- renderUI({
    text <- character(8)
    ifelse(input$netid == "", 
           {text[1] <- "<font color='red'>Please input your NetID</font>"; rv$errors[1] <- FALSE},
           {text[1] <- paste0("<strong>You have entered your NetID as: </strong>", input$netid); rv$errors[1] <- TRUE})
    ifelse(input$new_pin == "" | nchar(input$new_pin) != 4 | is.na(as.numeric(input$new_pin)),
           {text[2] <- "<font color='red'>Please create a 4-digit pin</font>"; rv$errors[2] <- FALSE},
           {text[2] <- "<strong>You have entered a pin</strong>"; rv$errors[2] <- TRUE})
    ifelse(input$first_name == "" | input$last_name == "",
           {text[3] <- "<font color='red'>Please enter your full name</font>"; rv$errors[3] <- FALSE},
           {text[3] <- paste0("<strong>You have entered your name as: </strong>", 
                              input$first_name, " ", input$last_name); rv$errors[3] <- TRUE})
    ifelse(input$year == "Select a year",
           {text[4] <- "<font color='red'>Please select a class year</font>"; rv$errors[4] <- FALSE},
           {text[4] <- paste0("<strong>You have entered your class year as: </strong>", input$year); rv$errors[4] <- TRUE})
    ifelse(input$major == "", 
           {text[5] <- "<font color='red'>Please enter your major(s)</font>"; rv$errors[5] <- FALSE},
           {text[5] <- paste0("<strong>You have entered your major(s) as: </strong>", input$major); rv$errors[5] <- TRUE})
    ifelse(input$why == "", 
           {text[6] <- "<font color='red'>Please explain why you would like to serve as a ULA</font>";  rv$errors[6] <- FALSE},
           {text[6] <- paste0("<strong>You have entered your reason for applying as: </strong>", input$why); rv$errors[6] <- TRUE})   
    if(!any(shinyValue('Desire', nrow(DF))=="Y") | is.na(any(shinyValue('Desire', nrow(DF))=="Y"))) {
      text[7] <- "<font color='red'>Please select at least one course you would like to ULA</font>"
      rv$errors[7] <- FALSE
    } else {
      ind <- which(shinyValue('Desire', nrow(DF))=="Y")
      preferences <- data.frame(Title= DF[ind,'Course Title'],
                                Taken = shinyValue('Taken', nrow(DF))[ind],
                                WhenTaken = shinyValue('WhenTaken', nrow(DF))[ind],
                                Professor = shinyValue('Professor', nrow(DF))[ind],
                                Grade = shinyValue('Grade', nrow(DF))[ind],
                                Suitable = shinyValue('Suitable', nrow(DF))[ind],
                                Available = shinyValue('Available', nrow(DF))[ind],
                                Rank = shinyValue('Rank', nrow(DF))[ind])
      if (any(sort(preferences$Rank) != 1:nrow(preferences)) | any(is.na(preferences$Rank))) {
        text[7] <- paste0("<font color='red'>Please pick unique ranks from 1 to ", nrow(preferences), " (the number of classes you chose).</font>")
        rv$errors[7] <- FALSE
      } else {
        rv$errors[7] <- TRUE
        text[7] <- paste0("<strong>You have selected (from highest to lowest preference): </strong> </br>", paste(DF[ind,'Course Title'][order(shinyValue('Rank', nrow(DF))[ind])], collapse=", "))
      }
      # Check that all the inputs are there for each selected class
      for (i in 1:nrow(preferences)) {
        if (preferences$Taken[i] == "") {
          rv$errors[8] <- FALSE
        } else if (preferences$Taken[i] == "Y" &
                   (is.na(preferences$WhenTaken[i]) | preferences$Professor[i] == "" |
                    preferences$Grade[i] == "" | preferences$Suitable[i] == "" |
                    preferences$Available[i] == "" | is.na(preferences$Rank[i]))) {
          rv$errors[8] <- FALSE
        } else if (preferences$Taken[i] == "Y" &
                   (!is.na(preferences$WhenTaken[i]) & preferences$Professor[i] != "" &
                    preferences$Grade[i] != "" & preferences$Suitable[i] != "" &
                    preferences$Available[i] != "" & !is.na(preferences$Rank[i]))) {
          rv$errors[8] <- TRUE
        } else if (preferences$Taken[i] == "N" &
                   (preferences$Suitable[i] == "" | preferences$Available[i] == "" | is.na(preferences$Rank[i]))) {
          rv$errors[8] <- FALSE
        } else if (preferences$Taken[i] == "N" &
                   (preferences$Suitable[i] != "" & preferences$Available[i] != "" & !is.na(preferences$Rank[i]))) {
          rv$errors[8] <- TRUE
        }
      }
      if(rv$errors[8] == FALSE) {
        text[8] <- "<font color='red'>Please input all the relevant information for each class you would like to ULA</font>"
      }
    }
    expr = HTML(paste(text, collapse="<br/>"))
  })
  
  # Write files if no errors ----
  
  # Saving ----
  # (could have errors, but netid and pin need to be set)
  
  observeEvent(input$save, {
    if(input$netid != "" & !is.na(as.numeric(input$new_pin)) & nchar(input$new_pin) == 4) {
      write.csv(as.data.frame(cbind("netid"=input$netid, 
                                    "new_pin"=input$new_pin, 
                                    "first_name"=input$first_name, 
                                    "last_name"=input$last_name,
                                    "year"=input$year, 
                                    "major"=input$major, 
                                    "why"=input$why)), 
                paste0("save_", input$netid, "_", input$new_pin, ".csv"))
      # Preferences file
      ind <- which(shinyValue('Desire', nrow(DF))=="Y")
      print(ind)
      preferences <- data.frame(Title= DF[ind,'Course Title'],
                                Taken = shinyValue('Taken', nrow(DF))[ind], 
                                WhenTaken = shinyValue('WhenTaken', nrow(DF))[ind],
                                Professor = shinyValue('Professor', nrow(DF))[ind],
                                Grade = shinyValue('Grade', nrow(DF))[ind],
                                Suitable = shinyValue('Suitable', nrow(DF))[ind],
                                Available = shinyValue('Available', nrow(DF))[ind],
                                Rank = shinyValue('Rank', nrow(DF))[ind])
      write.csv(preferences, paste0("save_", input$netid, "_preferences.csv"), row.names = FALSE)
      showNotification("Save successful!", duration=5, type="message")
    } else {
      showNotification("Please enter your netid and select a pin before saving", duration=5, type="error")
    }
  })
  
  # Write files upon completion
  observeEvent(input$submit, {
    # Error checking
    if(all(rv$errors)) {
      # Meta data file
      write.csv(as.data.frame(cbind("netid"=input$netid, 
                                    "new_pin"=input$new_pin, 
                                    "first_name"=input$first_name, 
                                    "last_name"=input$last_name,
                                    "year"=input$year, 
                                    "major"=input$major, 
                                    "why"=input$why)), 
                paste0(input$netid, "_", input$new_pin, ".csv"))
      try(file.remove(paste0("save_", input$netid, "_", input$new_pin, ".csv")), silent=TRUE)
      # Preferences file
      ind <- which(shinyValue('Desire', nrow(DF))=="Y")
      preferences <- data.frame(Title= DF[ind,'Course Title'],
                                Taken = shinyValue('Taken', nrow(DF))[ind], 
                                WhenTaken = shinyValue('WhenTaken', nrow(DF))[ind],
                                Professor = shinyValue('Professor', nrow(DF))[ind],
                                Grade = shinyValue('Grade', nrow(DF))[ind],
                                Suitable = shinyValue('Suitable', nrow(DF))[ind],
                                Available = shinyValue('Available', nrow(DF))[ind],
                                Rank = shinyValue('Rank', nrow(DF))[ind])
      write.csv(preferences, paste0(input$netid, "_preferences.csv"), row.names = FALSE)
      try(file.remove(paste0("save_", input$netid, "_preferences.csv")), silent=TRUE)
      showNotification("Application successful!", duration=5, type="message")
    } else {
      showNotification("Please fix the errors in red above", duration=5, type="error")
    }
  })
  
  
  # Log in functionality ----
  observeEvent(input$login, {
    tryCatch({
      mydata <- read.csv(list.files(pattern= paste0('.*', input$username, '_', input$pin, '.csv')), header=TRUE)
      updateTextInput(session, 'netid', value = mydata$netid)
      updateTextInput(session, 'new_pin', value = mydata$new_pin)
      updateTextInput(session, 'first_name', value = mydata$first_name)
      updateTextInput(session, 'last_name', value = mydata$last_name)
      updateSelectInput(session, 'year', selected = mydata$year)
      updateTextInput(session, 'major', value = mydata$major)
      updateTextAreaInput(session, 'why', value = mydata$why)
      rv$loginSuccess <- TRUE
    }, error= function(e) {showNotification('User and pin not found (beware user is case sensitive)', duration=5, type="error")})
    try({
      mypref <- read.csv(list.files(pattern= paste0(input$username, '_preferences.csv')), header=TRUE, as.is=TRUE)
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
      DF[chosen,7] <- unlist(lapply(1:length(chosen), function(x) as.character(textAreaInput(paste0("Suitable", chosen[x]), label=NULL, value=mypref$Suitable[row[x]]))))
      DF[chosen,8] <- unlist(lapply(1:length(chosen), function(x) as.character(selectizeInput(paste0('Available', chosen[x]), 
                                                                                              choices = c("", "Y", "N"), 
                                                                                              selected = mypref$Available[row[x]],
                                                                                              multiple = FALSE, width="60%", label=NULL))))
      DF[chosen,9] <- unlist(lapply(1:length(chosen), function(x) as.character(numericInput(paste0('Rank', chosen[x]),
                                                                                            value = mypref$Rank[row[x]], min = 1, 
                                                                                            max = nrow(DF), step = 1, width="60%",label=NULL))))
      print(shinyValue('Desire', nrow(DF)))
      
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
}

shinyApp(ui, server)
