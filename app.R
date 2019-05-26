library(shiny)
library(shinyjs)
library(DT)

courses_names <- c(read.csv("courses.csv", as.is = TRUE)$course, "unassigned")
dates <- read.csv("dates.csv", as.is=TRUE, header=TRUE, row.names=1)
dates$start_date <- as.Date(dates$start_date, "%m/%d/%Y")
dates$end_date <- as.Date(dates$end_date, "%m/%d/%Y")

# 1= student_part1, 2=faculty, 3=admin, 4=student_decision

if (Sys.Date() >= dates["student_ranking", "start_date"] & Sys.Date() <= dates["student_ranking", "end_date"]) {
  app_number <- 1
} else if (Sys.Date() >= dates["faculty_ranking", "start_date"] & Sys.Date() <= dates["faculty_ranking", "end_date"]) {
  app_number <- 2
} else if (Sys.Date() >= dates["admin_review", "start_date"] & Sys.Date() <= dates["admin_review", "end_date"]) {
  app_number <- 3
} else if (Sys.Date() >= dates["student_decision", "start_date"] & Sys.Date() <= dates["student_decision", "end_date"]) {
  app_number <- 4
} else (app_number <- 0)

if (app_number == 1) {
  open_to <- "<b>students</b>. If this is you, please press 'begin' below"
} else if (app_number == 2) {
  open_to <- "<b>faculty</b>. If this is you, please press 'begin' below"
} else if (app_number == 3) {
  open_to <- "<b>administrators</b>. If this is you, please press 'begin' below"
} else if (app_number == 4) {
  open_to <- "<b>students</b>. If this is you, please press 'begin' below"
} else {
  open_to <- "no one. Please return when the system opens"
}



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
           ",
           
           ".coursebox {
           border-color: black;
           border-style: solid;
           border-width: medium;
           padding-left: 20px;
           padding-bottom: 20px;
           padding-top: 10px;
           }
           "
      )
    )
  ),
  
  # Panels ----
  
  titlePanel("ULA Matching System"), 
  
  
  
  
  mainPanel(id="startPage", 
            HTML(paste0("<ul><li>The application will be open to students between ", format(dates["student_ranking", "start_date"], format="%B %d, %Y"), " and ", format(dates["student_ranking", "end_date"], format="%B %d, %Y"), ". </br><li> Faculty will have from ", format(dates["faculty_ranking", "start_date"], format="%B %d, %Y"), " to ", format(dates["faculty_ranking", "end_date"], format="%B %d, %Y"), " to submit preferences. </br><li> Decisions will be released ", format(dates["student_decision", "start_date"], format="%B %d, %Y"), ". </br> </br>")),
            HTML(paste0("Today is <b>", format(Sys.Date(), format="%B %d, %Y"), "</b>.</br>")),
            HTML(paste0("The system is currently open to ", open_to, ".</br>")),
            
            fluidRow(
              column(6,
                     actionButton("begin_app", "Begin!")
                     
              )
            )
  ),
  
  ## STUDENT APP 1 UI ----
  
  hidden(
    mainPanel(id="student1", width=12,
              tabsetPanel(type = "tabs", id= "inTabset",
                          # Collect all student meta data
                          tabPanel("My Info",
                                   value = "Tab 1",
                                   br(),
                                   textInput("netid", "NetID (New Users Only)*", "", width="60%"),
                                   textInput("new_pin", "Create a 4-digit pin for future login*", "", width="60%"),
                                   textInput("first_name", "First Name*", "", width="60%"),
                                   textInput("last_name", "Last Name*", "", width="60%"),
                                   textInput("email", "Email Address*", "", width="60%"),
                                   selectInput("year", "Class Year*", 
                                               c("Select a year", 2019, 2020, 2021, 2022), width="60%"),
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
                          tabPanel("View Summary and Submit Application", 
                                   value = "Tab 3",
                                   br(), 
                                   htmlOutput("summarytext"),
                                   br(), 
                                   fluidRow(
                                     column(3, actionButton("back1", "Return to info page")),
                                     column(3, actionButton("save", "Save and continue later"))
                                   ),
                                   br(),
                                   fluidRow(
                                     column(3, actionButton("back2", "Return to preferences page")),
                                     column(3, actionButton("submitStudent", "Submit application"))
                                   ),
                                   fluidRow(
                                     column(3, ""),
                                     column(3, HTML("<font color='red'>You must submit your application by hitting the submit button to be considered.</font>"))
                                   )
                          )
              )
    )
  ),
  
  #Log in box ----
  hidden(mainPanel(id="studentLogIn", 
                   fluidRow(
                     column(6, 
                            "To start a new application to ULA for the S&DS department, press 'Begin'",
                            br(),
                            br(),
                            actionButton("begin_student", "Begin New Application")
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
  )),
  
  ## FACULTY APP UI ----
  
  hidden(
    mainPanel(width=12, id="faculty1",
              HTML("The students willing to ULA your course are shown in the table below along with some more information about them.<ul><li>Please rank <b>all</b> students you would be willing to have ULA each course you are teaching. This will help ensure you have enough ULAs.<li>To unselect a student, select the 'please select a student' option.<li>If you have no preferences across the students or if you do not want to work with any of the students, please just check the appropriate box without entering rankings."),
              br(),
              br(),
              uiOutput("tabs1")
    )
  ),
  
  # Log in box ----
  
  hidden(mainPanel(id="facultyLogIn", 
                   HTML("Please log in using the credentials you have been emailed to view your courses and select students you would like to work with."),
                   fluidRow(
                     column(6,
                            div(id="loginbox", 
                                textInput("facultyUsername", "Username", "", width="90%"),
                                textInput("facultyPin", "4-digit Pin", "", width="90%"),
                                actionButton("facultyLogInButton", "Log in")
                            )
                     )
                   )
  )),
  
  ## ADMIN UI ----
  
  # Log in box ----
  
  hidden(mainPanel(id="adminLogIn", 
                   HTML("Welcome. Please log in using your administrative credentials to edit the matching of students to courses."),
                   fluidRow(
                     column(6,
                            div(id="loginbox", 
                                textInput("adminUsername", "Username", "", width="90%"),
                                textInput("adminPin", "4-digit Pin", "", width="90%"),
                                actionButton("adminLogInButton", "Log in")
                            )
                     )
                   )
  )),
  
  hidden(mainPanel(id = "admin1side",  width=4,
                   h2("Courses"),
                   HTML("<ul><li>To view a professor's preferences in order, click on 'preferences' below the course title. <ul><li>Students in blue are already assigned to that class. <li>Crossed out students are assigned to another class. <li>Bold students have not yet been assigned to a class and are willing to ULA that class.</ul></ul>"),
                   br(),
                   br(),
                   fluidRow(
                     column(6,
                            lapply(courses_names[1:ceiling((length(courses_names)-1)/2)], function(x) {
                              list(div(class="coursebox", uiOutput(x),
                                       actionLink(paste0(x, "_show"), "Preferences"),
                                       uiOutput(paste0(x, "_prefs")),
                                       br(),
                                       uiOutput(paste0(x, "_list")),
                                       br())
                              )
                            })
                     )
                     ,
                     column(6,
                            lapply(courses_names[(ceiling((length(courses_names)-1)/2)+1) : (length(courses_names)-1)], function(x) {
                              list(div(class="coursebox", uiOutput(x),
                                       actionLink(inputId= paste0(x, "_show"), "Preferences"),
                                       uiOutput(paste0(x, "_prefs")),
                                       br(),
                                       uiOutput(paste0(x, "_list")),
                                       br())
                              )
                            })
                     )
                   )
  )
  ),
  
  hidden(mainPanel(id="admin1main", width=8
                   ,
                   h2("Students"),
                   HTML("<ul><li>To unassign a student, press on their button and hit the unassign button. <li>To assign a student to a course, click their name and then click on the course name."),
                   HTML("<li>To view a student's preferences in order, click on their name."),
                   br(),
                   br(),
                   fluidRow(
                     column(6, br(), br(), actionButton(inputId= "unassigned", label="Unassign", style = "background-color: dodgerblue")),
                     column(6, img(src="Legend.png", width="200px"))
                   ),
                   br(),
                   br(),
                   uiOutput(paste0("unassigned_list")),
                   br(),
                   hr(),
                   actionButton("submitAdmin", "Finalize assignments"),
                   br(),
                   br()
  )
  ),
  
  ## DECISION APP UI ----
  
  shinyjs::hidden(
    mainPanel(width=12, id="main_assigned",
              htmlOutput("student_message_assigned"),
              selectInput("decision_assigned", "Do you wish to accept your assignment?",
                          c("", "Yes", "No")),
              shinyjs::hidden(selectInput("decision_rejected", "Do you wish to be considered for a different class if there is an opening? Note: It is unlikely that you will be assigned to another class if you reject this offer.",
                                          c("", "Yes", "No"))),
              actionButton("finalize_assigned", "Submit")
    )
  ),
  
  shinyjs::hidden(
    mainPanel(width=12, id="main_unassigned",
              htmlOutput("student_message_unassigned"),
              selectInput("decision_unassigned", "Do you wish to be considered for courses you ranked if there is an opening?",
                          c("", "Yes", "No")),
              actionButton("finalize_unassigned", "Submit")
    )
  ),
  
  # Log in box ----
  
  hidden(mainPanel(id="decisionLoginPage", 
                   HTML("Please log in using the same netID and 4 digit pin you used when applying. If you have forgotten your information, please email thomas.bischoff@yale.edu or nick.marwell@yale.edu."),
                   fluidRow(
                     column(6,
                            div(id="loginbox", 
                                textInput("usernameDecision", "Username", "", width="90%"),
                                textInput("pinDecision", "4-digit Pin", "", width="90%"),
                                actionButton("loginDecision", "Log in")
                            )
                     )
                   )
  )
  )
  
  
)



server <- function(session, input, output) {
  rv <- reactiveValues(page = 1, studentLogInSuccess=FALSE, facultyLogInSuccess=FALSE, 
                       adminLogInSuccess = FALSE, loginDecisionSuccess= FALSE, errors = rep(FALSE, 9))
  
  if (app_number == 0) {
    hide("begin_app")
  }
  
  observeEvent(input$begin_app, {
    # I would love to launch an app here
    if (app_number == 1) {
      rv$page <- 2
    } else if (app_number == 2) {
      rv$page <- 4
    } else if (app_number == 3) {
      rv$page <- 6
    } else if (app_number ==4) {
      rv$page <- 8
    }
  }
  )
  
  ## STUDENT APP 1 SERVER ----
  
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
  courses <- courses[,c(-1, -7)]
  colnames(courses) <- c("Course Code", "Course Name", "Day", "Meeting Time", "Professor")
  
  # Display course information
  output$t_course <- renderTable(courses)
  
  # Page structure ----
  
  # Switch pages after log in / start button
  
  observe({
    if(rv$page==2) {
      hide("startPage")
      show("studentLogIn")
    }
    if(rv$page==3) {
      hide("studentLogIn")
      show("student1")
    }
  })
  
  navPage <- function(direction) {
    rv$page <- rv$page + direction
  }
  
  observeEvent(rv$studentLogInSuccess, 
               if(rv$studentLogInSuccess == TRUE) {navPage(1)}
  )
  observeEvent(input$begin_student, navPage(1))
  
  observeEvent(input$nextPage1, updateTabsetPanel(session, "inTabset", selected = "Tab 2"))
  observeEvent(input$nextPage2, updateTabsetPanel(session, "inTabset", selected = "Tab 3"))
  
  observeEvent(input$back1, updateTabsetPanel(session, "inTabset", selected = "Tab 1"))
  observeEvent(input$back2, updateTabsetPanel(session, "inTabset", selected = "Tab 2"))
  
  
  # Instructions for preferences ----
  
  output$instructions_1 <- renderUI(HTML("Below are the courses that are looking for ULAs this semester. <br> Some professors may request that you attend class times, although this is not a requirement."))
  
  output$instructions_2 <- renderUI(HTML("<ul><li>Please fill out the relevant information for courses you would ULA if offered. <br><li>Please do not rank courses if you would not accept an offer to ULA that course. <br><li>Rankings must be unique, with 1 being your most preferred class."))
  
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
  DF$Available <- shinyInput(selectizeInput, nrow(DF), 'Available', choices = c("", "Y", "N", "Maybe"), 
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
      disable(paste0("WhenTaken",j))
      disable(paste0("Professor",j))
      disable(paste0("Grade",j))
      disable(paste0("Suitable",j))
      disable(paste0("Available",j))
      disable(paste0("Rank", j))
    })
    lapply(which(shinyValue("Taken", nrow(DF))=="N"), function(j) {
      disable(paste0("WhenTaken",j))
      disable(paste0("Professor",j))
      disable(paste0("Grade",j))
      enable(paste0("Suitable",j))
      enable(paste0("Available",j))
      enable(paste0("Rank", j))
    })
    lapply(which(shinyValue("Taken", nrow(DF))=="Y"), function(j) {
      enable(paste0("WhenTaken",j))
      enable(paste0("Professor",j))
      enable(paste0("Grade",j))
      enable(paste0("Suitable",j))
      enable(paste0("Available",j))
      enable(paste0("Rank", j))
    })
  })
  
  observeEvent(shinyValue("Desire",nrow(DF)), {
    lapply(which(shinyValue("Desire", nrow(DF)) ==""), function(j) {
      disable(paste0("Taken",j))
    })
    lapply(which(shinyValue("Desire", nrow(DF))=="N"), function(j) {
      disable(paste0("Taken",j))
    })
    lapply(which(shinyValue("Desire", nrow(DF))=="Y"), function(j) {
      enable(paste0("Taken",j))
    })
  })
  
  
  # Summary tab ----
  output$summarytext <- renderUI({
    text <- character(9)
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
    ifelse(input$email == "",
           {text[4] <- "<font color='red'>Please enter your email</font>"; rv$errors[4] <- FALSE},
           {text[4] <- paste0("<strong>You have entered your email as: </strong>", 
                              input$email); rv$errors[4] <- TRUE})
    ifelse(input$year == "Select a year",
           {text[5] <- "<font color='red'>Please select a class year</font>"; rv$errors[5] <- FALSE},
           {text[5] <- paste0("<strong>You have entered your class year as: </strong>", input$year); rv$errors[5] <- TRUE})
    ifelse(input$major == "", 
           {text[6] <- "<font color='red'>Please enter your major(s)</font>"; rv$errors[6] <- FALSE},
           {text[6] <- paste0("<strong>You have entered your major(s) as: </strong>", input$major); rv$errors[6] <- TRUE})
    ifelse(input$why == "", 
           {text[7] <- "<font color='red'>Please explain why you would like to serve as a ULA</font>";  rv$errors[7] <- FALSE},
           {text[7] <- paste0("<strong>You have entered your reason for applying as: </strong>", input$why); rv$errors[7] <- TRUE})   
    if(!any(shinyValue('Desire', nrow(DF))=="Y") | is.na(any(shinyValue('Desire', nrow(DF))=="Y"))) {
      text[8] <- "<font color='red'>Please select at least one course you would like to ULA</font>"
      rv$errors[8] <- FALSE
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
        text[8] <- paste0("<font color='red'>Please pick unique ranks from 1 to ", nrow(preferences), " (the number of classes you chose).</font>")
        rv$errors[8] <- FALSE
      } else {
        rv$errors[8] <- TRUE
        text[8] <- paste0("<strong>You have selected (from highest to lowest preference): </strong> </br>", paste(DF[ind,'Course Title'][order(shinyValue('Rank', nrow(DF))[ind])], collapse=", "))
      }
      # Check that all the inputs are there for each selected class
      for (i in 1:nrow(preferences)) {
        if (preferences$Taken[i] == "") {
          rv$errors[9] <- FALSE
        } else if (preferences$Taken[i] == "Y" &
                   (is.na(preferences$WhenTaken[i]) | preferences$Professor[i] == "" |
                    preferences$Grade[i] == "" | preferences$Suitable[i] == "" |
                    preferences$Available[i] == "" | is.na(preferences$Rank[i]))) {
          rv$errors[9] <- FALSE
        } else if (preferences$Taken[i] == "Y" &
                   (!is.na(preferences$WhenTaken[i]) & preferences$Professor[i] != "" &
                    preferences$Grade[i] != "" & preferences$Suitable[i] != "" &
                    preferences$Available[i] != "" & !is.na(preferences$Rank[i]))) {
          rv$errors[9] <- TRUE
        } else if (preferences$Taken[i] == "N" &
                   (preferences$Suitable[i] == "" | preferences$Available[i] == "" | is.na(preferences$Rank[i]))) {
          rv$errors[9] <- FALSE
        } else if (preferences$Taken[i] == "N" &
                   (preferences$Suitable[i] != "" & preferences$Available[i] != "" & !is.na(preferences$Rank[i]))) {
          rv$errors[9] <- TRUE
        }
      }
      if(rv$errors[9] == FALSE) {
        text[9] <- "<font color='red'>Please input all the relevant information for each class you would like to ULA</font>"
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
                                    "email"=input$email,
                                    "year"=input$year, 
                                    "major"=input$major, 
                                    "why"=input$why)), 
                paste0("save_", input$netid, "_", input$new_pin, ".csv"))
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
      write.csv(preferences, paste0("save_", input$netid, "_preferences.csv"), row.names = FALSE)
      showNotification("Save successful, but please finish the application and hit the submit button before the deadline!", duration=5, type="message")
    } else {
      showNotification("Please enter your netid and select a pin before saving", duration=5, type="error")
    }
  })
  
  # Write files upon completion
  observeEvent(input$submitStudent, {
    # Error checking
    if(all(rv$errors)) {
      # Meta data file
      write.csv(as.data.frame(cbind("netid"=input$netid, 
                                    "new_pin"=input$new_pin, 
                                    "first_name"=input$first_name, 
                                    "last_name"=input$last_name,
                                    "email"=input$email,
                                    "year"=input$year, 
                                    "major"=input$major, 
                                    "why"=input$why)), 
                paste0(input$netid, "_", input$new_pin, ".csv"))
      suppressWarnings(file.remove(paste0("save_", input$netid, "_", input$new_pin, ".csv")))
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
      suppressWarnings(file.remove(paste0("save_", input$netid, "_preferences.csv")))
      showNotification("Application successful!", duration=5, type="message")
    } else {
      if(input$netid != "" & !is.na(as.numeric(input$new_pin)) & nchar(input$new_pin) == 4) {
        write.csv(as.data.frame(cbind("netid"=input$netid, 
                                      "new_pin"=input$new_pin, 
                                      "first_name"=input$first_name, 
                                      "last_name"=input$last_name,
                                      "email"=input$email,
                                      "year"=input$year, 
                                      "major"=input$major, 
                                      "why"=input$why)), 
                  paste0("save_", input$netid, "_", input$new_pin, ".csv"))
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
        write.csv(preferences, paste0("save_", input$netid, "_preferences.csv"), row.names = FALSE)
      }
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
      updateTextInput(session, 'email', value = mydata$email)
      updateSelectInput(session, 'year', selected = mydata$year)
      updateTextInput(session, 'major', value = mydata$major)
      updateTextAreaInput(session, 'why', value = mydata$why)
      rv$studentLogInSuccess <- TRUE
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
  
  ## FACULTY SERVER ----
  
  prof_courses <- NULL
  
  observe({
    if(rv$page==4) {
      hide("startPage")
      show("facultyLogIn")
    }
    if(rv$page==5) {
      hide("facultyLogIn")
      show("faculty1")
    }
  })
  
  observeEvent(rv$facultyLogInSuccess, 
               if(rv$facultyLogInSuccess == TRUE) {navPage(1)}
  )
  
  
  # Log in functionality ----
  
  observeEvent(input$facultyLogInButton, {
    try({
      # What professor is this?
      mydata <- read.csv("Profs.csv", header=TRUE, as.is=TRUE)
      professor <- mydata$Prof[mydata$User==input$facultyUsername & mydata$Pin==input$facultyPin]
      # What courses are they teaching?
      courses <- read.csv("courses.csv", as.is=TRUE)
      prof_courses <- courses$course[courses$prof==professor]
      num_ulas <- courses$number[courses$prof==professor]
      
      # Load list of students ----
      
      # Create a list of all the students who have filled out BOTH forms (meta and preferences)
      saved_files <- c(list.files(pattern= '^save'))
      all_files <- list.files(pattern= '.*_[0-9]+')
      meta_files <- all_files[!all_files %in% saved_files]
      students <- substr(meta_files, start=0, stop=nchar(meta_files)-9)
      
      # Initializations ----
      
      # Ranking options
      select_extra <- c("1st", "2nd", "3rd", "4th", "5th", "6th", "7th", "8th", "9th", "10th",
                        "11th", "12th", "13th", "14th", "15th", "16th", "17th", "18th", "19th", "20th",
                        "21st", "22nd", "23rd", "24th", "25th", "26th", "27th", "28th", "29th", "30th")
      # "31st", "32nd", "33rd", "34th", "35th", "36th", "37th", "38th", "39th", "40th"
      # Initialize the list of students a professor ranks for each class
      chosen <- vector("list", length(prof_courses))
      z <- vector("list", length(prof_courses))
      
      if (length(prof_courses)>0) {
        rv$facultyLogInSuccess <- TRUE
        output$tabs1 <- renderUI({
          tabs <- list(NULL)
          
          # Load info about students ----
          
          studentInfo <<- list(NULL)
          currentStudents <- vector("list", length(prof_courses))
          # We want to gather the information about students for each course
          for (i in 1:length(prof_courses)) {
            # Find out which students ranked the current course
            for (j in 1:length(students)){
              temp <- read.csv(paste0(students[j], "_preferences.csv"), as.is=TRUE)
              if (prof_courses[i] %in% temp$Title) {
                currentStudents[[i]] <- c(currentStudents[[i]], students[j])
              }
            }
            # Make a data table showing the responses of the students who ranked that course
            if (length(currentStudents[[i]])>0) {
              DF <- data.frame(matrix(0,nrow = length(currentStudents[[i]]), ncol=9))
              names(DF) <- c( "Student", "Year", "Major", "Taken", "WhenTaken", "Professor", "Grade", "Suitable", "Available")
              DF$Student <- currentStudents[[i]]
              for (j in 1:length(currentStudents[[i]])){
                # Get their preferences
                temp <- read.csv(paste0(currentStudents[[i]][j], "_preferences", ".csv"), as.is=TRUE)
                DF[j,4:9] <- temp[temp$Title==prof_courses[i], 2:7]
                # Get their metadata
                temp2 <- read.csv(list.files(pattern= paste0(currentStudents[[i]][j], '_', '[^pd]')), as.is=TRUE)
                DF$Student[j] <- paste0(temp2$first_name, " ", temp2$last_name)
                DF$Year[j] <- temp2$year
                DF$Major[j] <- temp2$major
              }
              names(DF) <-  c("Student Name", 
                              "Student's Year", 
                              "Student's Major",
                              "Has the student taken the course?",
                              "When did they take it?",
                              "Who was their professor?",
                              "What was their grade?",
                              "Why they think they are suitable",
                              "Are they available during course times?")
              studentInfo[[i]] <<- DF
            }
          }
          
          # Tab per course ----
          
          # Now we construct a tab for each course
          tabs <- lapply(1:length(prof_courses), 
                         function(x) tabPanel(prof_courses[x],  
                                              br(), 
                                              checkboxInput(paste0("nopref", x), "I do not have preferences across any of the students below.", FALSE),
                                              checkboxInput(paste0("nodesire", x), "I do not want any of the students below as ULAs for this course.", FALSE),
                                              # Input how many ULAs desired
                                              # numericInput(paste0("optNum", "_", x), "How many ULAs would you like?", min=0, max=10, value=num_ulas[x], width='10%'),
                                              renderText(paste("This course is eligible for", num_ulas[x], "ULAs, but please rank as many students as possible.")),
                                              br(),
                                              fluidRow(
                                                if (length(currentStudents[[x]])>0) {
                                                  # Select rankings
                                                  column(4, lapply(1:(min(length(select_extra), length(currentStudents[[x]]))), function(y) {
                                                    if (y==1) {selectizeInput(paste0(select_extra[y], "_", x), label=paste0("Select your ", select_extra[y], " choice"), selected="<Please select a student>", choices=c("<Please select a student>", studentInfo[[x]][,'Student Name']))}
                                                    else {hidden(selectizeInput(paste0(select_extra[y], "_", x), label=paste0("Select your ", select_extra[y], " choice"), selected="<Please select a student>", choices=c("<Please select a student>", studentInfo[[x]][,'Student Name'])))}
                                                  }))
                                                },
                                                # Display rankings
                                                if (length(currentStudents[[x]])>0) {
                                                  column(3, "Rankings:", htmlOutput(paste0("current_choices", x)))
                                                }
                                              ),
                                              br(),
                                              # Display student information
                                              if (length(currentStudents[[x]])>0) {
                                                DT::renderDataTable( 
                                                  studentInfo[[x]], server = FALSE, escape = FALSE, selection='none', options = list( 
                                                    pageLength = length(currentStudents[[x]]),
                                                    preDrawCallback = JS('function() { Shiny.unbindAll(this.api().table().node()); }'), 
                                                    drawCallback = JS('function() { Shiny.bindAll(this.api().table().node()); } '),
                                                    dom = 't') )}
                                              else {renderText("No students ranked this course")}         
                         )
          )
          # Summary tab ----
          tabs[[length(prof_courses)+1]] <- tabPanel("Summary and submission", 
                                                     fluidRow(column(4, lapply(1:length(prof_courses), function(x) {
                                                       list(br(), 
                                                            htmlOutput(paste0("course", x)), 
                                                            htmlOutput(paste0("choices_summary", x)), 
                                                            br()
                                                       )
                                                     }))), 
                                                     actionButton("submitFaculty", "Submit your preferences")
          )
          
          do.call(tabsetPanel, tabs)     
        })    
      }
      
      # Show more ranking fields and update list of rankings ----
      lapply(1:length(prof_courses), function(x) {
        # Set up courses in summary tab
        output[[paste0('course', x)]] <- renderUI(HTML(paste0('<b>', prof_courses[x], ' Rankings: </b>')))
        extra_new <- paste0(select_extra, "_", x)
        z[[x]] <<- 1
        lapply(1:(length(select_extra)-1), function(y) {
          observeEvent(input[[extra_new[y]]], {
            if ((y+1) > z[[x]]) {
              if (input[[extra_new[y]]] != "<Please select a student>") {
                z[[x]] <<- y + 1
                show(extra_new[y+1])
              }
            }
            chosen[[x]][y] <<- input[[extra_new[y]]]
            output[[paste0('current_choices', x)]] <- renderUI(HTML(paste0(1:length(chosen[[x]]), ") ", chosen[[x]], "<br>")))
            # Update choices 
            lapply(1:z[[x]], function(a) {
              updateSelectizeInput(session, extra_new[a], selected = chosen[[x]][a], choices=c("<Please select a student>", chosen[[x]][a], studentInfo[[x]][,'Student Name'][!(studentInfo[[x]][,'Student Name']) %in% chosen[[x]]]))
            })
            # Display rankings for each course
            output[[paste0('choices_summary', x)]] <- renderUI(HTML(paste0(1:length(chosen[[x]]), ") ", chosen[[x]], "<br>")))
          })
        })
      })
    })
    
    # Log in errors ----
    if (!input$facultyUsername %in% mydata$User) {
      showNotification("User not found", duration=5, type="error")
    }
    else if (!input$facultyPin %in% mydata$Pin) {
      showNotification("Incorrect pin", duration=5, type="error")
    }
    else if (length(prof_courses)==0) {
      showNotification("No courses found for this professor", duration=5, type="error")
    }
    
    # Write CSV upon submit ----
    # Current format is that the file is called JL1234.csv for example. No _ between user and pin unlike for students.
    # Each line is a an evaluable character string with the course number, number of ULAs needed, and ordered 
    # preferences in an vector.
    # The files should be read as: read.table("JL1234.csv", header=FALSE)
    observeEvent(input$submitFaculty, {
      lapply(1:length(prof_courses), function(x) {
        write.table(as.character(list(c(prof_courses[x], num_ulas[x], list(chosen[[x]])))), 
                    paste0(input$facultyUsername, input$pin, ".csv"), append=TRUE, sep="\n", row.names=FALSE, col.names=FALSE)
        if (input[[paste0("nopref", x)]] | input[[paste0("nodesire", x)]]) {
          if (!file.exists("checkboxes.csv")) {
            write.table(matrix(c(prof_courses[x], input[[paste0("nopref", x)]], input[[paste0("nodesire", x)]]), nrow=1), file="checkboxes.csv", col.names=c("course", "no_preferences", "no_desire"), quote=FALSE, sep=",", row.names=FALSE)
          } else {
            write.table(matrix(c(prof_courses[x], input[[paste0("nopref", x)]], input[[paste0("nodesire", x)]]), nrow=1), file="checkboxes.csv", append=TRUE, col.names=FALSE, quote=FALSE, sep=",", row.names=FALSE)
          }
        }
      })
      showNotification("Submission Successful!", duration=5, type="message")
      
    })
  })
  
  ## ADMIN SERVER ----
  
  observe({
    if(rv$page==6) {
      hide("startPage")
      show("adminLogIn")
    }
    if(rv$page==7) {
      hide("adminLogIn")
      show("admin1side")
      show("admin1main")
    }
  })
  
  observeEvent(rv$adminLogInSuccess, 
               if(rv$adminLogInSuccess == TRUE) {navPage(1)}
  )
  
  observeEvent(input$adminLogInButton, {
    admins <- read.csv("Admins.csv", header=TRUE, as.is=TRUE)
    if (!input$adminUsername %in% admins$User) {
      showNotification("User not found", duration=5, type="error")
    }
    else if (!input$adminPin %in% admins$Pin) {
      showNotification("Incorrect pin", duration=5, type="error")
    }
    else {
      rv$adminLogInSuccess <- TRUE
      
      # UNCOMMENT THIS BEFORE LAUNCHING
      # source("Matching.R", local=TRUE)
      
      student_preferences <- readRDS("student_preferences.RDS")
      faculty_preferences <- readRDS("faculty_preferences.RDS")
      if (file.exists("checkboxes.csv")) {
        checkboxes <- read.csv("checkboxes.csv", as.is=TRUE)
        checkboxes <- checkboxes[!rev(duplicated(rev(checkboxes$course))),]
      } else {
        checkboxes <- NULL
      }
      
      if (file.exists("Final_assignments.csv")) {
        assignments <- read.csv("Final_assignments.csv", as.is=TRUE)
        assignments <- assignments[assignments$course != "unassigned",]
      } else {
        assignments <- read.csv("Assignments.csv", as.is=TRUE)
      }
      demand <- read.csv("Demand.csv", as.is=TRUE)
      # Make this list be all the unassigned and assigned people
      students <- names(student_preferences)
      unassigned <- students[!students %in% assignments$student]
      courses_names <- c(unique(demand$course), "unassigned")
      course_assignments <- vector("list", length(courses_names))
      names(course_assignments) <- courses_names
      for (course in courses_names) {
        course_assignments[[course]] <- assignments[assignments$course == course,"student"]
      }
      course_assignments[["unassigned"]] <- unassigned
      
      # for clicking purposes later
      s <- rep(list(FALSE), length(students))
      c <- rep(list(FALSE), length(courses_names))
      names(c) <- courses_names
      names(s) <- students
      
      clicked <- reactiveValues(s = s, c = c, change = FALSE)
      
      # Record whether students have been clicked
      lapply(students, function(x)
        observeEvent(input[[x]], 
                     clicked$s[[x]] <- !clicked$s[[x]]
        )
      )
      
      # Record whether courses have been clicked
      lapply(courses_names, function(x)
        observeEvent(input[[paste0(x, "_show")]], 
                     clicked$c[[x]] <- !clicked$c[[x]]
        )
      )
      
      lapply(courses_names, function(x) {
        output[[paste0(x, "_prefs")]] <- renderUI(
          if(clicked$c[[x]]) {
            if (x %in% checkboxes$course[checkboxes$no_desire==TRUE]) {
                text <- "<font color='red'>Professor would not like to hire any of the applicants.</font>"
            } else if (is.null(faculty_preferences[[x]])) {
              text <- "Professor has no preference across students."
            } else {
              text <- paste0(unlist(
                lapply(faculty_preferences[[x]], function(y) {
                  if (y %in% course_assignments[["unassigned"]]) {
                    return(paste0("<b>", y, "</b></br>"))
                  } 
                  else if (y %in% course_assignments[[x]]) {
                    return(paste0("<font color='blue'>", y, "</font></br>"))
                  }
                  else {return(paste0("<del>", y, "</del></br>"))}
                })))
            }
            HTML(text)
          }
        )
      })
      
      # Render buttons for students that change color when clicked
      lapply(students, function(x) {
        most_desired <- student_preferences[[x]]$Title[student_preferences[[x]]$Rank < 3]
        desired <- student_preferences[[x]]$Title[student_preferences[[x]]$Rank %in% c(3,4)]
        not_desired <- student_preferences[[x]]$Title[student_preferences[[x]]$Rank > 4]
        output[[paste0(x, "_prefs")]] <- renderUI(
          if(clicked$s[[x]]) {
            HTML(paste0(c("<table cellpadding='10' width='100%'> <tr> <th> Course     </th> <th> Taken      </th><th> Grade     </th><th> Reason </th> </tr><tr> <td>",
                          unlist(lapply(order(student_preferences[[x]]$Rank), function(y) {
                            c(paste0(student_preferences[[x]][y,c("Title", "Taken", "Grade", "Suitable")], "</td> <td>"), "</td> </tr> <tr> <td>")
                          })), "</td></tr></table>")))
          }
        )
        
        output[[x]] <- renderUI({
          if(clicked$s[[x]]) {
            actionButton(inputId= x, label=x, style = "border-color:red")
          } else if (x %in% course_assignments[["unassigned"]] ) {
            actionButton(inputId= x, label=x)
          } else if(x %in% course_assignments[[most_desired[1]]] | x %in% course_assignments[[most_desired[2]]]) {
            actionButton(inputId= x, label=x, style = "background-color:rgba(66, 244, 78, .6)")
          } else if(x %in% course_assignments[[desired[1]]] | x %in% course_assignments[[desired[2]]]) {
            actionButton(inputId= x, label=x, style = "background-color:rgba(244, 241, 65, .6)")
          } else if(x %in% course_assignments[[not_desired[1]]] | x %in% course_assignments[[not_desired[2]]] | x %in% course_assignments[[not_desired[3]]] | x %in% course_assignments[[not_desired[4]]]){
            actionButton(inputId= x, label=x, style = "background-color:rgba(244, 65, 65, .4)")
          } else {actionButton(inputId= x, label=x, style = "background-color:grey")}
          
          # actionButton(inputId= x, label=x, style = "background-color:grey")
        })
      })
      
      
      # Make buttons for the courses
      lapply(courses_names[-length(courses_names)], function(x)
        output[[x]] <- renderUI({
          actionButton(inputId= x, label=x, style = "background-color: dodgerblue")
        })
      )
      
      # Render the lists of students in each course
      observeEvent(clicked$change, {
        lapply(courses_names[-length(courses_names)], function(x)
          output[[paste0(x, "_list")]] <- renderUI({
            if(length(course_assignments[[x]]) != demand$desired[demand$course == x]) {
              HTML(c("<font color=red>", demand$desired[demand$course == x], " ULAs desired <hr> </font>", paste0(course_assignments[[x]], "</br>")))
            }
            else {
              HTML(c("<font color=black>", demand$desired[demand$course == x], " ULAs desired <hr> </font>", paste0(course_assignments[[x]], "</br>")))
            }
          })
        )
        output[['unassigned_list']] <- renderUI(
          fluidRow(
            column(6,
                   lapply(students[1:(ceiling(length(students))/2)], function(x) list(uiOutput(x), uiOutput(paste0(x, "_prefs")), br()))
            ), 
            column(6,
                   lapply(students[((ceiling(length(students))/2)+1):length(students)], function(x) list(uiOutput(x), uiOutput(paste0(x, "_prefs")), br()))
            )
          )
        )
      })
      
      # Move people around when course titles get clicked
      
      remove_students <- function(students) {
        for (student in students) {
          for(course in courses_names) {
            if(student %in% course_assignments[[course]]) {
              course_assignments[[course]] <<- course_assignments[[course]][course_assignments[[course]] != student]
            }
          }
        }
      }
      
      lapply(courses_names, function(x) {
        observeEvent(input[[x]], {
          changed_students <- students[which(unlist(clicked$s))]
          remove_students(changed_students)
          course_assignments[[x]] <<- c(course_assignments[[x]], changed_students)
          for (student in changed_students) {
            clicked$s[[student]] <- FALSE
          }
          clicked$change <- !clicked$change
        })
      })
      
      observeEvent(input$submitAdmin, {
        final_assignments <- data.frame("student" = students, "course" = rep(NA, length(students)))
        for (course in courses_names) {
          for (student in course_assignments[[course]]) {
            final_assignments$course[final_assignments$student == student] <- course
          }
        }
        write.csv(final_assignments, file="Final_assignments.csv")
        showNotification("Submission successful!", duration=5, type="message")
      })
    }
  })
  
  
  # DECISION APP SERVER ----
  observe({
    if (rv$page==8) {
      hide("startPage")
      show("decisionLoginPage")
    }
    if (rv$page==9) {
      hide("decisionLoginPage")
      if (student_data$course != "unassigned") {
        show("main_assigned") 
      } else if (student_data$course == "unassigned") {
        show("main_unassigned")
      }
    } 
  })
  
  observeEvent(rv$loginDecisionSuccess, 
               if(rv$loginDecisionSuccess == TRUE) {navPage(1)}
  )
  
  observeEvent(input$loginDecision, {
    tryCatch({
      # Data to be used throughout ----
      final_assignments <- read.csv("Final_assignments.csv", stringsAsFactors=FALSE)
      student_csv <- read.csv(list.files(pattern= paste0('.*', input$usernameDecision, '_', input$pinDecision, '.csv')), header=TRUE)
      courses <- read.csv("courses.csv", stringsAsFactors=FALSE)
      
      student_data <<- final_assignments[final_assignments$student == paste(student_csv$first_name, student_csv$last_name, sep=" "), ]
      if (nrow(student_data) < 1) {stop()}
      rv$loginDecisionSuccess <- TRUE
      
      
      # Render appropriate UI depending on whether or not the student was assigned
      # to a class
      output$student_message_assigned <- renderUI({
        text <- character(3)
        text[1] <- paste0("You have been assigned to serve as a ULA for <strong>", student_data$course, 
                          "</strong> being taught by <strong>", courses$prof[courses$course == student_data$course], "</strong>! Please respond ASAP, so that we can begin the hiring process and you can start working with the class. It is strongly recommended that you respond to your offer by 11:59 pm on ", format(dates["student_decision", "start_date"]+2, format="%B %d, %Y"),".")
        
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
        file_name <- paste0(input$usernameDecision, "_decision.csv")
        write.csv(decision, file_name, row.names=FALSE)
        showNotification("Decision submitted!", duration=5, type="message")
      })
      
      observeEvent(input$finalize_unassigned, {
        decision <- as.data.frame(cbind(student_data$course, input$decision_unassigned))
        names(decision) <- c("course", "decision")
        file_name <- paste0(input$usernameDecision, "_decision.csv")
        write.csv(decision, file_name, row.names=FALSE)
        showNotification("Decision submitted!", duration=5, type="message")
      })
      
      
    }, error=function(e) {showNotification("Incorrect username or pin", duration=5, type="error")})
  }) 
  
  
}

shinyApp(ui, server)
