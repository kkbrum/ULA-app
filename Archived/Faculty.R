library(shiny)
library(DT)
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
  
  titlePanel("Faculty View"), 
  shinyjs::hidden(
    mainPanel(width=12, id="main",
              uiOutput("tabs1")
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
  
  prof_courses <- NULL
  
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
  
  
  # Log in functionality ----
  
  observeEvent(input$login, {
    # What professor is this?
    mydata <- read.csv("Profs.csv", header=TRUE, as.is=TRUE)
    professor <- mydata$Prof[mydata$User==input$username & mydata$Pin==input$pin]
    # What courses are they teaching?
    courses <- read.csv("courses.csv", as.is=TRUE)
    prof_courses <- courses$course[courses$prof==professor]
    num_ulas <- courses$number[courses$prof==professor]
    
    # Load list of students ----
    
    # Create a list of all the students who have filled out BOTH forms (meta and preferences)
    saved_files <- list.files(pattern= '^save')
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
    
    if (length(prof_courses)>0) {
      rv$loginSuccess <- TRUE
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
              temp2 <- read.csv(list.files(pattern= paste0(currentStudents[[i]][j], '_', '[^p]')), as.is=TRUE)
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
                                            # Input how many ULAs desired
                                            numericInput(paste0("optNum", "_", x), "How many ULAs would you like?", min=0, max=10, value=num_ulas[x], width='10%'),
                                            fluidRow(
                                              # Select rankings
                                              column(4, lapply(1:(min(length(select_extra), length(currentStudents[[x]]))), function(y) {
                                                if (y==1) {selectizeInput(paste0(select_extra[y], "_", x), label=paste0("Select your ", select_extra[y], " choice"), selected=" ", choices=c(" ", studentInfo[[x]][,'Student Name']))}
                                                else {hidden(selectizeInput(paste0(select_extra[y], "_", x), label=paste0("Select your ", select_extra[y], " choice"), selected=" ", choices=c(" ", studentInfo[[x]][,'Student Name'])))}
                                              })),
                                              # Display rankings
                                              column(3, "Rankings:", htmlOutput(paste0("current_choices", x)))
                                            ),
                                            br(),
                                            # Display student information
                                            if (length(currentStudents[[x]])>0) {
                                              DT::renderDataTable( 
                                                studentInfo[[x]], server = FALSE, escape = FALSE, selection='none', options = list( 
                                                  preDrawCallback = JS('function() { Shiny.unbindAll(this.api().table().node()); }'), 
                                                  drawCallback = JS('function() { Shiny.bindAll(this.api().table().node()); } '),
                                                  dom = 't') )}
                                            else {renderText("No students ranked this course")}         
                       )
        )
        # Summary tab ----
        tabs[[length(prof_courses)+1]] <- tabPanel("Summary", 
                                                   fluidRow(column(4, lapply(1:length(prof_courses), function(x) {
                                                     list(br(), 
                                                          htmlOutput(paste0("course", x)), 
                                                          htmlOutput(paste0("choices_summary", x)), 
                                                          br(),
                                                          textOutput(paste0("num_summary", x)), 
                                                          br()
                                                     )
                                                   }))), 
                                                   actionButton("submit", "Submit")
        )
        
        do.call(tabsetPanel, tabs)     
      })    
    }
    
    # Log in errors ----
    if (!input$username %in% mydata$User) {
      showNotification("User not found", duration=5, type="error")
    }
    else if (!input$pin %in% mydata$Pin) {
      showNotification("Incorrect pin", duration=5, type="error")
    }
    else if (length(prof_courses)==0) {
      showNotification("No courses found for this professor", duration=5, type="error")
    }
    
    # Show more ranking fields and update list of rankings ----
    lapply(1:length(prof_courses), function(x) {
      # Set up courses in summary tab
      output[[paste0('course', x)]] <- renderUI(HTML(paste0('<b>', prof_courses[x], ' Rankings: </b>')))
      extra_new <- paste0(select_extra, "_", x)
      lapply(1:(length(select_extra)-1), function(y) {
        observeEvent(input[[extra_new[y]]], {
          if(input[[extra_new[y]]] != " ") {
            show(extra_new[y+1])
            chosen[[x]][y] <<- input[[extra_new[y]]]
            output[[paste0('current_choices', x)]] <- renderUI(HTML(paste0(1:length(chosen[[x]]), ") ", chosen[[x]], "<br>")))
            # Update choices in next selection
            updateSelectizeInput(session, extra_new[y+1], choices=c(" ", studentInfo[[x]][,'Student Name'][!(studentInfo[[x]][,'Student Name']) %in% chosen[[x]]]))
            # Display rankings for each course
            output[[paste0('choices_summary', x)]] <- renderUI(HTML(paste0(1:length(chosen[[x]]), ") ", chosen[[x]], "<br>")))
          }
        })
        observeEvent(input[[paste0("optNum_", x)]], {
          # Display number of desired ULAs
          output[[paste0('num_summary', x)]] <- renderText(paste0(input[[paste0("optNum_", x)]], " ULAs desired."))
        })
      })
    })
    
    # Write CSV upon submit ----
    # Current format is that the file is called JL1234.csv for example. No _ between user and pin unlike for students.
    # Each line is a an evaluable character string with the course number, number of ULAs needed, and ordered 
    # preferences in an vector.
    # The files should be read as: read.table("JL1234.csv", header=FALSE)
    observeEvent(input$submit, {
      lapply(1:length(prof_courses), function(x) {
        write.table(as.character(list(c(prof_courses[x], input[[paste0("optNum_", x)]], list(chosen[[x]])))), 
                    paste0(input$username, input$pin, ".csv"), append=TRUE, sep="\n", row.names=FALSE, col.names=FALSE)})
      showNotification("Submission Successful!", duration=5, type="message")
      
    })
  })
  
}

shinyApp(ui, server)
