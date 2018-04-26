library(shiny)
library(DT)


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
  
  titlePanel("Faculty View"), 
  
  mainPanel(width=12,
            uiOutput("tabs1")
  ),
  
  absolutePanel(id="loginbox", top="15%", right="8%", width="20%", draggable=TRUE,
                textInput("username", "Username", "", width="90%"),
                textInput("pin", "4-digit Pin", "", width="90%"),
                actionButton("login", "Log in")
  )
)

server <- function(session, input, output) {
  
  
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
  
  
  prof_courses <- NULL
  
  observeEvent(input$login, {
    mydata <- read.csv("Profs.csv", header=TRUE, as.is=TRUE)
    professor <- mydata$Prof[mydata$User==input$username & mydata$Pin==input$pin]
    courses <- read.csv("courses.csv", as.is=TRUE)
    prof_courses <- courses$course[courses$prof==professor]
    
    students <- read.csv("students.csv", header=FALSE, as.is=TRUE)
    
    if (length(prof_courses)>0) {
      output$tabs1 <- renderUI({
        tabs <- list(NULL)
        studentInfo <- list(NULL)
        currentStudents <- vector("list", length(prof_courses))
        for (i in 1:length(prof_courses)) {
          for (j in 1:length(students$V1)){
            temp <- read.csv(paste0(students$V1[j], ".csv"), as.is=TRUE)
            if (prof_courses[i] %in% temp$Title) {
              currentStudents[[i]] <- c(currentStudents[[i]], students$V1[j])
            }
          }
          
          if (length(currentStudents[[i]])>0) {
          DF <- data.frame(matrix(0,nrow = length(currentStudents[[i]]), ncol=9))
          names(DF) <- c("Rank", "Student", "Year", "Major", "Taken", "WhenTaken", "Professor", "Grade", "Suitable")
          
          DF$Rank <- shinyInput(numericInput, nrow(DF), 'num',
                                value = NA, min = 1, 
                                max = nrow(DF), step = 1, width="60%")
          DF$Student <- currentStudents[[i]]
          
          for (j in 1:length(currentStudents[[i]])){
            temp <- read.csv(paste0(currentStudents[[i]][j], "_preferences", ".csv"), as.is=TRUE)
            
            DF[j,5:9] <- temp[temp$Title==prof_courses[i], 2:6]
            
            temp2 <- read.csv(list.files(pattern= paste0(currentStudents[[i]][j], '_', '[^p]')), as.is=TRUE)

            DF$Student[j] <- paste0(temp2$first_name, " ", temp2$last_name)
            
            DF$Year[j] <- temp2$year
            
            DF$Major[j] <- temp2$major
          }
          
          
          names(DF) <-  c("Your ranking", "Student Name", "Student's Year", "Student's Major",
                          "Has the student taken the course?",
                          "When did they take it?",
                          "Who was their professor?",
                          "What was their grade?",
                          "Why they think they are suitable")
          
          
          studentInfo[[i]] <- DF
          }
        }
        
        
        tabs <- lapply(1:length(prof_courses), 
                       function(x) tabPanel(prof_courses[x],  br(), 
                                            if (length(currentStudents[[i]])>0) {
                                            DT::renderDataTable( 
                                              studentInfo[[x]], server = FALSE, escape = FALSE, selection='none', options = list( 
                                                preDrawCallback = JS('function() { 
                                                                     Shiny.unbindAll(this.api().table().node()); }'), 
                                                drawCallback = JS('function() { 
                                                                  Shiny.bindAll(this.api().table().node()); } '),
                                                dom = 't') )}
                                            else {renderText("No students ranked this course")}
                                            
                       )
        )
        tabs[[length(prof_courses)+1]] <- tabPanel("Summary") 
        
        do.call(tabsetPanel, tabs)
      })
    }
    
    # Log in errors
    
    if (!input$username %in% mydata$User) {
      showNotification("User not found", duration=5, type="error")
    }
    else if (!input$pin %in% mydata$Pin) {
      showNotification("Incorrect pin", duration=5, type="error")
    }
    else if (length(prof_courses)==0) {
      showNotification("No courses found for this professor", duration=5, type="error")
    }
  })
  
  
  
  
}

shinyApp(ui, server)

