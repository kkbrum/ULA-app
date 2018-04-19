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
  
  prof_courses <- NULL
  
  observeEvent(input$login, {
    mydata <- read.csv("Profs.csv", header=TRUE, as.is=TRUE)
    professor <- mydata$Prof[mydata$User==input$username & mydata$Pin==input$pin]
    courses <- read.csv("courses.csv", as.is=TRUE)
    prof_courses <- courses$course[courses$prof==professor]
    
    students <- read.csv("students.csv", header=FALSE, as.is=TRUE)
    studentmat <- matrix(0, ncol=length(students$V1), nrow=length(prof_courses), dimnames=list(prof_courses, students$V1))
    for (i in 1:length(students$V1)){
      temp <- read.csv(paste0(students$V1[i], ".csv"), as.is=TRUE)
      for (j in 1:length(temp$Course)) {
        studentmat[row.names(studentmat)==temp$Course[j], i] <- 1
      }
    }
    
    if (length(prof_courses)>0) {
      output$tabs1 <- renderUI({
        tabs <- list(NULL)
        currentStudents <- vector("list", length(prof_courses))
        for (i in 1:length(prof_courses)) {
          for (j in 1:length(students$V1)){
            temp <- read.csv(paste0(students$V1[j], ".csv"), as.is=TRUE)
            if (prof_courses[i] %in% temp$Course) {
              currentStudents[[i]] <- c(currentStudents[[i]], students$V1[j])
            }
          }
          tabs[[i]] <- tabPanel(prof_courses[i], renderPrint(currentStudents[[i]])
                                
          )
        }
        tabs[[length(prof_courses)+1]] <- tabPanel("Summary") 
        
        do.call(tabsetPanel, tabs)
      })
    }
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

