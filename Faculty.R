library(shiny)
library(DT)
library(shinyjs)

ui <- fluidPage(
  
  useShinyjs(),
  
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
    select_extra <- c("first", "second", "third", "fourth", "fifth", "sixth", "seventh", "eighth", "ninth", "tenth")
    chosen <- vector("list", length(prof_courses))
    
    if (length(prof_courses)>0) {
      output$tabs1 <- renderUI({
        tabs <- list(NULL)
        studentInfo <<- list(NULL)
        currentStudents <- vector("list", length(prof_courses))
        for (i in 1:length(prof_courses)) {
          for (j in 1:length(students$V1)){
            temp <- read.csv(paste0(students$V1[j], ".csv"), as.is=TRUE)
            if (prof_courses[i] %in% temp$Title) {
              currentStudents[[i]] <- c(currentStudents[[i]], students$V1[j])
            }
          }
          
          if (length(currentStudents[[i]])>0) {
            DF <- data.frame(matrix(0,nrow = length(currentStudents[[i]]), ncol=8))
            names(DF) <- c( "Student", "Year", "Major", "Taken", "WhenTaken", "Professor", "Grade", "Suitable")
            
            DF$Student <- currentStudents[[i]]
            
            for (j in 1:length(currentStudents[[i]])){
              temp <- read.csv(paste0(currentStudents[[i]][j], "_preferences", ".csv"), as.is=TRUE)
              
              DF[j,4:8] <- temp[temp$Title==prof_courses[i], 2:6]
              
              temp2 <- read.csv(list.files(pattern= paste0(currentStudents[[i]][j], '_', '[^p]')), as.is=TRUE)
              
              DF$Student[j] <- paste0(temp2$first_name, " ", temp2$last_name)
              
              DF$Year[j] <- temp2$year
              
              DF$Major[j] <- temp2$major
            }
            
            
            names(DF) <-  c("Student Name", "Student's Year", "Student's Major",
                            "Has the student taken the course?",
                            "When did they take it?",
                            "Who was their professor?",
                            "What was their grade?",
                            "Why they think they are suitable")
            
            
            studentInfo[[i]] <<- DF
          }
        }
        
        
        
        tabs <- lapply(1:length(prof_courses), 
                       function(x) tabPanel(prof_courses[x],  br(), numericInput(paste0("optNum","_",x), "How many ULAs would you like?", min=0, max=10, value=NA, width='10%'),
                                            lapply(1:(min(length(select_extra), length(currentStudents[[x]]))), function(y) {
                                              if (y==1) {selectizeInput(paste0(select_extra[y],"_",x), label=paste0("Select your ", select_extra[y], " choice"), selected=" ", choices=c(" ", studentInfo[[x]][,'Student Name']))}
                                              else {hidden(selectizeInput(paste0(select_extra[y],"_",x), label=paste0("Select your ", select_extra[y], " choice"), selected=" ", choices=c(" ", studentInfo[[x]][,'Student Name'])))}
                                            }),
                                            textOutput(paste0("current_choices",x)),
                                            br(),
                                            if (length(currentStudents[[x]])>0) {
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
        
        # Show or hide different rankings
        
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
    
    # Show more ranking fields and update list of rankings
    
    lapply(1:length(prof_courses), function(x) {
      extra_new <- paste0(select_extra, "_", x)
      lapply(1:(length(select_extra)-1), function(y) {observeEvent(input[[extra_new[y]]], {
        if(input[[extra_new[y]]] != " ") {
          show(extra_new[y+1])
          chosen[[x]][y] <<- input[[extra_new[y]]]
          print(chosen[[x]])
          output[[paste0('current_choices',x)]] <- renderPrint(data.frame('Rankings'=chosen[[x]]))
          updateSelectizeInput(session, extra_new[y+1], choices=c(" ", studentInfo[[x]][,'Student Name'][!(studentInfo[[x]][,'Student Name']) %in% chosen[[x]]]))
        }
      })})
    })
    
  })
  
  
  
}

shinyApp(ui, server)

