library(shiny)
# devtools::install_github('ayayron/shinydnd')
library(shinyDND)

assignments <- read.csv("Assignments.csv", as.is=TRUE)

# Make this list be all the unassigned and assigned people
students <- c("Maria", "Shah", "Katherine", assignments$student)
unassigned <- students[!students %in% assignments$student]

courses <- unique(assignments$course)

# for clicking purposes later
s <- rep(list(FALSE), length(students))
names(s) <- students


ui <- shinyUI(
  fluidPage(
    sidebarLayout(
      
      sidebarPanel(width =3, 
                   actionButton(inputId= paste0("course_", 0), label="Unassigned", style = "background-color: dodgerblue"),
                   br(),
                   br(),
                   uiOutput(paste0("course_", 0, "_list"))
      ),
      
      mainPanel(
        h2("Courses"),
        fluidRow(
          column(6,
                 lapply(1:ceiling(length(courses)/2), function(x) list(uiOutput(paste0("course_", x)), br(), uiOutput(paste0("course_", x, "_list"))))
          ),
          column(6,
                 lapply((ceiling(length(courses)/2)+1) : length(courses), function(x) list(uiOutput(paste0("course_", x)), br(), uiOutput(paste0("course_", x, "_list"))))
          )
        )
      )
    )
  )
)


# server with reactive for observing reactive drop event
server <- shinyServer(function(input, output,session) {
  
  clicked <- reactiveValues(s = s)
  
  # Record whether students have been clicked
  
  lapply(students, function(x)
    observeEvent(input[[x]], 
                 clicked$s[[x]] <- !clicked$s[[x]]
    )
  )
  
  # Render buttons for students that change color when clicked
  
  lapply(students, function(x)
    output[[x]] <- renderUI({
      if(clicked$s[[x]]) {
        actionButton(inputId= x, label=x, style = "background-color:grey")
      }
      else {
        actionButton(inputId= x, label=x)
      }
    })
  )
  
  # Make buttons for the courses
  
  lapply(1:length(courses), function(x)
    output[[paste0("course_", x)]] <- renderUI({
      actionButton(inputId= paste0("course_", x), label=courses[x], style = "background-color: dodgerblue")
    })
  )
  
  # Render the lists of students in each course
  
  lapply(1:length(courses), function(x)
    output[[paste0("course_", x, "_list")]] <- renderUI({
      lapply( assignments[assignments$course == courses[x],"student"], function(x) list(uiOutput(x), br()))
    })
  )
  output[['course_0_list']] <- renderUI(
    lapply(unassigned, function(x) list(uiOutput(x), br()))
  )
  
  # Attempt to move a student into "course_0" which is the unassigned group
  # I think the problem has to do with having the same inputID already loaded elsewhere,
  #   so it won't regenerate in the unassigned category
  
  observeEvent(input[["course_0"]],
               {
                 unassigned <- c(unassigned, students[which(unlist(clicked$s))])
                 output[['course_0_list']] <- renderUI(
                   lapply(unassigned, function(x) list(uiOutput(x), br()))
                 )
                 
               }
  )
  
})

shinyApp(ui, server)