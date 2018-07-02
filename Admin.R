library(shiny)
# devtools::install_github('ayayron/shinydnd')
library(shinyDND)
library(shinyBS)

# source("Matching.R")

student_preferences <- readRDS("student_preferences.RDS")

assignments <- read.csv("Assignments.csv", as.is=TRUE)
demand <- read.csv("Demand.csv", as.is=TRUE)
# Make this list be all the unassigned and assigned people
students <- names(student_preferences)
unassigned <- students[!students %in% assignments$student]
courses <- c(unique(assignments$course), "unassigned")
course_assignments <- vector("list", length(courses))
names(course_assignments) <- courses
for (course in courses) {
  course_assignments[[course]] <- assignments[assignments$course == course,"student"]
}
course_assignments[["unassigned"]] <- unassigned

# for clicking purposes later
s <- rep(list(FALSE), length(students))
names(s) <- students

ui <- shinyUI(
  
  fluidPage(
    # HTML formatting for notification
    tags$head(
      tags$style(
        HTML(".shiny-notification {
             position:fixed;
             top: calc(30%);
             left: calc(50%);
             width: calc(20%);
             }",
             "th, td {
             padding-right: 12px;
             }"
        )
      )
    ),
    
    sidebarLayout(
      sidebarPanel(
        h2("Courses"),
        fluidRow(
          column(6,
                 lapply(courses[1:ceiling((length(courses)-1)/2)], function(x) list(uiOutput(x), br(), uiOutput(paste0(x, "_list")), br()))
          ),
          column(6,
                 lapply(courses[(ceiling((length(courses)-1)/2)+1) : (length(courses)-1)], function(x) list(uiOutput(x), br(), uiOutput(paste0(x, "_list")), br()))
          )
        )
      ),
      
      mainPanel(
        h2("Students"),
        "To unassign a student, press on their button and hit the unassign button. To assign a student to a course, click their name and then click on the course name.",
        br(),
        br(),
        "To view a student's preferences in order, click on their name.",
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
        actionButton("submit", "Submit new assignments")
      )
    )
  )
)


# server with reactive for observing reactive drop event
server <- shinyServer(function(input, output,session) {
  
  clicked <- reactiveValues(s = s, change = FALSE)
  
  # Record whether students have been clicked
  lapply(students, function(x)
    observeEvent(input[[x]], 
                 clicked$s[[x]] <- !clicked$s[[x]]
    )
  )
  
  # Render buttons for students that change color when clicked
  lapply(students, function(x) {
    most_desired <- student_preferences[[x]]$Title[student_preferences[[x]]$Rank < 3]
    desired <- student_preferences[[x]]$Title[student_preferences[[x]]$Rank %in% c(3,4)]
    not_desired <- student_preferences[[x]]$Title[student_preferences[[x]]$Rank > 4]
    output[[paste0(x, "_prefs")]] <- renderUI(
      if(clicked$s[[x]]) {
      HTML(paste0(c("<table style='width=100%'> <tr> <th> Course     </th> <th> Taken      </th><th> Grade     </th><th> Reason </th><th> </tr><tr> <td>",
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
  lapply(courses[-length(courses)], function(x)
    output[[x]] <- renderUI({
      actionButton(inputId= x, label=x, style = "background-color: dodgerblue")
    })
  )
  
  # Render the lists of students in each course
  observeEvent(clicked$change, {
    lapply(courses[-length(courses)], function(x)
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
      for(course in courses) {
        if(student %in% course_assignments[[course]]) {
          course_assignments[[course]] <<- course_assignments[[course]][course_assignments[[course]] != student]
        }
      }
    }
  }
  
  lapply(courses, function(x) {
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
  
  observeEvent(input$submit, {
    final_assignments <- data.frame("student" = students, "course" = rep(NA, length(students)))
    for (course in courses) {
      for (student in course_assignments[[course]]) {
        final_assignments$course[final_assignments$student == student] <- course
      }
    }
    write.csv(final_assignments, file="Final_assignments.csv")
    showNotification("Submission successful!", duration=5, type="message")
  })
})

shinyApp(ui, server)