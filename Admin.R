library(shiny)
# devtools::install_github('ayayron/shinydnd')
library(shinyDND)
library(shinyBS)

# ======================= BEGIN MARIA'S FAILED ATTEMPTS ========================

# # Load this object in (generated from Matching.R)
student_preferences <- readRDS("student_preferences.RDS")

# # Function that theoretically creates a pop-up with student preferences
# hover <- function(student) {
#   stu <- student_preferences$temp
#   bsPopover(id=input$student, 
#             title=student, 
#             content=(paste0(stu[, "Title"], ": ", stu[, "Rank"], collapse=" | ")),
#             placement="left")
# }
# 
# 
# # This doesn't work... at all, but I tried this in server. Needs a reactive
# # component and I don't think that buttons have fine enough id's currently
# lapply(students, hover)

# ======================= END MARIA'S FAILED ATTEMPTS ==========================

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
        actionButton(inputId= "unassigned", label="Unassign", style = "background-color: dodgerblue"),
        br(),
        br(),
        uiOutput(paste0("unassigned_list")),
        br(),
        hr(),
        plotOutput("legend", width ="100%"),
        actionButton("submit", "Submit new assignments")
      )
    )
  )
)


# server with reactive for observing reactive drop event
server <- shinyServer(function(input, output,session) {
  
  output$legend <- renderPlot({
    plot(NULL ,xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=0:1, ylim=0:1)
    legend("topleft", cex = 1.5, pch=c(15, 15, 15, 15, 0), col=c(rgb(66, 244, 78, alpha=180, maxColorValue=255), rgb(244, 241, 65, alpha=180, maxColorValue=255), rgb(244, 65, 65, alpha=180, maxColorValue=255), "grey", "black"), 
           legend=c("First or second choice", "Third or fourth choice", "Fifth or lower choice", "Unwilling to ULA this course", "No assignment"))})
  
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
        print(length(course_assignments[[x]]))
        print (demand$desired[demand$course ==x])
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
               lapply(students[1:(ceiling(length(students))/2)], function(x) list(uiOutput(x), br()))
        ), 
        column(6,
               lapply(students[((ceiling(length(students))/2)+1):length(students)], function(x) list(uiOutput(x), br()))
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