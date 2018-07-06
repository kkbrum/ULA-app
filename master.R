library(shiny)
library(shinyjs)

#if (as.numeric(as.Date("2018-08-30") - Sys.Date()) >= 0 & as.numeric(as.Date("2018-08-30") - Sys.Date()) <= 7) {

#Currently for trial purposes the first line is amended
if (as.numeric(as.Date("2018-08-30") - Sys.Date()) >= 0 ) {
  open_to <- "<b>students</b>. If this is you, please press 'begin' below"
} else if (as.numeric(as.Date("2018-09-04") - Sys.Date()) >= 0 & as.numeric(as.Date("2018-09-04") - Sys.Date()) <= 4) {
  open_to <- "<b>faculty</b>. If this is you, please press 'begin' below"
} else if (as.numeric(as.Date("2018-09-06") - Sys.Date()) >= 0 & as.numeric(as.Date("2018-09-06") - Sys.Date()) <= 1) {
  open_to <- "<b>administrators</b>. If this is you, please press 'begin' below"
} else if (as.numeric(as.Date("2018-09-10") - Sys.Date()) >= 0 & as.numeric(as.Date("2018-09-10") - Sys.Date()) <= 3) {
  open_to <- "<b>students</b>. If this is you, please press 'begin' below"
} else {open_to <- "no one. Please return when the system opens"}


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
  
  titlePanel("ULA Matching System"), 
  HTML("The application will be open to students between August 23rd and August 30th. </br> Faculty will have from August 31st to September 4th to submit preferences. </br> Decisions will be released September 7th and must be accepted by September 10th. </br> </br>"),
  HTML(paste0("Today is <b>", Sys.Date(), "</b>.</br>")),
  HTML(paste0("The system is currently open to ", open_to, ".</br>")),
  
  
  
  # Log in box ----
  
  mainPanel(id="startPage", 
            fluidRow(
              column(6,
                     actionButton("begin", "Begin!")
                     
              )
            )
  )
)

server <- function(session, input, output) {
  if (open_to == "no one. Please return when the system opens") {
    shinyjs::hide("begin")
  }
  
  observeEvent(input$begin, {
    # I would love to launch an app here
  })
}

shinyApp(ui, server)