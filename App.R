library(shiny)

ui <- fluidPage(
  
  titlePanel("Student View"), 
  
  fluidRow(
    column(width=7, 
           mainPanel(width=12,
                     tabsetPanel(type = "tabs",
                                 tabPanel("My Info", 
                                          textInput("netid", "NetID (New Users Only)", "", width="60%"),
                                          textInput("new_pin", "Create a 4-digit pin for future login", "", width="60%"),
                                          textInput("first_name", "First Name", "", width="60%"),
                                          textInput("last_name", "Last Name", "", width="60%"),
                                          selectInput("year", "Class Year", 
                                                      c(2018, 2019, 2020, 2021), width="60%"),
                                          textInput("major", "Major", "", width="60%"),
                                          textAreaInput("why", 
                                                        "Why do you want to serve as a ULA?", 
                                                        "", width="60%", height="60%"),
                                          actionButton("submit1", "Submit")
                                 ),
                                 tabPanel("Course Preferences",  tableOutput("courses")),
                                 tabPanel("Summary", actionButton("submit", "Submit"))
                     )
           )
    ),
    
    column(width=5, 
           sidebarPanel(width=12,
                        textInput("username", "Username", ""),
                        textInput("pin", "4-digit Pin", "")
           )
    )
  )
)

server <- function(input, output) {
  
  observeEvent(input$submit1, {
    write.csv(as.data.frame(cbind("netid"=input$netid, 
                                  "new_pin"=input$new_pin, 
                                  "first_name"=input$first_name, 
                                  "last_name"=input$last_name,
                                  "year"=input$year, 
                                  "major"=input$major, 
                                  "why"=input$why)
    ), 
    paste0(input$netid, "_", input$new_pin, ".csv")
    )
  })
  output$courses <- renderTable(read.csv("courses.csv")
  )
  
}


shinyApp(ui, server)
