library(shiny)

ui <- fluidPage(
  
  titlePanel("Student View"), 
  
  fluidRow(
    column(width=7, 
           mainPanel(width=12,
                     tabsetPanel(type = "tabs",
                                 tabPanel("My Info",
                                          br(),
                                          textInput("netid", "NetID (New Users Only)", "", width="60%"),
                                          textInput("new_pin", "Create a 4-digit pin for future login", "", width="60%"),
                                          textInput("first_name", "First Name", "", width="60%"),
                                          textInput("last_name", "Last Name", "", width="60%"),
                                          selectInput("year", "Class Year", 
                                                      c("", 2018, 2019, 2020, 2021), selected="", width="60%"),
                                          textInput("major", "Major", "", width="60%"),
                                          textAreaInput("why", 
                                                        "Why do you want to serve as a ULA?", 
                                                        "", width="60%", height="60%"),
                                          actionButton("submit1", "Submit")
                                 ),
                                 tabPanel("Course Preferences",  tableOutput("courses")),
                                 tabPanel("Summary", br(), htmlOutput("summarytext"), br(), actionButton("submit", "Submit"))
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
  
  output$summarytext <- renderUI({
    
    
    condition1 <- c(input$netid == "", input$new_pin == "", input$first_name == "",
                    input$last_name == "", input$year == "", input$major == "",
                    input$why == "")
    
    if(all(condition1)) { #All entries are blank
      expr = HTML("You haven't selected anything yet, silly!") 
      
    } else if(!any(condition1)) { #No entries are blank
      expr = HTML("Super duper! You've filled in all of the fields. You are ready to submit!") 
      
    } else {
      text <- character(7)
      text[1] <- paste("You have input your netid as", input$netid)
      text[2] <- "You have input a pin"
      text[3] <- paste("You have entered your first name as", input$first_name)
      text[4] <- paste("You have entered your last name as", input$last_name)
      text[5] <- paste("You have input your class year as", input$year)
      text[6] <- paste("You have input your major as", input$major)
      text[7] <- paste("You have input why you want to be a ULA")
      expr = HTML(paste(text[which(c(input$netid, input$new_pin, input$first_name, input$last_name, 
                                     input$year, input$major, input$why) != "")], collapse = "<br/>"))
    }
  })
  
}


shinyApp(ui, server)
