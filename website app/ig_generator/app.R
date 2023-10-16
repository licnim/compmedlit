#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
  titlePanel("Instagram Profile Simulator"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Build your idol persona!"),
      
      selectInput("var", 
                  label = "Choose a gender",
                  choices = c("Male", 
                              "Female")
                  ),
      
      sliderInput("range", 
                  label = "Age",
                  min = 13, max = 60,
                  value = 13),
      
      textInput('input', 'Enter username'), 
      #verbatimTextOutput('output')
      
      actionButton("click", "Generate!")
    ),
    
  
    
    mainPanel(
      h4(textOutput("title")),
      textOutput("selected_var"),
      textOutput("min_max"),
      textOutput("output")
      )
    )
  )


# Define server logic
server <- function(input, output) {
  
  output$title <- renderText({
    paste0("Results for @",input$input)
  })
  
  output$selected_var <- renderText({ 
    input$click
    req(input$click)
    isolate(paste("You have selected", input$var))
  })
  
  output$min_max <- renderText({ 
    input$click
    req(input$click)
    isolate(paste("You have chosen age", input$range))
  })

  
  library(tidyverse)
  
  usernamealyser <- function(username) {
    
    if( nchar(username) > 10){ 
      followers = sample(1000000:2000000,1)
    } else {
      followers = sample(0:1000000,1)
    }
      
    congratulations <- paste0("Your predicted number of followers is: ", followers, "!")
    
    print(congratulations)
    
  }
  
  
  output$output <- renderText({
    #req(input$input)
    input$click
    req(input$click)
    isolate(usernamealyser(input$input))
  })
  
                           
}

# Run the application 
shinyApp(ui = ui, server = server)
