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
      
      selectInput("ent", 
                  label = "Choose a company to debut under:",
                  choices = c("Big Hit Music",
                              "JYP Entertainment",
                              "SM Entertainment",
                              "YG Entertainment",
                              "Other")
      ),
      
      conditionalPanel(
        condition = "input.ent=='Other'",
        textInput("customent", "Enter Company Name")
      ),
      
      sliderInput("range2", 
                  label = "Years Active",
                  min = 0, max = 30, 
                  value = 0),
      
      selectInput("nat", 
                  label = "Where is your idol persona from?",
                  choices = c("South Korea",
                              "Somewhere else!")
      ),
      
      textInput('input', 'Enter username'), 
      #verbatimTextOutput('output')
      
      actionButton("click", "Generate!")
    ),
    
    
    
    mainPanel(
      h4(textOutput("title")),
      textOutput("selected_var"),
      textOutput("age_out"),
      textOutput("min_max"),
      textOutput("output"),
    )
  )
)


# Define server logic
server <- function(input, output, session) {
  
  
  observeEvent(input$range,
               {
                 updateSliderInput(session, "range2",
                                   max = input$range-12)
               })
  
  output$title <- renderText({
    paste0("Results for @",input$input)
  })
  
  output$selected_var <- renderText({ 
    input$click
    req(input$click)
    isolate(paste("You have selected", input$var))
  })
  
  output$age_out <- renderText({ 
    input$click
    req(input$click)
    isolate(paste("You have chosen age", input$range))
  })
  
  observeEvent( req(input$ent == "Other"), {
    
    output$min_max <- renderText({ 
      input$click
      req(input$click)
      isolate(paste("You have chosen to debut with", input$customent))
    })
  })
  
  observeEvent( req(input$ent != "Other"), {
    
    output$min_max <- renderText({ 
      input$click
      req(input$click)
      isolate(paste("You have chosen to debut with", input$ent))
    })
  })
  
  
  
  library(tidyverse)
  
  usernamealyser <- function(username, age, company) {
    
    if( nchar(username) < 8){ 
      fame = 1
    } else {
        if( between(nchar(username), 8, 20)){
          fame = 2
        }
      else fame = 1
    }
    
    fame = fame + age
    
    if(company == "Big Hit Music"){
      fame = fame ^ 2
    } else if( company == "YG Entertainment"){
      fame = round(fame ^ 1.75, digits = 0)
    } else if( company == "SM Entertainment" | company == "JYP Entertainment"){
      fame = round(fame ^ 1.5, digits = 0)
    } else fame = fame
    
    followers = fame * sample(700:10000, 1)
    
    congratulations <- paste0("Your predicted number of followers is: ", followers, "!")
    
    print(congratulations)
    
  }
  

  output$output <- renderText({
    #req(input$input)
    input$click
    req(input$click)
    isolate(usernamealyser(input$input, input$range, input$ent))
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
