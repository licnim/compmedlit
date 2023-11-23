#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyjs)

# Define UI 
ui <- fluidPage(
  shinyjs:: useShinyjs(),
  tags$head(
    tags$style(HTML("
    body{
    background-color: #121212;
        color: #b3b3b3;
    }
    
    .help-block{
    color: #FFFFFF
    }
    
    .modal-content{
    background-color: #121212
    }
    
    .modal-header {
    border-bottom: 0 none;
    }

    .modal-footer {
    border-top: 0 none;
    }
    
    .well {
    background-color: #181818;
        }")
               )
  ),
  
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
                  choices = c("Afghanistan", "Albania", "Algeria", "Andorra", "Angola", "Antigua and Barbuda", "Argentina", "Armenia", "Australia", "Austria", "Azerbaijan", "Bahamas", "Bahrain", "Bangladesh", "Barbados", "Belarus", "Belgium", "Belize", "Benin", "Bhutan", "Bolivia", "Bosnia and Herzegovina", "Botswana", "Brazil", "Brunei", "Bulgaria", "Burkina Faso", "Burundi", "Cabo Verde", "Cambodia", "Cameroon", "Canada", "Central African Republic", "Chad", "Chile", "China", "Colombia", "Comoros", "Congo", "Costa Rica", "Croatia", "Cuba", "Cyprus", "Czechia", "Denmark", "Djibouti", "Dominica", "Dominican Republic", "Ecuador", "Egypt", "El Salvador", "Equatorial Guinea", "Eritrea", "Estonia", "Eswatini", "Ethiopia", "Fiji", "Finland", "France", "Gabon", "Gambia", "Georgia", "Germany", "Ghana", "Greece", "Grenada", "Guatemala", "Guinea", "Guinea-Bissau", "Guyana", "Haiti", "Honduras", "Hungary", "Iceland", "India", "Indonesia", "Iran", "Iraq", "Ireland", "Italy", "Jamaica", "Japan", "Jordan", "Kazakhstan", "Kenya", "Kiribati", "North Korea", "South Korea", "Kosovo", "Kuwait", "Kyrgyzstan", "Laos", "Latvia", "Lebanon", "Lesotho", "Liberia", "Libya", "Liechtenstein", "Lithuania", "Luxembourg", "Madagascar", "Malawi", "Malaysia", "Maldives", "Mali", "Malta", "Marshall Islands", "Mauritania", "Mauritius", "Mexico", "Micronesia", "Moldova", "Monaco", "Mongolia", "Montenegro", "Morocco", "Mozambique", "Myanmar", "Namibia", "Nauru", "Nepal", "Netherlands", "New Zealand", "Nicaragua", "Niger", "Nigeria", "North Macedonia", "Norway", "Oman", "Pakistan", "Palau", "Panama", "Papua New Guinea", "Paraguay", "Peru", "Philippines", "Poland", "Portugal", "Qatar", "Romania", "Russia", "Rwanda", "Saint Kitts and Nevis", "Saint Lucia", "Saint Vincent and the Grenadines", "Samoa", "San Marino", "Sao Tome and Principe", "Saudi Arabia", "Senegal", "Serbia", "Seychelles", "Sierra Leone", "Singapore", "Slovakia", "Slovenia", "Solomon Islands", "Somalia", "South Africa", "South Sudan", "Spain", "Sri Lanka", "Sudan", "Suriname", "Sweden", "Switzerland", "Syria", "Taiwan", "Tajikistan", "Tanzania", "Thailand", "Timor-Leste", "Togo", "Tonga", "Trinidad and Tobago", "Tunisia", "Turkey", "Turkmenistan", "Tuvalu", "Uganda", "Ukraine", "United Arab Emirates", "United Kingdom", "United States", "Uruguay", "Uzbekistan", "Vanuatu", "Vatican City", "Venezuela", "Vietnam", "Yemen", "Zambia", "Zimbabwe")
      ),
      
      textInput('input', 'Enter username'), 
      
      actionButton("click", "Generate!")
    ),
    
    
    
    mainPanel(
      h4(textOutput("title")),
      textOutput("selected_var"),
      textOutput("min_max"),
      textOutput("output"),
      textOutput("taunt")
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
  
  observeEvent(input$input,{
    if(nchar(input$input)>30)
    {
      showModal(modalDialog(
        title = "Error!",
        "Character limit exceeded!",
        easyClose = FALSE, 
        footer = modalButton("I understand")))
      shinyjs::hide("click")
    } else {
      shinyjs::show("click")
    }
      
  })
  
    
  observeEvent(input$input,{
    if(str_count(input$input, " |\\`|\\~|\\!|\\@|\\#|\\$|\\%|\\^|\\&|\\*|\\(|\\)|\\+|\\=|\\[|\\{|\\]|\\}|\\||\\\\|\\'|\\<|\\,|\\>|\\?|\\/|\"|\\;|\\:")>0)
    {
      showModal(modalDialog(
        title = "Error!",
        "Usernames must not contain spaces or special characters other than underscores or periods!",
        easyClose = FALSE, 
        footer = modalButton("I understand")))
      shinyjs::hide("click")
    } else {
      shinyjs::show("click")
    }
    
  })
  
  output$title <- renderText({
    paste0("Results for @",input$input)
  })
  
  output$selected_var <- renderText({ 
    input$click
    req(input$click)
    isolate(paste("You are an", input$range,"year old", tolower(input$var),"from",input$nat))
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
  
  usernamealyser <- function(username, age, company, gender, nationality) {
    
    if( nchar(username) < 8){ 
      fame = 1
    } else {
        if( between(nchar(username), 8, 20)){
          fame = 2
        }
      else fame = 1
    }
    
    fame = fame + age - (5*str_count(username, "_"))
    
    if( gender == "Male"){
      fame*2
    }
    
    if( nationality != "South Korea"){
      fame*3
    }
    
    if(company == "Big Hit Music"){
      fame = fame ^ 2
    } else if( company == "YG Entertainment"){
      fame = round(fame ^ 1.75, digits = 0)
    } else if( company == "SM Entertainment" | company == "JYP Entertainment"){
      fame = round(fame ^ 1.5, digits = 0)
    } else fame = fame
    
    
    
    followers = fame * sample(700:10000, 1)
    
    congratulations <- paste0("Your predicted number of followers is: ",followers,"!")
    
    print(congratulations)
    
  }

  output$output <- renderText({
    #req(input$input)
    input$click
    req(input$click)
    isolate(usernamealyser(input$input, input$range, input$ent, input$var, input$nat))
  })
  
  
  output$taunt <- renderText({
    input$click
    req(input$click)
    isolate(paste0("Experiment with the options available and try to beat this number!"))
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
