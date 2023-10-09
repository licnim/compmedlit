library(shiny)
# Define UI ---- Shiny uses fluidpage() to create a layout onto which elements are added
ui <- fluidPage(
  
  #titlePanel() adds a title element
  titlePanel("Waiting Times between Eruptions at Old Faithful Geyser"),
  
  #while sidebarLayout() creates a layout below the title onto which elements can also be added
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
<<<<<<< Updated upstream
    position = "right",
  #one such element is sidebarPanel
    sidebarPanel("This is some text on the sidebar made with sidebarPanel()",
                 img(src = "testHB.gif", height = 200, width = 200)
    ),
  #another such element is mainPanel
    mainPanel(
      h4("This is some text on the main layout made with mainPanel()"),
=======
>>>>>>> Stashed changes
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Slider for the number of bins ----
      sliderInput(inputId = "bins",
                  label = tags$span(style="color: black;","Number of Bins:"),
                  min = 1,
                  max = 50,
                  value = 30),
      
    div("The height of each bin shows how many values from that data fall into that range.", style="color:black"),
    
    br(),
    
    div(h6("Depicted below is a .gif of the Old Faithful geyser."), style="color:black"),
    
    br(),
    
    img(src = "oldfaithful.gif", height = 350, width = 200),
    
      
      
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Histogram ----
      plotOutput(outputId = "distPlot")
      
<<<<<<< Updated upstream
      p("p creates a paragraph of text."),
      p("A new p() command starts a new paragraph. Supply a style attribute to change the format"),
      strong("strong() makes bold text."),
      em("em() creates italicized (i.e, emphasized) text."),
      br(),
      code("code displays your text similar to computer code"),
      div("div creates segments of text with a similar style. This division of text is all blue...!", style = "color:blue"),
        br(),
        p("span does the same thing as div, but it works with",
          span("groups of words", style = "color:blue"),
          "that appear inside a paragraph."), #span() is hence used within p()
       a(href="https://shiny.posit.co/r/getstarted/shiny-basics/lesson1/", "Click me!")
        
=======
>>>>>>> Stashed changes
    )
      
  
  )
)
<<<<<<< Updated upstream
# Define server logic ----
server <- function(input, output) {}

# Run the app ----
shinyApp(ui = ui, server = server)
=======

# Define server logic required to draw a histogram ----
server <- function(input, output) {
  
  # Histogram of the Old Faithful Geyser Data ----
  # with requested number of bins
  # This expression that generates a histogram is wrapped in a call
  # to renderPlot to indicate that:
  #
  # 1. It is "reactive" and therefore should be automatically
  #    re-executed when inputs (input$bins) change
  # 2. Its output type is a plot
  output$distPlot <- renderPlot({
    
    x    <- faithful$waiting
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    hist(x, breaks = bins, col = "navy", border = "cyan",
         xlab = "Waiting time to next eruption (in mins)",
         main = "Histogram of waiting times")
    
  })
  
}
>>>>>>> Stashed changes

# Create Shiny app ----
shinyApp(ui = ui, server = server)