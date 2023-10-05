library(shiny)
# Define UI ---- Shiny uses fluidpage() to create a layout onto which elements are added
ui <- fluidPage(
  
  #titlePanel() adds a title element
  titlePanel("This is a title made with titlePanel()"),
  
  #while sidebarLayout() creates a layout below the title onto which elements can also be added
  sidebarLayout(
    position = "right",
  #one such element is sidebarPanel
    sidebarPanel("This is some text on the sidebar made with sidebarPanel()",
                 img(src = "testHB.gif", height = 200, width = 200)
    ),
  #another such element is mainPanel
    mainPanel(
      h4("This is some text on the main layout made with mainPanel()"),
    
      h6("This is some smaller text on the main layout made with mainPanel()"),
      
      h1("First level title"),
      h2("Second level title"),
      h3("Third level title"),
      h4("Fourth level title"),
      h5("Fifth level title"),
      h6("Sixth level title"),
      
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
        
    )
      
  
  )
)
# Define server logic ----
server <- function(input, output) {}

# Run the app ----
shinyApp(ui = ui, server = server)

