# Project: microsamplingDesign
# 
# Author: ablommaert
###############################################################################

shinyUI( 
  
  # side bar navigating sub ui's
  
  fluidPage(
    
#    titlePanel(
#      "Optimize microsampling schemes for non-compartmental analysis",
#      "Pkdesign"
#      )  , 
      
      useShinyjs(),
      
#      # For debugging
#      h4("Debugging & testing options"), 
#      fluidRow(
#        column(4, 
#          tags$b("Print objects"),
#          verbatimTextOutput("debug_text")
#        ),
#        column(4, 
#          tags$b("Connect with console"),
#          helpText(HTML("The console will display a Browse prompt: <code>Browse[1]></code>")),
#          helpText(HTML("Enter <code>c</code> at the prompt to stop communication with the console and resume with the shiny app")),
#          actionButton(inputId = "debug_console", label = "Connect with console")
#        ),
#        column(4,
#          tags$b("Save current state"),
#          verbatimTextOutput("debug_bookmark"),
#          tags$br(),
#          bookmarkButton()
#        )
#      ),
#      
#      tags$hr(),
#      
#      helpText(
#        h5(a(href = "http://shiny.rstudio.com", 
#            target = "_blank", "Shiny Support"), align = "right")
#      ),
      
      titlePanel(title = div(img(src = "logo.png", 
            float = "top", height = "60px", hspace = "50px"),
          "Optimize microsampling schemes for non-compartmental analysis"), 
        windowTitle = "Pkdesign" ),
      
      tags$br(),
    
  # pannels with steps in the algorithm   
    navlistPanel(
      "Pk model" ,
      source(file.path("uiFiles", "uiPkModel.R"), local = TRUE)$value ,
      "Time points" ,
      source(file.path("uiFiles", "uiGetTimePoints.R"), local = TRUE)$value ,
      source(file.path("uiFiles", "uiRankTimePoints.R"), local = TRUE)$value ,
      "Schemes" ,
      source(file.path("uiFiles", "uiGetSchemes.R"), local = TRUE)$value ,
      source(file.path("uiFiles", "uiRankSchemes.R"), local = TRUE)$value  , 
      widths = c(  2 ,  10 ) 
)
) 
)


