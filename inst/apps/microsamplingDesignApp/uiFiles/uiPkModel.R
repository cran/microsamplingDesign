# Project: microsamplingDesign
# 
# Author: ablommaert
###############################################################################


tabPanel( "Construct Pk-model" ,
  
  
  
  tags$br(),
  
  
  sidebarLayout( 
    sidebarPanel( width = 5 ,
      h3( "Data input" ) ,
      h4( "Model parameters" ) , 
      DT::dataTableOutput( "parameters" ),
      tags$br(),
      actionButton( "changeParam" , "Modify parameters" , icon = icon( "wrench" )  ),
      tags$br(),
      bsModal("modalnew", "Change Parameters", "changeParam", size = "large",
       helpText("Click to change") ,
       rHandsontableOutput( "modelParameterTable" ) 
      ),      
      h4("Dosing info"),
      rHandsontableOutput( "dosingInfoTable" ) ,
      tags$br(),
      actionButton( "calcSampleCurves" , "Generate example curves" , icon = icon("gears")   ) ,
      
#      h4("Additive error") ,
#      numericInput( inputId = "cvError" , label = "coefficient of variation of additive error " ,
#        value = 0 , min = 0 , max = 1 , step = 0.01),
#      tags$br(),
      
      ## plot settings (hidden)
      tags$br(),
      actionLink(inputId = "plotSettings" , label = "Graphical settings", icon = icon("cog")  ),
      conditionalPanel("input.plotSettings % 2 == 1" ,
        ## MM-kinetic plot
        h3("Capacity dependent absorption and clearance"),
        h4("Absorption") , 
        numericInput( inputId = "maxDose"             ,  label = "Maximum dose"                 ,  value =  5 ) ,
        numericInput( inputId = "yMaxAbsorption"      ,  label = "Maximum y-value absorption"   ,  value =  7 ) ,
        h4("clearance") , 
        numericInput( inputId = "maxConcentration"    ,  label = "Maximum concentration"        ,  value =  5 ) ,
        numericInput( inputId = "yMaxClearance"       ,  label = "Maximum y-value clearance "   ,  value =  35 ) ,
        ##  sample curves in time 
        h3("Plasma concentration curves"),
        numericInput( inputId = "nCurves" , label = "Number of curves" , value = 7 ) ,
#        numericInput( inputId = "maxPlasmaConcentration" , label = "Maximum y-value plasma concentration" , value = 0.03 ) ,
        numericInput( inputId = "maxTime" , label = "Maximum time " ,  value = 7   ) ,
        numericInput( inputId = "timeIncrement" , label = "Time step size  " , value = 0.02 )
      )
      
    ) ,
    mainPanel( width = 5 , style = "overflow-y:scroll; max-height: 800px" ,
 
      
      h3("Model visualization") ,
      h4( "Capacity dependent absorption and clearance" ) ,
      helpText( "Michaelis-Menten kinetics" , "plots displayed for average parameters" ) ,
      plotOutput( "MMPlot" ),
      tags$br(),
      
      h3( "Simulate example data" ) ,
      helpText( "Profiles of individual samples from input parameters" ),
      helpText( "only a few  samples taken for illustration model (slow computations) " ) ,
      plotOutput( "samplePlot" )
      )
    )
    
)