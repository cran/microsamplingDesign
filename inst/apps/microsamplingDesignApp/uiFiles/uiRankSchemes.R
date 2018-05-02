# Project: microsamplingDesign
# 
# Author: ablommaert
###############################################################################


tabPanel( "Rank schemes" ,
  
  tags$br() ,
  sidebarLayout( 
    sidebarPanel(  width = 5 ,
      h3("Data generation") ,
      numericInput( "nSamplesForSchemes"  , label = "Number of simulation samples" , value = 1000 , min = 10 , max = 10000 ),
      helpText( "Take a small number for testing, and a large number e.g. 1000 for the final run" ) ,
      actionButton( "getDataSchemes" , "Generate data to  rank schemes" , icon = icon( "cogs" ) ) ,
      tags$br() ,
      
      ## ranking settings
      h3( "Rank Schemes " ) ,
      
      h4( "Objective function" ) ,
      helpText( "" ) ,
      numericInput("wAUC" , "Relative importance area under the curve (AUC)" , value = 50 , min = 0 ) ,
      numericInput("wCmax" , "Relative importance maximum concentration (Cmax)" , value = 50 , min = 0  ) , 
#      numericInput("wTmax" , "Relative importance time at maximum concentration (Tmax) ", value = 0 , min = 0 ) ,
      
      actionButton( "rankSchemes" , "Rank schemes" , icon = icon( "cogs" ) ) ,

      conditionalPanel( "input.rankSchemesSettings % 2 == 1"  ,
        # TO move inside if user able to modify ranking settings 
        actionLink("rankSchemesSettings" , label = "Show ranking settings" ,
          icon = icon("cog") ) ,
        
        h4( "Variance and scaling measures" ) ,
        textInput( "varianceMeasure" , "Variance measure" , value = "var"  ) ,
        textInput( "scaleWith" , "Scale part of objective function with" , value = "max" ) ,
        helpText("function to scale variance measures with prior to weighing, e.g. 'max', 'min' , 'mean'")
        
      ) ,
      
      tags$br() ,
      
      h3( "Choice of Scheme" ) ,
      numericInput( "topSchemesToSelect" , "Number of top schemes to display"  , min = 1 , value = 10 , step = 1  ),
      tags$br() ,
      

      
      tags$br() ,
      ## generate report
      
      conditionalPanel(condition = "(typeof output.selectedSchemeTable !== 'undefined' )" ,
        h3( "Summary report" ) ,
        actionButton(inputId = "generateReport", label = "Generate Report"),
        downloadButton("downloadReport", "Download report")
        )

     
      
   ) ,
    mainPanel(  width = 5 ,  style = "overflow-y:scroll; max-height: 800px" ,
      h3("Schemes to Rank ") ,
      h4( "Info schemes ") ,
      wellPanel(
        textOutput("schemeSummary") 
        ) ,
        tags$br() ,
        h4("Schemes"),
      DT::dataTableOutput( "setOfSchemesForRanking" ) ,
      tags$br() ,
      
      h3( "Simulated data" ) ,
      addSpinner( plotOutput( "plotDataSchemes" ) ) ,
      helpText("Only a selection of data is shown" ) , 
      tags$br() ,
      
      h3("Scheme ranking") ,
      addSpinner( DT::dataTableOutput( "tableSchemeRanking" ) ) ,
      tags$br() ,
      
      h3( "Chosen scheme" ) ,
      DT::dataTableOutput( "selectedSchemeTable" ) ,
      
      tags$br() 
      
    ) # end main panel
  )
)

