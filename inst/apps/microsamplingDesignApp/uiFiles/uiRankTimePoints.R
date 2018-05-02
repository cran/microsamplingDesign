# Project: microsamplingDesign
# 
# Author: ablommaert
###############################################################################


tabPanel( "Rank time points" ,
  
  tags$br() ,
  sidebarLayout( 
    sidebarPanel( width = 5 ,
      
      ## Data generation 
      h3( "Data generation" ),
      numericInput( "nSubjectsPerSchemeTimePoints" , label = "Number of animals per scheme" , min = 1 , value = 5  ) ,
      numericInput( "nSamplesForTimePoints"  , label = "Number of simulation samples" , value = 1000 , min = 1 , max = 10000 ),
      helpText( "Greater number of samples will increase simulation time" ) ,
      actionButton( "getDataTimePoints" , "Generate data to rank time points" , icon = icon( "cogs" ) ) ,
  
      tags$br() ,
      
      ## ranking settings
      h3( "Rank time points " ) ,
      actionButton( "rankTimePoints" , "Rank time points" , icon = icon( "cogs" ) ) ,
#      actionLink("rankTimePointsSettings" , label = "Show ranking settings" ,
#        icon = icon("cog") ) ,
#      conditionalPanel( "input.rankTimePointsSettings % 2 == 1"  ,
#         numericInput( "nGrid" , "Number of grid points" , value = 50 , min = 10 , max = 100   ) ,
#        numericInput( "nSamplesAvCurve" ,  "Number of samples to integrate out average curve" , value = 1000 , min = 50 , max = 10000 )
#        ) ,
    
             
     ## extract top ranked scheme
     h3( "Choice of time points" ) ,
     numericInput( "topTimePointsToSelect" , "Number of top time point options to display" , min = 1 , value = 10 , step = 1 )
      ) ,
##    
    mainPanel( width = 5 , style = "overflow-y:scroll; max-height: 800px" ,
#      ## time points
      h3( "Time points options to rank" ) , 
      DT::dataTableOutput( "tableTimePointForRanking" )  , 
      tags$br() ,
#      ## data
      h3( "Simulated data") ,
      addSpinner( plotOutput("plotDataTimePoints") )  ,
      helpText("Only a selection of data is shown" ) , 
      tags$br() ,
#      
#      ## ranked time points 
      h3( "Time point ranking" ) ,
      addSpinner( DT::dataTableOutput( "tableRankedTimePoints" ) ) ,
      tags$br() ,
      
      
      h3( "Chosen sampling points" ) ,
      wellPanel(
        tableOutput( "selectedTimePoint"  )
        )
    )# end maim panel
##    
    
    )
)