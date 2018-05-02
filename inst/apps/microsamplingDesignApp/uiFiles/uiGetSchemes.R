# Project: microsamplingDesign
# 
# Author: ablommaert
###############################################################################


tabPanel( "Generate possible schemes" ,
  
tags$br()  ,
#
sidebarLayout(
  
  ## user inputs 
  sidebarPanel( width = 5 ,
    h3("Scheme dimensions") ,
    
    h4( "Number of subjects" ) ,
#    numericInput( "minNSubjects" , "min" , min = 1 , value = 1 , step = 1  ) ,
#    numericInput( "maxNSubjects" , "max" , min = 1 , value = 1 ,  step = 1 ) ,
     numericInput( "nSubjects" , "Number of subjects" , min = 1 , value = 4 ,  step = 1 ),
    
    h4( "Number of observations per subject" ) ,
    
    numericInput( "minObsPerSubject" , "min" , min = 1 , value = 4 , step = 1 ) ,
    numericInput( "maxObsPerSubject" , "max" , min = 1 , value = 5 , step = 1 ) ,
    
    h4("Repetition of individual schemes") ,
    helpText("Warning: repeating individual schemes will greatly increase the possible number of schemes") ,
    
    numericInput( "maxRepetitionIndSchemes" , "Maximum number of repetitions individual schemes",
      min = 1 , value = 1 , step = 1 ) ,
    
    h3("Scheme constraints") ,
    actionLink(inputId = "helpSchemeConstraints" , label = "Scheme constraints", icon = icon("question-circle")  ) ,
    
    conditionalPanel( "input.helpSchemeConstraints % 2 == 1" ,
      wellPanel(
        HTML('<ul type="disc">
            <li> <em> minObsPerTimePoint </em> mimimum observations in the scheme per time point  </li>
            <li>  <em> maxConsecSamples </em> maximum of consecutive samples per animal  </li>
            <li> exactNumberObsPerTimePoint <em>  </em> exact number of samples per time point ( do not use together with <em> minObsPerTimePoint </em>) </li>
            </ul>')     
        ) 
    ) ,
    tags$br() ,
    rHandsontableOutput( "tableSchemeConstraints" ) ,
    actionButton( "resetSchemeConstraints", "Reset scheme constraints" ) ,
##    
#    tags$br() ,
#    
    h3( "Generate schemes" ) ,
    
    actionButton( inputId = "checkNSchemes" , label = "Check number of schemes before constraints" , icon = icon("gears")  ) ,
    helpText("Too large a number will slow down computation, reconsider scheme dimensions") ,
    actionButton( inputId = "calcSchemes" , label = "Generate schemes " , icon = icon("gears")  )
  ) ,
#  
#  
#  ##  display results 
  mainPanel(  width = 5 ,
   h3( "Time points" ) ,
   wellPanel(
     textOutput( "selectedTimePoints" )
     ) ,
#   h3("Scheme constraints") ,  # for debugging 
#   tableOutput( "tableSchemeConstrFormat" ) ,
   
   h3( "Number of schemes before applying constraints" ) ,
   wellPanel(
     textOutput( "nrOfSchemesNoConstraints" )
     ) ,
   helpText( "If too large a number ( 100,000 ), reconsider 'scheme dimensions' " ) ,
   
   h3("Number of schemes meeting constraints" ) ,
   wellPanel(
      textOutput( "nrOfSchemes" ) 
   ) ,
   h3( "Schemes meeting constraints" ) ,
   addSpinner( DT::dataTableOutput( "setOfSchemesTable" ) )
  )
)
)
