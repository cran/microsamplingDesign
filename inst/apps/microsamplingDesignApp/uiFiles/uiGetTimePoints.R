# Project: microsamplingDesign
# 
# Author: ablommaert
###############################################################################


tabPanel( "Generate possible time points" ,
 
  
  tags$br() ,
  
  sidebarLayout(
    ##  user input  
    sidebarPanel(  width = 5 ,
#      h3( "Possible time points" ) ,
#      numericInput( inputId = "maxTimePoint" , label = "Maximum sampling time in hours" , min = 0 , value = 5   ) ,
#      numericInput( inputId = "stepTime" , label = "Minimal interval between sampling time points in hours" , value = 1 , min = 0 ) ,
#      
      h3( "Time constraints" ) ,
      actionLink(inputId = "helpTimeConstraints" , label = "Time constraints", icon = icon("question-circle")  ) ,
      conditionalPanel( "input.helpTimeConstraints % 2 == 1" ,
        wellPanel(
          "Table containing number of user specified time points per time zone.
            Each row is a time zone." ,
            HTML('<ul type="disc">
                <li> <em>startTime</em>  the start time of each time zone assumed to be included in
            that zone</li>
                <li> <em>endTime</em> the end time of the zone. It is not part of the current zone
            but the start time of the next zone</li>
                <li> <em>nPointsPerZone</em> the number of time points to be chosen within each zone</li>
                <li> <em>timeInterval</em> mimimum time difference between time points</li>
                </ul>') , 
            "the last time point is always included and not specified in time constraints"
            ) ,
            tags$br()
          	
               
        
        ) ,
        
      rHandsontableOutput( "tableTimeConstraints" ) ,
#      textOutput('result'),
      actionButton( "clearTimeConstraints", "clear time constraints") ,

      tags$br() ,
      
      h3( "Generate time points" ) ,
      
      actionButton( inputId = "calcTimePoints" , label = "Generate time points" , icon = icon("gears")  )
      ) ,
      
    ##  display results 
    mainPanel(  width = 5 ,  
      h3("Possible time points") ,
      wellPanel( textOutput( "textFullTimePoints" ) ) ,
      tags$br() ,
#      tableOutput( "timeConstraints" ) , # for debugging
      #TODO add plot with time constraints 
      h3("Time point combinations meeting constraints") ,
      h4("Number of combinations") ,
      wellPanel( 
        textOutput( "nTimePoints" )
      ) ,  
      h4("Time point combinations") , 
      addSpinner( DT::dataTableOutput( "tableSetOfTimePoints" ) )      
      )
    ) 
) 