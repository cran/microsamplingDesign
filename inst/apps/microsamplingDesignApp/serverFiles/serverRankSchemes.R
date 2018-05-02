# Project: microsamplingDesign
# 
# Author: ablommaert
###############################################################################


boolDevelopmentRankSchemes    <-  FALSE
nCurvesDataSchemes            <-  30

## for development ( fix input)
if( boolDevelopmentRankSchemes ) { cat( " TODO connect connect inputs into rank schemes") }
pkModelObjectSchemes                  <-  reactive( {
    if( boolDevelopmentRankSchemes) { 
      return(getExamplePkModel()  )
    }
    pkModelInApp()
  })

setOfSchemesObject          <-  reactive({
    if( boolDevelopmentRankSchemes ) {
      return( getExampleSetOfSchemes() )  
    }
    setOfSchemes()   
  })


## construct objective function 

objectiveFunction              <-  reactive({
    data.frame( 
#      criterion = c("auc"       , "cMax"      , "tMax"      ) ,
#      weight    = c( input$wAUC , input$wCmax , input$wTmax )
      criterion = c("auc"       , "cMax"       ) ,
      weight    = c( input$wAUC , input$wCmax  )
       )
  })


## generate Data

dataForSetOfSchemes            <-  reactive({
    
    validate( 
      need( input$getDataSchemes > 0 , "Click on 'Generate data to rank schemes'" ) 
    )
    cat("generation data Started " , "\n")
    isolate({
      data <-   getPkData( pkModelObjectSchemes() ,
          timePoints = getTimePoints( setOfSchemesObject() ) ,
          nSubjectsPerScheme = getNSubjects( setOfSchemesObject()  ) ,
          nSamples = input$nSamplesForSchemes ,
          nCores = coresInAppliation
          )
          cat("data generated" , "n")
       data
          
  })
})


rankedSchemes                 <-  reactive({
    validate(
      need( input$rankSchemes > 0 , "Click on 'Rank schemes'"  )
      )
    
    isolate({
#        rankObject(
#          object          =  setOfSchemesObject() ,
#          pkData          =  dataForSetOfSchemes() ,  
#          objective       =  objectiveFunction() ,
#          varianceMeasure =  input$varianceMeasure ,
#          scaleWith       =  input$scaleWith,
#          nCores = coresInAppliation
#        )
            
     fastRankSchemes(
          object          =  setOfSchemesObject() ,
          pkData          =  dataForSetOfSchemes() ,  
          objective       =  objectiveFunction() ,
          nCores = coresInAppliation
        )
        
      })
  })


##  outputs

  #  before ranking
output$setOfSchemesForRanking       <-  DT::renderDataTable({
   flattenSetOfSchemes( object = setOfSchemesObject() )
  })

output$schemeSummary                <-  renderText({
    summary( setOfSchemesObject() , printToConsole = FALSE )     
  })

  # Data

output$plotDataSchemes               <-  renderPlot({
    plotObject( dataForSetOfSchemes() , nCurves = nCurvesDataSchemes , addZeroIsZero = TRUE )
  })

  # Ranking

formatedSchemeRanking                <-  reactive({
    ranking                          <-  getRanking( rankedSchemes()  )
    namesColToFormat                 <-  c( "var_auc" , "var_cMax" , "var_tMax" , "combinedCriterion" )
    indColToFormat                   <-  colnames( ranking ) %in%  namesColToFormat
    ranking[ , indColToFormat ]      <-  round( ranking[ , indColToFormat ] , NDIGITSROUND )
    ranksToSelect                    <-  1 : input$topSchemesToSelect
    ranking[ ranksToSelect , , drop = FALSE ]
  })


output$tableSchemeRanking            <-  DT::renderDataTable(
  formatedSchemeRanking()   ,  server = TRUE , selection =  list( mode = 'single' , selected = 1  ) 
  )


  # Selected schemes


selectedScheme      <-  reactive({
    validate( need( ! is.null(input$tableSchemeRanking_rows_selected) , "Click on a row to select a Scheme" ) )
    
    isolate({
        schemesRanked             <-  rankedSchemes()
        selectedSchemes           <-  extractByRank( schemesRanked ,  input$tableSchemeRanking_rows_selected  )
        selectedSchemes
      })
  })

output$selectedSchemeTable        <- DT::renderDataTable({
    selectedScheme()
  })
