# Project: microsamplingDesign
# 
# Author: ablommaert
###############################################################################

## settings to display (repetition of already set parameters)
boolDevelopmentRankTimePoints  <-  FALSE  
nCurvesDataTimePoints          <-  30
defaultGridPoints              <-  25 #faster with less grid points but less precise 


## for development ( fix input)
if( boolDevelopmentRankTimePoints ) { cat( " TODO connect connect inputs into rank time points") }
pkModelObject                  <-  reactive( {
   if( boolDevelopmentRankTimePoints) { 
     return(getExamplePkModel()  )
   }
   pkModelInApp()
})


setOftimePointsObject          <-  reactive({
    if( boolDevelopmentRankTimePoints ) {
    return( getExampleSetOfTimePoints( 0:10 ) )  
  }
  setOfTimePoints()
   
})

## generate data

dataForTimePoints        <-  reactive({
    validate(
      need( input$getDataTimePoints > 0 , "Click on 'Generate data to rank time points'") 
    )
    
    isolate({
        getPkData( pkModelObject()  , 
          timePoints = getTimePoints( setOftimePointsObject() ) , 
          nSubjectsPerScheme = input$nSubjectsPerSchemeTimePoints ,
          nSamples           = input$nSamplesForTimePoints , 
          nCores = coresInAppliation
        )
      })
  })

##  output
  #  table with timePoints
output$tableTimePointForRanking    <-  DT::renderDataTable({
     getData( setOftimePointsObject() ) 
  }  )
  #  plot of generated data


output$plotDataTimePoints     <-  renderPlot({
    dataToPlot                <-  dataForTimePoints()
    plotObject( dataToPlot , nCurves = nCurvesDataTimePoints )
  })

  # rank time points 

integrationSamples             <-  reactive({
    input$nSamplesForTimePoints # by defayult integration samples 
  })

 
nGridPoints                    <-  reactive({ defaultGridPoints })

rankedTimePoints          <-  reactive( {
    
    
    validate(
      need( input$rankTimePoints > 0 , "Click on 'Rank time points' " )
    )
    
    
    isolate({
        Pkdata                <-  dataForTimePoints()
        rankObject( object = setOftimePointsObject() ,  pkData = dataForTimePoints() ,
          nGrid = nGridPoints() ,  nSamplesAvCurve = integrationSamples()  ,
          nCores = coresInAppliation
          )
      })
  })

   # output ranked time points
     #  ranking
output$tableRankingTimePoints    <-  DT::renderDataTable({
    setOfSchemesRanked      <-  rankedTimePoints()
    rankingTimePoints       <-  getRanking( setOfSchemesRanked  )
     rankingTimePoints 
  }  )

     # calculation for table

getRankingTable             <-  reactive({
    setOfSchemesRanked      <-  rankedTimePoints() 
    nSelect                 <-  min( input$topTimePointsToSelect , nrow( setOfSchemesRanked ) )
    ranking                 <-  getRanking( setOfSchemesRanked  )
    rankingToSelect         <-  ranking[ ( 1 : nSelect )  ,  ] 
    indTopTimePoints        <-  getTopNRanking( ranking , nSelect = nSelect )
    timePointsSelect        <-  setOfSchemesRanked[ indTopTimePoints , , drop = FALSE ]
    selectedTimePointsWithRank  <-  data.frame( rankingToSelect ,  timePointsSelect )
    selectedTimePointsWithRank$criterion     <-  round( selectedTimePointsWithRank$criterion , NDIGITSROUND )
    selectedTimePointsWithRank
  })

     # ordered ranked time points 
output$tableRankedTimePoints  <-  DT::renderDataTable( getRankingTable() , rownames = TRUE  ,
      server = TRUE , selection = list( mode = 'single' , selected = 1  )  )
      


## Selected time point TO DO change it to data table selection

selectedTimePointOption       <-  reactive({
    validate( need( ! is.null(input$tableRankedTimePoints_rows_selected) , "Click on a row to select a time point option" ) )
    
    isolate({
        timePointsRanked          <-  rankedTimePoints()
        selectedTimePoints        <-  extractByRank( timePointsRanked ,  input$tableRankedTimePoints_rows_selected  )
        selectedTimePoints

      })
    })

output$selectedTimePoint      <- renderTable({
    selectedTimePointOption()
  })
  
  
  
  
  
  
  