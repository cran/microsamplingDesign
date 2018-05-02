# Project: microsamplingDesign
# 
# Author: ablommaert
###############################################################################


### settings

NTIMECONSTRAINTS     <-  7





# vector of full time Points 
fullTimePoints        <-  reactive({
    timeInfo          <-  timeConstraintsFormat()
    timePointsPerZone <-  lapply( seq_len( nrow( timeInfo ) ) ,
      function( i ) {
        seq( timeInfo[ i , "startTime" ] , timeInfo[ i , "endTime" ] , by =  timeInfo[ i , "timeInterval" ] )
        
      } 
    )
    unique( unlist( timePointsPerZone ) )

  })




## test rhandsontable

baseDataFrame                      <-  data.frame(
                                      startTime          = rep( 0 , NTIMECONSTRAINTS ) ,
                                      endTime            = rep( 0 , NTIMECONSTRAINTS ) ,
                                      nPointsPerZone     = rep( 0 , NTIMECONSTRAINTS ) ,
                                      timeInterval       = rep( 0.5 , NTIMECONSTRAINTS )
                                  )
                                  
## default example time constraints
line1                              <-  c( 0 , 3 , 3 , 0.5 )
line2                              <-  c( 3 , 5 , 2 , 0.5 )
defaultExample                     <-  rbind( line1 , line2 )
colnames( defaultExample )         <-  colnames( baseDataFrame )
defaultTimeConstraints             <-  rbind( defaultExample  ,  baseDataFrame )[ 1:NTIMECONSTRAINTS , ]
rownames( defaultTimeConstraints ) <-  NULL

 

timeConstraints                    <-  reactiveValues( data =  defaultTimeConstraints )

observe({
    if( input$clearTimeConstraints > 0  )  {
      timeConstraints$data         <-  baseDataFrame
    }
    
  })

observe({
    if( !is.null( input$tableTimeConstraints  ))
      timeConstraints$data          <-  hot_to_r( input$tableTimeConstraints )
  })


output$tableTimeConstraints         <-  renderRHandsontable({
    rhandsontable( timeConstraints$data )
  })


timeConstraintsFormat               <-  reactive({
    colSelect                       <-  c( "startTime" , "endTime" , "nPointsPerZone" )
    inputTable                      <-  timeConstraints$data
    flagZeroRow                     <-  rowSums( abs( inputTable[ , colSelect ] ) ) == 0 
    tableFormat                     <-  inputTable[ !flagZeroRow ,  ]
    tableFormat
  })


## Generate Output

output$textFullTimePoints            <-  renderText({ 
    timePoints                        <-  fullTimePoints()
    formatTimePoints( timePoints )
  })

output$timeConstraints               <-  renderTable(
  timeConstraintsFormat()
)

# calculate only when good to go


#setOfTimePoints                      <-  data.table(  )

setOfTimePoints                       <-  reactive({
    validate(
      need( input$calcTimePoints > 0 , "Click on 'Generate time points'") 
    )
    isolate({    
        
        colSelect                        <-  c( "startTime" , "endTime" , "nPointsPerZone" ) 
        timeZones                        <-  timeConstraintsFormat()[ , colSelect ]
        getAllTimeOptions( timeZones = timeZones  , 
            fullTimePoints = fullTimePoints() 
        )
      }) 
  })


output$nTimePoints                   <-  renderText({
    nTimePoints                      <-  nrow( setOfTimePoints()  )
    format( nTimePoints , big.mark = "," ) 
  })

output$tableSetOfTimePoints           <-  DT::renderDataTable({

    allTimePoints                     <-  setOfTimePoints()
    pureTimePoints                    <-  getData( allTimePoints )
    tableTimePoints                   <-  data.table( pureTimePoints )
    cat( "test inside render table"  ,  getTimePoints(allTimePoints)  , "\n" )
    tableTimePoints
})





