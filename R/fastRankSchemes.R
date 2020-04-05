# Faster version of rank schemes to use in application with all options fixed
# 
# Author:Adriaan Blommaert
#  Changes 
#     + variance measure fixed 
#     + skipTests = TRUE 
###############################################################################



if( 0 == 1 ){
  ## test influence skip test
  library( microbenchmark )
  library( matrixStats )
  library( parallel )
  library( plyr )
  object     <-  getSetOfSchemes( 3 , 3,3,3, 1:6 )
  pkData     <-  getPkData( getExamplePkModel() , 1:6 , 3 , 15 )
  objective  <-  getExampleObjective()[-3 , ]
  
  ## test equality 
  1+1
  test1  <-  rankObject( object , pkData , objective , skipTests = FALSE  )
  test2  <-  rankObject( object , pkData , objective , skipTests = TRUE  )
  test3  <-  fastRankSchemes( object , pkData, objective )  
  
  identical( test1 , test2)
  identical( test1 , test3 )
  all.equal( getRanking( test1 )[,-1] , getRanking(test3 )[,-1] ) # not identical, precission difference
  rankOrig               <-  getRanking( test1 )
  rankNew                <-  getRanking(test3 )
  
  
  impactSkipTests    <-  microbenchmark(
      rankObject( object , pkData , objective , skipTests = FALSE  ),
      rankObject( object , pkData , objective , skipTests = TRUE  ) ,
      fastRankSchemes( object , pkData, objective )  ,
      times = 5 
      )
      plot(impactSkipTests)
      impactSkipTests   
      
  ## compare get AUCs 
  popAvCurves  
  times 
  getAUCs( )
}

#' @details \code{fastRankSchemes} is a faster version to rank\code{\link{SetOfSchemes-class}} objects ,
#'   with fixed settings ( objective AUC and cMax , summary measure is variance and scale measure is maximum ). It is meant to be used 
#' inside the shiny application
#' @rdname rankObject   
#' @export
fastRankSchemes       <-  function( object  , pkData, objective ,  nCores  = 1  ) {
  
#    iScheme = 1 
#    scheme =object[ , , iScheme ] 
#    dataForSchemesInput = pkData 
#    getResultFor1Curve
  getResultPerSchemeInt           <-  function( scheme , dataForSchemesInput   ) {
    timePoints                    <-  getTimePoints( dataForSchemesInput  )  
    estimatedPkCurves             <-  estimatePopCurve( scheme , dataArray =  dataForSchemesInput )
    varianceOfCriteria            <-  getFixedSchemePerformanceCpp( estimatedPkCurves , timePoints )
    return( varianceOfCriteria )
  }
  
  nSchemes                   <-  getNSchemes( object ) 
  schemeIndicator            <-  seq_len( nSchemes )
  schemeNames                <-  getNames( object )
#  cluster                     <-  makeCluster( nCores , type = "FORK" )
#  on.exit(stopCluster(cluster)) # clean up cluster  # TODO same form of cluster
  cat("start Ranking Schemes on cluster with " , nCores , "cores")
  resultList                 <-  mclapply( seq_len( nSchemes ) , function( iScheme ){
        getResultPerSchemeInt( object[ , , iScheme ] ,  dataForSchemesInput = pkData   )   
      } ,
      mc.cores = nCores
  )  
  
  result                      <-  matrix( unlist( resultList ) , byrow = TRUE , nrow = nSchemes )
#  colnames( result )          <-  paste0( "var" ,   "_" , objective$criterion )
  resultNames                 <-  paste0( "var" ,   "_" , objective$criterion )
  ## combine individual criteria into objective function ( here fixed 2 criteria )

    # calculate max var criteria (to normalise) so individual criteria between 0 and 1
    scalingMeasure               <-  colMaxs( result )
    scaleWeights                 <-  ( 1 / scalingMeasure )
    objectiveWeigthsNormalised   <-  objective$weight / sum(objective$weight )
    
    weightAndScaleVec            <-  scaleWeights * objectiveWeigthsNormalised 
    
    resultCombined               <-  result %*% weightAndScaleVec     
    orderSchemes               <-  order( resultCombined , method = "radix" )
    
    
    #  output element = "object with filled up slot in ranks"
#    rankSchemes               <-  rank( resultCombined , ties.method = "first" ) 
    
#    ranking                   <-  data.frame( name = schemeNames , result ,  criterion = resultCombined , rank = 0 )[ orderSchemes , ]
    NROWSRESULT               <-  5
    ranking                   <-  matrix( nrow = nSchemes , ncol = NROWSRESULT )
    ranking[ , 1 ]            <-  schemeIndicator 
    ranking[ , 2:3 ]          <-  result
    ranking[ , 4 ]            <-  resultCombined
    colnames( ranking )       <-  c( "name" , resultNames , "criterion" , "rank"  )
    rankingSort               <-  ranking[ orderSchemes , ]
    ranks                     <-  seq_len( nSchemes)
    rankingSort[ , 5]         <-  ranks
    rankingSortFrame          <-  data.frame( rankingSort , stringsAsFactors = TRUE)
    rankingSortFrame[["name"]] <-  as.factor( paste0( "scheme" , rankingSortFrame[["name"]] ) )
    outputSchemes             <-  object
    outputSchemes@ranking     <-  rankingSortFrame # no validation testing
    return( outputSchemes )
}



