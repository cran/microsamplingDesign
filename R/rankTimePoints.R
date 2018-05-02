##############################################################################
### Project: microsamplingDesign                                           ###
###                                                                        ###
### Description: rank time points by comparing them to population average curve   ###
###                                                                        ###
###                                                                        ###
###                                                                        ###
###                                                                        ###
### Author: ablommaert                                                        ### 
###         <adriaan.blommaert@openanalytics.eu>                           ###
###                                                                        ###
###                                                                        ###
### Maintainer:ablommaert                                                     ###
###       <adriaan.blommaert@openanalytics.eu>                             ###
###                                                                        ###  
###                                                                        ###
### version 0.1                                                            ###
###                                                                        ###
### changes:                 s                                             ###
###                                                                        ###
###                                                                        ###
##############################################################################

#########
###WORKING example  
##########

#'  generate example PkData object to be used in example rankTimePoints
#' 
#' @importFrom stats rnorm
#' @export 
getExampleTimeData           <-  function() { 
  times                      <-  seq( 0, 10, by = 0.5 ) # vector of time points
 
  nRats                      <-  2 
  nSamples                   <-  7 # number of sample datasets within each scenario this is a large set
#  nScenarios                 <-  1 # derived from number of parameters options (all combinaitons if multiple parameter definitions), generally 1 to start with
  model                      <-  getExamplePkModel()
  dataExample                <-  getPkData( pkModel = model , timePoints = times ,
      nSubjectsPerScheme = nRats, nSamples = nSamples  )
  return( dataExample )
}


if( 0 == 1 ) {
  
#   fullPkData                 <-  getExampleTimeData() # PkData object
#   fullTimePoints             <-  getTimePoints(fullPkData)
#   examplePopAvCurve          <-  fullTimePoints^2 
#   timePointIndicators        <-  c( 1, 5, 21 )
#   nGridPoints                <-  25
#   timeGrid                   <-  seq( min( fullTimePoints ), max( fullTimePoints ) , length.out = nGridPoints ) 
#   popCurveInterpolated       <-  interpolateVec( x = fullTimePoints ,  y = examplePopAvCurve, xout = timeGrid )
   
   ##  debugging
   pkModel                    <-  getPkModelArticle()
   timePoints                 <-  0:10   # zero should be included in pkData?
   nSubjectsPerScheme         <-  5
   nSamplesAvCurve            <-  13
   pkData                     <-  getPkData( getExamplePkModel() , timePoints , nSubjectsPerScheme  , nSamples )
   fullTimePoints             <-  getTimePoints( pkData )
   object                     <-  getExampleSetOfTimePoints( c(fullTimePoints) )   # SetOfTimePointsObject 
   nGrid                      <-  100
   useAverageRat             <-  TRUE
   
   rankObject( object = testObject ,  pkData = testData  ,  nGrid = 50 ,  nSamplesAvCurve = 10  )
   
   
   
   # bug in follwing code:
  
  testObject    <- getExampleSetOfTimePoints( 0:20 )  
  timePoints    <- getTimePoints( testObject  ) 
  testData      <-  getPkData( getExamplePkModel() ,  timePoints , nSubjectsPerScheme = 1 ,  nSamples = 5 )
  
  # debugging 
  pkData = testData
  object = testObject 
  nGrid = 100
  nSamplesAvCurve = 10 
  useAverageRat = FALSE 
  avCurve = NULL
  
  
  ### error with 3 cores, not with 2 cores
  object = bigTimePoints ;  pkData = dataToRankTimePoints ; dataToRankTimePoints ; nGrid = 50 ; nSamplesAvCurve = 2 ; useAverageRat = FALSE ; avCurve = NULL ;  nCores = 3 
  
 }
 
 # internal functions without defaults 
#rankObject.SetOfTimePoints    <-  function( object , pkData ,   nGrid = 100 , nSamplesAvCurve = 1000 , useAverageRat = FALSE , avCurve = NULL , ... ) {
rankObject.SetOfTimePoints     <-  function( object , pkData ,   nGrid  , nSamplesAvCurve  , useAverageRat  , avCurve , nCores  ) {
##  check inputs
  if( !class(object) == "SetOfTimePoints" ) { stop( "object input is not of class 'SetOfTimePoints' " ) }
  if( ! validObject(object) ){ stop( "object input not a valid SetTimePoints-class object" ) }
  if( !class(pkData) == "PkData" ) { stop( "pkData input is not of class 'PkData' " ) }
     
  # check same full time points
  fullTimePointsData          <-  getTimePoints( pkData )
  fullTimePointsObject        <-  getTimePoints( object )     
  equalTimes                  <-  identical( fullTimePointsData , fullTimePointsObject )
  if( !equalTimes )  { stop( "object input and pkData input have different time points " ) }   
  fullTimePoints              <-  fullTimePointsData
  
  minFullTimePoints           <-  min( fullTimePoints )
  if( ! minFullTimePoints == 0) { stop( "the minimum value of 'fullTimePoints' in 'pkData' should be zero (i.e. time of administration) " ) }
  
  ##  input processing 
  dataTimeOptions             <-  getData( object )
  nTimePointsOptions          <-  nrow( dataTimeOptions )
  dataTimeOptionsWithZero     <-  cbind( timeZero =  0 , dataTimeOptions  )
        
 
  # conververt dataTimeOptionsWithZero from actual to indicators
 if( 0 == 1 ) {
   times = dataTimeOptionsWithZero[1,]
   fullTimes  = fullTimePoints
 }
 
  convertToIndices            <-  function( times , fullTimes = fullTimePoints ) {
   which( fullTimes %in% times , fullTimes )
  }
  
  dataTimeOptionsWithZeroInd  <- t( apply( dataTimeOptionsWithZero , 1 ,  convertToIndices  ) )  #TODO: transpose can be slow? 
  
  ##  population averaged curve
  
  if( ! is.null(avCurve) ) {
    cat(" using loaded in population averaged curve " , "\n")
    popAveragedCurve          <-  avCurve
  } else { 
    pkModel                   <-  getPkModel( pkData ) # some sugar
    if( ! useAverageRat ) {
      popAveragedCurve        <-  getPopAveragedCurve( fullTimePoints , pkModel , nSamples = nSamplesAvCurve , nCores = nCores  )   
    } else {
      avRatModel              <-  setModelToAverageRat( pkModel )
      nSamplesForAvRat        <-  1
      popAveragedCurve        <-  getPopAveragedCurve( fullTimePoints , avRatModel , nSamples = nSamplesForAvRat  ) 
    }
  }

  # interpolate population average curve
  
  timeGrid                    <-  seq( 0 , max( fullTimePoints ) , length.out = nGrid ) 
#  cat( "gridPoints:" , timeGrid , "\n" )
  interpolPopAvCurve          <-  interpolateVec(  fullTimePoints ,   popAveragedCurve,  timeGrid ) 
   

  ## calculate results (this is the intensive step ) #
  
  
  if( nCores == 1 ) { # needed for profiling inside
      result                      <-  apply( dataTimeOptionsWithZeroInd , 1, "getTimeChoicePerformance",
      pkData ,
      popAvCurve = interpolPopAvCurve, timeGrid, printMCError = FALSE )
    
  } else {
    cluster                     <-  makeCluster( nCores , type = "FORK" )
    on.exit(stopCluster(cluster)) # clean up cluster 
#    clusterEvalQ( cluster , { library(microsamplingDesign) }  )
    cat( "starting cluster (forking) with " , nCores , "cores" , "\n")
#    clusterExport( cluster , varlist =  c( "arrange" ) ,   envir=environment() )
    result                      <-  parApply( cluster ,  dataTimeOptionsWithZeroInd , 1, "getTimeChoicePerformance",
        pkData , popAvCurve = interpolPopAvCurve, timeGrid, printMCError = FALSE )
  
  
#  resultList                    <-  mclapply( seq_len( nTimePointsOptions ) , function( iTimePoint ) {
#        
#        timepointOption         <- dataTimeOptionsWithZeroInd[ iTimePoint ,  ]   
#         getTimeChoicePerformance( timepointOption ,  pkData , popAvCurve = interpolPopAvCurve, timeGrid, printMCError = FALSE )
#      } , mc.cores = nCores  
#  )
#  result                        <-  unlist( resultList ,recursive = TRUE, use.names = TRUE)
  } # end else
  
    
 
  ## rank results # TODO skip to matrix from 
  rankTimeOptions             <-  rank( result  ,ties.method = "first" ) 
  namesResult                 <-  
  ranking                     <-  data.frame( name = names( result ),  criterion = result , rank = rankTimeOptions  )
  rankingSorted               <-  arrange( ranking, rankTimeOptions ) # TODO subset instead of rank 
  
  ## output object 
  outputObject                <-  object 
  setRanking( outputObject )  <-  rankingSorted
#  validObject( outputObject )
  outputObject 
}

# internal function to calculate population average curve from fullTimePoints and object 
if( 0 == 1 ) {
  pkModel                      <-  getExamplePkModel()
  timePoints                   <-  0:10
  nSamples                     <-  50
#  pkModel                      <- pkData
  
  # microbenchmarking performance 
  library( microbenchmark )
  rowMeansTest <-   microbenchmark(  apply( pkRawData , 2 , "mean" ) , 
      rowMeans( pkRawData , dims = 2)
  )
  plot( rowMeansTest )
}


getPopAveragedCurve            <-  function( timePoints , pkModel , nSamples , nCores = 1 ) {
   setCoeffVariationError( pkModel ) <-  0 # avoid additive noice 
   pkData                      <-  getPkData( pkModel , timePoints , nSubjectsPerScheme = 1 , nSamples , nCores = nCores )
   pkRawData                   <-  getData( pkData )
#   meanPerTimePoint            <-  apply( pkRawData , 2 , "mean" )
  meanPerTimePoint             <-  rowMeans( pkRawData , dims = 2)
   return( meanPerTimePoint )
}


#' @note if \code{\link{SetOfTimePoints-class}} timePoints are ranked according to mimimal distance between population average curve and 
#' the estimate of the population average curve based on a selection of time points.
#' @param nGrid number of equally spaced point to calculate the distance between sample and population averaged kinetic curve, defaults to 100
#' @param nSamplesAvCurve the number of samples to calculate the averaged curve ( only to rank \code{\link{SetOfTimePoints-class}}),
#' defaults to 1000
#' @param useAverageRat logical value if TRUE, the average rat (with random effects equal to zero and no additional error) is used instead of the integrated out population averaged curve, 
#' defaults to FALSE; this is faster but biased
#' @param avCurve a user specified averaged curve, when specified, the average curve is no longer calculated from the pkModel, defaults to \code{NULL}
#' @rdname rankObject
#' @importFrom parallel makeCluster  clusterEvalQ  stopCluster  clusterExport
#' @examples
#' \dontrun{
#'  fullTimePoints    <-  0:10
#'  setOfTimePoints   <-  getExampleSetOfTimePoints( fullTimePoints)
#'  pkDataExample     <-  getPkData( getExamplePkModel() , getTimePoints( setOfTimePoints ) , 
#'    nSubjectsPerScheme = 5 , nSamples = 17   )
#'  ex1               <-  rankObject( object = setOfTimePoints , pkData = pkDataExample ,
#'      nGrid = 75 , nSamplesAvCurve = 13)
#'  ex2               <-  rankObject( object = setOfTimePoints ,   pkData = pkDataExample , 
#'      nGrid = 75 , nSamplesAvCurve = 13 , useAverageRat = TRUE )
#'  ex3               <-  rankObject( object = setOfTimePoints ,   pkData = pkDataExample ,
#'      nGrid = 75 , avCurve = rep(0 , length(fullTimePoints) ) )
#' }
#' @importFrom plyr arrange
#' @importFrom parallel parApply makeCluster
#' @keywords export 
setMethod(f = "rankObject",
    signature = "SetOfTimePoints",
    definition =  function( object , pkData ,   nGrid = 100 , nSamplesAvCurve = 1000 , useAverageRat = FALSE , avCurve = NULL , nCores = 1 ) {
      rankObject.SetOfTimePoints( object = object , pkData = pkData , nGrid = nGrid , nSamplesAvCurve = nSamplesAvCurve , useAverageRat = useAverageRat , avCurve = avCurve , nCores = nCores )
    } 
)


if( 0 == 1 ) {
  fullPkData                 <-  getExampleTimeData() # PkData object
  fullTimePoints             <-  getTimePoints(fullPkData)
  examplePopAvCurve          <-  fullTimePoints^2 
  timePointIndicators        <-  c( 2 , 5, 21 )
  nGridPoints                <-  25
  timeGrid                   <-  seq( min( fullTimePoints ), max( fullTimePoints ) , length.out = nGrid ) 
  popCurveInterpolated       <-  interpolateVec(  fullTimePoints ,   examplePopAvCurve,  timeGrid )
  
  
  getTimeChoicePerformance( timePointInd = timePointIndicators, pkData = fullPkData ,
      popAvCurve = popCurveInterpolated, timeGrid) 
  
  getTimeChoicePerformance( timePointInd = timePointIndicators, pkData = fullPkData ,
      popAvCurve = popCurveInterpolated, timeGrid, printMCError = TRUE ) 
  
  
 # debugging 
  timePointInd               <-  timePointIndicators
  pkData                     <-  fullPkData
  popAvCurve                 <-  popCurveInterpolated
  timeGrid                   <-  nGridPoints
  printMCError               <-  TRUE

}
#' estimate the distance between population average  an average over sample datasets with given time points (zero point included)
#' 
#' @param timePointInd a vector indicating time points indicator selection of time points from fullTimePoints
#' @param pkData  \code{\link{PkData-class}}
#' @param popAvCurve an interpolated population average curve
#' @param timeGrid the grid point at which to interpolate the curve
#' @param printMCError logical indicater when true the MC error is printed to the terminal, defaults to FALSE
#' @examples
#'  # get example inputs
#'  fullPkData                 <-  getExampleTimeData() # PkData object
#'  fullTimePoints             <-  getTimePoints(fullPkData)
#'  examplePopAvCurve          <-  fullTimePoints^2 
#'  timePointIndicators        <-  c( 1 , 5, 21 ) # zero point included
#'  nGridPoints                <-  25
#'  timeGrid                   <-  seq( min( fullTimePoints ),
#'     max( fullTimePoints ) , length.out = nGridPoints ) 
#'  popCurveInterpolated       <-  microsamplingDesign:::interpolateVec( fullTimePoints ,
#'    examplePopAvCurve,  timeGrid )
#' 
#'  getTimeChoicePerformance( timePointInd = timePointIndicators, pkData = fullPkData ,
#'    popAvCurve = popCurveInterpolated, timeGrid ) 
#'
#'  getTimeChoicePerformance( timePointInd = timePointIndicators, pkData = fullPkData ,
#'    popAvCurve = popCurveInterpolated, timeGrid, printMCError = TRUE ) 
#' 
#' @return numeric value of the timePoint choice performance 
#' @importFrom stats sd
#' @export
getTimeChoicePerformance  <-  function( timePointInd, pkData, popAvCurve, timeGrid, printMCError = FALSE ) {
  

  
  # average time points
  pkDataOnly              <-  getData( pkData )
  fullTimePoints          <-  getTimePoints( pkData )
  sampleTimePoints        <-  fullTimePoints[ timePointInd ]
  
    # internal function averge per time point 
  getAveragePerTimePoint       <-  function( pkData, timePointSelect ){
    dataSelect                 <-  pkData[ , timePointSelect , , drop = FALSE ]
#    nSubjects                  <-  dim( pkData )[1]
#    ratAveragedData            <-  apply( dataSelect, c(2,3) , sum )/nSubjects # 3 times faster like that
    ratAveragedData             <-  colMeans( dataSelect )
    ratAveragedData
  }
  
  averageData             <-  getAveragePerTimePoint( pkDataOnly, timePointInd )

  # interpolate and distance function
  calcInterAndDist        <-  function( sampleCurve ) {
    interpolCurve         <-  interpolateVec( sampleTimePoints ,  sampleCurve,  timeGrid )
    distance              <-  mean( abs( ( interpolCurve -  popAvCurve ) ) )
    distance
  } 
  # calculation per sample
  distancePerSampleCurve  <-  apply( averageData, 2 ,   "calcInterAndDist" )
  
  # mean over samples (# TODO MC-error?)
  performance             <-  mean( distancePerSampleCurve )
  if( printMCError ) {
    MCError                 <-  sd( distancePerSampleCurve )/ length(distancePerSampleCurve)
    cat("MC error:" ,  MCError , "\n" ) 
  }
  return( performance )
}


if( 0 == 1 ) {
  xValues                  <-  0:30
  curve1                   <-  data.frame( x = xValues , y = xValues^2 )
  curve2                   <-  data.frame( x = xValues , y = xValues^2.3  )
  curve3                   <-  data.frame( x = xValues , y = (xValues^3 - 0.5*xValues^3)  )
  x11()
  plot(curve1, type = "l", lwd=3)
  lines(curve2, col= "red", lwd = 3)
  lines(curve3, col= "green", lwd = 3)
  abline(v = xValues )
  legend( ""  )
  
  
  getCurveDistance( curve1, curve2 )
  getCurveDistance( curve2, curve1 )
  getCurveDistance( curve3, curve1 )
  getCurveDistance( curve3, curve2 ) + getCurveDistance( curve2, curve1 )
}
##' Calculate average absolute distance between 2 curves
##' 
##' @param curve1 a dataframe with an x and y value representing values of a function
##' @param curve2 a dataframe with an x and y value representing values of a function
##' @return positive numeric value 
##' @examples 
##'  xValues                  <-  0:30
##'  curve1                   <-  data.frame( x = xValues , y = xValues^2 )
##'  curve2                   <-  data.frame( x = xValues , y = xValues^2.3  )
##'  curve3                   <-  data.frame( x = xValues , y = (xValues^3 - 0.5*xValues^3)  )
##'  #x11()
##' plot(curve1, type = "l", lwd=3)
##'  lines(curve2, col= "red", lwd = 3)
##'  lines(curve3, col= "green", lwd = 3)
##'  legend( x = "topleft" , legend =  paste0( "curve" , 1:3 ) , lwd  = 3 ,   col = c("black" , "red" , "green"))
##'  abline(v = xValues )
##'  getCurveDistance( curve1, curve2 )
##'  getCurveDistance( curve2, curve1 )
##'  getCurveDistance( curve3, curve1 )
##'  getCurveDistance( curve3, curve2 ) + getCurveDistance( curve2, curve1 )
##' @export
#getCurveDistance             <-  function( curve1 ,  curve2 ) {
##  if(! identical( curve1$x, curve2$x)  ) { stop("cannot calculate distances, 2 curves measured at different time points") }
##  distance                   <-  abs( curve1$y - curve2$y )
##  distance                   <-  abs( curve1 - curve2 )
##  meanDistance               <-  mean(distance)
#  meanDistance                <-  mean( abs( curve1 - curve2 ))
#  meanDistance
#}






















