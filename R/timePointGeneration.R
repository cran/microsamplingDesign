##############################################################################
### Project: microsamplingDesign                                           ###
###                                                                        ###
### Description: Generate time points                                      ###
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
### changes:                                                               ###
###                                                                        ###
###                                                                        ###
##############################################################################



#' working example time zone dataframe to use in examples 
#' 
#' @export
getExampleTimeZones          <-  function() {
  timeZones                   <-  c( 0 , 5 , 21 )
  nTimeZones                  <-  length( timeZones ) - 1
  indStartTimes               <-  1 : nTimeZones
  startTimes                  <-  timeZones[ indStartTimes ]
  endTimes                    <-  timeZones[ indStartTimes + 1 ]
  zonePoints                  <-  c( 2, 1 )
  timeZoneConstraint          <-  data.frame( startTime = startTimes , endTime = endTimes, nPointsPerZone = zonePoints )
  timeZoneConstraint
}


# function to make a  matrix containing all combinations of matrix rows
if( 0 == 1 ) {
  matrix1            <-  matrix( paste0( "a", 1:10 ) , nrow = 5 , ncol = 2 )
  matrix2            <-  matrix( paste0( "b", 1:6 )  , nrow = 3 , ncol = 2 )
  test1              <-  matrix1 %ARC% matrix2
  test2              <-  matrix1 %ARC% matrix2 %ARC% matrix1
}
#' All Row Combinations (ARC) function 
#' take all combination of rows of 2 matrices and bind them together
#' 
#' @param matrix1 numeric matrix
#' @param matrix2 numeric matrix
#' @return numeric matrix
#' @export
`%ARC%`               <-  function( matrix1 ,  matrix2 ) {
  nRowMatrix1         <-  nrow( matrix1 )
  nRowMatrix2         <-  nrow( matrix2 )
  nColMatrix1         <-  ncol( matrix1 )
  nColMatrix2         <-  ncol( matrix2 )
  resultMatrix        <-  matrix(NA, nrow = nRowMatrix1 * nRowMatrix2 , ncol =  ( ncol( matrix1 ) + ncol( matrix2 ) ) )
  resultDim           <-  dim( resultMatrix )
  nrowResult          <-  resultDim[1]
  ## extend matrices
  rowSelectMat1       <-  rep( 1:nRowMatrix1 , nRowMatrix2 )  
  rowSelectMat2       <-  rep( 1:nRowMatrix2 , rep( nRowMatrix1, nRowMatrix2 ) )
  matrix1Extended     <-  matrix1[ rowSelectMat1 ,  ]
  matrix2Extended     <-  matrix2[ rowSelectMat2 , ]
  
  ## combine matrices
  resultMatrix        <-  cbind( matrix1Extended, matrix2Extended )
  dimnames(resultMatrix)  <- NULL
  return( resultMatrix )
}

#getFullTimePoints       <-  function( Tzero , Tend , Tinterval ) {
#  fullTimePoints        <-  seq( Tzero , Tend , by = Tfinterval )  
#  fullTimePoints
#}


if( 0 == 1 ){
  # debugging
  timeZones                <-  getExampleTimeZones()
  fullTimePoints           <-  seq( 0 , 21 , 1 ) 
  getAllTimeOptions( timeZones , fullTimePoints )
  
  # test error messages (TODO)
  
  # example 
  timeZonesEx              <-  getExampleTimeZones()
  fullTimePointsEx         <-  seq( 0 , 21 , 1 )
  print(timeZones)
  setOfTimePoints          <-  getAllTimeOptions( timeZones = timeZonesEx , fullTimePoints = fullTimePointsEx )
 
  # debug example article 
  
  
  timeZonesEx              <-  getExampleTimeZones()
  fullTimePointsEx         <-  seq( 0 , 21 , 1 )
  print(timeZones)
  setOfTimePoints          <-  getAllTimeOptions( timeZones = timeZonesEx , fullTimePoints = fullTimePointsEx )
  
  
  # error 
  timeZones       <-  data.frame(
      startTime      = c(  0, 1   , 4.5) ,
      endTime        = c(  1   , 4.5 , 12 ) ,
      nPointsPerZone = c(  1   , 3   , 1  )
  )
   fullTimePoints      <-  seq(0, 12 , 0.5)
  timeZones2       <-    data.frame(
      startTime      = c(  0   ,  1  ) ,
      endTime        = c(  1   , 12  ) ,
      nPointsPerZone = c(  1   , 1   )
  )
  fullTimePoints
  xx    <-  getAllTimeOptions( timeZones = timeZones2 , fullTimePoints = fullTimePoints )
  
  
  # miminal example with bug
  
  fullTimePoints  <-  0:6
  
  timeZones     <-    data.frame(
      startTime      = c(  0   ,  2  ) ,
      endTime        = c(  2   , 6  ) ,
      nPointsPerZone = c(  1   , 1   )
  )
  xx    <-  getAllTimeOptions( timeZones = timeZones , fullTimePoints = fullTimePoints )
  
  # look into change
test_orig                  <-  microsamplingDesign::getAllTimeOptions( getExampleTimeZones() , seq( 0 , 21 , 1 ) )
test_unchanged             <-  getAllTimeOptions( getExampleTimeZones() , seq( 0 , 21 , 1 ) )

identical(test_orig , test_unchanged )

# bug one row does not work
timeZones                   <-  data.frame( startTime = 0 , endTime = 5 , nTimePoints = 1 )
fullTimePoints              <-  0:5

}
#' generate all possible time options from eligible time points and number of samples per time interval ( time zone )
#'
#' @param timeZones a data.frame containing information on the number of points to be chosen in each time zone. 
#' Each row is a time zone. 
#' \itemize{
#'   \item \code{startTime} the start time of each time zone assumed to be included in that zone
#'   \item \code{endTime} the end time of the zone. It is not part of the current zone but the start time of the next zone
#'   \item \code{nPointsPerZone} the number of time points to be chosen within each zone. 
#' }
#' @param fullTimePoints a numeric vector containing all possible time points to be considered including time point zero and the last time point
#' @details time point zero is never included in any time option and the last time point is always included. Note that the last 
#' time point is not a member of any zone. The number of time points in every time options is therefore the total number of time
#' points specified in \code{timeZone} plus 1 for the last time point.
#' @return \code{\link{SetOfTimePoints-class}} 
#' @importFrom gtools combinations
#' @examples
#'   timeZonesEx              <-  getExampleTimeZones()
#'   fullTimePointsEx         <-  seq( 0 , 21 , 1 )
#'   print(timeZonesEx)
#'   setOfTimePoints          <-  getAllTimeOptions( timeZones = timeZonesEx , 
#'      fullTimePoints = fullTimePointsEx )
#'   setOfTimePoints          <-  getAllTimeOptions( 
#'     timeZones = data.frame(startTime = 0 , endTime = 21 , nPointsPerZone = 1) , 
#'     fullTimePoints = fullTimePointsEx 
#'    )
#' @export
getAllTimeOptions          <-  function( timeZones , fullTimePoints ) {
 
  ## check inputs 
  namesToCheck             <-  c( "startTime" , "endTime" , "nPointsPerZone" )
  checkNames               <-  identical( namesToCheck ,  names(timeZones) )
  if( ! checkNames ) { stop( "names 'timeZones' should be: ", "\n" , "  " ,  paste0(namesToCheck, "  ") ) }
  minTimePoint             <-  min( fullTimePoints )
  maxTimePoint             <-  max( fullTimePoints  )
  minZoneTime              <-  min( timeZones$startTime ) 
  maxZoneTime              <-  max(  timeZones$endTime )
  checkZeroPoint           <-  ( minTimePoint == 0 ) && ( minTimePoint == minZoneTime ) 
  if( ! checkZeroPoint )  { stop( "zero point not compatible between 'timeZones' and 'fullTimePoints'  or not equal to 0" ) }
  checkEndPoint            <-  ( maxTimePoint == maxZoneTime ) 
  if( ! checkEndPoint ) { stop( "end point not compatible between 'timeZones' and 'fullTimePoints' " ) }
  
  zoneTimePoints           <-  unique( c( timeZones$startTime , timeZones$endTime ) )
  flagTimeIncuded          <-  zoneTimePoints %in% fullTimePoints
  checkZoneInPoints        <-  all( flagTimeIncuded ) 
  if( ! checkZoneInPoints ) { stop( "some times in 'timeZones' not included in 'fullTimePoints' " )  }
  
  nZones                   <-  nrow( timeZones  )
  if( nZones == 1 ) {
    checkLastIsFirst       <-  TRUE
    
  } else {
    checkLastIsFirst       <-  all( timeZones$startTime[ 2:nZones ]  ==  timeZones$endTime[ 1:(nZones-1)  ] )
  }
  if( ! checkLastIsFirst ){
    stop( "startTime' of a timeZone should equal the 'endTime' of the previous time zone")
  }
   
  ## add indicators instead of time points   
  nZones                   <-  nrow( timeZones )
  nFullTimePoints          <-  length( fullTimePoints )
  timeZones$startInd       <-  which( fullTimePoints %in% timeZones$startTime )
  timeZones$EndInd         <-  which( fullTimePoints %in% timeZones$endTime ) 
  nTimePointToSelect       <-  sum( timeZones$nPointsPerZone )  
  
  ## internal function combine possibilities sample per time zone
  # timeZone  = timeZones[ 1 , ]
  #
  nTimePointsInput         <-  nFullTimePoints
  getTimeOptionsPerZone    <-  function( timeZone , nFullTimePoints = nTimePointsInput ) {
    indSampleFrom          <-  timeZone$startInd: (timeZone$EndInd -1 )  
    flagExclude            <-  indSampleFrom %in% c( 1 , nFullTimePoints ) # exclude timepoint zero and the last time point = not a free choice
    allIndOptions          <-  indSampleFrom[ !flagExclude ]
#    zICombinations         <-  t( combn( allIndOptions , timeZone$nPointsPerZone ) ) # TODO unexpected behaviour , bug, if no vector it things the number 
     zICombinations        <-  combinations( n = length(allIndOptions) , v = allIndOptions ,   r = timeZone$nPointsPerZone  )
  }
  
  ## calculation first zone
  iZone                    <-  1
  result                   <-  getTimeOptionsPerZone( timeZones[ iZone , ]  )
  combinedResult           <-  result 
  ##  calculation other zones and take All Row combinations
  #iZone = 2
  if( nZones > 1 )  {
    for( iZone in  ( 2 : nZones ) ){
      result                 <-  getTimeOptionsPerZone( timeZones[ iZone , ] )
      combinedResult         <-  combinedResult %ARC% result 
    }   
  }
  # specific operations first and last timePoint  
  
  timeOptions              <-  cbind( combinedResult,  nFullTimePoints ) # include the last timePoint with indicattor nFullTimePoints
  dimnames(timeOptions)    <-  NULL
  
  ##transform indicator back to orignial time points
  timeOptionsInTime        <-  matrix( fullTimePoints[timeOptions] , nrow = nrow(timeOptions) )
  
  ## set names
  nOptions                 <-  nrow( timeOptionsInTime )
  nTimePoints              <-  nTimePointToSelect + 1 # +1 because last timePoint is included
  optionNames              <-  paste0( "timePointOption" , ( 1 : nOptions ) )
  rownames( timeOptionsInTime )  <-  optionNames 
  colnames( timeOptionsInTime )  <-  paste0( "TimePoint", ( 1 : nTimePoints ) )
  
  ## generate output 
  setOfTimePoints         <-  new( "SetOfTimePoints",
      .Data                =  timeOptionsInTime  ,
      fullTimePoints       =  fullTimePoints  ,
      nFullTimePoints      =  nFullTimePoints   ,
      nTimePointsSelect    =  nTimePoints ,  
      nTimePointOptions    =  nOptions 
  )
  return( setOfTimePoints )
}
  



