##############################################################################
### Project: microsamplingDesign                                           ###
###                                                                        ###
### Description : object and methods for SetOfTimePoints                   ###
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

### validation function
checkInteger               <- function( x ) {
  all( x  ==  as.integer(x) ) 
}  
#checkInteger(c(1,2,3, 2))


if ( 0 == 1 ) {
  object                   <-  getExampleSetOfTimePoints(0:21) 
  str( object )
  # correct object 
  checkSetOfTimePoints( object )
  
  # wrong object
  wrongObject              <-  object
  wrongObject@.Data        <-  object@.Data[1:2,1:2]  
  colnames(wrongObject@.Data) <-  c("bla", "bla")
  wrongObject@.Data[1,1]   <-  1.5
  wrongObject@nFullTimePoints  <-  300
  checkSetOfTimePoints( wrongObject )
  
  ## check time point message
  
  objectWrongTime         <-  object
  objectWrongTime@.Data   <-  object@.Data-1
  checkSetOfTimePoints()
  
  
  # debugging
  object                  <-   getExampleSetOfTimePoints( 0:21 )
}

checkSetOfTimePoints       <-  function( object ) {
  errors                   <-  character( ) 
  dimData                  <-  dim( object@.Data )
  nDimensions              <-  length( dimData )
  
  ##  check dimensions
  if( !length(object@fullTimePoints) == object@nFullTimePoints ) {
    msg                    <-  paste0( "Number of fullTimePoints do no match nFullTimePoints", "\n" ) 
    errors                 <-  c ( errors , msg ) 
  }
  if( !nDimensions == 2 ) {
    msg                    <-  paste0( "Dimensions of data array should be 2, not:  ", nDimensions, "\n" )
    errors                 <-  c ( errors , msg )
  }
  if( !object@nTimePointsSelect== dimData[ 2 ] ) {
    msg                    <-  paste0( "Number of timePoints: ", object@nTimePointsSelect ," does not match number 
of timePoints in time point options: ", dimData[ 2 ], "\n" ) 
    errors                 <-  c ( errors , msg )
  }
  
  
  ## check unique identifiers
  timeChoiceNames         <-  rownames( object@.Data )
  nNames                  <-  length( timeChoiceNames )
  uniqueNames             <-  unique( timeChoiceNames )
  nUniqueNames            <-  length( uniqueNames )
  checkUnique             <-  ( nNames == nUniqueNames ) && ( ! is.null( timeChoiceNames ) ) 
  if ( ! checkUnique ) {
    msg                   <-  paste0( "non-unique names of time point options ( .Data slot )" , "\n" )
    errors                <-  c( errors , msg )
  } 
  ## check zeropoint  and last object are included
  firstTimePoint          <-  object@.Data[ , 1 ]
  lastTimePoint           <-  object@.Data[ , object@nTimePointsSelect]
  fullTimePoints          <-  object@fullTimePoints
  indZeroPoint            <-  any( firstTimePoint == min( fullTimePoints ) )
  indAllLast              <-  all( lastTimePoint == max( fullTimePoints )  )
    # messag zero point included
  if ( indZeroPoint ) {
    msg                   <-  paste0( "Zero point cannot be included as sample option" , "\n" )
    errors                <-  c( errors , msg )
  }
    # message last
  if ( ! indAllLast ) {
    msg                   <-  paste0( "Not all time points options end with the last time" , "\n" )
    errors                <-  c( errors , msg )
  }
  
  ##  print errors if object not defined according to the rules 
  if ( length(errors) == 0 ) {
    TRUE 
  } else {
    cat( errors )
    FALSE
  }
}


### object definition 


#' S4 class SetOfTimePoints representing a set of designs with given time points
#' 
#' @name SetOfTimePoints-class
#' @slot .Data a numerics array of 2 dimensions ( nTimePointChoices  x nTimePointsSelect) contains per time point choice the selected time points in hours
#' @slot fullTimePoints  numeric vector of all time points one is willing to consider
#' @slot nFullTimePoints  number of all time points one is willing to consider
#' @slot nTimePointsSelect  number of time points selected from the fullTimePoints
#' @slot  nTimePointOptions  number of possible timePoint choices  
#' @slot ranking is a data.frame which is the rank of the timePointChoices according to a specific criterion.
#' @author Adriaan Blommaert
#' @aliases SetOfTimePoints setOfTimePoints
#' @export
setClass( "SetOfTimePoints", 
    representation = representation(
        .Data                = "array"     ,
        fullTimePoints       = "numeric"   ,
        nFullTimePoints      = "numeric"   ,
        nTimePointsSelect    = "numeric"   ,
        nTimePointOptions    = "numeric"   ,
        ranking              = "data.frame" 
   )  , 
   validity = checkSetOfTimePoints
)


if( 0 == 1 ) {
  #debuggin
  fullTimePoints = 0:10
  nTimePointsSelect = 5
  nChoicesSubset = 7
}

#' get a minimal example set of time points to test functions with
#' 
#' @param fullTimePoints numeric vector of time points
#' @param  nTimePointsSelect number of time points to select from the full time points, defaults to 5
#' @param  nChoicesSubset number of all selection to retain for the example to avoid a large object defaults to 7
#' @examples 
#' getExampleSetOfTimePoints( fullTimePoints = 0:10, nTimePointsSelect = 5, nChoicesSubset = 7 ) 
#' @importFrom gtools  combinations 
#' @export
getExampleSetOfTimePoints     <-  function( fullTimePoints, nTimePointsSelect = 5, nChoicesSubset = 7 ) { 
  nFulltimePoints             <-  length( fullTimePoints )
  nTimeOptions                <-  nFulltimePoints - 2 # min never included and max always included
  nTimeOptionsToSelect        <-  nTimePointsSelect - 1 # max always inclucdes
  timePointChoices            <-  combinations( nTimeOptions, nTimeOptionsToSelect ) 
  ncomb                       <-  dim( timePointChoices )[ 1 ]  # min and max shoud be incuded 
  # take small number of random sccepms
  set.seed(1234)
  indSample                   <-  sample( ncomb, nChoicesSubset, replace = FALSE )
  samplePointsInternal        <-  timePointChoices[ indSample , ]
  samplePoints                <-  data.frame( samplePointsInternal + 1, nFulltimePoints)
  samplePoints                <-  as.matrix(samplePoints) 
  
  #tranform to actual time points
  timePointMatrix             <-  matrix( fullTimePoints[ samplePoints ] , nrow = nrow(samplePoints ) )    
  
  
  colnames( timePointMatrix ) <-  paste0( "time" , 1:nTimePointsSelect)
  rownames( timePointMatrix ) <-  paste0( "timeOption" , 1:nChoicesSubset )
              
  
  exampleTimes                <-  new("SetOfTimePoints", 
      .Data                = timePointMatrix , # .Data should be first slot
      fullTimePoints       = fullTimePoints  ,
      nFullTimePoints      = nFulltimePoints   ,
      nTimePointsSelect    = nTimePointsSelect  ,
     nTimePointOptions     = nChoicesSubset    
  )
#  str(exampleTimes)
  exampleTimes
}





#' @rdname getTimePoints
#' @export
setMethod( "getTimePoints", "SetOfTimePoints",
    function( object ) { 
      return( object@fullTimePoints )
    }
)

if( 0 == 1 ) {
  # example set of timepoints 
  timeZonesEx                 <-  getExampleTimeZones()
  fullTimePointsEx            <-  seq( 0 , 21 , 1 )
  object                      <-  getAllTimeOptions( timeZones = timeZonesEx ,
      fullTimePoints = fullTimePointsEx )
  str( object )
}

#summarySetOfTimePoints        <-  function( object ) {
#  
#  dataIndicators              <-  getData( object )
#  fullTimePoints              <-  getTimePoints( object ) 
#  timesPerOption              <-  fullTimePoints[ dataIndicators ]
#  timesPerOptionMatrix        <-  matrix(timesPerOption , nrow = nrow( dataIndicators ) ) 
#}
#
#setMethod( f = "summary" ,
#  signature = "SetOfTimePoints" ,
#  definition = summarySetOfTimePoints
#)


#' @rdname getData
#' @export
setMethod( "getData", "SetOfTimePoints",
    function( object ) { 
      return( object@.Data)
    }
)




#' @rdname getNames
#' @export
setMethod( "getNames" , "SetOfTimePoints" ,
  function( object ){
    dimNames         <-  dimnames( object@.Data )
    names            <-  dimNames[[ 1 ]]
    return( names )
  }
)

#' @rdname getRanking
#' @export
setMethod( "getRanking", "SetOfTimePoints",
    function( object ) { 
      return( object@ranking)
    }
)

#' @rdname setRanking
#' @export
setReplaceMethod( f = "setRanking",
    signature = "SetOfTimePoints",
    definition = function( object, value) {
      object@ranking     <-  value
      validObject( object )
      return( object )
    }
)


if( 0 == 1 ){
  object                             <-  getExampleSetOfTimePoints( 0 :10 ) 
  pkData                             <-  getPkData(getExamplePkModel() , getTimePoints( object ) , 1 , 5 )
  objectRanked                       <-  rankObject( object , pkData )
  object                             <-  objectRanked
  rank                               <-  2
  extractRank.timePoints( object, 5 )  
  extractByRank( object, 1)
}


extractRank.timePoints               <-  function( object , rank ) {
  ## check inputs 
  ranking                            <-  getRanking( object )
  checkNoRank                        <-  all( dim( ranking ) == c(0,0) ) 
  if( checkNoRank  )  {
    stop( "SetOfTimePoints is not ranked" )
  } 
  checkRankOk                          <-  length( rank ) == 1 
  if( ! checkRankOk) {
    stop( "Only one rank should be selected" )  
  }
  ## extract rank 
  indRanks                           <-  getTopNRanking( ranking , rank  )
  rankToSelect                       <-  indRanks[ rank ]
  ## subset on rank 
  object[rankToSelect  , , drop = FALSE ]
}

#' @rdname extractByRank
#' @examples 
#'   object                             <-  getExampleSetOfTimePoints( 0 :10 ) 
#'   pkData                             <-  getPkData(getExamplePkModel() ,
#'  getTimePoints( object ) , 1 , 5 )
#' objectRanked                       <-  rankObject( object , pkData , nGrid = 20,
#'   nSamplesAvCurve = 25 )
#'   extractByRank( objectRanked , 1)
#'   extractByRank( object = objectRanked , rank = 5 )
#' @export 
setMethod( "extractByRank" , signature = c( "SetOfTimePoints" , "numeric" ) ,
  definition = extractRank.timePoints  )

# TODO: plot method, full time points and time point selection

if( 0 == 1 ) {
  extrac
}


