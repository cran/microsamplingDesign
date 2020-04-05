##############################################################################
### Project: microsamplingDesign                                           ###
###                                                                        ###
### Description : overview S4 objects  and inheritance                     ###
###                                                                        ###
###                                                                        ###
###                                                                        ###
###                                                                        ###
### Author: ablommaert                                                     ### 
###         <adriaan.blommaert@openanalytics.eu>                           ###
###                                                                        ###
###                                                                        ###
### Maintainer:ablommaert                                                  ###
###       <adriaan.blommaert@openanalytics.eu>                             ###
###                                                                        ###  
###                                                                        ###
### version 0.1                                                            ###
###                                                                        ###
### changes:                                                               ###
###                                                                        ###
###                                                                        ###
##############################################################################

if( 0 == 1 ) {
  xxx    <-  getExampleSetOfSchemes()
  new( "SetOfSchemes" , getData( xxx ),
    timePoints = getTimePoints( xxx ) ,
    nSchemes  = 100,
    nSubjects = 50)

}

checkSetOfSchemes          <-  function( object ) {
  errors                   <-  character( ) 
  dimData                  <-  dim( object@.Data )
  nDimensions              <-  length( dimData )
  if( !nDimensions == 3 ) {
    msg                    <-  paste0( "Dimensions of data array should be 3, not:  ", nDimensions, "\n" )
    errors                 <-  c ( errors, msg )
  }
  
  subjectDimension         <-  dimData[ 1 ]
  checkSubjectConsistency  <-  subjectDimension == object@nSubjects
  if( ! checkSubjectConsistency  ) {
    msg                    <-  paste0( "subject dimension in data does not correspond to number of subjects per scheme", "\n" )
    errors                 <-  c( errors , msg )
  }
  
  timeDimension            <-  dimData[ 2 ]
  if( !timeDimension == length( object@timePoints ) ) {
    msg                    <-  paste0( "time dimension in data does not correspond to number of timePoints", "\n" )
    errors                 <-  c( errors, msg ) 
  }
  schemeDimension          <-  dimData[ 3 ]
  if ( !schemeDimension == object@nSchemes ) {
    msg                    <-  paste0("number of schemes: ", object@nSchemes, "does not match scheme dimension in data: ", schemeDimension, "\n")
    errors                 <-  c( errors , msg )
  }
  if( ! is.logical( object@.Data ) ) {
    msg                    <-  paste("Data values should be logical, non-logical values observed" , "\n" )
    errors                 <-  c( errors, msg )
  }
  samplesPerTimePoint      <-  apply( object, c(2,3), "sum")
  testSamples              <-  samplesPerTimePoint == 0
  if( sum(testSamples) > 0 ) {
    msg                    <-  paste0( "Some schemes have a timePoint without observations" , "\n" )
    errors                 <-  c(errors, msg)
  }
  ##  print errors if object not defined according to the rules 
  if ( length(errors) == 0 ) {
    TRUE 
  } else {
    cat( errors )
    FALSE # need to return FALSE to not define false object
  }
}


#' S4 class SetOfSchemes representing a set of designs with given time points
#' 
#' @name SetOfSchemes-class
#' @slot .Data a logical array of 3 dimensions ( nSubjects x nTimePoints x nSchemes )
#' @slot timePoints  numeric vector of time Points 
#' @slot nSchemes  integer value number of schemes 
#' @slot nSubjects numeric maximum number of subjects per scheme
#' @slot designConstraints  a data.frame of constraints on possible sampling schemes as background information
#' @slot ranking  is a data.frame which is the rank of the schemes according to a specific criterion
#' @aliases setOfSchemes SetOfSchemes
#' @author Adriaan Blommaert
#' @export
setClass( "SetOfSchemes", 
    slots = c(.Data = "array",  timePoints = "numeric" , nSchemes = "numeric" , nSubjects = "numeric"  , designConstraints = "data.frame", ranking = "data.frame" ) ,
        validity = checkSetOfSchemes 
)


if( 0 == 1 ) {
 setOfSchemes   <-  getExampleSetOfSchemes() 
 scheme1        <-  setOfSchemes[ , , 1]
 scheme2        <-  setOfSchemes[ , , 2]
}

### example SetOfSchemes object creation and method 
#' get a minimal example of a set of schemes object
#' 
#' @export 
getExampleSetOfSchemes       <-  function() {
  timePoints                <-  c(0.5 , 1 , 2 , 10) # vector of time points
  nTimePoints               <-  length(timePoints)
  nRats                     <-  2 
  nSamples                  <-  7 # number of sample datasets within each scenario this is a large set
  nScenarios                <-  1 # derived from number of parameters options (all combinaitons if multiple parameter definitions), generally 1 to start with
  
  nSchemes                  <-  3
  setOfSchemes              <-  array( dim = c( nRats, nTimePoints, nSchemes) )
  scheme1                   <-  matrix( c(T, F, T, T,
          F, T, F, T),                                  ,
      byrow = TRUE , nrow = nRats )
  
  scheme2                   <-  matrix( c(T, T, T, T,
          T, T, T, T),                                  ,
      byrow = TRUE, nrow = nRats )
  
  scheme3                   <-  matrix( c(F, T, T, T,
          T, T, F, T),                                  ,
      byrow = TRUE, nrow = nRats ) 
  
  setOfSchemes[  , , 1 ]    <-  scheme1
  setOfSchemes[  , , 2 ]    <-  scheme2
  setOfSchemes[  , , 3 ]    <-  scheme3
  dimnames( setOfSchemes )  <-  list( paste0("rat", 1:nRats) , paste0("timePoint", 1:nTimePoints), c( "schemexx1" , "schemexx2" , "schemexx3" )  )
#  str(setOfSchemes)
  setOfSchemes              <-  new( "SetOfSchemes" , .Data = setOfSchemes, timePoints = timePoints, nSchemes = nSchemes , nSubjects = nRats ) 
  return( setOfSchemes )
} 





#' @rdname getTimePoints
#' @export
setMethod( "getTimePoints", "SetOfSchemes",
  function( object ) { 
    return( object@timePoints )
  }
)



#' @rdname getNSchemes
#' @export
setMethod( "getNSchemes", "SetOfSchemes",
  function( object ) { 
    return( object@nSchemes )
  }
)



#' @rdname getNSubjects
#' @export
setMethod( "getNSubjects", "SetOfSchemes",
  function( object ) { 
    return( object@nSubjects )
  }
)

#' @rdname getNames
#' @export
setMethod( "getNames" , "SetOfSchemes" ,
  function(object) {
    dimNames       <-  dimnames( object@.Data )
    names          <-  dimNames[[ 3 ]]
    return(names)
  }
)


#' @rdname setTimePoints
#' @export
setReplaceMethod( f = "setTimePoints",
    signature = "SetOfSchemes",
    definition = function( object, value) {
      object@timePoints     <-  value
      validObject( object )
      return( object )
    }
)




#' @rdname setRanking
#' @export
setReplaceMethod( f = "setRanking",
    signature = "SetOfSchemes",
    definition = function( object, value) {
      object@ranking     <-  value
      validObject( object )
      return( object )
    }
)




### extract scheme by rank 

if( 0 == 1 ) {
  object        <-  getExampleSetOfSchemes()
  pkData        <-  getPkData( getExamplePkModel() , 
    getTimePoints( object ) , getNSubjects( object ) , nSamples = 10  )
  objectRanked    <- rankObject(  object ,  pkData  , data.frame(criterion = "auc" , weight = 1, stringsAsFactors = TRUE) ) 
  object          <-  objectRanked
  
  rank = 3
}

extractRank.setOfSchemes             <-  function( object , rank ){
  ## check inputs 
  ranking                            <-  getRanking( object )
  checkNoRank                        <-  all( dim( ranking ) == c(0,0) ) 
  if( checkNoRank  )  {
    stop( "SetOfSchemes is not ranked" )
  } 
  checkRankOk                          <-  length( rank ) == 1 
  if( ! checkRankOk) {
    stop( "Only one rank should be selected" )  
  }
  ## extract rank 
  indRanks                           <-  getTopNRanking( ranking , rank  )
  rankToSelect                       <-  indRanks[ rank ]
  ## subset on rank 
  object[  , , rankToSelect  , drop = TRUE ] # in matrix form 
} 

#' @rdname extractByRank
#' @examples
#'   object        <-  getExampleSetOfSchemes()
#'   pkData        <-  getPkData( getExamplePkModel() , 
#'    getTimePoints( object ) , getNSubjects( object ) , nSamples = 10  )
#'   objectRanked    <- rankObject(  object ,  pkData  ,
#'     data.frame(criterion = "auc" , weight = 1 , stringsAsFactors = TRUE) ) 
#'  extractByRank( object = objectRanked  , rank = 1 )
#'  extractByRank( objectRanked  , rank = 5 )
#' @export 
setMethod( "extractByRank" ,  signature = c( "SetOfSchemes" , "numeric" ) ,
  definition = extractRank.setOfSchemes )


#' @rdname getData
#' @export
setMethod( "getData", "SetOfSchemes",
    function( object ) { 
      return( object@.Data )
    }
)

#' @rdname getRanking
#' @export
setMethod( "getRanking", "SetOfSchemes",
    function( object ) { 
      return( object@ranking)
    }
)




#' summarize object 
#' 
#' @param object \code{\link{SetOfSchemes-class}}
#' @param printToConsole logical value if \code{TRUE} prints to console ,
#'  if \code{FALSE} outputs text element ,
#'  defaults to \code{TRUE}
#' @export
setMethod( "summary", "SetOfSchemes" ,
    function( object , printToConsole = TRUE ) {
      TimePoints            <-  getTimePoints( object )
      dimData               <-  dim( getData( object ) )
      nRats                 <-  dimData[ 1 ]
      nSchemes              <-  object@nSchemes
      output                <-  paste0("Contains " ,  nSchemes , " distinct sparse microsampling schemes; "  ,
           "sampled at time points: {", paste(TimePoints , collapse = ", ") , "}" ,  "; "  ,
           "with a maximum of ", nRats, " animals used in each sparse microsampling scheme." 
           )
      if( ! printToConsole ) {
        return( output ) 
      } else {
        outputFormat     <-  sub("; " , paste0("; " , "\n") , output )
        cat( outputFormat )
      }
           
    } 
)




#' construct user defined \code{\link{SetOfSchemes-class}}
#' 
#' @param schemes array representing \code{.Data} slot of \code{\link{SetOfSchemes-class}}
#' @param timePoints numeric vector, \code{timePoinst} slot of \code{\link{SetOfSchemes-class}}
#' @examples 
#'   schemes                      <-  getData( getExampleSetOfSchemes() ) 
#'   timePoints                   <-  exp(1:4) 
#'   constructSetOfSchemes( schemes , timePoints)
#' @export
constructSetOfSchemes                  <-  function( schemes , timePoints   ) {
  nSchemes                    <-  dim( schemes )[ 3 ]
  nSubjects                   <-  dim( schemes )[ 1 ]
  new( "SetOfSchemes" , 
    .Data             =  schemes ,
    nSchemes          =  nSchemes , 
    timePoints        =  timePoints ,
    nSubjects         =  nSubjects
    ) 
}

if( 0 == 1 ) {
  setOfSchemes              <-  getExampleSetOfSchemes()
  schemes                   <-  getData( setOfSchemes )
  extendedSetOfSchemes      <-  addSchemes( setOfSchemes , schemes )
}



#' add user defined scheme to an existing \code{\link{SetOfSchemes-class}}
#'  or extend an existing set of schemes object with additional schemes
#' 
#' @param setOfSchemes  \code{\link{SetOfSchemes-class}} object or a matrix of indiviudual schemes 
#' @param extraSchemes array of schemes to add, see code{\link{SetOfSchemes-class}}
#' @importFrom abind abind 
#' @export 
addSchemes                 <-  function( setOfSchemes , extraSchemes ) {
  ## data element
  schemesData                <-  getData( setOfSchemes )
  newSchemeData              <-  abind( schemesData , extraSchemes , along = 3  )
  numberOfSchemes            <-  dim( newSchemeData )[ 3 ]
  ## set all other elements
  setOfSchemesExtended       <-  new( "SetOfSchemes" , 
    .Data = newSchemeData , 
    timePoints =  getTimePoints( setOfSchemes ) ,
    nSchemes  = numberOfSchemes , 
    nSubjects = getNSubjects( setOfSchemes )
    ) 
  return( setOfSchemesExtended )
   
}









