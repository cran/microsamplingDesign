##############################################################################
### Project: microsamplingDesign                                           ###
###                                                                        ###
### Description: gerate                                                             ###
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

#' @include constraintFunctions.R
NULL

if( 0  == 1 ) {

## for debugging  

minNSubjects        <-  4
maxNSubjects        <-  4
minObsPerSubject    <-  4
maxObsPerSubject    <-  4
timePoints          <-  1:7
constraints = NULL # this should not effect N schemes before constraints
constraints  <-  getConstraintsExample()
maxRepetitionIndSchemes <-  1 
maxNumberOfSchemesBeforeChecks = 10^7
returnNSchemesBeforeConstraints = FALSE
}

#' Generate a  \code{\link{SetOfSchemes-class}} object of speficified dimensions ( subjects, observations per t) for a given 
#' set of time points which  meets user specified constraints
#' 
#' @note keep number of subjects , range of number of subjects and observations per subject and number of timep points restricted 
#' to avoid a large number of potential schemes slowing down computation and increasing memory usage
#' @note only schemes with minimal one observation per subject are contained even if not specified in constraints 
#' @param minNSubjects numeric, the mimimum number of subjects per scheme
#' @param maxNSubjects numeric, the maximum number of subjects per scheme
#' @param minObsPerSubject numeric, the mimimum number of sampling occasions per subject 
#' @param maxObsPerSubject numeric, the maximum number of sampling occasions per subject 
#' @param timePoints numeric vector of time points larger then zero, at which subject can be sampled
#' @param constraints data.frame specifying constraints the scheme should meet.
#'   with columns:
#'   \itemize{
#'    \item check: identifier of the function to perform the check 
#'    \item level: the level at wich the check is applied: either at the subject level or scheme level
#'    \item value: input value used by the check function
#' }  
#'  (a user can add constraint functions following 
#' naming convention \code{check_[level]_[check]}  see examples:  ( \code{\link{check_scheme_minObsPerTimePoint}}  and \code{\link{check_subject_maxConsecSamples}}) )
#' remark: number of subjects per scheme or number of observations per scheme should not be specified in \code{constraints}
#' @param maxRepetitionIndSchemes the maximum number of times an individual subject scheme can be repeated, defaults to 1
#' @param maxNumberOfSchemesBeforeChecks the maximum number of schemes to consider before applying scheme constraints,
#'  to avoid to long processing and using up memory.
#' defaults to 10^5
#' @param returnNSchemesBeforeConstraints if \code{TRUE} return only number of schemes before checking constraints instead of the schemes themselves, defaults to \code{FALSE}
#' @examples 
#'   timePoints          <-  c( 1.2 , 1.3 ,  2, 5  )
#'   constraints         <-  getConstraintsExample()
#'   ex1   <-  getSetOfSchemes( minNSubjects = 4 , maxNSubjects = 4 ,
#'     minObsPerSubject = 3 , maxObsPerSubject = 3 , timePoints , constraints )
#'   ex2   <-  getSetOfSchemes( minNSubjects = 4 , maxNSubjects = 4 ,
#'     minObsPerSubject = 3 , maxObsPerSubject = 3 , timePoints ,
#'     constraints ,  maxRepetitionIndSchemes = 1 )
#'   ex3   <-  getSetOfSchemes( minNSubjects = 4 , maxNSubjects = 4 , 
#'     minObsPerSubject = 2 , maxObsPerSubject = 3 , timePoints ,
#'     constraints , maxRepetitionIndSchemes = 1 )
#'   ex4   <-  getSetOfSchemes( minNSubjects = 2 , maxNSubjects = 5 ,
#'     minObsPerSubject = 2 , maxObsPerSubject = 3 , timePoints ,
#'     constraints , maxRepetitionIndSchemes = 1 )
#'   ex5   <-  getSetOfSchemes( minNSubjects = 2 , maxNSubjects = 5 ,
#'     minObsPerSubject = 2 , maxObsPerSubject = 3 , timePoints , 
#'     maxRepetitionIndSchemes = 2 )
#'   \dontrun{
#'     # this should trow an error (to many combinations required )
#'     ex6   <-  getSetOfSchemes( minNSubjects = 2 , maxNSubjects = 5 , 
#'        minObsPerSubject = 2 ,  maxObsPerSubject = 3 , timePoints ,
#'        maxRepetitionIndSchemes = 2 ,  maxNumberOfSchemesBeforeChecks = 1000 )
#' }
#' @importFrom gtools combinations
#' @importFrom plyr aaply 
#'@export
getSetOfSchemes            <-  function( minNSubjects , maxNSubjects , minObsPerSubject , maxObsPerSubject ,
    timePoints , constraints = NULL , maxRepetitionIndSchemes = 1  , maxNumberOfSchemesBeforeChecks = 10^5 , returnNSchemesBeforeConstraints = FALSE ) {
  
  nTimePoints                   <-  length( timePoints )
  
  # constraints 
  minimalConstraint             <-  data.frame( check = "minObsPerTimePoint" , level = "scheme" , value = 1 , stringsAsFactors = TRUE) 
  if( is.null( constraints ) ) {
    constraintsSet              <-  minimalConstraint
  } else {
    # check constraint object 
    checkConstraintsOk( constraints )
    constraintsFull             <-  rbind( constraints , minimalConstraint )
    constraintsSet              <-  unique(constraintsFull)
  }
  
  subjectConstraints            <- constraintsSet[ constraintsSet$level == "subject" ,  ]
  schemeConstraints             <- constraintsSet[ constraintsSet$level == "scheme" ,  ]
  
  ##  all sampling options per rat (indicators)
  obsPerSubjectPossible         <-  minObsPerSubject : maxObsPerSubject 
  allSchemes1Subject            <-  lapply( obsPerSubjectPossible , function( x ) {
        combinations( r = x ,  n = nTimePoints )
      } 
  )
  
  ##  transform schemes to logical
  #  internal convert to logical function
  convertSchemeToLogical        <-  function( indicators , times = timePoints ) {
    timeSelect                  <-  times[ indicators ]
    logicalIndicators           <-  times %in% timeSelect
    return( logicalIndicators )
  }
  
  convertBlockToLogical         <-  function( x ) {  aaply( .data = x , .margins = 1 , .fun = convertSchemeToLogical ) }
  
  ## convert schemes to logical on each block then join blocks together
  logicalBlocks                 <-  lapply( allSchemes1Subject , convertBlockToLogical)
  allSchemes1SubjectLogical     <-  do.call( rbind , logicalBlocks )
  
  ## check 1 subject schemes meets subject level constraints and select
  checkNoSubjectConstraints     <- ( nrow( subjectConstraints ) == 0 ) ||  (  returnNSchemesBeforeConstraints )
  if( checkNoSubjectConstraints ) {
    subjectSchemesOk            <-  allSchemes1SubjectLogical
  } else { 
    flagPassChecks              <-  apply( allSchemes1SubjectLogical , 1 , FUN = doAllSchemeChecks , 
        level = "subject" , checks = subjectConstraints )
    
    
    subjectSchemesOk            <-  allSchemes1SubjectLogical[ flagPassChecks ,  ]
  }
  
  
  nSubjectSchemesOk             <-  nrow( subjectSchemesOk )
  
  ##  check whether we have any schemes left 
  if( (nSubjectSchemesOk * maxRepetitionIndSchemes < maxNSubjects )  ) { 
    stop( "number of 1 subject schemes meeting constraints is smaller then the number of subjects to choose from" )
  }
  
  ##  extract samples per scheme
  subjectOptions                <-   minNSubjects : maxNSubjects
   
    # check number of schemes (combinations)
    nSchemesPerNSubject         <-   lapply( subjectOptions , function( i ) {
      getCombinationsWithMaxNRepetitions( sourceVector = 1:nSubjectSchemesOk , nDraws = i , maxRepetitions = maxRepetitionIndSchemes , 
          nCombinationsOnly = TRUE  )
      }
    )      
    nSchemesToGenerate          <-  sum( unlist( nSchemesPerNSubject ) )
    if( returnNSchemesBeforeConstraints  ) { return(nSchemesToGenerate) }
    checkNumberSchemesOk        <-  nSchemesToGenerate  <=  maxNumberOfSchemesBeforeChecks
    if( ! checkNumberSchemesOk ) {
      stop( "The number of schemes to generate " , nSchemesToGenerate ,
          " (before applying scheme constraints) ", "is larger than the  maximum allowed: " , maxNumberOfSchemesBeforeChecks , "\n" )
    }

    #  calculate schemes themselves
  indSchemePerSubject           <-   lapply( subjectOptions , function( i ) {
        getCombinationsWithMaxNRepetitions( sourceVector = 1:nSubjectSchemesOk , nDraws = i , maxRepetitions = maxRepetitionIndSchemes  )
      }
  ) 
  
  ## equalise subject per scheme to max number of subjects per scheme 
  #  create empty scheme i.e. no subject  
  emptySubjScheme               <-  rep( FALSE , nTimePoints )  
  nrEmptySubjScheme             <-  nSubjectSchemesOk + 1
  subjectSchemesOkplusEmpty     <-  rbind( subjectSchemesOk , emptySubjScheme )
  
  # fill up scheme indicators and match together 
  indSchemePerSubjectFilled     <-  lapply( indSchemePerSubject , extendMatrixColumns ,  with = nrEmptySubjScheme , nColAim = maxNSubjects )
  indSchemePerSubjectMatrix     <-  do.call( rbind , indSchemePerSubjectFilled )
  vectorIndSchemes              <-  as.vector(  indSchemePerSubjectMatrix  ) # selection of schemes 
  rawSchemes                    <-  subjectSchemesOkplusEmpty[ indSchemePerSubjectMatrix ,  ] 
  nSchemes                      <-  prod( dim( rawSchemes ) ) / ( maxNSubjects * nTimePoints ) 
  arraySchemes                  <-  array( rawSchemes , dim = c(  nSchemes , maxNSubjects ,  nTimePoints  ) )  
  arraySchemesFormat            <-  aperm( arraySchemes , c( 2 , 3 , 1 )  )  
  
  
#   if( 0 == 1 ){
#     ## compare alternatives speed
#     library( microbenchmark )
#     compareOldNew    <-  microbenchmark(   
#         flagSchemesMeetingConstraints(  arraySchemesFormat   , schemeConstraints ) ,
#         apply( arraySchemesFormat , 3 , "doAllSchemeChecks" , level = "scheme" , checks = schemeConstraints ) ,
#         times = 5
#     ) # about 500 times faster
#     
#   }
 
  ##  check constraints on scheme level (
  flagSchemesOk                 <-  flagSchemesMeetingConstraints(  arraySchemesFormat   , schemeConstraints )
#  flagSchemesOk                 <-  apply( arraySchemesFormat , 3 , "doAllSchemeChecks" , level = "scheme" , checks = schemeConstraints )
  
  nOutputSchemes                <-  sum( flagSchemesOk )
  if( nOutputSchemes == 0 ) {  stop("no schemes meet constraints, alter parameters or constraints" ) }
  outputSchemes                 <-  arraySchemesFormat[ , , flagSchemesOk , drop = FALSE  ] 
  
  dimnames( outputSchemes )     <-  list(    paste0( "subject"  , 1 : maxNSubjects    ),
      paste0( "timePoint", 1 : nTimePoints     ),
      paste0( "scheme"   , 1 : nOutputSchemes  )
  )
  
  ## return output  
  outputSetOfSchemes            <-  new( "SetOfSchemes" , 
      .Data             = outputSchemes ,                       # remark .Data always first slot 
      timePoints        = timePoints , 
      nSchemes          = nOutputSchemes ,
      nSubjects         = maxNSubjects , 
      designConstraints = constraintsSet
  ) 
  
  return( outputSetOfSchemes )
}


if( 0 == 1 ) {
  # test no errors
  constraints                    <-  getConstraintsExample( ) 
  checkConstraintsOk( constraints ) 
  # test get all errors 
  constraintsErrors              <-  constraints
  colnames(constraintsErrors)    <-  c("checks" , "level" , "value")
  constraintsErrors              <-  rbind(constraintsErrors , c( "balabakla" , "ebe" , 100)  )
  constraintsErrors              <-  as.matrix( constraintsErrors )
  # TODO test in function
  checkConstraintsOk( constraintsErrors )  
}

#' test constraints well specified
#' @keywords internal  
checkConstraintsOk              <-  function( constraints ) {
  errors                        <-  character( )
  checkClass                    <-  class( constraints ) == "data.frame"
  if( ! checkClass ) {
    msg                         <-  paste0( "'constraints' should be data.frame" , "\n" )
    errors                      <-  c( errors , msg )
  }
  names                         <-  c( "check" ,  "level" ,  "value")
  checkNames                    <-  all( names %in% colnames( constraints ) )
  if( ! checkNames ) {
    msg                         <-  paste0( " column names of 'constraints' should be: " ,  paste0( names , collapse = ", " )  , '\n')
    errors                      <-  c( errors , msg )
  }
  levels                        <-  c( "subject" , "scheme" )
  checkLevels                   <-  all( constraints[  , "level" ] %in% levels )
  if( ! checkLevels ) {
    msg                         <-  paste0( " column 'levels' in 'constraints' should only contain values in : (" ,
        paste0( levels , collapse = ", " ) , ')' , "\n" )
    errors                      <-  c( errors , msg )
  }
  # stop or not 
  if( length( errors ) > 1  ) {
    stop( errors )
    return( FALSE )
  } else {
    return( TRUE )
  }
}



##  check constraints on scheme level (
if( 0 == 1 ){
  dataSchemes   = arraySchemesFormat
  schemeConstraints
 
}

#' internal function to check scheme constraints ( faster )
#' 
#' @param dataSchemes data array for schemes 
#' @param schemeConstraints data frame with schemeConstraints
#' @importFrom matrixStats colMins colMaxs
#' @keywords internal
flagSchemesMeetingConstraints    <-  function( dataSchemes , schemeConstraints ){
  ## process inputs 
  constraints                    <-  schemeConstraints$check
  optimizedConstraints           <-  c( "minObsPerTimePoint" , "exactNumberObsPerTimePoint" )
  boolCheckExactObs              <-  "exactNumberObsPerTimePoint" %in% constraints
  minObsPerTime                  <-  max( schemeConstraints[ schemeConstraints$check == "minObsPerTimePoint" , "value" ] ) # when multiple values use stricted one
  flagOptimizedConstraints       <-  constraints %in%  optimizedConstraints
  schemeConstraintsOther         <-  schemeConstraints[ !flagOptimizedConstraints ,  ]
  
  ## efficient calc array
  obsPerTimePoint                <-  colSums( dataSchemes ) # errror here wrong dimension?
  minPerTimePoint                <-  colMins( obsPerTimePoint ) 
  
  ## min obse per time point (always needed)
  flagMinObs                     <-  minPerTimePoint >= minObsPerTime
  
  # test it does the correct thing
#  ind <-  50 :300
#  data.frame(minPerTimePoint[ind] ,  flagMinObs[ind]    )
  
  
  ## exact obse per time point 
  if( boolCheckExactObs ) {
    exactObs                     <-  schemeConstraints[ schemeConstraints$check == "exactNumberObsPerTimePoint" , "value" ]     
    maxPerTimePoint              <-  colMaxs( obsPerTimePoint )
    flagRight                    <- maxPerTimePoint == exactObs 
    flagLeft                     <- minPerTimePoint == exactObs  
    flagExactObs                 <- flagRight * flagLeft
  } else {
    flagExactObs                 <-  1 
  }   
        
  
  ##  check other constraints not optimized
  nOtherChecks                   <-  nrow( schemeConstraintsOther ) 
  if( nOtherChecks > 0 ) {
    flagOtherConstraints         <- apply( dataSchemes , 3 , "doAllSchemeChecks" , level = "scheme" , checks = schemeConstraintsOther )
  } else {
    flagOtherConstraints         <-  TRUE
  }
  ##  combine results all checks
  flagPassAllConstraints         <-  flagMinObs & flagExactObs & flagOtherConstraints
  return( flagPassAllConstraints )
}

