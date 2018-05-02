##############################################################################
### Project: microsamplingDesign                                           ###
###                                                                        ###
### Description: library of constraint functions   ("check_")              ###
###                                                                        ###
###                                                                        ###
###  Remarks:
###      check_[level in subject or scheme]_[function name]
###      #TRUE is pass constraint FALSE is failed to meet constraint       ###
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

# TODO add more checkFunctions  

                    

### examples for debugging 
if( 0 == 1 ) {
  exampleSubject1                      <-  c( TRUE  , TRUE  , TRUE , FALSE , FALSE , TRUE )
  exampleSubject2                      <-  c( FALSE , FALSE , TRUE , FALSE , FALSE , TRUE )
  exampleScheme                        <-  rbind( exampleSubject1, exampleSubject2 ) 
  
  subjectChecks                        <-  data.frame( check = c( "maxConsecSamples" , "maxConsecSamples" ) ,
                                                       level = c( "subject" , "subject" ) ,
                                                       value =  c( 5, 1), stringsAsFactors = FALSE ) 
  schemeChecks                         <-  data.frame( check = c( "minObsPerTimePoint" , "minObsPerTimePoint" ) ,
                                                       level = c( "scheme" , "scheme" ) ,
                                                       value =  c( 1, 2) , stringsAsFactors = FALSE ) 
  exampleChecks                        <-  rbind( subjectChecks , schemeChecks )
  
}

#' get a minimal example of a constraint data frame
#' 
#' @export
getConstraintsExample                  <-  function( ) {
  subjectChecks                        <-  data.frame( check = c( "maxConsecSamples" , "maxConsecSamples" ) ,
      level = c( "subject" , "subject" ) ,
      value =  c( 5, 3), stringsAsFactors = FALSE ) 
  schemeChecks                         <-  data.frame( check = c( "minObsPerTimePoint" , "minObsPerTimePoint" ) ,
      level = c( "scheme" , "scheme" ) ,
      value =  c( 1, 2) , stringsAsFactors = FALSE ) 
  exampleChecks                        <-  rbind( subjectChecks , schemeChecks )
  return( exampleChecks )
}

if( 0 == 1 ) {
  # development
  object                               <-  exampleSubject1
  checks                               <-  exampleChecks 
  level                                <-  "subject"
  
  #examples
  doAllSchemeChecks( exampleSubject1 , "subject" , checks = exampleChecks )
  doAllSchemeChecks( exampleSubject2 , "subject" , checks = exampleChecks )
  doAllSchemeChecks( exampleScheme , "scheme" , checks = exampleChecks )
  
}


#' check whether either a 1 subject or multiple subject microsampling scheme meets imposed constraints 
#' 
#' @param object a logical vector or matrix, TRUE when a sample is taken for a subject (row) and time point (column) combination
#' @param level a character vector indicating either "subject" or "scheme" level 
#' @param checks a dataframe with check definitions
#' @return logical value TRUE when all checks are pasted and FALSE if at least one check fails
#' @examples 
#'   exampleChecks    <- getConstraintsExample()
#'   exampleSubject1  <-  c( TRUE  , TRUE  , TRUE , FALSE , FALSE , TRUE )
#'   exampleSubject2  <-  c( FALSE , FALSE , TRUE , FALSE , FALSE , TRUE )
#'   exampleScheme    <-  rbind( exampleSubject1, exampleSubject2 ) 
#'   doAllSchemeChecks( exampleSubject1 , "subject" , checks = exampleChecks )
#'   doAllSchemeChecks( exampleSubject2 , "subject" , checks = exampleChecks )
#'   doAllSchemeChecks( exampleScheme , "scheme" , checks = exampleChecks )
#' @export
doAllSchemeChecks                           <-  function( object ,  level  , checks ) { 
  ## filter out level
  flagLevel                           <-  checks$level == level
  checksLevel                         <-  checks[ flagLevel  , ]
  checksLevel$checkFunction           <-  paste( "check" , level , checksLevel$check , sep = "_" ) 
  ## one failed test is return FALSE 
  output                              <-  TRUE
  iCheck                              <-  1
  nCheck                              <-  nrow( checksLevel )
  while( ( output == TRUE ) && ( iCheck <=  nCheck ) ) {
    checkLine                         <-  checksLevel[ iCheck , ]
    checkFunction                     <-  get( checkLine$checkFunction ) 
    output                            <-  checkFunction( object , checkLine$value )
#    print(checkLine)
#    cat( iCheck , "of" , nCheck , "result =",   output , '\n' )
    iCheck                            <-  iCheck + 1
  }
  output
}

### per subject checks (one line)

if( 0 == 1 ) {
  subjectScheme                        <-  exampleSubject1
  value                                <- 2
  check_subject_maxConsecSamples( exampleSubject1 , 2 ) 
  check_subject_maxConsecSamples( exampleSubject1 , 3 ) 
  check_subject_maxConsecSamples( exampleSubject1 , 4 ) 
  check_subject_maxConsecSamples( exampleSubject2 , 1 )  
}
#' check the maximum of consecutive samples per subject falls below the specified value
#' 
#' @param subjectScheme a one subject scheme, one line of a scheme
#' @param value to compare scheme with
#' @export
check_subject_maxConsecSamples         <-  function( subjectScheme , value ) {
  maxStreaks                           <-  getMaxStreaks( subjectScheme )  
  pass                                 <-  maxStreaks  <=  value
  return( pass )
} 

# streak length help function https://baseballwithr.wordpress.com/2014/07/07/team-streaks-part-i-2/
getMaxStreaks                            <-  function( y ) {
  n <- length(y)
  where                                  <-  c(0, y, 0) == 0
  location.zeros                         <-  ( 0 : (n + 1) )[ where ]
  streak.lengths                         <-  diff(location.zeros) - 1
  maxStreak                              <-  max(streak.lengths[streak.lengths > 0] )
  return( maxStreak )
}


### scheme level checks 
if( 0 == 1 ) {
  scheme                                 <-  exampleScheme
  value                                  <-  1
  check_scheme_minObsPerTimePoint( exampleScheme , 0 )
  check_scheme_minObsPerTimePoint( exampleScheme , 1 )
  check_scheme_minObsPerTimePoint( exampleScheme , 2 )
  check_scheme_minObsPerTimePoint( exampleScheme , 200 )
}
#' check the mimimum observations per time points is above a specified value  
#'
#' @param  scheme a microsampling scheme
#' @param value numeric constant 
#' @export                            
check_scheme_minObsPerTimePoint          <-  function( scheme , value ) {
  nObsPerTimePoint                       <-  colSums( scheme )
  inCheck                                <-  nObsPerTimePoint >= value 
  pass                                   <-  all( inCheck )
  pass  
}



### scheme level checks 
if( 0 == 1 ) {
  scheme                                 <-  exampleScheme
  value                                  <-  1
  check_scheme_exactNumberObsPerTimePoint( exampleScheme , 0 )
  check_scheme_minObsPerTimePoint( exampleScheme , 1 )
  check_scheme_minObsPerTimePoint( exampleScheme , 2 )
  check_scheme_minObsPerTimePoint( exampleScheme , 200 )
}
#' check the number of observations per time points is equal specified value  
#'
#' @param  scheme a microsampling scheme
#' @param value numeric constant 
#' @export                            
check_scheme_exactNumberObsPerTimePoint          <-  function( scheme , value ) {
  nObsPerTimePoint                          <-  colSums( scheme )
  inCheck                                   <-  nObsPerTimePoint == value 
  pass                                      <-  all( inCheck )
  pass  
}

