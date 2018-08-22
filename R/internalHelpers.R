##############################################################################
### Project: microsamplingDesign                                           ###
###                                                                        ###
### Description: function used in internal funcitons to avoid replication  ###
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
### version 1.0.4                                                          ###
###     * debugging write down in addAdditiveErrorToPkMatrix               ###
###                                                                        ###
###                                                                        ###
###                                                                        ###
##############################################################################

if( 0 == 1 ) {
  vector <-  c(1,2,3)
  nReplicates = 5
  replicateVectorRows( vector , nReplicates )
  replicateVectorColumns( vector , nReplicates )

x <- matrix(1:10,ncol=2)
as.list(x)
colnames( x ) <-  c( "a" , "b" )
convertMatrixRowsToList(x)

}


# convert a vector into a matrix by row
replicateVectorRows             <-  function( vector , nReplicates ) {
    matrix(  vector , nrow = nReplicates , ncol = length( vector ) , byrow = TRUE  )
}


# convert a vector into a matrix by columns

replicateVectorColumns       <-  function( vector , nReplicates ) {
    matrix(  vector , ncol = nReplicates , nrow = length( vector ) , byrow = FALSE )
  }
  
# convert matrix to a list by rows
convertMatrixRowsToList          <-  function( matrix ){
  lapply( seq_len( nrow(matrix) ), function(i) as.list(matrix[ i , ]) )
}


  # internal function fill up a matrix with number of columns
if( 0 == 1 ) {
  matrix                 <- indSchemePerSubject[[ 1 ]]
  nColAim                <-  6
  with                   <-  50
  test                   <-  extendMatrixColumns( matrix, nColAim, with )
}
extendMatrixColumns      <-  function( matrix , nColAim , with  ) {
  currentColumns         <-  ncol( matrix )
  nExtraColumns          <-  nColAim - currentColumns
  if( nExtraColumns < 0 ) { stop("nColAim lower than number of columns in a matrix") }
  extraColumns           <-  matrix( with , nrow = nrow( matrix ) ,  ncol = nExtraColumns , byrow = FALSE ) 
  outputMatrix           <-  cbind( matrix , extraColumns )
  return( outputMatrix )
}


## add random error to a mean value
if( 0 == 1 ) {
  pkData                <-  getPkData( getExamplePkModel() , 1:5 , 4 , 3  ) 
#  dataArray             <-  getData( pkData ) 
#  dataArray             <- PkData  
  coeffVariation        <-  0.5 # one value 
  timeCorrelation = diag(5)
#  timeCorrelation   <-  matrix( 1, ncol = 5 , nrow = 5)
}

addAdditiveErrortoPkData                <-  function( pkData , coeffVariation , timeCorrelation ) {
  # extract info 
  nTimePoints                           <-  dim( pkData )[ 2 ]
  nSubjectsPerScheme                    <-  dim( pkData ) [1]
#  if( is.null( timeCorrelation ) ) {
#    corrMatrix                  <-  diag( nTimePoints )
#  }
  meanMatrix                            <-  flattenPkData( pkData )
  pkMatWithError                        <-  addAdditiveErrorToPkMatrix( meanMatrix , coeffVariation , timeCorrelation )
  pkDataFormat                          <-  putPkDataInSchemeForm( pkMatWithError , nSubjectsPerScheme  )
  return( pkDataFormat )
}


##


# example
if( 0 == 1 ) {
  exampleCurve                          <-  c(0,1,2,3,2,1,0)
  nSamples                              <-  1000
  pkMatrix                              <-  matrix( rep(exampleCurve , nSamples ) , nrow = nSamples , ncol = 7 , byrow = TRUE )
  CV                                    <-  0.2
  pkMatrixwithError                     <- addAdditiveErrorToPkMatrix( pkMatrix , CV  , diag( rep(1 , 7)) )
  meanCurve                             <-  apply( pkMatrixwithError , 2 ,  mean )
  lowerCurve                            <-  apply( pkMatrixwithError , 2, quantile , probs = 0.025 )
  upperCurve                            <-  apply( pkMatrixwithError , 2, quantile , probs = 0.975 )
  x11()
  plot( x= 1:7 , y = meanCurve , type = "l" ,  col ="black" , lwd = 3 ,main = paste0( "CV = " , CV ) )
  lines( lowerCurve , type = "l" , lwd = 1 , col ="red" )
  lines( upperCurve , type = "l" , lwd = 1 , col ="green" )
  
  # repeat above plot by changing CV 
}




addAdditiveErrorToPkMatrix             <-  function( pkMatrix , coeffVariation , timeCorrelation ) {
  
  boolWriteDown                         <-  FALSE  # for debugging on different systems 
  dirIntermediateOutput                 <-  ""
  
  nTotalSubjects                        <-  nrow( pkMatrix )
  nTimePoints                           <-  nrow( timeCorrelation )
  sigmaMatrix                           <-  coeffVariation * pkMatrix 
  
  if( boolWriteDown ){
    saveRDS( object = .Random.seed , file.path( dirIntermediateOutput , "seedStartAddError_addError.rds")  )     
    
  }
  
  
#  standardNormalDraws                   <-  mvrnorm( n = nTotalSubjects , mu = rep(0 , nTimePoints ),  Sigma = timeCorrelation  )
  standardNormalDraws                   <-  genMVN( n = nTotalSubjects , mu = rep(0 , nTimePoints ),  Sigma = timeCorrelation  )
    
  pkMatWithError                        <-  pkMatrix + sigmaMatrix * standardNormalDraws 
  
  
  if( boolWriteDown ){
    saveRDS( object = sigmaMatrix , file.path( dirIntermediateOutput , "sigmaMatrix_addError.rds")  ) 
    saveRDS( object = timeCorrelation , file.path( dirIntermediateOutput , "timeCorrelation_addError.rds")  ) 
    saveRDS( object = standardNormalDraws  , file.path( dirIntermediateOutput , "standardNormalDraws_addError.rds")  ) 
    saveRDS( object = pkMatWithError , file.path( dirIntermediateOutput , "pkMatWithError_addError.rds")  ) 
    saveRDS( object = .Random.seed , file.path( dirIntermediateOutput , "seedEndAddError_addError.rds")  )     
  }
  
  return( pkMatWithError )
}

# function to flatten pk data 
flattenPkData                 <-  function( pkData ) {
  nTimePoints                 <-  dim( pkData )[2]
  pkDataReformat              <-  aperm( pkData , c( 1,  3 , 2 ) )  
  pkDataMatrix                <-  matrix( pkDataReformat , ncol =  nTimePoints )
  rownames( pkDataMatrix )    <-  paste0( "subject" , 1:nrow(pkDataMatrix) )
  colnames( pkDataMatrix )    <-  dimnames( pkData )[[2 ]]
  return( pkDataMatrix )
}



# inverse functions (nSubjects per scheme, or nSamples ) , test whether divisable 
putPkDataInSchemeForm         <-  function( pkDataMatrix , nSubjectsPerScheme ) {
  nTotalSubjects              <-  nrow( pkDataMatrix ) 
  checkDivisable              <-  ( nTotalSubjects %% nSubjectsPerScheme ) == 0
  if( ! checkDivisable ) {
    stop( "number of subjects should be divisable by the 'nSubjectsPerScheme'" )
  }
  nTimePoints                 <-  ncol( pkDataMatrix )
  nSamples                    <-  nTotalSubjects / nSubjectsPerScheme
 
  
  pkData                      <-  array( pkDataMatrix , dim = c( nSubjectsPerScheme , nSamples , nTimePoints ) )
  pkDataFormat                <-  aperm( pkData , c( 1 , 3 , 2 ) )  
  dimnames( pkDataFormat )    <- list(
      paste0( "subject"   , 1:nSubjectsPerScheme ) , 
      paste0( "timePoint" , 1:nTimePoints        ) ,
      paste0( "sample"    , 1:nSamples           )
  )  
  return( pkDataFormat )
} 



#' get all combinations with a maximum number of repetitions
#' 
#' @param sourceVector is a vector with options to draw from
#' @param nDraws the combination size
#' @param maxRepetitions the number of times an element of the \code{sampleVector} can occur in a group
#' @param nCombinationsOnly if TRUE it returns the number of combinations instead of the combinations itself, defaults to FALSE
#' @importFrom Rcpp evalCpp
#' @return a matrix with as a combination per row, unless \code{nCombinationsOnly} is \code{TRUE}
#' @examples 
#' test1    <-  getCombinationsWithMaxNRepetitions( c("a" , "b" , "c" ) ,
#'  nDraws = 2, maxRepetitions = 2 )
#' test2    <-  getCombinationsWithMaxNRepetitions( 1:5 , nDraws = 3, maxRepetitions = 3 )
#' test3    <-  getCombinationsWithMaxNRepetitions( 1:5 , nDraws = 3, maxRepetitions = 3 ,
#'  nCombinationsOnly = TRUE  )
#' @export 
getCombinationsWithMaxNRepetitions    <-  function( sourceVector , nDraws , maxRepetitions = 1   , nCombinationsOnly = FALSE  ) {
  ## check inputs
  checkUnique                         <-  length( unique( sourceVector ) ) == length( sourceVector ) 
  if( ! checkUnique ) {
    stop( "sampleVector has non-unique elements in it " )
  }
  
  sourceVectorExtended                <-  rep( sourceVector , maxRepetitions )
  ## check number of combinations , stop when its gets to big 
  nOptions                            <-  length( sourceVectorExtended ) 
  numberOfCombinations                <-  choose( nOptions , nDraws  )
#  if( numberOfCombinations > MAXNCOMBINATIONS ) {
#    stop( paste0("these settings lead to " , numberOfCombinations , " combinations, while only " , MAXNCOMBINATIONS, " are allowed", "\n" ))
#  }
#  cat( paste0("number of schemes: " ,  numberOfCombinations , "\n"   ))
  if( nCombinationsOnly == TRUE ) {
    return( numberOfCombinations )
  }
  combinationsIndicators              <-  combinations( n = , r = nDraws , nOptions ,  repeats.allowed = FALSE )
  combinationsOriginal                <-  array( sourceVectorExtended[ combinationsIndicators ] , dim = dim( combinationsIndicators ) )                  
  return( combinationsOriginal )
}



### extract rank from object , generic function extract rank , internal function from it 
 if( 0 == 1) {
	 ranking                            <-  getRanking( setOfSchemesRanked )
	 head( ranking )
	 timePointsOption
	 N = 3  
 }
 
 #' extract the top n rankings as numeric vector
 #' 
 #' @param ranking ranking slot of a \code{\link{SetOfTimePoints-class}} or \code{\link{SetOfSchemes-class}}
 #' @param nSelect the number of items to select 
 #' @param top logical value if \code{TRUE} the top of the ranking is selected, if \code{FALSE} the bottom of the ranking is selected,
 #' defaults to  \code{TRUE} 
 #' @return numeric vector of items (number of timePointOption or scheme ) from highest to lowest rank
 #' @importFrom stringr str_extract
 #' @export 
  getTopNRanking                      <-  function( ranking , nSelect , top = TRUE ) {
	if( top == TRUE ) {
		ranksToSelect                     <-  1 : nSelect	
	} else {
		nRankedParts                      <-  nrow( ranking )
		ranksToSelect                     <-  (nRankedParts - nSelect + 1  ) : nRankedParts   
	}
	
	nameInd                             <-  c( "name" )
	nameFlag                            <-  colnames( ranking ) %in% nameInd
	charIndicators                      <-  as.character( ranking[ ,  nameFlag ]  )
	numericIndicators                   <-  str_extract( charIndicators, "[[:digit:]]+" )
	rankToSelectSure                    <-  ranking$rank  %in%  ranksToSelect
	numericIndicatorsSelect             <-  as.numeric( numericIndicators[ rankToSelectSure ] )
 }

 

 
 

 #' Transform 3 way array to 2 way array
 #' 
 #' @param object \code{\link{SetOfSchemes-class}}
 #' @export
 flattenSetOfSchemes               <-  function( object ) {
   checkSetOfScheme                <-  class( object ) == "SetOfSchemes"  
   if( ! checkSetOfScheme  ) {
     stop( "object is not of class 'SetofScheme'" )
   }
   data                            <-  getData( object )
   dataNames                       <-  dimnames( object )
   schemeNames                     <-  dataNames[[ 3 ]]
   nSchemes                        <-  length( schemeNames )
   timeNames                       <-  dataNames[[ 2 ]]
   subjectNames                    <-  dataNames[[ 1 ]]
   nSubjects                       <-  length( subjectNames )
   nTimePoints                     <-  length( getTimePoints( object ) )
   dataTwist                       <-  aperm( data , perm = c( 1 , 3 , 2 ) )
   dataFlat                        <-  matrix( data , ncol = nTimePoints )
   colnames( dataFlat )            <-  timeNames
   schemeField                     <-  rep( schemeNames , rep( nSubjects , nSchemes ) )
   subjectField                    <-  rep( subjectNames , nSchemes  )
   infoColumns                     <-  data.frame( scheme = schemeField , subject =  subjectField )
   dataFlattened                   <-  cbind( infoColumns , dataFlat ) 
 }
 
 
 

 
 #' Internal function to generate multivariate data via a cholesky decompostion ( to have replicability accross systems )
 #' 
 #' @param n number of sample to take
 #' @param mu mean vector
 #' @param Sigma covariance matrix
 #' @keywords internal 
 genMVN                 <-  function(n, mu, Sigma ){
   ## obtain number of variables
   numVar               <-  length( mu )
   ## generate independent data
   indData              <-  matrix( rnorm(n*numVar),n , numVar )
 ## find the Cholesky decomposition of the covariance matrix
 choleskyDecompSigma    <- chol(Sigma)
 ## make the indepedent data dependent
 genData                <- ( indData%*% choleskyDecompSigma) + mu[col(indData%*% choleskyDecompSigma)]
 return(genData)
 }
 
# ## a simple simulation to evaluate the method
# estCov <- NULL
# estMean <- matrix( 0 , 1000, 3)
# for (iTest in 1:1000){
#   generatedMVN <- genMVN(1000, mu, sigmaMat)
#   estCov[[iTest]] <- cov(generatedMVN)
#   estMean[iTest, ] <- apply(generatedMVN, 2, mean)
# }
# 
# estMean - mu
# estCov- sigmaMat
# 
 
 