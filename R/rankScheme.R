##############################################################################
### Project: microsamplingDesign                                           ###
###                                                                        ###
### Description: given data rank different design via                      ###
### the sample variance AUC.                                               ###
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
### changes:                                                               
###    # do not rescale after combining different criteria
       # option to run ranking without default values 
###                                                                        ###
###                                                                        ###
##############################################################################




#' @include  schemeStatistics.R
NULL


if( 0 ==1 ){
  ## minumul example
  schemes           <-  getExampleSetOfSchemes()
  scheme            <-  schemes[ ,, 1 ] 
  timePoints        <-  getTimePoints( schemes )
  dataExample       <-  getPkData( getExamplePkModel() , timePoints , 2 , 25  )
  dataArray          <-  getData( dataExample )
  
  estimatedPkCurves        <-  estimatePopCurve( scheme , dataArray =  getData( dataForSchemesInput ) )
}

#' estimate population Pk curve from a scheme and pkData
#' 
#' @param scheme  one scheme from \code{\link{SetOfSchemes-class}}
#' @param dataArray \code{\link{PkData-class}}
#' @keywords internal 
estimatePopCurve           <-  function( scheme , dataArray  ) {
  dimData                  <-  dim( dataArray ) 
  nDatasets                <-  dimData[3]
  repScheme                <-  array( rep( scheme , nDatasets ), dim = dimData ) 
  sparcedData              <-  dataArray * repScheme
  
  # calculate estimate pop everage curve zeros
#  sumPerTime               <-  apply( sparcedData, c(2, 3) , "sum" )
#  nObserPerTime            <-  apply( repScheme  , c(2, 3) , "sum" )
#  averagePerTime           <-  sumPerTime / nObserPerTime
  
sumPerTime               <-  colSums( sparcedData )
nObserPerTime            <-  colSums( repScheme  )
averagePerTime           <-  sumPerTime / nObserPerTime
  return( averagePerTime )
}



if( 0 == 1 ){
  # debugging error in set of schems
  
  scheme = setOfSchemes[,,1]
  dataForSchemesInput = pkData
}

#' get a result of the criteria  per scheme zero concentration at time zero
#' 
#' @param scheme one scheme of \code{\link{SetOfSchemes-class}} 
#' @param dataForSchemesInput \code{\link{PkData-class}}
#' @param criteria a character vector of criteria such as \code{auc} see \code{\link{pkCurveStat}}
#' @param varianceMeasure a function such as \code{var}
#' 
#' @importFrom plyr aaply
#' @keywords internal 
getResultPerScheme         <-  function( scheme , dataForSchemesInput , getResultFor1Curve , varianceMeasure ) {
  timePoints               <-  getTimePoints( dataForSchemesInput  )  
  estimatedPkCurves        <-  estimatePopCurve( scheme , dataArray =  dataForSchemesInput )

  
  # add time zero, concentration zero to estimated curves 
  estimatedPkCurvesWithZero                 <-   rbind( 0 , estimatedPkCurves )
  timeWithZero                              <-  c( 0 , timePoints )		  
  ## internal function to  calculate statistics per estimate of pkCurve 
 
    ## done removed get result per criteria 
  
  criteriaPerCurve                <-  aaply( estimatedPkCurvesWithZero  ,  2 , getResultFor1Curve , times = timeWithZero , .drop = FALSE )
  varianceOfCriteria              <-  apply( criteriaPerCurve , 2 , varianceMeasure  )
  origNames                       <-  names( varianceOfCriteria  )   
  newNames                        <-  paste0( varianceMeasure , "_" , origNames  ) 
  names( varianceOfCriteria )     <-  newNames
  return( varianceOfCriteria )
}




if ( 0 == 1) {


  
  ## change: add multiple ranking functions ( in a data.frame )
  object                    <-  getExampleSetOfSchemes()
  pkData                    <-  getExampleData()
  criterion                 <-  c( "auc" , "cMax" )
  criterionWeights          <-  c( 9 , 1 ) 
  objective                 <-  data.frame( criterion = criterion , weight = criterionWeights )
  varianceMeasure           <-  "var"
  
  rankObject.SetOfSchemes ( object, pkData , getExampleObjective() )
  
  # check error messeges variance measure 
  varianceMeasure = var
  checkInputsRankSchemes( object = object , pkData = pkData , objective = objective , varianceMeasure = varianceMeasure  )
  varianceMeasure = c( "var" , "mean" )
  checkInputsRankSchemes( object = object , pkData = pkData , objective = objective , varianceMeasure = varianceMeasure  )
  
  # scale criterion
  
  object  ; pkData; objective ; varianceMeasure = "var" ; scaleCriterion = "max" 
  
  
  # check strange result in example 
	
	exampleObjective    <-   data.frame( criterion = c( "auc" , "cMax" , "tMax" )  , weight = c( 9 , 1, 1 ) ) 
	object = setOfSchemes 
	pkData = pkData 
	objective = exampleObjective 
	varianceMeasure = "var" 
	scaleWith  = "max" 
  skipTests = FALSE
  
  # debugging optimized version
 object = setOfSchemes
 pkData = dataForSchemes 
 objective = data.frame(
          criterion = "auc" , weight = 1 ) 
      
      objective = data.frame(
          criterion = c("auc" , "cMax") , weight = c(0.6 , 0.4) )
      varianceMeasure = "var" 
      scaleWith = "max" 
      nCores = 1 
      skipTests = FALSE
      
      # debugging error in output processing 

object <-  setOfSchemes 
pkData <-  dataForSchemes 
    objective <-  data.frame( criterion = "auc" , weight = 1 ) 
    skipTests = FALSE 
    nCores = 1 
    varianceMeasure = "var"
    scaleWith = "max"
    
##  problem with warnings ( duplicate names are not supported )

setOfSchemesRanked            <-  rankObject(setOfSchemes , pkData = pkData ,
  objective = exampleObjective , 		varianceMeasure = "var" , scaleWith  = "max" , 
  nCores = 1 ) 

object = setOfSchemes ; pkData = pkData ;
objective = exampleObjective ; 		varianceMeasure = "var" ; scaleWith  = "max" ; 
nCores = 1 
  
}

rankObject.SetOfSchemes        <-  function( object  , pkData, objective , varianceMeasure  , scaleWith , skipTests , nCores  ) {

  ## check input objects compatability 
  if( ! skipTests ) {
    checkInputsRankSchemes( object = object , pkData = pkData , objective = objective , varianceMeasure = varianceMeasure , scaleWith = scaleWith )
  }
  PREFIXFUNCTION                  <-    "pkCurveStat_"
  ## look up criterion function (lika AUC)
  criterionFunctions              <-    paste0( PREFIXFUNCTION  , objective$criterion )
  
  # function definition
  
  if( 0 == 1 ){
    # debug warnings
  }
  getResultFor1Curve              <-  function( curve , times   ){
    pkCurve                       <-  curve #data.frame( time = times  , concentration = curve ) # TODO not necessary to calculate data.frame 
     
    outputList                    <-  lapply( criterionFunctions , function( f ){
          do.call( f , list( curve , times ) )
        }
    )
    
    ## output formatting
    outputFormat                  <-  unlist( outputList ) 
    names( outputFormat )         <-  objective$criterion # not needed
    return( outputFormat )  
  }

  ## calculate var of criterion per scheme 

#  result_orig                    <-  aaply( getData( object ) , 3 , getResultPerScheme , 
#    dataForSchemesInput = pkData ,  getResultFor1Curve  , varianceMeasure = varianceMeasure  )                

  nSchemes                   <-  getNSchemes( object ) 
  schemeNames                <-  getNames( object )
#  cluster                     <-  makeCluster( nCores , type = "FORK" )
#  on.exit(stopCluster(cluster)) # clean up cluster  # TODO same form of cluster
  cat("start Ranking Schemes on cluster with " , nCores , "cores")
  resultList                 <-  mclapply( seq_len( nSchemes ) , function( iScheme ){
        getResultPerScheme( object[ , , iScheme ] ,  dataForSchemesInput = pkData ,  getResultFor1Curve  , varianceMeasure = varianceMeasure )   
      } ,
      mc.cores = nCores
  )  
  
  result                      <-  matrix( unlist( resultList ) , byrow = TRUE , nrow = nSchemes )
  colnames( result )          <-  paste0(varianceMeasure ,   "_" , objective$criterion )
  #all.equal( unname(result) , unname(result_orig) )
  
  
  ## combine individual criteria into objective function (if multiple criteria)
  nCriteria                  <-  nrow(  objective )
  if( nCriteria == 1 ) {
    # no scaling and weighing  
    scale                      <-  get( scaleWith )( result )
    scaledResult               <-  result / scale
    orderSchemes               <-  order( scaledResult , method = "radix" )
#    rankSchemes                <-  rank( scaledResult , ties.method = "first" ) 

#    ranking                    <-  data.frame( scheme = schemeNames , result , scaledResult , rank = rankSchemes)  #result is a vector here 
    ranking                    <-  data.frame( name = schemeNames , result , scaledResult , rank = 0 )[ orderSchemes ,  ]  
    ranking$rank               <-  seq_len( nSchemes )
#    colnames( ranking )        <-  c( "scheme" , paste0(varianceMeasure ,   "_" , objective$criterion  , c("" , "_scaled") ) , "rank" )  
    colnames( ranking )        <-  c( "name" , paste0(varianceMeasure ,   "_" , objective$criterion  , c("" , "_scaled") ) , "rank" )  
    
  } else {
    ## ranking and scaling for multiple criteria 
      # calculate max var criteria (to normalise) so individual criteria between 0 and 1
    scalingMeasure               <-  apply( result , 2 ,  scaleWith )
    scaleWeights                 <-  ( 1 / scalingMeasure )
    objectiveWeigthsNormalised   <-  objective$weight / sum(objective$weight )
    
    weightAndScaleVec            <-  scaleWeights * objectiveWeigthsNormalised 
        
    resultCombined               <-  result %*% weightAndScaleVec     
    orderSchemes               <-  order( resultCombined , method = "radix" )
    
    
    #  output element = "object with filled up slot in ranks"
#    rankSchemes               <-  rank( resultCombined , ties.method = "first" ) 
    
    ranking                   <-  data.frame( name = schemeNames , result ,  criterion = resultCombined , rank = 0 )[ orderSchemes , ]
    ranks                     <-  seq_len( nSchemes)
    ranking$rank              <-  ranks
    rownames( ranking )       <-  ranks
  }
  
   
      # remark if more than 1 measure "combinedCriterion" does not necessarily sum up to 1
#  rankingSorted             <-  arrange( ranking, rankSchemes )  # done previous step

  # output correct object 
  outputSchemes             <-  object
#  setRanking( outputSchemes )    <-  rankingSorted
if( skipTests  ) {
  outputSchemes@ranking      <-  ranking # no validation testing
} else {
  setRanking( outputSchemes )    <-  ranking  
}
  return( outputSchemes )
}


checkInputsRankSchemes      <-  function( object , pkData  , objective , varianceMeasure , scaleWith   ) {
  # schemes
  if( !class( object ) == "SetOfSchemes") { stop( "object input is not of class 'SetOfSchemes' " ) }
  if( !validObject( object ) ) { stop( "object input not a valid SetOfSchemes-class object " ) }
  # data
  if( !class( pkData ) == "PkData") { stop( "dataForSchemes input is not of class PkData" ) }
  if( !validObject( pkData ) ) { stop( "dataForSchemes is not a valid PkData-class object" ) }
  
  #  extract required data
  schemeArray               <-  getData( object )
  dimSchemes                <-  dim( schemeArray )
  schemeTimePoints          <-  getTimePoints( object )
  
  dataArray                 <-  getData( pkData )
  dimData                   <-  dim( dataArray )
  dataTimePoints            <-  getTimePoints( pkData  )
  
  
  #  check corresponding elements
  if( !identical(schemeTimePoints, dataTimePoints) ) { stop("schemes and data have a different timepoints" ) }
  if( !identical(dimSchemes[1:2]  , dimData[1:2] ) ) { stop("schemes and data have a different number of subjects or timepoints " ) }
  
  #  check objective
  
  objectiveNames          <-  c( "criterion" , "weight" )
  checkObjectiveNames     <-  all( colnames( objective ) %in% objectiveNames )
  if( !checkObjectiveNames ) {
    stop( "variables in 'objective' should be: " , "\n" ,  paste0(" ",  objectiveNames , collapse = "\n" ) , "\n" )
  }
  
  #  variance measure
  checkVarCharacter      <-  is.character( varianceMeasure )
  if( !checkVarCharacter ) {
    stop( "'varianceMeasure' should be a character variable (between quotes)"  )
  }
  checkVar1              <-  length( varianceMeasure) == 1
  if( ! checkVar1 )
  stop( "'varianceMeasure' allows only 1 variance measure to be specified"  )

  #  scaleWith
  checkScaleCharacter      <-  is.character( scaleWith )
  if( !checkScaleCharacter ) {
    stop( "'scaleCriterion' should be a character variable (between quotes)"  )
  }
  checkScale1              <-  length( scaleWith ) == 1
  if( ! checkVar1 )
    stop( "'scaleCriterion' allows only 1 scale function to be specified"  )
}


if( 0 == 1 ) {
  # reproduce old result
  
   setOfSchemes             <-  getExampleSetOfSchemes()
   dataForSchemes           <-  getExampleData()
  
   testOrig                 <-  microsamplingDesign::rankObject( object = setOfSchemes, dataForSchemes , getExampleObjective() )
   
  
   # new code
   testNew1                 <-  rankObject( object = setOfSchemes, dataForSchemes , objective =  getExampleObjective()  )
   
   identical( testOrig , testNew1 )
   
   
  
   
}


#' @param objective a \code{data.frame} with columns:
#'   \itemize{
#'     \item{criterion}{ summary function of an estimeted pkCurve (data frame with columns time and concentration):  area under the curve (\code{auc}) ; maximum concentration 
#'        (\code{cMax}) and time when the maximum concentration is reached (\code{tMax}); 
#'         user defined functions are alowed but prefix \code{pkCurveStat_} should be added in function definition, see examples \code{\link{pkCurveStat}} 
#'      }
#'      \item{weight}{ relative importance of the different criteria}
#'   }
#' @param varianceMeasure variance criteria applied to the objective, defaults to summarise objective over sample data, defaults to  \code{var}
#' @param scaleWith function to scale different criteria in \code{objective} before combining results by taking a weighted sum
#' @param skipTests if \code{TRUE} object validity and compatibility is not tested, defaults to \code{FALSE} , doing these tests is slow
#' @param nCores number of cores used in parellel processing, defaults to 1
#' @note when ranking \code{\link{SetOfSchemes-class}} using if multiple criteria, the combined criterion is rescaled such that the best result is 1 
#' @return \code{\link{SetOfSchemes-class}} object 
#' @importFrom plyr aaply arrange
#' @importFrom methods validObject
#' @importFrom  parallel mclapply
#' @rdname rankObject
#' @examples 
#' \dontrun{
#'   setOfSchemes             <-  getExampleSetOfSchemes()
#'   dataForSchemes            <-  getExampleData()
#'   ex1       <-  rankObject( object = setOfSchemes, dataForSchemes ,
#'     objective = data.frame( criterion = "auc" , weight = 1 ) )
#'   getRanking(ex1) # to get the dataframe and not the whole object
#'   ex2       <-  rankObject( object = setOfSchemes, dataForSchemes ,
#'     objective = data.frame( criterion = "auc" , weight = 1 )  ,
#'     varianceMeasure = "sd" , scaleWith = "min" ) 
#'   getRanking(ex2) 
#'   ex3       <-  rankObject( object = setOfSchemes, dataForSchemes ,
#'     objective = data.frame( criterion = c( "auc" , "cMax" , "tMax" )  ,
#'      weight = c( 9 , 1, 1 ) ) ) 
#'   getRanking(ex3)
#' 
#'   # example with own defined varianceMeasure
#'   rangeWidth     <-  function( x ){
#'      range <-  range(x) ;
#'      rangeWith  <-  range[2] - range[1]; rangeWith
#'    }
#'   ex4       <-  rankObject( object = setOfSchemes, dataForSchemes , 
#'       objective = data.frame( criterion = c( "auc" , "cMax" , "tMax" )  , 
#'       weight = c( 9 , 1, 1 ) ) ,
#'       varianceMeasure = "rangeWidth" ,
#'       scaleWith = "mean" ) 
#' }
#' @export 
setMethod(f = "rankObject",
    signature = "SetOfSchemes",
    definition =  function( object , pkData , objective , varianceMeasure = "var" , scaleWith = "max" , skipTests = FALSE , nCores = 1 ) {
      rankObject.SetOfSchemes( object = object , pkData  = pkData, objective = objective  , varianceMeasure = varianceMeasure , scaleWith = scaleWith , skipTests = skipTests , nCores = nCores  )
    }
  ) 

#rm( rankObject.SetOfSchemes  )

### change: add multiple ranking functions ( in a data.frame )
#object                    <-  getExampleSetOfSchemes()
#pkData                    <-  getExampleData()
#criterion                 <-  c( "auc" , "cMax" )
#criterionWeights          <-  c( 9 , 1 ) # TODO: normalize inside 
#objective                 <-  data.frame( criterion = criterion , weight = criterionWeights )
#varianceMeasure           <-  "var"

#' example objective function for \code{\link{rankObject}} 
#' @export 
getExampleObjective             <-  function() {
	objective = data.frame( criterion = c( "auc" , "cMax" , "tMax" ) , weight = c(9,1,1) ) 
	return( objective )
}

