# Project: microsamplingDesign
# 
# Author: Adriaan Blommaert
#
# Specify pramenter() value + coefficient of variation ranges)
#
###############################################################################


#' @include objectSetOfSchemes.R
#' @include objectPkModelParent.R
NULL

#' S4 class PkModel representing a pharmacokinetic model and its parameters and uncertainty of parameter choices by ranges 
#' 
#' @template pkmodelParentSlots
#' @name PkModelRange-class
#' @aliases PkModelRange pkModelRange pkmodelrange
#' @export
setClass( "PkModelRange" , contains = c( "PkModelParent" ) )



### validity

validatePkModelRange       <-  function( object ) {
  
  errors                   <-  character( ) 
  ##parameters
  parameterSlot            <-  object@parameters
  nParameters              <-  nrow( parameterSlot )
  parameterSlotNamesMin    <-  c( "parameter" , "minValue" , "maxValue" , "minCoeffVariation" , "maxCoeffVariation" )
  paramSlotNames           <-  colnames( parameterSlot )
  checkParamNames          <-  all( parameterSlotNamesMin %in% paramSlotNames )
  if( ! checkParamNames ) {
    msg                    <-  paste0( "column names of slot parameters should be: ( " ,
      paste0( parameterSlotNamesMin , collapse = ", " ) ,")" , "\n" )
    errors                 <- c( errors , msg )
  }
  ## CV of residual error
  CVResidual               <-  object@coeffVariationError
  checkOneValue            <-  length( CVResidual ) == 2
  checkNumeric             <-  is.numeric( CVResidual )
  
  if( ! ( checkOneValue && checkNumeric ) ) {
    msg                    <-  paste0( "coeffVariationError should be 2 numeric values (min and max)" , "\n")
    errors                 <-  c( errors , msg )
  }
  ##  print errors if object not defined according to the rules 
  if ( length( errors)  == 0 ) {
    return( TRUE ) 
  } else {
    cat( errors )
    return( FALSE )
  }
 
}

setValidity( "PkModelRange" , validatePkModelRange )


### example 

#' get minimal example of \code{\link{PkModelRange-class}}
#' 
#' @examples 
#' getExamplePkModelRange()
#' @export
getExamplePkModelRange                 <-  function(){
  dataParametersFile      <-  system.file( "extData", "examplePkParameterRanges.csv" ,   package = "microsamplingDesign" )
  exampleParameters       <-  read.csv( dataParametersFile , stringsAsFactors = FALSE , na.strings = NULL )
  nParam                  <-  nrow( exampleParameters )
  parameterNames          <-  exampleParameters$parameter
  correlationMatrix       <-  diag( rep( 1 , nParam )  )
  colnames( correlationMatrix )  <-  parameterNames
  rownames( correlationMatrix )  <-  parameterNames
  
  PkModelRange            <-  new( "PkModelRange" ,
    modelFunction           = get2ComptModelCurve ,
    parameters              = exampleParameters ,
    correlationMatrix       =  correlationMatrix ,
    coeffVariationError     =  c( 0 , 0.1) , # allow also here for min-max approach 
    dosingInfo  = data.frame( time = c( 0 , 2 ) , dose = c( 20 , 30 )   )
  )
  return( PkModelRange )
}



### generatePkModels

if( 0 == 1 ) {
  object                  <-  getExamplePkModelRange()
  outputDirectory         <-  "/home/ablommaert/Desktop/testPkDesign"
  directorySave           <-  getPkModels( object  )
  
  outputDirectory         <-  directorySave
  
  ## TODO test output messagessages
  
  # TODO what if already full 
}

#' Generate all possible \code{\link{PkModel-class}} from \code{\link{PkModelRange-class}} combination of ranges 
#' 
#' @param object \code{\link{PkModelRange-class}} 
#' @param outputDirectory directory to save models as \code{.Rds}  objects, defaults to \code{NULL}
#'   when a temporary directory is made to save models 
#' @note the \code{outputDirectory} is should be empty
#' @return  \code{\link{PkModelRange-class}} objects saved as a subsdirectory of the \code{outputdirectory}
#' @importFrom gtools combinations
#' @export 
getPkModels               <-  function( object , outputDirectory = NULL ){
  if( is.null( outputDirectory ) ){
    outputDir             <-  tempdir( )
  } else {
    outputDir             <-  outputDirectory
  }
  
  ##  Input checking
    # object
  checkvalidPkRange       <-  ( class( object ) == "PkModelRange" ) && validObject( object ) 
  if( !checkvalidPkRange  ) { 
    stop(" Object should be 'PkModelRange'") 
  }
    # directory
  checkDirectory          <-  dir.exists( outputDir )
  if( ! checkDirectory ) {
    stop( " 'outputDir' does not exist" ) 
  }
  checkEmptyDir           <-  length( dir( outputDir  ) ) == 0
  if( ! checkEmptyDir ) {
    stop("'outputDir' is not empty ")
  } 
  
    ## extract parameters 
  paramRanges             <-  getParameters( object )
  parameterNames          <-  paramRanges[ ,  "parameter" ]
  
  ## clean up function (from list of values to attributes )
  cleanUpConversion       <-  function( x ) {
    unique( unname( unlist(x) ) )     
  }

  ## all parameter value combinations
  paramValues               <-  paramRanges[ ,  c( "minValue", "maxValue" )  ]
  rownames( paramValues )   <-  parameterNames
  paramValueList            <-  split( paramValues ,  rownames( paramValues ) )
  paramValueListClean       <-  lapply( paramValueList , cleanUpConversion )
  # take all combinations
  allValueCombinations      <-  expand.grid( paramValueListClean , KEEP.OUT.ATTRS = FALSE ,stringsAsFactors = FALSE   ) 
  nValueOptions             <-  nrow( allValueCombinations )
  
  ## all coefficient of variation combinations
  paramCV                   <-  paramRanges[ ,  c( "minCoeffVariation" , "maxCoeffVariation" )  ]
  rownames( paramCV )       <-  parameterNames
  paramCVList               <-  split( paramCV , rownames( paramCV )  )
  paramCVListClean          <-  lapply( paramCVList , cleanUpConversion )
  allCVCombinations         <-  expand.grid( paramCVListClean , KEEP.OUT.ATTRS = FALSE ,stringsAsFactors = FALSE  ) 
  nCVOptions                <-  nrow( allCVCombinations )
  
  ## all combinations of: values, coefficient of variations and additive error 
  additiveErrorOptions      <-  unique(  getCoeffVariationError( object ) )
  nAddError                 <-  length( additiveErrorOptions  )
  
  ## combine 'values' and 'combinations' and additive error 
  allCombinations           <-  expand.grid(
    value     =  seq_len(nValueOptions) ,
    CV        =  seq_len( nCVOptions ) ,
    addError  =  seq_len( nAddError )  ,   KEEP.OUT.ATTRS = FALSE ,stringsAsFactors = FALSE 
  )
  nPkModelScenarios         <- nrow( allCombinations ) 
  
   ## empty parameter model
  emptyParameters           <-  data.frame( parameter = parameterNames ,value = NA , coeffVariation = NA , stringsAsFactors = TRUE  )  
  baseModel                 <-  new( "PkModel" ,
      modelFunction       =  getModelFunction( object ) ,
      parameters          =  emptyParameters ,
      correlationMatrix   =  getCorrelationMatrix( object )  ,
      coeffVariationError =  NA*1 , 
      dosingInfo          =  getDosingInfo( object )   
    )
  
  ## function to construct PkModels 
  # iRow = 1 
  constructSavePkModel       <-  function( iRow , infoOptions = allCombinations , directory = outputDir  ) {
    pkModel                  <-  baseModel
    scenarioId               <-  paste0( "Sc" ,  iRow )
    rowInfo                  <-  infoOptions[ iRow  , ]
    iValue                   <-  rowInfo$value              
    iCV                      <-  rowInfo$CV
    iAddError                <-  rowInfo$addError
    
       
    modelParameters          <-  data.frame(
      parameter        =  unname( parameterNames ) , 
      value            =  as.numeric( allValueCombinations[ iValue , parameterNames ] ) , 
      coeffVariation   =  as.numeric( allCVCombinations[    iCV    , parameterNames   ] ),
      stringsAsFactors = TRUE
    )
    setParameters( pkModel )  <-  modelParameters 
    setCoeffVariationError( pkModel )  <-  additiveErrorOptions[ iAddError ]
    
    ## save each in its own directory 
    scenarioDirectory         <-  paste0( outputDir , "/" ,  scenarioId )
    dir.create( scenarioDirectory )
    saveRDS( pkModel , file.path( scenarioDirectory , "PkModel.rds"  ) )
   }
   
   ## save all models 
   
   lapply( seq_len( nPkModelScenarios  ) , constructSavePkModel )
   cat( "PkModels created in directory: " , outputDir , "\n")
   return( outputDir )
}



if( 0 == 1 ) {
   ## rank SetOfSchemes 
   setOfSchemesExample    <-  getExampleSetOfSchemes()
   pkModelRange           <-  getExamplePkModelRange()
   nSim                   <-  13 
   testDirectory1         <-  file.path( tempdir() , "test1" )
   dir.create( testDirectory1 )
   rankObjectWithRange( object = setOfSchemesExample , pkModelRange ,  nSim = 13  ,  summaryFunctionOverScenarios = "max" ,  directory  = testDirectory1 , varianceMeasure = "sd" , objective = getExampleObjective()  )

  
   ## rank set of timePoints 
   timePoints          <-  getExampleSetOfTimePoints( 0:10 )
   testDirectory2      <-  file.path( tempdir() , "test2" )
   dir.create( testDirectory2 )
   rankObjectWithRange( object = timePoints , pkModelRange ,  nSim = 13  ,  summaryFunctionOverScenarios = "max" ,  directory  = testDirectory2 , nGrid = 20 , nSamplesAvCurve = 25 , nSubjectsPerScheme = 3 )
   
   # remark : use larger number of simulations in realistic context
  
  ## clean up directories 
  unlink( testDirectory1 , recursive = TRUE )
   unlink( testDirectory2 , recursive = TRUE )
   
   
   ### debug example
   
   object = setOfSchemesExample ; pkModelRange ;  nSim = 13  ;  summaryFunctionOverScenarios = "max" ;  directory  = testDirectory1 ; extraSettings = list(varianceMeasure = "sd" , objective = getExampleObjective() ) 
    object = setOfSchemesExample ; pkModelRange ;  nSim = 13  ;  summaryFunctionOverScenarios = "max" ;  directory  = testDirectory1 ; varianceMeasure = "var" ; objective = getExampleObjective()  
 }

#' Rank a \code{\link{SetOfSchemes-class}} or a \code{\link{SetOfTimePoints}} object using data generated per scneario defined by \code{\link{PkModelRange-class}}
#' 
#' @param object to be ranked
#' @param pkModelRange see \code{\link{PkModelRange-class}} 
#' @param nSim number of samples dataset to generate per scenario (= combination of uncertain parameters)
#' @param summaryFunctionOverScenarios function to summarize performance over different scenarios, defaults to \code{max} which 
#'   corresponds to the min-max criterion
#' @param directory directory to save models as \code{.Rds}  objects, defaults to \code{NULL}
#'   when a temporary directory is made to save models and additional info  on simulation settings, ranks, ...
#' @param seed random seed reset when ranking on each directory ( for reproducibility ) , defaults to \code{123}
#' @param nCores number of cores used internally for ranking
#' @param ... additional parameters to pass to \code{\link{rankObject}}
#' @note parallel computing at level of individual ranking and data generation
#' @note see \code{\link{rankObject}} for additional arguments, whem ranking a \code{link{SetOfTimePoints-class}} , \code{nSubjectsPerScheme} should be included 
#' @importFrom readr parse_number
#' @examples 
#' 
#'   \dontrun{ # takes to much time for CRAN
#'    ## rank SetOfSchemes 
#'   setOfSchemesExample    <-  getExampleSetOfSchemes()
#'   pkModelRange           <-  getExamplePkModelRange()
#'   nSim                   <-  13 
#'   testDirectory1         <-  file.path( tempdir() , "test1" )
#'   dir.create( testDirectory1 )
#'   rankObjectWithRange( object = setOfSchemesExample , pkModelRange ,  nSim = 13  , 
#'     summaryFunctionOverScenarios = "max" ,
#'     directory  = testDirectory1 , varianceMeasure = "sd" , objective = getExampleObjective() 
#'     , nCores = 1 )
#'
#'  
#'   ## rank set of timePoints 
#'   timePoints          <-  getExampleSetOfTimePoints( 0:10 )
#'   testDirectory2      <-  file.path( tempdir() , "test2" )
#'   dir.create( testDirectory2 )
#'   rankObjectWithRange( object = timePoints , pkModelRange ,  nSim = 13  ,
#'     summaryFunctionOverScenarios = "max" ,  directory  = testDirectory2 , nGrid = 20 , 
#'     nSamplesAvCurve = 25 , nSubjectsPerScheme = 3 
#'     , nCores = 1 )
#'   
#'   # remark : use larger number of simulation in realistic context
#'  
#'  ## clean up directories 
#'   unlink( testDirectory1 , recursive = TRUE )
#'   unlink( testDirectory2 , recursive = TRUE )
#'  }
#' @note The same random seed is used when using parallel computations
#' @importFrom utils write.csv read.csv
#' @export
rankObjectWithRange          <-  function( object , pkModelRange , nSim , summaryFunctionOverScenarios = "max" , directory = NULL , nCores = 1 , seed = 123 ,  ... ) {
  extraSettings              <-  list( ... )
  classObject                <-  class( object )
  allowedClasses             <-  c("SetOfSchemes" , "SetOfTimePoints")
  checkClassAllowed          <-  classObject %in% allowedClasses
  if( !checkClassAllowed ) {
    stop( paste0( "Class of 'object' should be in  allowedClasses" )  )
  }
  
  ## use and change default ranking settings
  defaultRankingSettings     <-  getDevRankingSettings( classObject )
  rankSettings               <-  changeDevSettings( defaultRankingSettings , extraSettings )
  
  namesSubObjects            <-  getNames( object )
  nSSubObjects               <-  length( namesSubObjects ) 
  
  ## split model over directory
  modelDirectory              <-  getPkModels( object = pkModelRange , outputDirectory = directory )
  subDirectories              <-  dir( modelDirectory )
  subDirectoriesSort          <-  subDirectories[ order( parse_number( subDirectories )  ) ] 
  
  nScenarios                  <-  length( subDirectoriesSort )
  
    #  ranks for every scenario in a subdirectory ( in parellel as cluster) , remark same seed used for reproducibility 
#  mclapply( subDirectoriesSort ,
#      function( dir ){
#        fullDir                    <-  file.path( modelDirectory , dir )
#        rankBasedOnDirectory( objectToRank = object , subDir = fullDir , nSimulations = nSim  , rankingSettings = rankSettings  ) 
#      }
#    , mc.silent = FALSE , mc.set.seed = FALSE   
#  )  
  # only parallel computing inside, no cluster within cluster
  lapply( subDirectoriesSort ,
      function( dir ){
        fullDir                    <-  file.path( modelDirectory , dir )
        set.seed( seed ) # reset seed to have the same seeding (not really necessary )
        rankBasedOnDirectory( objectToRank = object , subDir = fullDir , nSimulations = nSim  , rankingSettings = rankSettings , nCores = nCores  ) 
      }   
  )  
  
    
  ## read in all ranking scenarios 
  overviewRanking                 <-  vapply( seq_along( subDirectoriesSort ) , function( iScenario , names = namesSubObjects ){
      # read 
      # iScenario = 1
      subdir                      <-  subDirectoriesSort[ iScenario ]
      fileName                    <-  paste0( "ranking_" , classObject , ".csv" )
      fileToRead                  <-  file.path( modelDirectory , subdir , fileName )
      rankingFile                 <-  read.csv( fileToRead , stringsAsFactors = FALSE , header = TRUE  )
      rankingOrder                <-  match( names ,  rankingFile[ , "name" ] ) 
      rankingFile[ rankingOrder  , "criterion" ]
    } , FUN.VALUE = rep( 1 , nSSubObjects )
  )
  colnames( overviewRanking )     <- subDirectoriesSort
  rownames( overviewRanking )     <- namesSubObjects
  
  ## apply summary criterion to have final rank
  
  summaryRanking                  <-  getSummaryRanks( rankingOverview = overviewRanking , names = namesSubObjects , summaryFunction = summaryFunctionOverScenarios )
  
  ## write and return output
  write.csv( summaryRanking , file.path( directory , paste0("summaryRanking_" , classObject , ".csv" )  ) )
  return( summaryRanking )
}


#' function default ranking settings
#' 
#'  @param class you want to get the default ranking settings for
#' @keywords internal
getDevRankingSettings             <-  function( objectClass ){
  if( objectClass == "SetOfSchemes" ) {
    defaultSettings  <-  list( varianceMeasure = "var", scaleWith = "max" )
  }
  if( objectClass == "SetOfTimePoints" ) {
    defaultSettings  <-  list( nGrid = 100,
      nSamplesAvCurve = 1000, useAverageRat = FALSE, avCurve = NULL  )
  }
  return( defaultSettings )
}


if( 0 == 1 ) {
  defaultSettings          <-  getDevRankingSettings( "SetOfSchemes" )
  replaceSettings          <-  c( varianceMeasure = "sd" , testExtra = "blablabla"  , scaleWith = "mean" )
  replaceSettings         <-  c(  varianceMeasure = "var" , scaleWith = "mean"  )
 
  changeDevSettings( defaultSettings ,  replaceSettings  )
  
  # tODO test no replacement 
  
}

#' Function to overwrite default settings, print to console when changing default settings and add to default settings 
#' 
#' @param defaultsSettings list of settings
#' @param replaceSettings list of settings to overwrite defaults or add to defaults
#' @return list
#' @keywords internal 
changeDevSettings                <-  function( defaultSettings , replaceSettings ){
  newSettings                    <-  defaultSettings # initialize 
  flagElementsToReplace          <-  names( defaultSettings ) %in% names( replaceSettings )
  elementsToReplace              <-  names( defaultSettings )[ flagElementsToReplace ]
  newSettings[ elementsToReplace ] <-  replaceSettings[ elementsToReplace ]
  # note changes 
  if( length( elementsToReplace ) > 0  ) {
    cat("deviation from default settings:" , paste(  elementsToReplace , " = " , newSettings[elementsToReplace]  , sep = "" , collapse = ";\n")   , "\n" )
  }
  indSettingsNotInDefault        <-  names( replaceSettings )[ !( names( replaceSettings ) %in% names( defaultSettings ) ) ]
  newSettingsFull                <-  c( newSettings , replaceSettings[ indSettingsNotInDefault ]  )
  return( newSettingsFull )
}

#' internal function apply ranking per directory
#' 
#' @note use version of rankObject without defaults
#' @keywords internal  
rankBasedOnDirectory          <-  function( objectToRank , subDir , nSimulations , rankingSettings , nCores = 1   ){
  objectClass                 <-  class( objectToRank )
  scPkModel                   <-  readRDS( file.path( subDir , "PkModel.rds" ) )
  rankingFunction             <-  paste0( "rankObject." , objectClass )
  
   # for SetofTime points object should be in the settings 
  if( objectClass == "SetOfTimePoints" ){
    nSubjectsPerScheme        <-  rankingSettings$nSubjectsPerScheme
    rankingSettings[ "nSubjectsPerScheme" ] <-  NULL # remove from list 
  } else {
    nSubjectsPerScheme        <-  getNSubjects( objectToRank )
  }
  
  ## Generate data 
  dataSettings                <-  list(
      pkModel            =  scPkModel ,
      nSamples           =  nSimulations ,
      timePoints         =  getTimePoints( objectToRank ) ,
      nSubjectsPerScheme =  nSubjectsPerScheme ,
      nCores = nCores
    )
  
  pkDataSet                   <-  do.call( getPkData , args =  dataSettings
    )
  
  # prepare arguments before ranking 
  argumentsForRanking         <-  c( rankingSettings , list(nCores = nCores ) ) # pass ncores to ranking settings 
  argumentsForRanking$pkData  <-  pkDataSet
  argumentsForRanking$object  <-  objectToRank
  
  
  ranks                       <-  getRanking( do.call( rankObject , args = argumentsForRanking ) )
 
  ## save results and settings used  
  # save ranks
  baseName                    <-  paste0( "ranking_" , objectClass , ".csv" )
  rankingFile                 <-   file.path( subDir , baseName )            
  cat(  "File: " , rankingFile , "generated" , "\n"  )
  write.csv( ranks , rankingFile , row.names = FALSE  )
  # save ranking setttings 
  baseNameSettings            <-  paste0( "rankingSettings_" , objectClass , ".rds"  )
  rankingSettingsTosave       <-  c( nSamples = nSimulations  , rankingSettings )
  rankingSettingsTosave
  saveRDS( rankingSettingsTosave , file.path( subDir, baseNameSettings  ) ) 
}




# get min max rank from a set of list of rankings 
if( 0 == 1 ){
  rankingOverview                  <-  data.frame( 
    name        =  c( "scheme1" , "scheme2" , "scheme3" ) ,
    objective1  =  c(  2.1      , 4.2       , 1.3 )  ,
    objective2  =  c(  4.2      , 2.4       , 3.3 )  , stringsAsFactors = FALSE
    )
    summaryFunction = "max"
    
    
    # debugging
  rankingOverview = overviewRanking ; names = namesSubObjects ; summaryFunction = summaryFunctionOverScenarios  
}

#' internal function to rank by a summary function over criterio
#' 
#' @param rankingOverview matrix with columns scenarios and columns objects names to rank 
#' @param names of the objects to return
#' @param summaryFunction function to apply over scenarios
#' @return data.frame with name and criterion ordered
#' @keywords internal
getSummaryRanks                    <-  function( rankingOverview , names ,  summaryFunction  ) {
  summaryOverObjectives            <-  apply( rankingOverview , 1 , summaryFunction )
  orderSmallToLarge                <-  order( summaryOverObjectives , method = "radix" )
  summaryRanks                     <-  data.frame( name = names , criterion = summaryOverObjectives , stringsAsFactors = TRUE )[ orderSmallToLarge , ]
  summaryRanks$rank                <-  seq_along( names) 
  summaryRanks
}



