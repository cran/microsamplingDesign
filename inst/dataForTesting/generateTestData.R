# Project: microsamplingDesign
# 
# Author: ablommaert
###############################################################################


boolRegenerateTests     <-  FALSE
cat("Regererate data for testing =  " , boolRegenerateTests , "\n")
#testDataDirectory       <-  "/home/ablommaert/git/microsamplingDesign/microsamplingDesign/tests/dataForTesting"
testDataDirectory       <-  "../dataForTesting"
seed                    <-  1234


#' file is characterstring of the name of the object to be saved 
safeAsRds               <-  function( file , directory = testDataDirectory ) {
  filePath              <-  file.path( directory ,  file )
  rdsName               <-  paste0( filePath , ".rds")
  saveRDS( get( file ) , rdsName )
}


if( boolRegenerateTests  ){
  cat("Data is being generated for testing")
  
  ### PkData generation 
  set.seed( seed ) # to test we get an error message  on data only 
  safeAsRds( "seed" )
  pkModel               <-  getExamplePkModel()
  times                 <-  c( 0 , 0.1 , 0.5 , 2 , 15 )
  nTimes                <-  length( times )
  corrMatEx             <-  diag( 0.8 , nTimes , nTimes ) + matrix( 0.2 , nTimes , nTimes  )
  
  
	pkData                <-  getPkData( getExamplePkModel() , times , nSubjectsPerScheme = 3 , nSamples = 5 , 
    errorCorrelationMatrixIntime = corrMatEx )   
  
  safeAsRds( "pkData" ) 
  
  ### scheme generation
  cat("Schemes are being generated for testing")
  schemeConstraints          <-  getConstraintsExample()
  setOfSchemes               <-  getSetOfSchemes( 2 , 3 , 4 ,6 , 1:7 , 
      constraints = schemeConstraints , returnNSchemesBeforeConstraints = FALSE )
   safeAsRds( "setOfSchemes" ) 
     
  
  
  ### rank Time points 
  set.seed( seed )
  fullTimePoints             <-  0:10
  setOfTimePoints            <-  getExampleSetOfTimePoints( fullTimePoints)
  pkDataExample              <-  getPkData( getExamplePkModel() , getTimePoints( setOfTimePoints ) ,  nSubjectsPerScheme = 5 , nSamples = 17   )
  set.seed( seed )
  rankedTimePoints           <-  rankObject( object = setOfTimePoints , pkData = pkDataExample , nGrid = 75 , nSamplesAvCurve = 13)
  safeAsRds( "rankedTimePoints" ) 
  

  ### test ranking set of schemes 
  
  set.seed( seed )
  exSetOfSchemes             <-  getExampleSetOfSchemes()
  pkDataFSOS                 <-  getPkData( getExamplePkModel() , getTimePoints(exSetOfSchemes) ,  
    nSubjectsPerScheme = getNSubjects(exSetOfSchemes) , 
    nSamples = 17  )
  rankedSetOfSchemes         <-  rankObject( exSetOfSchemes , pkDataFSOS ,
    objective = getExampleObjective() ,
    varianceMeasure = "var" ,
    scaleWith = "max" )
  safeAsRds( "rankedSetOfSchemes" )
  
  ### test raking with PkModelRange
  
    ## rank SetOfSchemes
   set.seed( seed )
   setOfSchemesExample    <-  getExampleSetOfSchemes()
   pkModelRange           <-  getExamplePkModelRange()
   testDirectory1         <-  file.path( tempdir() , "test1" )
   dir.create( testDirectory1 )
   set.seed( seed )   
   rankSetOfSchemeswithRange     <-  rankObjectWithRange( object = setOfSchemesExample , pkModelRange ,  nSim = 13  ,  summaryFunctionOverScenarios = "max" ,  directory  = testDirectory1 , varianceMeasure = "var" , objective = getExampleObjective()  )
   safeAsRds( "rankSetOfSchemeswithRange" )
  
   ## rank set of timePoints
   set.seed( seed )
   timePoints          <-  getExampleSetOfTimePoints( 0:10 )
   testDirectory2      <-  file.path( tempdir() , "test2" )
   dir.create( testDirectory2 )
   set.seed( seed )
   rankSetOfTimePointsWithRange  <- rankObjectWithRange( object = timePoints , pkModelRange ,  nSim = 13  ,  summaryFunctionOverScenarios = "max" ,  directory  = testDirectory2 , nGrid = 20 , nSamplesAvCurve = 25 , nSubjectsPerScheme = 3 )
   safeAsRds( "rankSetOfTimePointsWithRange" )
   
   
   # remark : use larger number of simulation in realistic context
  
  ## clean up directories 
   unlink( testDirectory1 , recursive = TRUE )
   unlink( testDirectory2 , recursive = TRUE )
     
}
