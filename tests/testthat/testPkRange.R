# Project: microsamplingDesign
# 
# Author: ablommaert
###############################################################################



testthat::context("PkModelRange functions")


#source( "/home/ablommaert/git/microsamplingDesign/microsamplingDesign/tests/testthat/beforeTesting.R" )
#source( "../testthat/beforeTesting.R" )


## preliminary

correctObject              <-  getExamplePkModelRange()
wrongObject                <-  getExamplePkModel()
nonExistendDirectory       <-  "blabalablab/balbalabla"
nonEmptyDirectory          <-   paste0( getwd() , "/" , "PkrangesDir")





test_that("Error messages getPkModels" , {
    testthat::skip_on_cran()
    dir.create( nonEmptyDirectory )
    file.create( file.path(nonEmptyDirectory , "testfile")  )
    expect_error( getPkModels( object = wrongObject, outputDirectory) ) 
    expect_error( getPkModels( object = correctObject , outputDirectory = nonExistendDirectory ) )
    expect_error( getPkModels( object = correctObject , outputDirectory = nonEmptyDirectory ) )
    
  }
)
# clean up 
unlink( nonEmptyDirectory  , recursive = TRUE )




### RankObjectwithRange on SetOfSchemes 


## Read in datra 

rankSchemeFileOrig              <-  system.file(  "dataForTesting" , "rankSetOfSchemeswithRange.rds" , package = "microsamplingDesign" )
seedFile                        <-  system.file(  "dataForTesting" , "seed.rds" , package = "microsamplingDesign" )

#rankSetOfSchemeswithRangeOrig   <-  getRdsFile( "rankSetOfSchemeswithRange" )
  rankSetOfSchemeswithRangeOrig  <-  readRDS( rankSchemeFileOrig )

#seed                            <-  getRdsFile( "seed" )
seed                             <-  readRDS( seedFile )






set.seed( seed )
setOfSchemesExample    <-  getExampleSetOfSchemes()
pkModelRange           <-  getExamplePkModelRange()




test_that( "rankObjectwithRange functioning on SetofSchemes" , {
    testthat::skip_on_cran()
    
    ## calculations
    testDirectory1         <-  file.path( tempdir() , "test1" )
    dir.create( testDirectory1 )
    rankSetOfSchemeswithRangeNew     <-  rankObjectWithRange( object = setOfSchemesExample , pkModelRange ,  nSim = 13  ,  summaryFunctionOverScenarios = "max" ,  directory  = testDirectory1 , varianceMeasure = "var" , objective = getExampleObjective()  )
#safeAsRds( "rankSetOfSchemeswithRange" )
    unlink( testDirectory1 , recursive = TRUE )
    
# different baseline setting
    testDirectory1         <-  file.path( tempdir() , "test1" )
    dir.create( testDirectory1 )
    set.seed( seed )   
    rankSetOfSchemeswithRangeDiff     <-  rankObjectWithRange( object = setOfSchemesExample , pkModelRange ,  nSim = 13  ,  summaryFunctionOverScenarios = "max" ,  directory  = testDirectory1 , varianceMeasure = "sd" , objective = getExampleObjective()  )
#safeAsRds( "rankSetOfSchemeswithRange" )
    unlink( testDirectory1 , recursive = TRUE )
    
    
    testDirectory1         <-  file.path( tempdir() , "test1" )
    dir.create( testDirectory1 )
    set.seed( seed )   
    rankSetOfSchemeswithRangeDiffSum   <-  rankObjectWithRange( object = setOfSchemesExample , pkModelRange ,  nSim = 13  ,  summaryFunctionOverScenarios = "mean" ,  directory  = testDirectory1 , varianceMeasure = "sd" , objective = getExampleObjective()  )
    unlink( testDirectory1 , recursive = TRUE )
    
    
    
    
    
#    expect_equal( rankSetOfSchemeswithRangeOrig , rankSetOfSchemeswithRangeNew )
    expect_false( identical( rankSetOfSchemeswithRangeNew , rankSetOfSchemeswithRangeDiff ) )
    expect_false( identical( rankSetOfSchemeswithRangeNew , rankSetOfSchemeswithRangeDiffSum ) )
  } 
)






### RankObjectwithRange on SetOfTimePoints 


  ## read in data 
  
  rankTPRangeFile                   <-  system.file( "dataForTesting" , "rankSetOfTimePointsWithRange.rds" , package = "microsamplingDesign" )
  rankSetOfTimePointsWithRangeOrig  <-  readRDS( rankTPRangeFile )

#rankSetOfTimePointsWithRangeOrig        <-  getRdsFile( "rankSetOfTimePointsWithRange" )
#seed                            <-  getRdsFile( "seed" )
set.seed( seed )
timePoints          <-  getExampleSetOfTimePoints( 0:10 )



test_that( "rankObjectwithRange functioning on SetOfTimePoints" , {
    testthat::skip_on_cran() # takes to long
    
    ## calculations
    
    testDirectory2      <-  file.path( tempdir() , "test2" )
    dir.create( testDirectory2 )
    set.seed( seed )   
    rankSetOfTimePointsWithRangeNew  <- rankObjectWithRange( object = timePoints , pkModelRange ,  nSim = 13  ,  summaryFunctionOverScenarios = "max" ,  directory  = testDirectory2 , nGrid = 20 , nSamplesAvCurve = 25 , nSubjectsPerScheme = 3 )
    unlink( testDirectory2 , recursive = TRUE )
    
    set.seed( seed )
    testDirectory2      <-  file.path( tempdir() , "test2" )
    dir.create( testDirectory2 )
    rankSetOfTimePointsWithRangeDiff <- rankObjectWithRange( object = timePoints , pkModelRange ,  nSim = 13  ,  summaryFunctionOverScenarios = "min" ,  directory  = testDirectory2 , nGrid = 20 , nSamplesAvCurve = 25 , nSubjectsPerScheme = 3 )
    unlink( testDirectory2 , recursive = TRUE )
    
#    expect_equal( rankSetOfTimePointsWithRangeOrig , rankSetOfTimePointsWithRangeNew )
    expect_false( identical( rankSetOfTimePointsWithRangeNew , rankSetOfTimePointsWithRangeDiff ) )

  }
)


    


