# Project: microsamplingDesign
# 
# Author: ablommaert
###############################################################################

context( "test Rank timePoints " )

#source( "/home/ablommaert/git/microsamplingDesign/microsamplingDesign/tests/testthat/beforeTesting.R" )
#source( "../testthat/beforeTesting.R" )


 ## Load in existing data 
 
#seed                       <-  getRdsFile( "seed" )

seedFile                    <-  system.file(  "dataForTesting" , "seed.rds" , package = "microsamplingDesign" )
seed                        <-  readRDS( seedFile )

#rankTimePointsOrig         <-  getRdsFile( "rankedTimePoints" )'

rankTimePointsFile          <-  system.file(  "dataForTesting" , "rankedTimePoints.rds" , package = "microsamplingDesign" )
rankTimePointsOrig          <-  readRDS( rankTimePointsFile )

### generate new data 


set.seed( seed , kind = "Mersenne-Twister", normal.kind = "Inversion") # change to
fullTimePoints             <-  0:10
setOfTimePoints            <-  getExampleSetOfTimePoints( fullTimePoints)
pkDataExample              <-  getPkData( getExamplePkModel() , getTimePoints( setOfTimePoints ) ,  nSubjectsPerScheme = 5 , nSamples = 17   )
set.seed( seed , kind = "Mersenne-Twister", normal.kind = "Inversion") # change to
rankedTimePointsNew        <-  rankObject( object = setOfTimePoints , pkData = pkDataExample , nGrid = 75 , nSamplesAvCurve = 13)
set.seed( seed , kind = "Mersenne-Twister", normal.kind = "Inversion") # change to
rankedTimePointsNew2        <-  rankObject( object = setOfTimePoints , pkData = pkDataExample , nGrid = 75 , nSamplesAvCurve = 13)
set.seed( seed , kind = "Mersenne-Twister", normal.kind = "Inversion") # change to
rankedTimePointsNewDiffGrid    <-  rankObject( object = setOfTimePoints , pkData = pkDataExample , nGrid = 10 , nSamplesAvCurve = 13)
set.seed( seed , kind = "Mersenne-Twister", normal.kind = "Inversion") # change to
rankedTimePointsNewDiffCurves    <-  rankObject( object = setOfTimePoints , pkData = pkDataExample , nGrid = 75 , nSamplesAvCurve = 20)

### execute tests 

test_that( "Equal ranking timePoints" , {
  expect_equal( rankTimePointsOrig@ranking , rankedTimePointsNew@ranking  )
  }
)
test_that( "Different ranking timePoints with different number of grid poings" ,  {
    expect_false( identical( rankedTimePointsNew , rankedTimePointsNewDiffGrid )   )
  }
)
test_that( "Different ranking timePoints with different number of sample curves" ,  {
    expect_false( identical( rankedTimePointsNew , rankedTimePointsNewDiffCurves)   )
  }
)

