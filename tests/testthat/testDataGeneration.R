# Project: microsamplingDesign
# 
# Author: ablommaert
# changes, 1.0.3: setting seed kind, and performing tests on windows
###############################################################################


testthat::context("Data generation")

#source( "/home/ablommaert/git/microsamplingDesign/microsamplingDesign/tests/testthat/beforeTesting.R" )
#source( "../testthat/beforeTesting.R" )

## read in date 

seedFile                <-  system.file(  "dataForTesting", "seed.rds" ,  package = "microsamplingDesign" )
seed                    <-  readRDS( seedFile )
pkDataOrigFile          <-  system.file( "dataForTesting" , "pkData.rds" , package = "microsamplingDesign" ) 


pkDataOrig              <-  readRDS( pkDataOrigFile )


### generate New data ( same inputs, tests for same outputs )

set.seed( seed , kind = "Mersenne-Twister", normal.kind = "Inversion") # change to
pkModel                 <-  getExamplePkModel()
times                   <-  c( 0 , 0.1 , 0.5 , 2 , 15 )
nTimes                  <-  length( times )
corrMatEx               <-  diag( 0.8 , nTimes , nTimes ) + matrix( 0.2 , nTimes , nTimes  )
pkDataNew               <-  getPkData( getExamplePkModel() , times , nSubjectsPerScheme = 3 , nSamples = 5 , 
  errorCorrelationMatrixIntime = corrMatEx ) 

### execute tests 

test_that("getIndividualParam is unbiased" , {
    set.seed( seed , kind = "Mersenne-Twister", normal.kind = "Inversion") # change to
    expect_true( mean( getIndividualParameters( 1 , 0.2 , 100000 ) ) <  1.001 )
  }
)

test_that("getMMCurve equal to getMMrateFast" , {
    expect_equal( getMMCurve( 3, 2, 1 )$rate , getMMRateFast( 3 , 2 , 1 )  )
    expect_equal( getMMCurve( 3, 2, 1 , 5 )$rate , getMMRateFast( 3 , 2 , 1 , 5 )  )
  }
)

test_that( "Equal model" , {
    expect_equal( getPkModel( pkDataOrig ) , getPkModel( pkDataNew ) )
  }
)

dataOrig       <-  getData( pkDataOrig )
dataNew        <-  getData( pkDataNew )

test_that( "Identical dimensions" , {

    expect_identical( dim( dataOrig ), dim( dataNew ) )
  }
)

test_that( "Equal data" , {
#    testthat::skip_on_os("windows") # random data generation not equal on window as on linux
    expect_equal( dataOrig , dataNew )
  }
)


