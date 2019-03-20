# Project: microsamplingDesign
# 
# Author: ablommaert
###############################################################################

context( "test Rank SetOfSchemes " )

#source( "/home/ablommaert/git/microsamplingDesign/microsamplingDesign/tests/testthat/beforeTesting.R" )
#source( "../testthat/beforeTesting.R" )


## read in data 

seedFile                       <-  system.file(  "dataForTesting" , "seed.rds" , package = "microsamplingDesign" )
seed                           <-  readRDS( seedFile )

rankedSetOfschemesFile         <-  system.file( "dataForTesting" , "rankedSetOfSchemes.rds" , package = "microsamplingDesign" )

#rankedSetOfSchemesOrig         <-  getRdsFile( "rankedSetOfSchemes" )
rankedSetOfSchemesOrig         <-  readRDS( rankedSetOfschemesFile )



### generate new data 

suppressWarnings(RNGversion("3.5.0"))
set.seed( seed , kind = "Mersenne-Twister", normal.kind = "Inversion") # change to
exSetOfSchemes             <-  getExampleSetOfSchemes()
pkDataFSOS                 <-  getPkData( getExamplePkModel() , getTimePoints(exSetOfSchemes) ,  
  nSubjectsPerScheme = getNSubjects(exSetOfSchemes) , 
  nSamples = 17  )
rankedSetOfSchemesNew         <-  rankObject( exSetOfSchemes , pkDataFSOS ,
  objective = getExampleObjective() ,
  varianceMeasure = "var" ,
  scaleWith = "max" )

rankedSetOfSchemesNewSkipTests         <-  rankObject( exSetOfSchemes , pkDataFSOS ,
    objective = getExampleObjective() ,
    varianceMeasure = "var" ,
    scaleWith = "max" , skipTests = TRUE )



rankedSetOfSchemesNewDiffVar  <-  rankObject( exSetOfSchemes , pkDataFSOS ,
  objective = getExampleObjective() ,
  varianceMeasure = "sd" ,
  scaleWith = "max" )

rankedSetOfSchemesNewDiffSum  <-  rankObject( exSetOfSchemes , pkDataFSOS ,
  objective = getExampleObjective() ,
  varianceMeasure = "var" ,
  scaleWith = "mean" )


rankedSetOfSchemes1component         <-  rankObject( exSetOfSchemes , pkDataFSOS ,
    objective = data.frame( criterion = "auc" , weight = 1 ) ,
    varianceMeasure = "var" ,
    scaleWith = "max" )

rankedSetOfSchemes1weight         <-  rankObject( exSetOfSchemes , pkDataFSOS ,
    objective = data.frame( criterion = c("auc", "cMax") , weight = c(1 , 0 ) ) ,
    varianceMeasure = "var" ,
    scaleWith = "max" )


  ## fast rank scheme function
  

  object     <-  getSetOfSchemes( 3 , 3,3,3, 1:6 )
  pkData     <-  getPkData( getExamplePkModel() , 1:6 , 3 , 15 )
  objective  <-  getExampleObjective()[-3 , ]
 

  origExample           <-  rankObject( object , pkData , objective , skipTests = FALSE  )
  origExampleSkipTests  <-  rankObject( object , pkData , objective , skipTests = TRUE  )
  fastExample           <-  fastRankSchemes( object , pkData, objective )  
  
  test_that( "skipping test no influence on ranking" , {

      
        expect_equal( origExample , origExampleSkipTests )
      } 
  )

  
  test_that( "fastRank schemes , default fixed options identcal results " , {
        expect_true( all.equal( getRanking( origExample  ) , getRanking(fastExample)) )
      } 
  )
  

### execute tests 

test_that( "Equal ranking SetOfSchemes (consistent with original ranking)" , {
    expect_equal(  rankedSetOfSchemesOrig@ranking  , rankedSetOfSchemesNew@ranking )
  }
)
test_that( "skip test no influence on results" , {
    expect_equal(rankedSetOfSchemesNew , rankedSetOfSchemesNewSkipTests  )
  }
)

test_that( "number of CPU cores no impact on results" , {
      expect_equal( getRanking(rankedSetOfSchemes1component)$var_auc_scaled , getRanking(rankedSetOfSchemes1weight)$criterion  )
    }
)

test_that( "one criterion of multiple criteria with one getting all weight equals one criterion" , {
    testthat::skip_on_os("windows") # forking not possible on windows
    rankedSetOfSchemesNewSkipMulticore         <-  rankObject( exSetOfSchemes , pkDataFSOS ,
      objective = getExampleObjective() ,
      varianceMeasure = "var" ,
      scaleWith = "max" , nCores = 2 )
    
      expect_equal( rankedSetOfSchemesNew , rankedSetOfSchemesNewSkipMulticore  )
    }
)

test_that( "Change in ranking with other variance measure" , {
    expect_false( identical( rankedSetOfSchemesNew , rankedSetOfSchemesNewDiffVar )  )
  } )
test_that( "Change in ranking with other scale measure" , {
    expect_false( identical( rankedSetOfSchemesNew , rankedSetOfSchemesNewDiffSum )  )
  } )


