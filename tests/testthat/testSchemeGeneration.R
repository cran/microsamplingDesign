# Check scheme generation function
# 
# Author: Adriaan Blommaert
###############################################################################


context( "test SetOfSchemes generation" )

#source( "/home/ablommaert/git/microsamplingDesign/microsamplingDesign/tests/testthat/beforeTesting.R" )
#source( "../testthat/beforeTesting.R" )


## read in existing data 

#seed                          <-  getRdsFile( "seed" )

seedFile                    <-  system.file(  "dataForTesting" , "seed.rds" , package = "microsamplingDesign" )
seed                        <-  readRDS( seedFile )

#setOfSchemesOrig            <-  getRdsFile( "setOfSchemes" )

setOfSchemesFile            <-  system.file(  "dataForTesting" , "setOfSchemes.rds" , package = "microsamplingDesign" )  
setOfSchemesOrig            <-  readRDS( setOfSchemesFile )

###  generate new data
  
##  full schemes
schemeConstraints             <-  getConstraintsExample()
setOfSchemesNew               <-  getSetOfSchemes( 2 , 3 , 4 ,6 , 1:7 , 
    constraints = schemeConstraints , returnNSchemesBeforeConstraints = FALSE )

## number of schemes does not change when changing constraints

nSchemes1    <-  getSetOfSchemes( 2 , 3 , 4 ,6 , 1:7 , 
    constraints = schemeConstraints , returnNSchemesBeforeConstraints = TRUE )
nSchemes2    <- getSetOfSchemes( 2 , 3 , 4 ,6 , 1:7 , 
    constraints = NULL , returnNSchemesBeforeConstraints = TRUE )


  ##  impact of each constraint 
    # no constraints  
schemesNoConstraints      <-  getSetOfSchemes( 2 , 3 , 4 ,6 , 1:7 , 
        constraints = NULL , returnNSchemesBeforeConstraints = FALSE )

    # 1 constraints  
constraint1              <-  schemeConstraints[ 1 ,]
schemes1Constraint       <-  getSetOfSchemes( 2 , 3 , 4 ,6 , 1:7 , 
    constraints = constraint1 , returnNSchemesBeforeConstraints = FALSE )

#    # 2 constraints # this constraint is always satisfied
#constraint2              <-  schemeConstraints[ 1:2 ,]
#schemes2Constraint      <-  getSetOfSchemes( 2 , 3 , 4 ,6 , 1:7 , 
#    constraints = constraint2 , returnNSchemesBeforeConstraints = FALSE )
    
    # 3 constraints
constraint3              <-  schemeConstraints[ 1:3 ,]
schemes3Constraint      <-  getSetOfSchemes( 2 , 3 , 4 ,6 , 1:7 , 
    constraints = constraint3 , returnNSchemesBeforeConstraints = FALSE )



### execute tests 

test_that( "Unchanged scheme generation" , {
      expect_equal( setOfSchemesNew@.Data , setOfSchemesOrig@.Data )
    }
 )
 
test_that( "N schemes before checks not influenced by constraints" ,{
      expect_equal( nSchemes1 , nSchemes2 )
    }
)

test_that( "each constraint influence number of test results" ,{
      expect_true( getNSchemes( schemesNoConstraints) > getNSchemes( schemes1Constraint)  )
#      expect_true( getNSchemes( schemes1Constraint) > getNSchemes( schemes2Constraint)  )
#      expect_true( getNSchemes( schemes2Constraint) > getNSchemes( schemes3Constraint)  )
      expect_true( getNSchemes( schemes1Constraint) > getNSchemes( schemes3Constraint)  )
      expect_true( getNSchemes( schemes3Constraint) > getNSchemes( setOfSchemesNew )  )
    }
)


