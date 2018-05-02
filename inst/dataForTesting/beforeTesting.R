# Project: microsamplingDesign
# 
# Author: ablommaert
###############################################################################

###  directories 
#testDataDirectory       <-  "/home/ablommaert/git/microsamplingDesign/microsamplingDesign/tests/dataForTesting"
testDataDirectory       <-  "../dataForTesting"

###  functionallity 
getRdsFile              <-  function( fileName , directory = testDataDirectory ){
  filePath              <-  file.path(  directory , fileName )
  fullFilePath          <-  paste0( filePath , ".rds" )
  file                  <-  readRDS( fullFilePath )
  return( file )
}


#' file is characterstring of the name of the object to be saved 
safeAsRds               <-  function( file , directory = testDataDirectory ) {
  filePath              <-  file.path( directory ,  file )
  rdsName               <-  paste0( filePath , ".rds")
  saveRDS( get( file ) , rdsName )
}