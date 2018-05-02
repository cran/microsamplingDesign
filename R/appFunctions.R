##############################################################################
### Project: microsamplingDesign                                           ###
###                                                                        ###
### Description: functions related to shiny app functioning                ###
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
### changes:                                                               ###
###                                                                        ###
##############################################################################


##' Run the PkPlotApp in a new tab in your browser
##' 
##' @note on windows computer, nCores set to 1 because forking not possible on windows machine
##' @return no return value
##' @importFrom shiny runApp
##' @importFrom utils installed.packages install.packages
##' @keywords internal
#runPkPlotApp <- function() {
#  
#  # List all packages used in the shiny app but not in R functions
#  requiredPackages <- c("ggplot2", "shiny" ) 
#  newPackages <- requiredPackages[!(requiredPackages %in% installed.packages()[,"Package"])]
#  
#  if(length(newPackages) > 0) {
#    
#    install.packages(newPackages)
#    
#  } 
#  lapply( requiredPackages , "require" , character.only = TRUE  )
#  tmpDir                  <-  tempdir()  
#  # Copy server.R and ui.R (not folder www)
#  uiDir                   <-  system.file("apps/PkPlotApp", package = "microsamplingDesign")
#  uiFiles                 <-  list.files(path = uiDir, full.names = TRUE)
#  uiFiles                 <-  uiFiles[!grepl("www", uiFiles)]
#  
#  sapply(uiFiles, function(x){
#        file.copy(from = x, to = file.path(tmpDir, basename(x)),
#            overwrite = TRUE)
#      }
#  )
#  
#  # Make www directory and copy its files
#  if (!dir.exists(file.path(tmpDir, "www"))) {
#    
#    dir.create(path = file.path(tmpDir, "www"))
#    
#  }
#  
#  wwwFiles <- list.files(path = file.path(uiDir, "www"), full.names = TRUE)
#  
#  sapply(wwwFiles, function(x){
#        file.copy(from = x, to = file.path(tmpDir, "www", basename(x)),
#            overwrite = TRUE)
#      }
#  ) 
#  runApp(appDir = tmpDir) 
#}



#' Run the bestShiny application
#' 
#' @param installDependencies boolean, whether to first install packages listed
#' in the Suggests field of DESCRIPTION; default value is FALSE
#' @param ... further arguments that can be passed to \code{\link[shiny]{runApp}}
#' @note The shiny application contains a 2 compartmental oral dosing pharmacokinetic model which does not include measurement error
#' @return no return value
#' @import shiny
#' @importFrom devtools install_github dev_package_deps
#' @importFrom utils update.packages
#' @export
runMicrosamplingDesignApp         <- function( installDependencies = FALSE, ... ) {
  
  # (1) Install all suggested R packages (see DESCRIPTION)
  if (installDependencies) {
    
    ## (a) CRAN packages
    update.packages(dev_package_deps(pkg = system.file("apps/microsamplingDesignApp", package = "microsamplingDesign"), 
        dependencies = "Suggests"))
    
    
    ## (b) non-CRAN packages - by hand
    if (!requireNamespace( "rhandsontable" ) ) {
      
      install_github( "jrowen/rhandsontable" )
      
    }
    
  }
  
  
  # (2) Run the application
  ldots <- list(...)
  
  if( !is.null( ldots$appDir) )
    runApp(...) else
    runApp( appDir = system.file("apps/microsamplingDesignApp", package = "microsamplingDesign") , ... )
  
}

if( 0 == 1  ) {
  timePoints <-  c( 1 , 2 , 3 )
}
### internal formatting
#' Format time points as a set 
#' 
#' @param timePoints numeric vector of timme points
#' @export
formatTimePoints      <-  function( timePoints  ) {
  timesSpread         <-  paste0( timePoints , collapse = " ;    " )
  paste0("{ " , timesSpread , " }") 
}



