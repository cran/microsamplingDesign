# Executed before ui.R and server.R
# use this script load dependencies and fixed data structures
# 
# Author: Daan Seynaeve <daan.seynaeve@openanalytics.eu>
###############################################################################

# package and app IDs

corePackage             <-  "microsamplingDesign" # package containing the application
appID                   <-  "apps/microsamplingDesignApp"          # folder in package containing the application 

library( corePackage , character.only = TRUE  ) 

# specific shiny packages 

#library(rbokeh) # For renderBokeh() # not used here
#library(plotly)  # For renderPlotly()
library(shinyjs) # for useShinyjs()
library(shinyBS)

library(data.table)     # For data.table: better alternative for data.frame
library(rmarkdown)      # For automatic reports
library(shinyjs)        # For toggle()
library(rhandsontable)  # For rhandsontable
library(shinycssloaders) # get spinners while calculating

`%then%`              <- shiny:::`%OR%`

## global settings

NDIGITSROUND                   <-  6 #rounding digits in tables 

## package specific spinner option: to use 

addSpinner                     <-  function( ui_output ) {
  withSpinner( ui_output , type = 6   )
}

#
## (1) Copy the UI files & folders from "inst/ui" for local use
#tmpDir                <-  tempdir()
#setwd( tmpDir )
#
##uiDir                 <-  system.file( "ui" , package = corePackage )
#uiDir                 <-  system.file( appID , package = corePackage )
##uiDirOrig                 <-  system.file( "ui" , package = "bestOfShiny" )
#
#uiFiles               <-  list.files( path = uiDir, full.names = FALSE, recursive = TRUE )
#uiFiles               <-  uiFiles[ !( uiFiles %in% c("global.R" ) ) ]
#
#sapply( uiFiles, function( from ) {
#      
#      to              <-  file.path( tmpDir, from )
#      toDir           <-  dirname( to )
#      
#      if( !dir.exists( toDir ) ) {
#        
#        dir.create( path = toDir , recursive = TRUE )
#        
#      }
#      
#      file.copy( from = file.path( uiDir , from ), to = to, overwrite = TRUE ) 
#      
#    })
# 
#


  