# Project: microsamplingDesign
# 
# Author: ablommaert
###############################################################################



shinyServer( function( input , output , session  ) {
    # Create reactive values object: all will have name results$<etc>
    results <- reactiveValues()
    
    # Basic debugging
    output$debug_text <- renderPrint({

        nSubjectsPerScheme  =  getNSubjects( setOfSchemesObject() ) 

      })
    
    # Advanced debugging
    observe({
        
        if (is.null(input$debug_console))
          return(NULL)
        
        if (input$debug_console > 0) {
          
          options(browserNLdisabled = TRUE)
          saved_console <- ".RDuetConsole"
          if (file.exists(saved_console)) {load(saved_console)}
          isolate(browser())
          save(file = saved_console, list = ls(environment()))
          
        }
        
      })
    
    output$debug_bookmark <- renderPrint({
        
        cat("In R console:\n",
          "> enableBookmarking(store = 'server')\n",
          "> runApp()\n")
        
      })
    
  #  numer of cores 
#   coresInAppliation    <-  parallel::detectCores()
   checkwindowsSystem   <-  Sys.info()[["sysname"]] == "Windows"
   if(checkwindowsSystem   ){
     coresInAppliation    <-  1
   } else {
     coresInAppliation    <- parallel::detectCores()
   }
   cat( "shiny application started, running on " ,  coresInAppliation ,  "cores" )
  
  #  Load code for all tabpages
    source( file.path( "serverFiles" , "serverPkModel.R" )        , local = TRUE )
    source( file.path( "serverFiles" , "serverGetTimePoints.R" )  , local = TRUE )
    source( file.path( "serverFiles" , "serverRankTimePoints.R" ) , local = TRUE )
    source( file.path( "serverFiles" , "serverGetSchemes.R" )     , local = TRUE )
    source( file.path( "serverFiles" , "serverRankSchemes.R" )    , local = TRUE )
    source( file.path( "serverFiles" , "serverSummaryReport.R" )  , local = TRUE )
  
  })