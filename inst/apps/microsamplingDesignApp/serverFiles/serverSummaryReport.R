# Project: microsamplingDesign
# 
# Author: ablommaert
###############################################################################




### code Maxim: automatic trigger download addapted to take template and .docx


reportFile <- reactiveVal()

observeEvent(input$generateReport, {
    
    reportFile(NULL)  # reset on each button press
    
    withProgress(message = 'Generating report...\n', value = 0, {
        
        ## AB using my template 
        tempReport            <- system.file("extData", "reportTemplate.Rmd" ,   package = "microsamplingDesign")
#        file.copy( "report.Rmd" , tempReport , overwrite = TRUE ) # avoid writing in wrong directory
        
        inputFile             <- tempfile(fileext = ".Rmd")
        file.append( inputFile , tempReport )
        outFile               <- tempfile(fileext = ".docx")
        
        # make a large report
#        writeLines("```{r}\nplot(rnorm(100))\n```", inputFile)
        
        reportFile(
          rmarkdown::render( input = inputFile , output_file = outFile )
        )
        
        # report is ready, trigger download
        setProgress(1)
        
        session$sendCustomMessage(type = "reportReady", 
          message = list(id = "downloadReport"))
        
      })
    
  })

output$downloadReport <- downloadHandler(
  filename = function() "report.docx", 
  content = function(file) {
    file.copy(reportFile(), file, overwrite = TRUE)
  }
)


#output$report            <- downloadHandler(
#  # For PDF output, change this to "report.pdf"
#  filename   = "report.docx",
#  content    = function(file) {
#    # Copy the report file to a temporary directory before processing it, in
#    # case we don't have write permissions to the current working dir (which
#    # can happen when deployed).
#    tempReport            <- system.file("extData", "reportTemplate.Rmd" ,   package = "microsamplingDesign")
#    file.copy( "report.Rmd" , tempReport , overwrite = TRUE )
#    
#    # Set up parameters to pass to Rmd document
##    params <- list(n = input$slider)
#    
#    # TODO include real parameters
#    
##    paramList                      <-  list()
##    model                          <-  getExamplePkModel() 
##    paramList$pkParameters         <-  getParameters( model )
##    paramList$kineticsPlot         <-  plotMMKinetics( model , doseRange = c( 0 , 10 ) , concentrationRange = c(0,5) )
##    settings                       <-  list( NsimTimePoints = 100 ,  nSimSchemes = 1000 , nSubjectsForTimePoints = 3 , weightAUC = 50 , weightCmax = 50  )
##    paramList$settings             <-  settings 
##    paramList$optimalTimePoints    <-  "{ 1, 2 , 3.5 , 8}" 
##    paramList$optimalScheme        <-  getExampleSetOfSchemes()[ ,, 1]
#    
#    # Knit the document, passing in the `params` list, and eval it in a
#    # child of the global environment (this isolates the code in the document
#    # from the code in this app).
#    rmarkdown::render(tempReport, output_file = file )
    
#  }
#)
