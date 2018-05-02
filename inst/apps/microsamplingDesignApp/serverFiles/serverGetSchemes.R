# Project: microsamplingDesign
# 
# Author: ablommaert
###############################################################################

## control constants 
MAXNUMBEROFSCHEMESBEFORECHECKS  <-  10^7 

#TODO : get selected time points out of 
selectedTimePoints              <-  reactive({ 
    as.vector(
      selectedTimePointOption()
    )
  })


output$selectedTimePoints    <-  renderText({ formatTimePoints(selectedTimePoints() ) })

## scheme constraints table input

baseSchemeConstraints        <-  data.frame( check = factor( "Choose a constraint" , levels = c( "Choose a constraint" ,  "minObsPerTimePoint" , "maxConsecSamples" , "exactNumberObsPerTimePoint" ) ) , 
                                             value = rep( 1 , 5 ) 
              ) 

schemeConstraints            <-  reactiveValues( data = baseSchemeConstraints  )


observe({
    input$resetSchemeConstraints
    schemeConstraints$data   <-  baseSchemeConstraints
  })
 
observe({
    if( !is.null( input$tableSchemeConstraints ) )
      schemeConstraints$data      <-  hot_to_r( input$tableSchemeConstraints )
  })


output$tableSchemeConstraints      <-  renderRHandsontable({
    rhandsontable( schemeConstraints$data )
  })


##  format schemes input for generating schemes

minSchemeConstraints               <-  data.frame( check =  "minObsPerTimePoint"  , value = 1 )
checkLevel                         <-  data.frame(
  check = c( "minObsPerTimePoint" , "maxConsecSamples" , "exactNumberObsPerTimePoint" ) ,
  level = c( "scheme"             , "subject"          , "level" )
)
  

schemeConstraintsFormat            <-  reactive({
    schemeConstraints              <-  schemeConstraints$data 
    schemeConstraintsPlus          <-  rbind( schemeConstraints , minSchemeConstraints )
    schemeConstraintsUnique        <-  unique( schemeConstraintsPlus )
    schemeConstraintsUnique$check  <-  as.character( schemeConstraintsUnique$check )
    flagRowDelete                  <-  schemeConstraintsUnique[ , "check" ] == "Choose a constraint"
    constraintsEnd                 <-  schemeConstraintsUnique[ !flagRowDelete , ]
    constraintsFormat              <-  merge( constraintsEnd ,  checkLevel , by = "check"   , all.x = TRUE ) 
    constraintsFormat2             <-  constraintsFormat[ , c( 1 , 3 , 2 ) ]
    constraintsFormat2
  })  

## schemes constraints as check  (for debugging)
output$tableSchemeConstrFormat     <-  renderTable( schemeConstraintsFormat()  )

## one function to nSchemes ena number of schemes

getSetOfSchemesInternal            <-  reactive({
    intFunction                    <-  function( boolNrNotSchemes ) {
#      getSetOfSchemes( minNSubjects                    = input$minNSubjects ,
#                       maxNSubjects                    = input$maxNSubjects ,
#                       minObsPerSubject                = input$minObsPerSubject ,
#                       maxObsPerSubject                = input$maxObsPerSubject ,
#                       timePoints                      = selectedTimePoints() ,
#                       constraints                     = schemeConstraintsFormat() ,
#                       maxRepetitionIndSchemes         = input$maxRepetitionIndSchemes  ,
#                       maxNumberOfSchemesBeforeChecks  = MAXNUMBEROFSCHEMESBEFORECHECKS ,
#                       returnNschemes                  = boolNrNotSchemes )
      getSetOfSchemes(
        minNSubjects                      = input$nSubjects ,
        maxNSubjects                      = input$nSubjects ,
        minObsPerSubject                  = input$minObsPerSubject ,
        maxObsPerSubject                  = input$maxObsPerSubject ,
        timePoints                        = selectedTimePoints() ,
        constraints                       = schemeConstraintsFormat() ,
        maxRepetitionIndSchemes           = input$maxRepetitionIndSchemes  ,
        maxNumberOfSchemesBeforeChecks    = MAXNUMBEROFSCHEMESBEFORECHECKS ,
        returnNSchemesBeforeConstraints   = boolNrNotSchemes  )
    } 
  })


## Calculate number of schemes (test)

output$nrOfSchemesNoConstraints      <-  renderText({
    cat( "update nrOfSchemesNoConstraints" ,  input$calcSchemes , "\n" )
    validate(
      need( input$checkNSchemes > 0 , "Click on 'Check number of schemes before constraints'" )
    )
    isolate({
        getSchemeFunction            <-  getSetOfSchemesInternal()
        nrSchemes                    <-  getSchemeFunction( boolNrNotSchemes = TRUE )
        format( nrSchemes , big.mark = ","  )
      })
  })


## Calculate actual schemes

setOfSchemes                         <-  reactive({
    
    validate( 
      need( input$calcSchemes > 0 , "Click on 'Generate schemes' " )
    )
    
    input$checkNSchemes
    
    isolate({
        getSchemeFunction                <-  getSetOfSchemesInternal()
        getSchemeFunction( boolNrNotSchemes = FALSE )        
      })
  })

## Display schemes (flatten)

output$nrOfSchemes                  <-  renderText({
    nSchemes                        <-  getNSchemes( setOfSchemes() )
    format( nSchemes , big.mark = "," )
  })

output$setOfSchemesTable            <-  DT::renderDataTable({
    schemesTable                    <-  flattenSetOfSchemes( object = setOfSchemes() )
  })






