# Project: microsamplingDesign
# 
# Author: ablommaert
###############################################################################

# settings for the app display 
boolPrintConsole                          <-  FALSE  # check inside of the app 

###  Settings for plots app 
tickSizeMM                                <-  0.01 # for 

timeInterval                              <-  0.1
timeZero                                  <-  0 
nCurves                                   <-  7
rangeDoseDefault                          <-  seq( 0,  5 , 0.01 )
rangeConcentrationDefault                 <-  seq( 0 , 1.5 , 0.01 )

# y limits for comparable plots 
limDoseAbsorption                        <-  c( 0 , 5 ) 
limDoseClearance                         <-  c( 0 , 0.05 )
limConcentrPlasma                        <-  c( 0 , 35 )

defaultAddError                          <-  0
defaultDose                              <-  2.5   # To avoid rouding 



## Model parameters (dynamic table)

dataParametersFile                       <-  system.file("extData", "examplePkParameters.csv" ,   package = "microsamplingDesign")
exampleParameters                        <-  read.csv( dataParametersFile , stringsAsFactors = FALSE , na.strings = NULL )
baseParameterTable                       <-  exampleParameters
# TODO plug in good defaults 

#baseParameterTable$value                 <-  exampleParameters$value * 0
#baseParameterTable$coeffVariation        <-  exampleParameters$coeffVariation * 0

modelParameters                          <-  reactiveValues( data = baseParameterTable  )

observe({
    if( !is.null( input$modelParameterTable  ))
      modelParameters$data          <-  hot_to_r( input$modelParameterTable )
  })


output$modelParameterTable         <-  renderRHandsontable({
    rhandsontable( modelParameters$data )
  })


## Dosing info ( dynamic table)

baseDosingInfo                          <-  data.frame( time = rep( 0.00 , 7 ) , dose = rep( 0.00 , 7 ) )
baseDosingInfo[ 1 , "dose"]             <-  defaultDose
dosingInfo                              <-  reactiveValues( data =  baseDosingInfo )

observe({
    if( !is.null( input$dosingInfoTable  ))
      dosingInfo$data                   <-  hot_to_r( input$dosingInfoTable )
  })

output$dosingInfoTable                  <-  renderRHandsontable({
    rhandsontable( dosingInfo$data , digits = 2  )
  })

dosingInfoFormat                        <-  reactive({
    inputTable                          <-  dosingInfo$data
    flagZeroRow                         <-  rowSums( abs( inputTable ) ) == 0 
    tableFormat                         <-  inputTable[ !flagZeroRow , ]
    tableFormat
  })



### process graphical settings
##  MM kinentics 
rangeDose                               <-  reactive({
    seq( 0 , input$maxDose , tickSizeMM )
  })
rangeConcentration                      <-  reactive({
    seq( 0 , input$maxConcentration , tickSizeMM )
  })
ylimAbsorption                          <-  reactive({
    c( 0 , input$yMaxAbsorption )
  })
ylimClearance                           <-  reactive({
    c( 0 , input$yMaxClearance )
  })
## sample concentrations curves in time 
#yLimitsSamples                          <-  reactive({
#    c( 0 , input$maxPlasmaConcentration  )
#  })
timesPlot                              <-  reactive({
    seq( 0  , input$maxTime  , by = input$timeIncrement )
  })   

### Calculations for dose 

pkModelInAppNoDose                         <-  reactive({
#    validate( need( input$parameterFile , "load in parameters" ) )
    parameterFile            					    <-  input$parameterFile
    
    ## read in files
    parameterInput            				    <-  modelParameters$data
    dosingInfoInput               		    <-  data.frame( time = 0 , dose = 0 )
    
    ##  replace defaults by input 
    pkModel                               <-  construct2CompModel( parameters = parameterInput , dosingInfo = dosingInfoInput  ) 
    if( boolPrintConsole ) {
      cat( "PkModel:" , "\n" )
      cat( str( pkModel ) , "\n"  )
    }
    return( pkModel )
    
  })



# include dosingInfo and additive eror  in app 
pkModelInApp                               <-  reactive({
#    validate(  need( input$dosingFile ,  "load in dosing information" ) ) 
    dosingFile               					   <-  input$dosingFile
#    dosingInfoInput               		   <-  read.csv( dosingFile$datapath , stringsAsFactors = FALSE  )
    modifiedModel                        <-  pkModelInAppNoDose()
    setDosingInfo( modifiedModel )       <-  dosingInfoFormat()
    setCoeffVariationError(modifiedModel )              <-  defaultAddError
    return( modifiedModel )
  })


## output parameters in the model 

 modelParametersTable                           <-  reactive({
    getParameters( object = pkModelInAppNoDose() )[ c( "parameter" ,  "value" ,  "coeffVariation") ] 
  })

output$parameters                         <-  DT::renderDataTable({
    return( modelParametersTable() )
  })


## table of dosing info  

#output$dosingInfo                         <-  DT::renderDataTable({
#    dosingInfo                         <-  getDosingInfo( object = pkModelInApp() )
#    return( dosingInfo )
#  })


### ggplot for MM-kinetics

MMKineticsPlot                           <-  reactive({
    MMPlot                              <-  plotMMKinetics( pkModel = pkModelInAppNoDose() ,
      doseRange = rangeDose() ,
      concentrationRange = rangeConcentration() ,  absorptionYRange = ylimAbsorption()  ,  clearanceYRange = ylimClearance() ) 
})

output$MMPlot                            <-  renderPlot({
    MMKineticsPlot()
      })


### curve average individual   

#averageRatPlot                            <-  reactive({
#      plotAverageRat( pkModelInAppNoDose() , doseZero = input$doseZero , timePoints = timesPlot() )
#      
#    })
#
#output$averageRatPlot                     <- renderPlot({
#      averageRatPlot()
#  })


### sample plots ( avoid recalculation for setting axis)
samplePlot                                <-  reactive({ 
    
    validate(
      need( input$calcSampleCurves > 0 , "Click on 'Generate example curves'") 
    )
    
    isolate({
        plotObject( object = pkModelInApp() , times = timesPlot()   , nSamplesIntegration = input$nCurves , nCurves = input$nCurves ) 
      })
})
output$samplePlot                         <-  renderPlot({
    samplePlot() #+ ylim( yLimitsSamples() )
  })

