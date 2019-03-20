##############################################################################
### Project: microsamplingDesign                                           ###
###                                                                        ###
### Description: model functions (input is parameters and time points,     ###
###                       output is prediction at each time point)         ###
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
###                                                                        ###
##############################################################################


if( 0 == 1 ) {
  


  
  
# example  
pkModel                  <-  getPkModelArticle()  
parametersFrame          <-  getParameters( pkModel )
parameters               <-  parametersFrame$value
names(parameters)        <-  parametersFrame$parameter
dosingModel              <-  getDosingInfo( pkModel )
times                    <-  seq( 0, 12 , 0.5 ) 
plasmaConcentration      <-  oneCompartmentOralModel( parameters , time = times , dosingInfo = dosingModel )
plot( x = times , y =  plasmaConcentration , type = "line" ,  lwd = 3 , xlab = 'Conc.' , ylab = "Time (hrs)")

# test error messages dosing
  multipleDoses  <-  data.frame( time = c( 0 , 2 ) , dose = c(1,2) )
  testErrorMultipleDoses      <-  oneCompartmentOralModel( parameters , time = times , dosingInfo = multipleDoses )
 

# test data generation in function getPkData
  
  parameters               <-  indParamList[[1]] 
  time                     <-  timePoints
  dosingInfo               <-  getDosingInfo( pkModel )
 
# check not change in data generation ( individual parameter )
  library(microsamplingDesign)
  set.seed(123)
  test_orig                 <-  microsamplingDesign::getPkData( microsamplingDesign::getExamplePkModel() , 1:5 , 3 , 7 )  
  R.utils::sourceDirectory("/home/ablommaert/git/microsamplingDesign/microsamplingDesign/R")
  set.seed(123)
  test_new                  <-  getPkData( getExamplePkModel() , 1:5 , 3 , 7 )  # error 
  identical( str(test_orig) , str(test_new)  )
  identical( test_orig@.Data , test_new@.Data  ) # identicla data is ok 
  identical( test_orig@.Data , test_new@.Data  )
  
  # test whether other model is working
  
  test_new                  <-  getPkData( getPkModelArticle() , 1:5 , 3 , 7 )  # error 
  identical( test_orig , test_new ) # works but no correlation matrix 
  # TODO test on example with correlaiton matrix specifid 
  
}

#' solution of one compartmental oral administration model
#' only use one set of parameters, times can input can be an numeric array
#' @param parameters a numeric verctor of parameters as input to the model with names
#'   \itemize{
#'     \item Ka: constant absorption rate
#'     \item Ke: constant elimination rate
#'     \item dose: initial dose
#'     \item volume: volume to which the dose is administered
#' }
#' @param time a numeric vector containing timePoints at which the concentration should be predicted
#' timepoint zero is defined as the moment the dose is administered
#' @param dosingInfo see \code{link{PkModel-class}} but opnly one dose at time zero allowed
#' @return vector of concentrations corresponding to the input timePoints 
#' @export 
oneCompartmentOralModel        <-  function( parameters , time , dosingInfo  ) {
  
  ## dosing info ( only one dose at time zero is allowed )
 errorMessageDose              <-   "For the 'oneCompartmentOralModel' only one dose at time zero is allowed" 
 boolNDosesOk                  <-  nrow( dosingInfo ) == 1
 boolDosingTimeOk              <-  dosingInfo$time  == 0
 boolDosingInfoOk              <-  (  boolNDosesOk && boolDosingTimeOk )
 
 if( ! boolDosingInfoOk ) {
   stop( errorMessageDose )
 }
 dose                     <-  dosingInfo$dose
   
  ## extract parameters
  Ke                       <-  unlist( parameters[ "Ke" ]     )
  Ka                       <-  unlist( parameters[ "Ka" ]     )
  volume                   <-  unlist( parameters[ "volume" ] )
  
  ## calculations
  C                        <-  ( dose / volume )
  AEt                      <-  exp( - Ke * time ) - exp( - Ka * time )  
  Cnst                     <-  ( Ka / ( Ka - Ke ) )
  concentration            <-  Cnst * C * AEt
  return( concentration )
}


### example Pkmodel object

if( 0 == 1 ) {
  pkHBModel        <-  getPkModelArticle()
  summary( pkHBModel )
  
  # get example data
  getPkData( pkHBModel , c(0 ,1 , 2 , 4  ) ,  nSubjectsPerScheme = 2  , nSamples = 3 )
  
  plotAverageRat( pkHBModel , doseZero = 5 ,  timePoints = seq( 0, 10 , 0.5 ) )
  plotObject( pkHBModel , seq( 0, 10 , 0.5 ) , ncurves = 10 , nSamplesIntegration = 10  )
}

#' reproduce the example of the article of Helen Barnet et al. 
#' 
#' @note this models serves only to reproduce results of the article, 
#' and allows only 1 dose administered at time 0. 
#' @examples
#'   model       <-  getPkModelArticle()
#'   summary( model )
#'   testData    <-    getPkData( model , 1:12 , nSubjectsPerScheme = 3 , nSamples = 7 )
#'   plotObject( model , times = 0:12  )
#'    plotAverageRat( model , doseZero = 100 , timePoints = seq(0,12,0.5) )
#' @export
getPkModelArticle       <- function() {
  
  ## parameters is in article
  parametrizationHB           <-  data.frame(
      parameter                   =  c( "volume"              , "Ka"                       , "Ke" ) ,
      description                 =  c( "volumeplasma"  , "absorption coefficient"   , "excretion coefficient" ) ,
      value                       =  c( 15  , 2 ,  0.25 ) ,
      varianceRandomEffects       =  c( 0.1 , 1 , 0.25  ) 
  ) 
  
  doseArticle                 <-  100
  
  parameterNames              <-  parametrizationHB$parameter
  betaVector                  <-  parametrizationHB$value
  sHat2Vector                 <-  parametrizationHB$varianceRandomEffects
  
  ## parameters is in this package 
  parametrizationPackage      <-  data.frame(
      parameter                  =  parameterNames ,
      value                      =  getExpectationHB( beta = betaVector , sHat2 = sHat2Vector ) ,
      coeffVariation             =  getCoefVariationHB( beta = betaVector , sHat2 = sHat2Vector )
  )
  
  dosingInfoModel             <-  data.frame( time = 0 , dose = doseArticle )
  
  ## correlation between parameters 
  
  nParam                  <-  nrow( parametrizationPackage )
  correlationMatrix       <-  diag( rep( 1 , nParam )  )
  colnames( correlationMatrix )  <-  parameterNames
  rownames( correlationMatrix )  <-  parameterNames
  
  ## construct model
  pkModel                     <- new( "PkModel" ,
      modelFunction       =  oneCompartmentOralModel , 
      parameters          =  parametrizationPackage ,
      dosingInfo          =  dosingInfoModel,
      coeffVariationError = 0.5 ,
      correlationMatrix   =  correlationMatrix )
  return( pkModel )
}  

#some helper functions 

getExpectationHB               <-  function( beta , sHat2 ) {
  beta * exp( sHat2 /2  )
}

getCoefVariationHB             <-  function( beta , sHat2 ) {
  
  top                          <- sqrt( ( exp( sHat2 ) - 1 ) * exp( sHat2)  )
  bottom                       <- exp( sHat2 /2  )
  return( top / bottom ) 
}






#' get example parameters to use in \code{\link{pkOdeModel2Compartments}} example
getExampleParameters                  <- function() {
  list(F = 1 ,   volumePlasma = 10 , Cld = 1.5*10 , volumeTissue = 15 , 
      VmaxAbsorption = NULL , kappaMMAbsorption = NULL , KaConstant = 5 ,
      VmaxClearance = NULL , kappaMMClearance = NULL   , ClConstant = 2*10
  )
}  


#' Set of differential equations representing two compartmental pk model with possible Michaelis-Menten kinetics at absorption and clearance
#' used as interal function
#' 
#' @param t numeric representing time in the system
#' @param y list of state variables ( dose in gut , concentration in plasma , concentration in tissue )
#' @keywords internal
pkOdeModel2Compartments             <-  function( t , y , parameters ) {
  # extract parameters
  with( parameters , {

        
        # MM kinetic
#        MMAbsorption                <-  getMMCurve( x = y[ "gutDose" ] , VmaxAbsorption , kappaMMAbsorption ,  KaConstant   ) 
#        AbsorptionRate              <-  MMAbsorption[ , "rate" ]                             # full rate: (ka * C)
#        MMClearance                 <-  getMMCurve( x = y[ "plasmaConcentration" ] , VmaxClearance , kappaMMClearance , ClConstant     )                             
#        clearanceRate               <-  MMClearance[ , "rate" ]
      
       # faster MM kinetics 
         AbsorptionRate              <-  getMMRateFast( x = y[ "gutDose" ] , VmaxAbsorption , kappaMMAbsorption ,  KaConstant   )                           # full rate: (ka * C)
         clearanceRate               <-  getMMRateFast( x = y[ "plasmaConcentration" ] , VmaxClearance , kappaMMClearance , ClConstant  )
     
        
        #differential equations (correct order!)
        dGut                        <-  - AbsorptionRate 
        dC                          <-  (
              F * AbsorptionRate + 
              -clearanceRate +
              -Cld * y[ "plasmaConcentration" ] +
              Cld * y[ "tissueConcentration" ]  
              )   / volumePlasma 
        dCt                         <-  ( 
              Cld * y[ "plasmaConcentration" ] + 
              -Cld * y[ "tissueConcentration" ] 
              )   / volumeTissue
        outputVector                <-  c( gutDose = dGut , plasmaConcentration = dC , tissueConcentration = dCt )
        names( outputVector )       <-  c( "gutDose" , "plasmaConcentration" ,  "tissueConcentration" )
        list( outputVector )
      }
  )
}



if( 0 == 1 ) {
  # debugging with example individual parameters
  
  testParam      <-  allIndividualParameters[ 1, ] # from function getPkData
  parameters     <-  testParam
  time           <-  seq(0,5,0.1)
  dosingInfo     <-  data.frame( time = c(0,1) , dose = c( 5, 2.5 ) )
  internalODEs   <-  pkOdeModel2Compartments 
  returnAll      <-  TRUE 


}

#' provides solution of two compartmental pharmacodynamic model at specified time points
#' 
#' 
#' @param parameters a list with correclty named input parameters
#' @param time a numeric vector of times
#' @param dosingInfo a data.frame with 2 colunmns
#' \itemize{
#'  \item time at which a dose is administered
#'  \item dose the amount administred to the gut
#' } 
#' @param internalODEs the model function used defaults to \code{pkOdeModel2Compartments}
#' @param returnAll logical indicator if \code{TRUE} the solutions of all response variables is returned as a \code{data.frame}
#' if \code{FALSE} only the plasma concentration is returned as a vector, defaults to \code{FALSE} 
#' @return \code{data.frame} or numeric vector of solutions, depending on the value of \code{returnAll}
#' @importFrom deSolve ode
#' @examples 
#'   pkModel               <-  getExamplePkModel()
#'   parameters            <-  getParameters( pkModel )
#'   testParameters        <-  parameters[ , "value"] 
#'   names(testParameters)  <-  parameters[ , "parameter"]
#'   time                  <-  seq( 0 , 3 , 0.1 )
#'   dosingInfo            <-  data.frame( time = c( 0 , 1 , 2) , 
#'        dose = c( 5 , 2 , 1.5 ) )
#'   get2ComptModelCurve( parameters = testParameters , time , dosingInfo )
#'   get2ComptModelCurve( parameters = testParameters, time ,
#'     dosingInfo , returnAll = TRUE )  
#' @export
get2ComptModelCurve                  <-  function( parameters , time , dosingInfo , internalODEs = pkOdeModel2Compartments ,
    returnAll = FALSE) {
  
 
  ##  format dosing information
  dosingEvents                <-  data.frame( var = "gutDose" , time = dosingInfo[ , "time" ] , 
      value =  dosingInfo[ , "dose" ], method = "add" )
  
  ## initial conditions -- all zero dosing info completely in "dosingInfo"
  yInit                               <-  c( gutDose = 0 , plasmaConcentration = 0 , tissueConcentration = 0   )
  ##  solve equations 
  solution                            <-  ode( yInit , time , func =  internalODEs , 
      parms = as.list( parameters )  , events = list( data = dosingEvents  ) )
  ##   output processing
  if(returnAll == TRUE) {
    return( solution )
  } else {
    return( solution[ , "plasmaConcentration" ] )
  }
}


#' calculate Michealis-Menten relation between x and velocity and rate  
#' 
#' @param x numeric vector,  independent variable in Michaelis-Menten function representing a concentration or dose
#' @param Vmax is the maximum rate ( x * Vmax / (kappaMM + x ) ) with increasing x
#' @param kappaMM scalar representing Michaelis-Menten constant wich is the x at the rate reaches half of Vmax
#' @param constantValue mumeric constant if not \code{NULL} , the rate equals
#'   x*constantValue with  Vmax and kappaMM are ignored,  defaults to \code{NA}
#' @return data.frame given te relation between concentration and velocity and rate with columns
#' \itemize{
#'   \item x
#'   \item velocity wich is rate/concentration
#'   \item rate rate  ( x * Vmax / (kappaMM + x ) 
#'   \item Vmax input value
#'   \item kappaMM input value
#' }
#' @export
#' @examples
#'   getMMCurve( x = seq( 0 , 1 , 0.01 ) , Vmax = 5 , kappaMM = 0.3 )
#'   getMMCurve( x = seq( 0 , 3 , 0.01 ) , Vmax = 5 , kappaMM = 0.3 )
#'   getMMCurve( x = seq( 0 , 1 , 0.01 ) , Vmax = 5 , kappaMM = 0.3 , constantValue = 3 ) 
getMMCurve              <-  function( x , Vmax  , kappaMM , constantValue = NA  ) {
  if( is.na(constantValue)  ) {
    velocity            <-  Vmax / ( kappaMM + x )
  } else {
    velocity            <-  constantValue
    Vmax                <-  NA
    kappaMM             <-  NA
  }
  rate                  <-  velocity * x
  data.frame( x = x , velocity = velocity , rate = rate , Vmax = Vmax , kappaMM = kappaMM )
}
getMMRateFast      <-  function(  x , Vmax  , kappaMM , constantValue = NA  ){
  ifelse( is.na(constantValue) ,  Vmax / ( kappaMM + x ) * x , constantValue * x ) 
}

getMMRateFaster      <-  function(  x , Vmax  , kappaMM , constantValue = NA  ){
   Vmax / ( kappaMM + x ) * x  
} # no NA calculations 


if( 0 == 1 ) {
  pkModel                      <-  getExamplePkModel()
  times                        <-  seq( 0 ,  )
  doseRange                    <-  seq( 0 , 5 , 0.1 )
  concentrationRange           <-  seq( 0 , 2 , 0.1 )
  plotMMKinetics( pkModel = getExamplePkModel() , doseRange = seq( 0 , 5 , 0.1 ) , concentrationRange = seq( 0 , 2.5 , 0.1  ) )
 
  # reproduce strange result linear plot
  parameterInput            				    <-  read.csv( parameterFile$datapath , stringsAsFactors = FALSE   )
  dosingInfoInput               		    <-  read.csv( dosingFile$datapath , stringsAsFactors = FALSE  )
  pkModel                               <-  construct2CompModel( parameters = parameterInput , dosingInfo = dosingInfoInput  ) 
  plotMMKinetics( pkModel , doseRange = 1:10 , concentrationRange = 1:10)
  
 # adaptation: set ylimits (but not by default)
  
}

#' plot MM kinetics of both absorption and clearance 
#' 
#' @param pkModel an object of \code{\link{PkModel-class}}
#' @param doseRange numeric vector representing the range of doses for absorption plot
#' @param concentrationRange numeric vector representing the range of concentrations for the clearance plot
#' @param absorptionYRange numeric vector of size 2 specifying y-limits for the absorption plot, defaults to \code{NULL}
#' @param clearanceYRange numeric vector of size 2 specifying y-limits for the clearance plot, defaults to \code{NULL}
#' @return ggplot2 object
#' @importFrom gridExtra grid.arrange
#' @importFrom ggplot2 ylim
#' @examples
#'    plotMMKinetics( pkModel = getExamplePkModel() , 
#'       doseRange = seq( 0 , 5 , 0.1 ) , 
#'       concentrationRange = seq( 0 , 2.5 , 0.1  ) )
#'    plotMMKinetics( pkModel = getExamplePkModel() , 
#'        doseRange = seq( 0 , 5 , 0.1 ) , 
#'        concentrationRange = seq( 0 , 2.5 , 0.1  ) , 
#'        clearanceYRange = c( 0 , 50 ) , absorptionYRange = c( 0 , 10 )  )
#' @export 
plotMMKinetics                  <-  function( pkModel , doseRange , concentrationRange , absorptionYRange = NULL ,  clearanceYRange = NULL ) {
  ## extract model parameters
  modelParameters               <-  getParameters( pkModel )
  meanParameters                <-  as.vector( modelParameters[ , "value" ] , mode = "double" )
  names( meanParameters )       <-  modelParameters[ , "parameter" ]
  
  ## calculate kinetics 
  absorptionMMData              <-  getMMCurve( x = doseRange ,
    Vmax           =  as.vector( meanParameters[ "VmaxAbsorption" ] )   ,
    kappaMM        =  as.vector( meanParameters[ "kappaMMAbsorption" ]  ),
    constantValue  =  as.vector( meanParameters[ "KaConstant"]    )     
  )
  clearanceMMData               <-  getMMCurve( x = concentrationRange ,
    Vmax           =  as.vector( meanParameters[ "VmaxClearance" ] )    ,
    kappaMM        =  as.vector( meanParameters[ "kappaMMClearance" ] ) ,
    constantValue  =  as.vector( meanParameters[ "ClConstant" ] )
  )
  
  ## plot individual plots
  absorptionPlot                <-  plotMMCurve( absorptionMMData , "absorption" )
  clearancePlot                 <-  plotMMCurve( clearanceMMData  , "clearance"  )
  
  ## include y limits if specified 
  if( ! is.null(absorptionYRange) ) {
    absorptionPlot              <-  absorptionPlot + ylim( absorptionYRange )
  }
  if( ! is.null(clearanceYRange) ) {
    clearancePlot               <-  clearancePlot + ylim( clearanceYRange )
  }
  
  ## combine 2 plots
  twoPlots                      <-  grid.arrange( absorptionPlot , clearancePlot , nrow = 2)
  return( twoPlots )
}





#' plot Michealis-Menten curve for either capacity dependent absorption or clearance
#' 
#' @param dataInput output of function \code{\link{getMMCurve}}
#' @param parameter character value indicating either \code{absorption} or \code{clearance}
#' @return ggplot2-object
#' @import ggplot2
#' @importFrom reshape2 melt
#' @export 
#' @examples 
#'   plotMMCurve( dataInput = getMMCurve( seq(0, 5 , 0.01 ) ,
#'       Vmax = 5 , kappaMM = 0.3 ) , parameter = "absorption" )
#'   plotMMCurve( dataInput = getMMCurve( seq(0, 5 , 0.01 ) ,
#'       Vmax = 5 , kappaMM = 0.3 , constantValue = 4 ) , parameter = "absorption" ) 
#'   plotMMCurve( dataInput = getMMCurve( seq(0, 1 , 0.01 ) ,
#'       Vmax = 2 , kappaMM = 0.3 ) , parameter = "clearance" )
#'   plotMMCurve( dataInput = getMMCurve( seq(0, 1 , 0.01 ) ,
#'        Vmax = 2 , kappaMM = 0.3 , constantValue = 1.5 ) , parameter = "clearance" )  
plotMMCurve             <-  function( dataInput , parameter ) {
  
  ## test inputs
  testNames             <-   colnames( dataInput ) == c( "x" , "velocity" , "rate" , "Vmax" , "kappaMM"   )
  if( ! all( testNames) ) { stop( "incorrect column names for Michealis-Menton plot " ) }
  parameterTypes         <- c( "absorption" , "clearance")
  testParamOk            <-  parameter %in% parameterTypes
  if( !testParamOk ) { stop("incorrect parameter, choose from:", "\n" , paste(parameterTypes , "\n") ) }
  
  ##  data processing 
  dataSelect            <-  dataInput[, c("x" , "rate", "Vmax")] 
  longData              <-  melt( dataSelect , id.vars = "x" )
  kappa                 <-  dataInput[ 1 ,"kappaMM" ]
  halfVmax              <-  dataInput[ 1 ,"Vmax" ] / 2
  partHorizonLine       <-  data.frame( x = c( 0 , kappa ) , variable = "halfVmax" , value = rep( halfVmax , 2  ) )
  partVerticalLine      <-  data.frame( x = rep( kappa , 2 ) , variable = "kappaVertical" , value = c( 0 , halfVmax) )
  
  dataForPlot           <-  rbind( longData , partHorizonLine , partVerticalLine  )
  
  ## parameter specific plotting settings
  if( parameter == "absorption" ) {
    xLabel              <-  "dose (D)"
    rateLabel           <-  expression( "k"[a]*~D == frac("V"[paste("a", "," , "m")] , kappa[paste("a", "," , "m")] + "D" ) * ~ "D"  )
    rateLabelSimpel     <-  expression("k"[a]*~D)
    colorSet            <-  c( "red" , "black" , "gray" , "brown" ) 
    legendLabels        <-  c( rateLabel ,
        expression("V"[paste("a", "," , "m")]) ,
        expression("V"[paste("a", "," , "m")] / 2 ) ,
        expression(  kappa[paste("a", "," , "m")] )
    )
  }
  
  if( parameter == "clearance" ) {
    xLabel              <-  "concentration (C)"
    rateLabel           <-  expression( "Cl"*~C == frac("V"[paste("e", "," , "m")] , kappa[paste("e", "," , "m")] + "C" ) * ~ "C"  )
    rateLabelSimpel     <-  expression("Cl"*~C)
    colorSet            <-  c( "yellow" , "black" , "gray" , "brown" )
    legendLabels        <-  c( rateLabel ,
        expression("V"[paste("e", "," , "m")]) ,
        expression("V"[paste("e", "," , "m")] / 2 ) ,
        expression(  kappa[paste("e", "," , "m")] )
    )
  }
  
  # if non-linearities, simplify plot
  
  if( is.na( kappa ) ) {
    indRateVariable      <-  longData$variable == "rate" 
    dataForPlot          <-  longData[ indRateVariable  , ]
    legendLabels         <-  rateLabelSimpel 
    colorSet             <-  colorSet[ 1 ]
  }
  
  ## ggplot object and formatting ()
  with( dataForPlot , {
        ggplotObject                  <-  ggplot(dataForPlot, aes( x = x , y = value , group = variable ,  col = variable , size = variable ) ) +
            geom_path(  ) +
            labs( x = xLabel ,  y = paste0(parameter , " [dose/time]" ) ) +
            scale_color_manual( labels = legendLabels , values = colorSet ) +
            scale_size_manual( values = c(2 , 1 , 1 , 1) ) +
            theme( legend.position="right" , legend.text = element_text( size = 12 ) , legend.text.align = 0	
                , legend.title = element_blank()   ) +
            guides(colour = guide_legend(override.aes = list(size=3 )) , size = FALSE  ) +
            ggtitle( parameter )
        return( ggplotObject )
        
      }

  )
  
}



if( 0 == 1 ) {
  
  pkModel              <-  getExamplePkModel()
  summary( pkModel )
  doseZero             <-  5 
  timePoints           <-  seq( 0 , 10 , 0.01 )
  # debug on model used in the article
  
 testResult_orig        <-  microsamplingDesign::plotAverageRat( pkModel = getExamplePkModel() , doseZero = 10 , timePoints = 0:5 )
 testNew                <-   plotAverageRat( pkModel = getExamplePkModel() , doseZero = 10 , timePoints = 0:5 )
 identical( testResult_orig$data , testNew$data )
 
 # test works on 1 comp
  test   <-  plotAverageRat( getPkModelArticle() , doseZero = 100 , timePoints = seq( 0 ,  12 , 0.25 ))
  test$data
  
  # this gives an error 
  plotAverageRat( model , doseZero = 100 , timePoints = seq(0,12,0.5) ) 
  pkModel     <-  model
  doseZero    <-  100
  timePoints  <-  seq(0,12,0.5)
}

#' plot plasma concentration for average individual (i.e average parameter values) in function of dose at time zero
#' 
#' @param pkModel \code{\link{PkModel-class}}
#' @param doseZero numeric value, dose given at time zero 
#' @param timePoints a numeric vector of time points to plot the plasma concentration at
#' @return ggplot object 
#' @examples
#'  plotAverageRat( getExamplePkModel() , 2 , seq( 0 , 20, 0.1 ) )
#' @note dose inside de \code{pkModel} is not used 
#' @export
plotAverageRat                                <-   function( pkModel , doseZero , timePoints ) {
  
  # modify model to average rat and one dose 
  pkModelAverageRat                           <-  setModelToAverageRat( pkModel )
  oneDosingInfo                               <-  data.frame( time = 0 , dose = doseZero )
  pkModelModified                             <-  pkModel
  setDosingInfo( pkModelAverageRat )          <-  oneDosingInfo
  
  # plot data (1 individual)
  plotObject( pkModelAverageRat , times = timePoints ,  nCurves = 1 , sampleCurvesOnly = TRUE  ) # modify plotting function  
} 

#' get a model with all variances to zero 
#' 
#' @param pkModel \code{\link{PkModel-class}}
#' @export
setModelToAverageRat                           <-  function( pkModel ) {
  # reset pk object , no variation, 
  parametersForModel                          <-  getParameters( pkModel )
  parametersForModel["coeffVariation"]        <-  0
  pkModelModified                             <-  pkModel
  setParameters( pkModelModified )            <-  parametersForModel
  setCoeffVariationError( pkModelModified )   <-  0 # no noice on top of it
  pkModelModified 
}




if( 0 == 1 ) {
  pkModel                 <-  getExamplePkModel() 
  timePoints              <-  c( 0 , 1 , 3 , 4 )
  nSubjectsPerScheme      <-  3
  nSamples                <-  7
  errorCorrelationMatrixIntime = diag( 1 , length( timePoints ) )  
  dirIntermediateOutput        <- "/home/ablommaert/Desktop/compare/microsamplDebug" 
  
  # test differences in cores on ramdom number generation 
  set.seed( 1235 )
  test1 <-  getPkData( getExamplePkModel() , 0:5 , nSubjectsPerScheme = 3 , nSamples = 4 , nCores = 1 )
  set.seed( 1235 )
  test2 <-  getPkData( getExamplePkModel() , 0:5 , nSubjectsPerScheme = 3 , nSamples = 4 , nCores = 2 )
  set.seed( 1235 )
  test3 <-  getPkData( getExamplePkModel() , 0:5 , nSubjectsPerScheme = 3 , nSamples = 4 , nCores = 3 )
  
 identical( getData( test1 ) , getData( test2 ) ) 
 identical( getData( test1 ) , getData( test3 ) )  # no influence number of cores on random data generation
 
  
}

#' simulate \code{\link{PkData-class}} from \code{\link{PkModel-class}}
#' 
#' @param pkModel an object of \code{\link{PkModel-class}}
#' @param timePoints numeric vector of time points
#' @param nSubjectsPerScheme numeric constant, number of subjects per dataset on which a sampling scheme can be applied
#' @param nSamples number of datasets to sample 
#' @param errorCorrelationMatrixIntime the correlation between additive error terms within a subject, by default no correlation
#' @param nCores number of cores used for parallel computing, defaults to 1 (remark no random numbers are generated in parallel)
#' @param dirIntermediateOutput directory to write intermediate output to for debugging, defaults to NULL, when no intermediate output is written down
#' @return \code{\link{PkData-class}} object  
#' @importFrom MASS mvrnorm
#' @importFrom parallel parRapply makeCluster stopCluster 
#' @examples 
#'   getPkData( getExamplePkModel() , 0:5 , nSubjectsPerScheme = 3 , nSamples = 4  )
#'   getPkData( getExamplePkModel() , 0:5 , nSubjectsPerScheme = 7 , nSamples = 1  ) 
#' @export
getPkData                      <-  function( pkModel , timePoints , nSubjectsPerScheme , nSamples , errorCorrelationMatrixIntime = diag( 1 , length( timePoints ) ) , nCores = 1 , 
  dirIntermediateOutput = NULL ) {
#  
#  boolWriteDown                <<-  ! is.null( dirIntermediateOutput ) 
#  dirIntermediateOutput        <<- dirIntermediateOutput
  
  
     # debug write down 
#  if( boolWriteDown ){
#    saveRDS( object = .Random.seed , file.path( dirIntermediateOutput , "seedState0.rds")  )
#  }
#  
 
  ## extract info for calculation
  nTimePoints                  <-  length( timePoints )
  modelFunction                <-  getModelFunction( pkModel )
  modelFunctionLabel           <-  as.list( args( modelFunction ) )$internalODEs  # TODO not go to internal ODEs
  parameters                   <-  getParameters( pkModel )
  parameterValues              <-  parameters[[ "value" ]] 
  parameterNames               <-  parameters[[ "parameter" ]]
  parameterVector              <-  parameterValues   
  names(  parameterVector )    <-  parameterNames
  coeffVariation               <-  parameters[[ "coeffVariation" ]]
  names( coeffVariation )      <-  parameterNames
  nTotalSubjects               <-  nSubjectsPerScheme *  nSamples              # total number of subjects , overall schemes 
  correlationMatrix            <-  getCorrelationMatrix( pkModel ) 
  
  if( is.null(modelFunctionLabel ) ){
    modelFunctionLabel              <-  "no set of differential equations defined"
  }
  
  ##  check parameters  (specific to modelFunction)
  if( modelFunctionLabel  ==   "pkOdeModel2Compartments" ) { 
    errors                      <-  character()
    #  check bioavailability parameter 
    FValue                      <-  parameterVector[ "F" ] 
    checkFvalue                 <-  ( FValue <= 1 ) && ( FValue >= 0 ) 
    if( ! checkFvalue ){
      msg                       <-  paste0( "Bio-availability parameter (F) should be above 0 ane below 1 " , "\n" )
      errors                    <-  c( errors , msg )
    }
    checkFCVzero                <-  coeffVariation[ "F" ] == 0 
    if( ! checkFCVzero ){
      msg                       <-  paste0( "Coefficient of variation of the bio-availability  (F) should be zero" , "\n" )
      errors                    <-  c( errors , msg )
    }
    #  check CV input misspecified
    flagNaParameter             <-  is.na( parameterVector ) 
    coeffVariationNoNA          <-  coeffVariation[ ! flagNaParameter  ]
    checkCVbetweenZeroAndOne    <-  all( coeffVariationNoNA <= 1 ) && all( coeffVariationNoNA >= 0 )
    if( ! checkCVbetweenZeroAndOne ) {
      msg                       <-  paste0( "Coefficient of variation of all parameters should be between 0 and 1"  , "\n" )
      errors                    <-  c( errors , msg )
    }
    #   produce error message
    if( length( errors )  >  0 ){
      stop( errors )
    }
  }
  
    
  ## seperate parameters to be sampled from the ones not being sampled 
  indParamNoSample             <-  is.na( parameterVector ) | coeffVariation == 0
  parameterVectorNoNA          <-  parameterVector[ !indParamNoSample ]
  coeffVariationNoNA           <-  coeffVariation[ !indParamNoSample ]
  correlationMatrixNoNA        <-  correlationMatrix[ !indParamNoSample , !indParamNoSample ]
         
  
  ##  check compatibility dosing info and times, add if necessary
  dosingInfo                   <-  getDosingInfo( pkModel )
  timesDosing                  <-  dosingInfo$time
  flagDoseTimeIncluded         <-  timesDosing %in% timePoints
  boolDosingTimesInTimePoints  <-  all( flagDoseTimeIncluded )
  
  if( !boolDosingTimesInTimePoints ) {
    timePointsFull             <-  sort( c( timePoints , timesDosing[ !flagDoseTimeIncluded] ) )
    indTimePointsOnlyDose      <-  timePointsFull %in% timesDosing[ !flagDoseTimeIncluded ] 
  } else {
    timePointsFull             <-  timePoints 
  }
  nTimePointsFull              <-  length( timePointsFull )
  
  ## individual not sampled parameters (constants or NA )
  paramNoSample                <-  parameterVector[ indParamNoSample ]
  individualParamNoSample      <-  replicateVectorRows( paramNoSample , nTotalSubjects )
  colnames( individualParamNoSample )  <-  names( paramNoSample )
  
  
  # debug write down 
#  if( boolWriteDown ){
#    saveRDS( object = .Random.seed , file.path( dirIntermediateOutput , "seedState1.rds")  )
#  }
    
  ## simulate subject specific parameters (of nonNA parameters)
  boolNoParam                  <-  ( length( parameterVectorNoNA ) == 0 )
  if( !boolNoParam ) {
    individualParameters       <-  getIndividualParameters( meanParam = parameterVectorNoNA , 
        coeffVariation =  coeffVariationNoNA , nSubjects = nTotalSubjects , corrMatrix = correlationMatrixNoNA )
    allIndividualParameters      <-  cbind( individualParameters ,  individualParamNoSample ) # note no data.frame to aaply
    
  } else {
    allIndividualParameters     <-  individualParamNoSample
  }

#  # debug write down 
#  if( boolWriteDown ){
#    saveRDS( object = allIndividualParameters , file.path( dirIntermediateOutput , "indivParameters.rds")  )
#    saveRDS( object = .Random.seed , file.path( dirIntermediateOutput , "seedState2.rds")  )
#    
#  }
  

  # get individual curves 
       # replaced by parellel processing 

#  
#  Parallelization here

 if( nCores == 1 ){
   indParamList                 <-  convertMatrixRowsToList( allIndividualParameters )
   exampleFunValue              <-  vector( mode = "double" , length = nTimePointsFull ) 
   individualPkCurvesTransp     <-  vapply( indParamList , FUN.VALUE = exampleFunValue ,  modelFunction , time = timePointsFull , dosingInfo = getDosingInfo( pkModel  ) )
   individualPkCurves           <-  t( individualPkCurvesTransp )

 } else {
   cluster                     <-  makeCluster( nCores , type = "FORK" )
   on.exit(stopCluster(cluster)) # clean up cluster 
   cat( "starting cluster (forking) with " , nCores , "cores" , "\n")
   individualPkCurvesVec       <-  parRapply( cluster , allIndividualParameters , modelFunction ,
       time = timePointsFull , dosingInfo = getDosingInfo( pkModel  ) )
   
   individualPkCurves           <-  matrix( individualPkCurvesVec , byrow = TRUE , nrow = nTotalSubjects)
}
  
#  # debug write down 
#   if( boolWriteDown ){
#     saveRDS( object = individualPkCurves , file.path( dirIntermediateOutput , "individualPkCurves.rds")  ) 
#     saveRDS( object = .Random.seed , file.path( dirIntermediateOutput , "seedState3.rds")  ) 
#  }

  
  ## subset on original time points (get out dosing times )
  if( ! boolDosingTimesInTimePoints  ) {
    individualPkCurvesTimeSelect  <- individualPkCurves[ , !indTimePointsOnlyDose ]  
  } else {
    individualPkCurvesTimeSelect  <- individualPkCurves  
  }
    
  ##  output formatting
  colnames( individualPkCurvesTimeSelect )  <-  paste0( "timePoint" , 1:nTimePoints )
  
  ## add additive error # TODO difference here check all inputs
#  if( boolWriteDown ){
#    saveRDS( object = individualPkCurvesTimeSelect , file.path( dirIntermediateOutput , "individualPkCurvesTimeSelect.rds")  ) 
#    saveRDS( object = getCoeffVariationError( pkModel ) , file.path( dirIntermediateOutput , "coeffVarError.rds")  ) 
#    saveRDS( object = errorCorrelationMatrixIntime , file.path( dirIntermediateOutput , "errorCorrelationMatrixIntime.rds")  )    
#  }
  
  individualCurvesWithError                 <-  addAdditiveErrorToPkMatrix( individualPkCurvesTimeSelect ,
      coeffVariation  = getCoeffVariationError( pkModel ) ,
      timeCorrelation = errorCorrelationMatrixIntime      
      )
      
      
#      # debug write down 
#      if( boolWriteDown ){
#        saveRDS( object = individualCurvesWithError , file.path( dirIntermediateOutput , "individualCurvesWithError.rds")  ) 
#        saveRDS( object = .Random.seed , file.path( dirIntermediateOutput , "seedState4.rds")  ) 
#      } 
#  
  
  dataArray                    <-  array( individualCurvesWithError , dim = c( nSubjectsPerScheme , nSamples , nTimePoints  )  )
  dataArrayFormat              <-  aperm( dataArray , c( 1 , 3 , 2 ) )
  
  dimnames( dataArrayFormat )  <-  list( paste0( "subject"   , 1:nSubjectsPerScheme ) , 
      paste0( "timePoint" , 1:nTimePoints        ) ,
      paste0( "sample"    , 1:nSamples           )
  )
   
  
  ## construct PkData object
  
  outputObject                 <-  new( "PkData" ,
      .Data              =  dataArrayFormat ,
      timePoints         =  timePoints ,
      pkModel 
      ) 
  return( outputObject )
}





# TODO is the internal function working change the dimensions to test it 

#if( 0 == 1 ) {
#  meanParam               <-  c( 1 , 0.1 , 10 , 3 )
#  names( meanParam )      <-  c( "Ka", "Ke" , "volume" , "dose" ) 
#  coeffVariation          <-  c( 0.05 , 0.05 , 0.05, 0 )
#  names(coeffVariation)   <-  names( meanParam )
#  nSubjects               <-  9
#  
#  # example correlation matrix 
#  corrMatrix              <-  matrix(0.2 , nrow = 4 , ncol = 4) + diag( rep( 0.8 , 4 ) )
#  
#  # assuning independence between parameters
#  getIndividualParameters( parameters , coeffVariation ,  nSubjects = 9 )
#  getIndividualParameters( parameters , coeffVariation ,  nSubjects = 9 , corrMatrix)
#  getIndividualParameters( parameters , coeffVariation ,  nSubjects = 3 , corrMatrix)
#  # assuming correlations between parameters
#  
#}


if( 0 == 1 ) {
  parameters              <-  c( 1 , 0.1 , 10 , NA ) # influence of NA in parameters? 
  names( parameters )     <-  c( "Ka", "Ke" , "volume" , "dose" ) 
  meanParam               <-  parameters
  coeffVariation          <-  c( 0.05 , 0.05 , 0.05, 0 )
  names(coeffVariation)   <-  names( parameters )
  nSubjects               <-  9
  
  getIndividualParameters( parameters , coeffVariation ,  nSubjects = 9 )
  
  # debugging zero variance
  meanParam           <-  c(F = 1)
  coeffVariation      <-  c(F = 0)
  nSubjects           <-  9
  
  # coeff variation non zero
  meanParam           <-  c( F = 1 , B = 5 )
  coeffVariation      <-  c( F = 0.2 , B = 0.2)
  nSubjects           <-  1000
  # TODO test correlation part 
  corrMatrix          <-  matrix( c( 1, 0.9 , 0.9 , 1 ) , nrow = 2 ) 
  
  # test non positive deff matrix
  
  corrMatrix          <-  matrix( c( 1, 0.5 , 0.9 , 1 ) , nrow = 2 ) 
  
  
  ### does get Individual parameters retun ok mean values? 
  
  testIndPar    <-   getIndividualParameters( 1 , 0.2 , 10000 )
  mean( testIndPar ) # this is very far of 1 
  
  
  testIndPar    <-   getIndividualParameters( 0.0025 , 0.1 , 100000 )
  mean( testIndPar ) # this is very far of 1 
  
  # debugging 
  meanParam          <-  1
  coeffVariation     <-  0.5
  nSubjects          <- 100 
  corrMatrix = NULL
 
}


#' sample subject specific parameters to input in pharmacodynamic model
#' paramaters are sample from a log-normal distribution 
#' 
#' @param meanParam numeric vector containing mean information of a set of parameters
#' @param coeffVariation coefficient of variantion to inform the variance of the subject
#' @param nSubjects  the number of subjects which should be sampled
#' @param corrMatrix optional correlation matrix when not specified parameters are assumed independent
#' @return a matrix with rows subject and columns parameters
#' @importFrom matrixcalc is.positive.definite
#' @importFrom MASS mvrnorm
#' @examples 
#'   parameters              <-  c( 1 , 0.1 , 10 , 3 )
#'   names( parameters )     <-  c( "Ka", "Ke" , "volume" , "dose" ) 
#'   coeffVariation          <-  c( 0.05 , 0.05 , 0.05, 0 )
#'   names(coeffVariation)   <-  names( parameters )
#'   nSubjects               <-  9
#' 
#'   # example correlation matrix 
#'   corrMatrix              <-  matrix(0.2 , nrow = 4 , ncol = 4) +
#'      diag( rep( 0.8 , 4 ) ) # correlation on the the log scale
#'   
#'   # assuming independence between parameters
#'   getIndividualParameters( parameters , coeffVariation ,  nSubjects = 9 )
#'   
#'   # assuming correlations between parameters 
#'   getIndividualParameters( parameters , coeffVariation ,  nSubjects = 9 , corrMatrix)
#'   getIndividualParameters( meanParam = parameters , coeffVariation ,  nSubjects = 3 , corrMatrix)
#' 
#' 
#' @export
getIndividualParameters    <-  function( meanParam , coeffVariation , nSubjects , corrMatrix = NULL ) {
  # work in dimensions nsubject * n parameters 
  nParameters              <-  length( meanParam )
  outputDim                <-  c( nSubjects , nParameters )
  outputDimNames           <-  list( paste0( "subject" , 1:nSubjects ) ,  # rows are subjects
                                     names( meanParam )                           # colums are parameters
  )
  
  # convert CV and mean to my and sigma parameters
  sigma                    <-  sqrt( log( coeffVariation^2 + 1) )
  mu                       <-  log( meanParam ) - ( sigma^2) /2   # this was corrected 
  
#  variances                <-  coeffVariation * abs( meanParam )
  
  if( is.null( corrMatrix ) ) {
    corrMatrix        <-  diag( rep( 1 , nParameters ) )                         # default is assuming independence of parameters 
  }
 # checking moved to individual parameters 
#  if( !is.positive.definite( corrMatrix ) ) {
#    stop("corrMatrix not positive definite" )
#  }
#  
#  standardNormalDraws      <-   mvrnorm( n = nSubjects , mu = rep( 0 , nParameters) , Sigma = corrMatrix )
  standardNormalDraws      <-   genMVN( n = nSubjects , mu = rep( 0 , nParameters) , Sigma = corrMatrix )
  
   ##  fix dimensions to output dimension
  muMat                    <-  replicateVectorRows( mu , nSubjects  )
  sigmaMat                 <-  replicateVectorRows( sigma , nSubjects )
          
  individualParameters     <-   exp( muMat + sigmaMat * standardNormalDraws ) # lognormal distribution 
  dimnames( individualParameters )   <-  outputDimNames
  return( individualParameters )
}



