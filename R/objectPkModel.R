###############################################################################
#### Project: microsamplingDesign                                           ###
####                                                                        ###
#### Description: build Pk model given constraints and generate data from it  ###
####                                                                        ###
####                                                                        ###
####                                                                        ###
####                                                                        ###
#### Author: ablommaert                                                     ### 
####         <adriaan.blommaert@openanalytics.eu>                           ###
####                                                                        ###
####                                                                        ###
#### Maintainer:ablommaert                                                  ###
####       <adriaan.blommaert@openanalytics.eu>                             ###
####                                                                        ###  
####                                                                        ###
#### version 0.1                                                            ###
####                                                                        ###
#### changes:    coeffVariationError to 0.1 instead of 0.5 to still have data                                                           ###
####                                                                        ###
####                                                                        ###
###############################################################################


#' @include objectSetOfSchemes.R
#' @include objectPkModelParent.R
NULL



#' get minimal example of \code{\link{PkModel-class}}
#' 
#' @examples 
#' getExamplePkModel()
#' @export
getExamplePkModel          <-  function( ) {

    
    dataParametersFile      <-  system.file("extData", "examplePkParameters.csv" ,   package = "microsamplingDesign")
    exampleParameters       <-  read.csv( dataParametersFile , stringsAsFactors = FALSE , na.strings = NULL )
    nParam                  <-  nrow( exampleParameters )
    parameterNames          <-  exampleParameters$parameter
    correlationMatrix       <-  diag( rep( 1 , nParam )  )
    colnames( correlationMatrix )  <-  parameterNames
    rownames( correlationMatrix )  <-  parameterNames
 
  pkModel                  <-  new( "PkModel" ,
      modelFunction           = get2ComptModelCurve ,
      parameters              = exampleParameters ,
      correlationMatrix       =  correlationMatrix ,
      coeffVariationError     =  0.1 , 
      dosingInfo  = data.frame( time = c( 0 , 2 ) , dose = c( 20 , 30 )   )
  )
  return( pkModel )
}


#' construct a 2 compartmental \code{\link{PkModel-class}} by providing parameters and dosing info
#' 
#' @param parameters see  \code{\link{PkModel-class}}
#' @param  dosingInfo see \code{\link{PkModel-class}}
#' @param correlationMatrix see \code{\link{PkModel-class}}, if \code{NULL} idendity matrix is constructed 
#' @param  coeffVariationError  see \code{\link{PkModel-class}} , defaults to 0
#' @note model function is \code{\link{get2ComptModelCurve}}
#' @examples 
#'   dosingInfo              <-  data.frame( time = 0 , dose = 1 )
#'   dataParametersFile      <-  system.file(  "extData",
#'  "examplePkParameters.csv" ,   package = "microsamplingDesign" )
#'   exampleParameters       <-  read.csv( dataParametersFile ,
#'  stringsAsFactors = FALSE , na.strings = NULL )
#'   pkModel                 <-  construct2CompModel( exampleParameters , dosingInfo ) 
#'   plotObject( pkModel , times = seq( 0, 5 , 0.1) , nSamplesIntegration = 12 )
#' @export
construct2CompModel        <-  function( parameters , dosingInfo , correlationMatrix = NULL , coeffVariationError = 0 ) {
  nParameters              <-  nrow( parameters )
  parameterNames           <-  parameters$parameter
  if( is.null( correlationMatrix ) ) {
    correlationMatrix      <-  diag( 1 , nParameters )
    colnames( correlationMatrix ) <-  parameterNames
    rownames( correlationMatrix ) <-  parameterNames
  }
  
  
  pkModel                  <-  new( "PkModel" ,
      modelFunction = get2ComptModelCurve ,
      parameters = parameters  ,
      dosingInfo  = dosingInfo  ,
     
      correlationMatrix = correlationMatrix,
      coeffVariationError = coeffVariationError
  )
  return( pkModel )
}


if( 0 == 1 ) {
 model <-  getExampleModel
 object <-  model
 setParameters(object) <-  data.frame(zever = 1)
 
 # test validity correlation matrix , produce all error messages 
  object                   <-  getExamplePkModel()
  validatePkModel( object )
  # should be true, but error incorrect dimensions
  
  object@correlationMatrix   <-  rbind(cbind(object@correlationMatrix , 1 ) , 1 )
  validatePkModel( object )
  
}


# additinional validity testing on top of PkModelParent 
validatePkModel            <-  function( object ) {

  errors                   <-  character( ) 
  ##parameters
  parameterSlot            <-  object@parameters
  nParameters              <-  nrow( parameterSlot )
  parameterSlotNamesMin    <-  c( "parameter" , "value" , "coeffVariation"  )
  paramSlotNames           <-  colnames( parameterSlot )
  checkParamNames          <-  all( parameterSlotNamesMin %in% paramSlotNames )
  if( ! checkParamNames ) {
    msg                    <-  paste0( "column names of slot parameters should be: ( " ,
        paste0( parameterSlotNamesMin , collapse = ", " ) ,")" , "\n" )
    errors                 <- c( errors , msg )
  }
  ## CV of residual error
  CVResidual               <-  object@coeffVariationError
  checkOneValue            <-  length( CVResidual ) == 1
  checkNumeric             <-  is.numeric( CVResidual )
  
  if( ! ( checkOneValue && checkNumeric ) ) {
    msg                    <-  paste0( "coeffVariationError should be one numeric value" , "\n")
    errors                 <-  c( errors , msg )
  }
  ##  print errors if object not defined according to the rules 
  if ( length( errors)  == 0 ) {
    return( TRUE ) 
  } else {
    cat( errors )
    return( FALSE )
  }
}


#' S4 class PkModel representing a pharmacokinetic model and its parameters
#'
#' @template pkmodelParentSlots
#' 
#' @author Adriaan Blommaert
#' @importFrom methods new
#' @aliases PkModel pkModel pkmodel
#' @export
setClass("PkModel", contains = c( "PkModelParent" ) ) 


### validity 

setValidity( "PkModel" ,  validatePkModel ) 








# adaptation: plotPkModel, option not to plot average curve
plotPkModel                <-  function( object , times , nCurves = 12 , nSamplesIntegration  = 1000 , seed = 134 ,
    sampleCurvesOnly = FALSE , indSamplingPoints = FALSE ) {
  
  #fix randon element 
  set.seed( seed )
  ## extract from object 
  nTimes                   <-  length( times )
  
  ##  sample  curves
  set.seed( seed ) # use same seed for sample curves as population average curve
  exampleCurves            <-  getPkData( pkModel = object, timePoints = times ,  nSubjectsPerScheme = 1 , nSamples = nCurves )
  plotObject( object = exampleCurves , nCurves = NULL , nSamplesIntegration = nSamplesIntegration , 
    sampleCurvesOnly =  sampleCurvesOnly , seed = seed , indSamplingPoints = indSamplingPoints ) # for internale consistency use Pkdata object for plotting 

 }
 
 
#' @rdname plotObject
#' @param times  numeric vector at of times at which the model should be simulated for \code{\link{PkModel-class}}
#' @param nCurves the number of sample curves defaults to 12 for \code{\link{PkModel-class}} ,
#'  if \code{\link{PkData-class}} defaults to \code{NULL} meaning all data are plotted
#' @param nSamplesIntegration number of simulated curves to calculate averaged curve, defaults to  1000 
#' @param seed specify the random seed to draw samples to get the same plot each time
#' @param sampleCurvesOnly logical value if \code{TRUE} only sample curves are displayed and the averaged curve omitted ,
#'  defaults to \code{FALSE} for \code{\link{PkModel-class}} and \code{TRUE} for \code{\link{PkData-class}}
#' @param indSamplingPoints logical indicator if \code{TRUE} sample times are indicated on the plot, 
#' defaults to \code{FALSE} for \code{\link{PkModel-class}} and \code{TRUE} for \code{\link{PkData-class}}
#' @param addZeroIsZero logical value, when \code{TRUE} the zero point is added 
#'    to the plot with value zero ( only for  \code{\link{PkData-class}} , defaults to \code{FALSE} )
#' @import ggplot2  
#' @examples 
#' \dontrun{
#' # examples with limited number of samples, increase samples in practice
#' plotObject( object =  getExamplePkModel() , 
#'    times = seq( 0 , 10 , 1 )  , nSamplesIntegration = 25 )
#' plotObject( object =  getExamplePkModel() ,
#'    times = seq( 0 , 10 , 1 ) , nCurves = 3 , nSamplesIntegration = 5  )
#' plotObject( object =  getExamplePkModel() ,
#'     times = seq( 0 , 10 , 1 ) , nCurves = 3 , sampleCurvesOnly = TRUE  )
#' }
setMethod( f = "plotObject" , signature = "PkModel" , definition = plotPkModel )




checkPkData                <-  function( object ) {
  errors                   <-  character( ) 
  dimData                  <-  dim( object@.Data )
  nDimensions              <-  length( dimData )
  if( !nDimensions == 3 ) {
    msg                    <-  paste0( "Dimensions of data array should be 3, not:  ", nDimensions, "\n" )
    errors                 <-  c ( errors, msg )
  }
  timeDimension            <-  dimData[ 2 ]
  if( !timeDimension == length( object@timePoints ) ) {
    msg                    <-  paste0( "time dimension in data does not correspond to number of timePoints", "\n" )
    errors                 <-  c( errors, msg ) 
  }
  if( ! is.numeric( object@.Data ) ) {
    msg                    <-  paste("Data values should be numeric" , "\n" )
    errors                 <-  c( errors, msg )
  }

  
  
  ##  print errors if object not defined according to the rules 
  if ( length(errors) == 0 ) {
    TRUE 
  } else {
    cat( errors )
    FALSE
  }
}



#' An S4 object containing samples from a Pk model
#' 
#' @name PkData-class
#' @template pkmodelParentSlots
#' @slot timePoints vector of time points
#' @slot .Data  a numerical array of 3 dimensions ( nSubjects x nTimePoints x nSamples )
#' @note other slots are inherited from \code{{\link{PkModel-class}}}
#' @author Adriaan Blommaert
#' @rdname PkData-class
#' @aliases PkData pkData pkdata
#' @export
setClass("PkData",
    contains = "PkModel",
    slots = c( .Data = "array",  timePoints = "vector" ) ,
    validity = checkPkData
)



#if( 0 == 1 ) {
#  pkModel             <-  getExamplePkModel()
#  pkData              <-  getPkData( pkModel , timePoints =- 1:7 , nSubjectsPerScheme = 5 , nSamples = 3 )
#  object              <-  pkData
#  identical( pkModel , getModel(pkData) )
#  
#}
#' @rdname getPkModel
#' @examples getPkModel( getExampleData() )
#' @export
setMethod( f = "getPkModel" , signature = "PkData" , definition = function( object ) {
      model         <-  object@modelFunction
      parameters    <-  object@parameters
      dosingInfo    <-  object@dosingInfo
      corrMat       <-  object@correlationMatrix
      addError     <-  object@coeffVariationError
      
      pkModel        <-  new( "PkModel" , modelFunction = model , parameters = parameters ,
        correlationMatrix =  corrMat , 
        coeffVariationError  = addError , dosingInfo = dosingInfo  )
      return( pkModel )
    } 
)


# TODO generate alternative example once the Pk modeling is implemented 

#' generate an mimimal example of a Pk data without a model 
#' 
#' @importFrom stats rnorm
#' @importFrom utils  read.csv
#' @examples 
#' getExampleData()
#' @export
getExampleData              <-  function( ) { 
  timePoints                <-  c( 0.5 , 1 , 2 , 10 ) # vector of time points
  pkModel                   <-  getExamplePkModel()
  dataExample               <-  getPkData( pkModel ,  timePoints , nSubjectsPerScheme = 2 , nSamples = 7  )
  return( dataExample )
}


#' @rdname getTimePoints
#' @export
setMethod( "getTimePoints", "PkData",
    function( object ) { 
      return( object@timePoints )
    }
)

#' @rdname getData
#' @export
setMethod( "getData", "PkData",
    function( object ) { 
      return( object@.Data )
    }
)





#TODO: test this function
#Select subset of time points 
if( 0 == 1 ) {
  # development 
  object                       <-  getExampleData()
  timePointsSelect             <-  c( 0.5 , 1 )
  # example 
  .PkDataClass.timePointSubset( object, timePointsSelect )
  
  # test error wrong time points
  subsetOnTimePoints.PkData( object, c( 1 , 3 , 20 ) )            # wrong time points 
  subsetOnTimePoints.PkData( object, "blablabla" )                # wrong time point input
  subsetOnTimePoints.PkData( object, "blablabla" )                # wrong input class
  
}
subsetOnTimePoints.PkData      <-  function( object, timePointsSelect ) {
  #  test subset 
  timePoints                   <-  getTimePoints(object)
  flagSubset                   <-  timePointsSelect %in% timePoints
  if( !all( flagSubset ) ) {
    stop( "timePointsSelect is not a subset of the objects' timePoints" ) 
  }
  # subset time points
  flagTimeSelect               <-  timePoints %in% timePointsSelect
  dataNew                      <-  getData( object )[ , flagTimeSelect , ]  
  timePointsNew                <-  getTimePoints( object ) [ flagTimeSelect ]
  
  # test equality of time points 
  flagGoodTimePointsSelect     <-  identical( timePointsNew , timePointsSelect)
  if( !flagGoodTimePointsSelect ) { stop( "incorrect timePoint selection" ) }
  
    # create output object
  output                       <-  object
  output@timePoints            <-  timePointsNew
  output@.Data                 <-  dataNew
  validObject( output )
  return( output )
}

#' @param timePointsSelect a subset of time points to select data for
#' @rdname  subsetOnTimePoints
#' @examples 
#' \dontshow{ pkData  <- getExampleData() }
#' subsetOnTimePoints( pkData, c( 1 , 2 ) )
#' subsetOnTimePoints( object = pkData, timePointsSelect =  c( 1 , 2 ) )
#' @export
setMethod( f   = subsetOnTimePoints , 
    signature  = "PkModel",
    definition =  subsetOnTimePoints.PkData
) 
# remove function inclusion into the method
rm( subsetOnTimePoints.PkData  )


if( 0 == 1 ) {
  object           <-   getPkData( getExamplePkModel() , 1:10 ,  5 , 10 )
  nCurves          <-  3
#  nCurves          <-  NULL
  
  # debugg argument nCores not found
  
  plotObject( object = exampleCurves , nCurves = NULL , nSamplesIntegration = nSamplesIntegration , 
      sampleCurvesOnly =  sampleCurvesOnly , seed = seed , indSamplingPoints = indSamplingPoints )
  
}

# adaptation: plot selection of PkCurve 
plotPkData                 <-  function( object , nCurves = NULL , nSamplesIntegration = 1000 ,  sampleCurvesOnly = TRUE  , seed = NULL ,
    indSamplingPoints = TRUE , addZeroIsZero = FALSE  ) {

  ##  extract curves and data 
  pkDataFlat               <-  flattenPkData( object )
  times                    <-  getTimePoints( object )
  
  if( addZeroIsZero ){
    pkDataFlat             <-  cbind( 0 , pkDataFlat )
    times                  <-  c( 0 , times )
  }
  
  nSubjects                <-  nrow( pkDataFlat )
 if( ( is.null( nCurves ) )  ) {
    dataSelect             <-  pkDataFlat
  } else if ( nCurves >= nSubjects  ) {
    dataSelect             <-  pkDataFlat
  } else {
    dataSelect             <-  pkDataFlat[ seq_len( nCurves ) , , drop = FALSE ]   
  }
  nCurvesSelect            <-  nrow( dataSelect )
   nTimes                   <-  length( times )
  
  ## data in format to use ggplot 
  exampleCurvesVector      <-  as.vector( t( dataSelect ) ) # put observations after each other
  curveDataPlot            <-  data.frame( 
    subject = rep( 1 : nCurvesSelect , rep( nTimes , nCurvesSelect ) ) ,
    time = rep( times , nCurvesSelect)   ,
    concentration  = exampleCurvesVector ,
    curve      = "sample curve" 
  )
  
  ## set seed (to use with plotObject of signature pkModel )
  if( !is.null(seed) ) {
    set.seed( seed )
  }
  
  ## population averaged curve
  if( ! sampleCurvesOnly ) {
    popAveragedcurve         <-  getPopAveragedCurve(  timePoints = times , pkModel = object , nSamples = nSamplesIntegration  )  # erro ncores is missing
    avCurve                  <-  data.frame(
      subject =  NA ,
      time = times ,
      concentration = as.vector( popAveragedcurve ) , 
      curve = " averaged curve" 
    )
    plotData                  <-  rbind( curveDataPlot , avCurve ) 
    lineWidth                 <-  c( 1.75 , 3 )
  } else {
    plotData                  <-  curveDataPlot
    lineWidth                 <-  2.5
  }
  
  
  ## plot object 
  # x11()
  with( data = plotData , {
      
     plot  <-   ggplot(data =  plotData , aes( x = time , y = concentration , group = subject , size = curve , color = curve ) )  +
        geom_path() +
        ylab("\n Concentration in plasma \n")  +
        xlab("\n Time in hours ") +
        theme( axis.title = element_text(size = rel(1.2)),
          axis.text = element_text(size = rel(1.2)) 
        ) +
        scale_colour_grey( start = 0.8 , end = 0.2  ) +
        scale_size_manual( values = lineWidth )	
      
     if( indSamplingPoints ) {
       plot   = plot + geom_point( colour = "dodgerblue3" ) 
     }
     plot
    }
  )
}


#' @rdname plotObject
#' @import ggplot2  
#' @examples 
#' \dontrun{
#' pkData    <-   getPkData( getExamplePkModel() , 1:10 ,  5 , 10 )
#' plotObject( object =  pkData  )
#' plotObject( object =  pkData , nCurves = 2 )
#' plotObject( object =  pkData , nCurves = 2 , addZeroIsZero = TRUE )
#' plotObject( object = pkData , nCurves = 3 , 
#'    sampleCurvesOnly = FALSE , nSamplesIntegration = 25   ) 
#' } 
setMethod( f = "plotObject" , signature = "PkData" , definition = plotPkData )


