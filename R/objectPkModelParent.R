# Project: microsamplingDesign
# Adriaan Blommaert
# virtual class containing valdity and function common to PkModel and PkModelRange
###############################################################################




#' virtual class, for testing \code{\link{PkModel-class}} and \code{\link{PkModelRange-class}} together
#' 
#' @name PkModelParent-class
#' @keywords internal 
#' @template pkmodelParentSlots
#' @author Adriaan Blommaert
#' @importFrom methods new
#' @keywords internal
setClass( "PkModelParent"  , slots = c( modelFunction = "function", parameters = "data.frame" , correlationMatrix = "matrix" , coeffVariationError = "vector" ,
    dosingInfo = "data.frame" ) )



### validity


validityPkModelParent      <- function( object ) {
  errors                   <-  character( ) 
  ##parameters
  parameterSlot            <-  object@parameters
  nParameters              <-  nrow( parameterSlot )
#  parameterSlotNamesMin    <-  c( "parameter" , "value" , "coeffVariation"  )
#  paramSlotNames           <-  colnames( parameterSlot )
#  checkParamNames          <-  all( parameterSlotNamesMin %in% paramSlotNames )
#  if( ! checkParamNames ) {
#    msg                    <-  paste0( "column names of slot parameters should be: ( " ,
#      paste0( parameterSlotNamesMin , collapse = ", " ) ,")" , "\n" )
#    errors                 <- c( errors , msg )
#  }
  ## correlation matrix 
  corrMat                  <-  object@correlationMatrix
  # dimensions
  checkDimCorr             <-  ( nrow( corrMat ) == ncol( corrMat ) )  && ( nrow( corrMat ) == nParameters )
  if( ! checkDimCorr ) {
    msg                    <-  paste0( "incorrect dimensions of slot 'correlationMatrix' "  , "\n")
    errors                 <-  c( errors ,  msg )
  }
  # positive definite
  checkPosDef              <-  is.positive.definite( corrMat ) 
  if( ! checkPosDef ) {
    msg                    <-  paste0( "'correlationMatrix' is not positive definite" , "\n")
    errors                 <-  c( errors , msg )
  }
  
#  ## CV of residual error
#  CVResidual               <-  object@coeffVariationError
#  checkOneValue            <-  length( CVResidual ) == 1
#  checkNumeric             <-  is.numeric( CVResidual )
#  
#  if( ! ( checkOneValue && checkNumeric ) ) {
#    msg                    <-  paste0( "coeffVariationError should be one numeric value" , "\n")
#    errors                 <-  c( errors , msg )
#  }
  
  
  ## dosingInfo
  dosingInfo               <-  object@dosingInfo
  dosingInfoNamesMin       <-  c( "time" , "dose" )
  checkDosingNames         <-  all( dosingInfoNamesMin %in% colnames( dosingInfo ) )
  if( ! checkDosingNames  ) {
    msg                    <-  paste0( "column names of slot dosingInfo should be: (",
      paste0( dosingInfoNamesMin , collapse = ", " ) ,")" , "\n" )
    errors                 <-  c ( errors, msg )
  }
  ##  print errors if object not defined according to the rules 
  if ( length( errors)  == 0 ) {
    return( TRUE ) 
  } else {
    cat( errors )
    return( FALSE )
  }
  
  
}


setValidity( "PkModelParent" ,  validityPkModelParent  )


### common functions 

## getters  
#' @rdname getParameters
#' @export
setMethod( f = "getParameters" , signature = "PkModelParent" , 
  definition = function( object ) { 
    return( object@parameters ) 
  }
)

#' @rdname getCorrelationMatrix
#' @export
setMethod( f = "getCorrelationMatrix" , signature = "PkModelParent" , 
  definition = function( object ) { 
    return( object@correlationMatrix ) 
  }
)

#' function to summarize an object
#' 
#' @param object  \code{\link{PkModel-class}}
#' @rdname summary
#' @importFrom knitr kable
setMethod( f = "summary" , signature = "PkModelParent" ,  
  definition = function( object ) {
    func        <-  as.list( args( getModelFunction( object ) ) )$internalODEs
    param       <- getParameters( object )
    dose        <-  getDosingInfo( object )
    
    ##  print output to terminal 
    cat( "Model function: " , func , "\n"  )
    cat( "\n" )
    cat( "Parameters: " ,  "\n"  )
    print( kable( param ) )
    cat( "\n" )
    cat( "\n" )
    cat( "Dosing information: " ,  "\n"   )
    print( kable( dose ) )
    
  } )




#' @rdname getModelFunction
#' @export
setMethod( f = "getModelFunction" , signature = "PkModelParent" ,
  definition = function( object ) {
    return( object@modelFunction )      
  } 
)

#' @rdname getDosingInfo
#' @export
setMethod( f = "getDosingInfo" , signature = "PkModelParent" , 
  definition = function( object ) {
    return( object@dosingInfo )
  }
)

#' @rdname getCoeffVariationError
#' @export
setMethod( f = "getCoeffVariationError" , signature = "PkModelParent" , 
  definition = function( object ) {
    return( object@coeffVariationError )
  }
)



#' @rdname setDosingInfo
#' @export
setReplaceMethod(f = "setDosingInfo" , signature = "PkModelParent" ,
  def = function( object , value ){
    object@dosingInfo <- value 
    validObject( object )
    return( object )
  }
)


## setters


#' @rdname setParameters
#' @export
setReplaceMethod(f = "setParameters" , signature = "PkModelParent" ,
  def = function( object , value ){
    object@parameters <- value 
    validObject( object )
    return( object )
  }
)

#' @rdname setCorrelationMatrix
#' @export
setReplaceMethod(f = "setCorrelationMatrix" , signature = "PkModelParent" ,
  def = function( object , value ){
    object@correlationMatrix <- value 
    validObject( object )
    return( object )
  }
)


#' @rdname setCoeffVariationError
#' @export
setReplaceMethod(f = "setCoeffVariationError" , signature = "PkModelParent" ,
  def = function( object , value ){
    object@coeffVariationError <- value 
    validObject( object )
    return( object )
  }
)

