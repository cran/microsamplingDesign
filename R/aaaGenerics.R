##############################################################################
### Project: microsamplingDesign                                           ###
###                                                                        ###
### Description: all generics methods for S4 objects in the package        ###
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


#' @importFrom Rcpp evalCpp
#' @useDynLib microsamplingDesign
NULL


#' generic function to subset the \code{timePoints}-slot and generate an object of the same class
#' 
#' @param object a S4 class object
#' @param ...  additional parameters
#' @docType methods
#' @name subsetOnTimePoints
#' @export
setGeneric( "subsetOnTimePoints", function( object , ... ) {
      standardGeneric ( "subsetOnTimePoints" )
    } 
)



#' generic function extract the names of an S4-object 
#' 
#' @param object a S4 class object
#' @param ...  additional parameters
#' @docType methods
#' @name getNames
#' @export
setGeneric( "getNames", function( object , ... ) {
    standardGeneric ( "getNames" )
  } 
)


#' generic function to extract \code{timePoints}-slot
#' 
#' @param object a S4 class object
#' @param ...  additional parameters
#' @docType methods
#' @name getTimePoints
#' @export
setGeneric( "getTimePoints", function( object, ... ) {
      standardGeneric ( "getTimePoints" )
    } 
)


#' generic function to extract \code{nSchemes}-slot
#' 
#' @param object a S4 class object
#' @param ...  additional parameters
#' @docType methods
#' @name getNSchemes
#' @export
setGeneric( "getNSchemes", function( object, ... ) {
    standardGeneric ( "getNSchemes" )
  } 
)

#' generic function to extract \code{nSubjects}-slot
#' 
#' @param object a S4 class object
#' @param ...  additional parameters
#' @docType methods
#' @name getNSubjects
#' @export
setGeneric( "getNSubjects", function( object, ... ) {
    standardGeneric ( "getNSubjects" )
  } 
)

#' generic function to replace \code{timePoints}-slot
#' 
#' @param object a S4 class object
#' @param value a vector of time points
#' @usage setTimePoints(object) <- value
#' @docType methods
#' @rdname setTimePoints
#' @export setTimePoints<-
setGeneric( "setTimePoints<-", function( object, value ) {
      standardGeneric("setTimePoints<-")
    } 
)



#' replace \code{ranking}-slot
#' 
#' @param object a S4 class object
#' @param value a data.frame containing a ranking 
#' @docType methods
#' @usage setRanking(object) <-  value
#' @rdname setRanking
#' @export setRanking<-
setGeneric( "setRanking<-", function( object, value ) {
      standardGeneric("setRanking<-")
    } 
)


#' generic function to extract the \code{.Data}-slot 
#' 
#' @param object a S4 class object
#' @param ...  additional parameters
#' @docType methods
#' @name getData
#' @export
setGeneric( "getData", function( object , ... ) {
      standardGeneric ( "getData" )      
    } 
)

#' generic function to extract the \code{correlationMatrix}-slot
#' 
#' @param object a S4 class object
#' @param ...  additional parameters
#' @docType methods
#' @name getCorrelationMatrix
#' @export
setGeneric( "getCorrelationMatrix", function( object , ... ) {
      standardGeneric ( "getCorrelationMatrix" )      
    } 
)

#' generic function to extract \code{coeffVariationError} slot
#' 
#' @param object a S4 class object
#' @param ...  additional parameters
#' @docType methods
#' @name getCoeffVariationError
#' @export
setGeneric( "getCoeffVariationError", function( object , ... ) {
      standardGeneric ( "getCoeffVariationError" )      
    } 
)


# remark: no setter functions because always want to set the model function and parameters together

#' generic function to extract \code{parameter}-slot 
#' 
#' @param object a S4 class object
#' @param ...  additional parameters
#' @docType methods
#' @name getParameters
#' @export 
setGeneric( "getParameters" ,  function( object , ... ) {
      standardGeneric( "getParameters" )
    }  
)


#' generic function to extract \code{modelFunction} slot from S4-class object
#' 
#' @param object a S4 class object
#' @param ...  additional parameters
#' @docType methods
#' @name getModelFunction
#' @export 
setGeneric( "getModelFunction" , function( object , ... ) {
      standardGeneric( "getModelFunction" )
    }
)


#' generic function to extract \code{dosingInfo}-slot 
#' 
#' @param object a S4 class object
#' @param ...  additional parameters
#' @docType methods
#' @name getDosingInfo
#' @export 
setGeneric( "getDosingInfo" , function( object , ... ) {
      standardGeneric( "getDosingInfo" )
    }
)


#' replace \code{dosingInfo}-slot
#' 
#' @param object a S4 class object
#' @param value a data.frame containing dosing information
#' @docType methods
#' @rdname setDosingInfo
#' @usage  setDosingInfo(object) <-  value
#' @export setDosingInfo<-
setGeneric( "setDosingInfo<-", function( object, value ) {
      standardGeneric("setDosingInfo<-")
    } 
)

#' replace \code{parameters}-slot
#' 
#' @param object a S4 class object
#' @param value a data.frame containing parameters 
#' @docType methods
#' @rdname setParameters
#' @usage  setParameters( object ) <-  value
#' @export setParameters<-
setGeneric( "setParameters<-", function( object, value ) {
      standardGeneric("setParameters<-")
    } 
)

#' replace \code{correlationMatrix}-slot
#' 
#' @param object a S4 class object
#' @param value a matrix containing correlations between parameters  
#' @docType methods
#' @rdname setCorrelationMatrix
#' @usage  setCorrelationMatrix( object ) <-  value
#' @export setCorrelationMatrix<-
setGeneric( "setCorrelationMatrix<-", function( object, value ) {
      standardGeneric("setCorrelationMatrix<-")
    } 
)

#' replace \code{coeffVariationError}-slot
#' 
#' @param object a S4 class object
#' @param value a value containing the coefficient of variation of the error term
#' @docType methods
#' @rdname setCoeffVariationError
#' @usage  setCoeffVariationError( object ) <-  value
#' @export setCoeffVariationError<-
setGeneric( "setCoeffVariationError<-", function( object, value ) {
      standardGeneric("setCoeffVariationError<-")
    } 
)


#' generic function to calulate a \code{ranking}-slot 
#' 
#' @param object a S4 class object
#' @param ...  additional parameters
#' @param pkData \code{\link{PkData-class}}
#' @docType methods
#' @name rankObject
#' @export 
setGeneric( "rankObject", function(object, ... ) {
      standardGeneric( "rankObject" )
    }
)


#' generic function to extract the  \code{ranking}-slot 
#' 
#' @param object a S4 class object
#' @param ...  additional parameters
#' @docType methods
#' @name getRanking
#' @export 
setGeneric( "getRanking", function(object, ... ) {
      standardGeneric( "getRanking" )
    }
)

#' generic function to plot an object
#'
#' @param object a S4 class object
#' @param ...  additional parameters
#' @name plotObject
#' @docType methods
#' @export
setGeneric( "plotObject" , function( object, ... ) {
      standardGeneric( "plotObject" )
    } 
)

#' generic function extract a \code{\link{PkModel-class}}
#' 
#' @param object an S4 object
#' @param ... additional parameters
#' @name getPkModel
#' @export 
setGeneric( "getPkModel" , function( object , ... ) {
      standardGeneric( "getPkModel" )
      
    }
)


#' extract a timepoint or Scheme choice by its rank
#' 
#' @param object an S4 object
#' @param rank integer
#' @name extractByRank
#' @export 
setGeneric( "extractByRank" , function( object , rank ) {
    standardGeneric(  "extractByRank" ) 
  }
)

