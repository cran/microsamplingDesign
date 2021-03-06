% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/objectPkModel.R
\name{construct2CompModel}
\alias{construct2CompModel}
\title{construct a 2 compartmental \code{\link{PkModel-class}} by providing parameters and dosing info}
\usage{
construct2CompModel(parameters, dosingInfo, correlationMatrix = NULL,
  coeffVariationError = 0)
}
\arguments{
\item{parameters}{see  \code{\link{PkModel-class}}}

\item{dosingInfo}{see \code{\link{PkModel-class}}}

\item{correlationMatrix}{see \code{\link{PkModel-class}}, if \code{NULL} idendity matrix is constructed}

\item{coeffVariationError}{see \code{\link{PkModel-class}} , defaults to 0}
}
\description{
construct a 2 compartmental \code{\link{PkModel-class}} by providing parameters and dosing info
}
\note{
model function is \code{\link{get2ComptModelCurve}}
}
\examples{
  dosingInfo              <-  data.frame( time = 0 , dose = 1)
  dataParametersFile      <-  system.file(  "extData",
 "examplePkParameters.csv" ,   package = "microsamplingDesign" )
  exampleParameters       <-  read.csv( dataParametersFile ,
 stringsAsFactors = FALSE , na.strings = NULL )
  pkModel                 <-  construct2CompModel( exampleParameters , dosingInfo ) 
  plotObject( pkModel , times = seq( 0, 5 , 0.1) , nSamplesIntegration = 12 )
}
