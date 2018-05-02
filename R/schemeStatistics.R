##############################################################################
### Project: microsamplingDesign                                                                                                                 
### Description :
###     statistic to be estimated population average curve                                                                       
###     population average curve is 
###     all function here sumarise the population average curve by one statistic
###     all functions work are a of dataframe with columns time and concentration 
###     all functions start with "pkCurveStat"
###                                                                                               
### Author: ablommaert                                                     
###         <adriaan.blommaert@openanalytics.eu>                           
###                                                                        
###                                                                        
### Maintainer:ablommaert                                                    
###       <adriaan.blommaert@openanalytics.eu>                             
###                                                                       
###                                                                        
### version 0.1                                                            
###                                                                        
### changes:                                                               
###                                                                        
###                                                                        
##############################################################################

### TODO option to smooth data before calculating statistics

# 
#' example of 1 pk curve to be used to test pkCurveStat_[function] 
#' 
#' @param times a numeric vector of timePoints
#' @return a \code{data.frame} with time and concentration as columns 
#' @examples getExamplePkCurve( times = 0:10  )
#' @export
getExamplePkCurve             <-  function( times ) {
  timePoints                  <-  times 
  pkModel                     <-  getExamplePkModel() 
  setDosingInfo( pkModel )    <-  data.frame( time = 0 , dose = 30 )    # only one dose 
#  set.seed(123435)
  pkData                      <-  getPkData( pkModel , timePoints , 1 , 1 )
  pkCurve                     <-  pkData[ 1 , , 1] 
  return( pkCurve )
}


#' calculate summary statistics from a pkCurve
#' 
#' implemented statistics:
#' \itemize{
#'   \item{auc }{ area under the curve , between first and last time points }
#'   \item{cMax }{ maximum concentration}
#'   \item{tMax }{ time at maximum concentration }
#' } 
#' @name  pkCurveStat 
#' @param concentration numeric vector of concentrations corresponding to \code{timePoints}  
#' @param timePoints  \code{time} and \code{concentration}
#' @return a numeric value 
#' @examples
#'   ## toy example
#'   timeToy               <-  1:2
#'   concToy               <-  1:2 
#'	 
#'   pkCurveStat_auc( concToy ,  timeToy ) 
#'   pkCurveStat_cMax( concToy ,  timeToy  )
#'   pkCurveStat_tMax( concToy ,  timeToy  )
#' 
#'   ## real example
#'   times                 <-  c(0 , 1.5 , 2:10)
#'   concentration         <-  getExamplePkCurve( times )
#'   pkCurveStat_auc( concentration , times  ) 
#'   pkCurveStat_cMax( concentration , times   )
#'   pkCurveStat_tMax( concentration , times   )
#' 
#' @name pkCurveStat
NULL

#' @rdname  pkCurveStat
#' @aliases pkCurveStat pkCurveStat_auc auc
#' @export
pkCurveStat_auc                <-  function( concentration , timePoints ) {
#  timePoints                   <-  pkCurve$time
  obs                          <-  concentration
  nTimes                       <-  length( timePoints )
  flagFirstObs                 <-  1:(nTimes - 1) 
  timeFirst                    <-  timePoints[ 1 ]
  timeMinOne                   <-  c( timeFirst , timePoints[ flagFirstObs ]  )
  obsMinOne                    <-  c( 0, concentration [ flagFirstObs ]) 
  deltaTime                    <-  timePoints - timeMinOne
  trapezoids                   <-  deltaTime * ( concentration +  obsMinOne )/2 
  AUC                          <-  sum( trapezoids )
  AUC
}


# check what if observation starts with zero
if( 0 == 1 ){
	pkCurve               <-  data.frame( time = 0:10 , concentration = 0:10  )
	pkCurveStat_auc( pkCurve ) # its ok no problem
	
	# what if observation not zero
	
	pkCurve               <-  data.frame( time = 0:10 , concentration = 2:12  )
	pkCurveStat_auc( pkCurve ) 
	
	# what if negative time and concentration
	pkCurve               <-  data.frame( time = -3:7 , concentration = -3:7  ) # here it breaks downs 
	pkCurveStat_auc( pkCurve )
	
	
	
	# what if unsorted times -- not allowed (or sort before checking )
	pkCurve   <-  data.frame( time = c( 1 , 2 , 3 ) - 5.5  , concentration = c( 0 , 1 , 0 )  )
	pkCurve
	pkCurveStat_auc( pkCurve )
	
	
	

	
}




#' @rdname  pkCurveStat
#' @aliases pkCurveStat pkCurveStat_cMax cMax
#' @export
pkCurveStat_cMax                <-  function( concentration , timePoints  ) {
  cMax                         <-  max( concentration )
  return( cMax )
  
}

#' @rdname  pkCurveStat
#' @aliases pkCurveStat pkCurveStat_tMax tMax
#' @export
pkCurveStat_tMax               <-  function( concentration , timePoints ) {
  cMax                         <-  pkCurveStat_cMax( concentration , timePoints )  
  indCmax                      <-  cMax == concentration
  tMax                         <-  timePoints[ indCmax ]
  return( tMax )  
}

#
## first time half the concentration is reached ( by linear interpolation )
#pkCurveStat_tOneHalf           <-  function( pkCurve ) {
#  ## calculate 
#  cMax                         <-  pkCurveStat_cMax( pkCurve = pkCurve )
#  tMax                         <-  pkCurveStat_tMax( pkCurve = pkCurve )
#  cHalf                        <-  cMax / 2
#  indCHalf                     <-  pkCurve$concentration > halfCMax
#  indAboveTmax                 <- 
#  indTBelowT                   <-  indBelowCmax  & indAboveTmax
#  
#  
#  # linear interpolation to find time of half concentration
#  concVec                      <-  pkCurve$concentration
#  timeVec                      <-  pkCurve$time 
#  indCbelow                    <-  max( which(  concVec < halfCmax  ) )
#  indCabove                    <-  min( which(  concVec > halfCmax  )  )
#  
#  tOneHalf                     <-  approx( x =   )
#    
#  #linear interpolation to find the result 
#  
#}
