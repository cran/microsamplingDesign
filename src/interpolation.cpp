#include "RcppArmadillo.h"
#include <iostream>
#include <iomanip>
#include <vector>
using namespace std;
using namespace Rcpp;
using namespace RcppArmadillo;



// [[Rcpp::export]]
double interpolate( std::vector<double> &xData,  std::vector<double> &yData, double x )
{
   int size = xData.size();

   int i = 0;                                                                  // find left end of interval for interpolation
   if ( x >= xData[size - 2] )                                                 // special case: beyond right end
   {
      i = size - 2;
   }
   else
   {
      while ( x > xData[i+1] ) i++;
   }
   double xL = xData[i], yL = yData[i], xR = xData[i+1], yR = yData[i+1];      // points on either side (unless beyond ends)
//   if ( !extrapolate )                                                         // if beyond ends of array and not extrapolating
//   {
      if ( x < xL ) yR = yL;
      if ( x > xR ) yL = yR;
//   }

   double dydx = ( yR - yL ) / ( xR - xL );                                    // gradient

   return yL + dydx * ( x - xL );                                              // linear interpolation
}



// Interpolate over a vector of x values //

// [[Rcpp::export]]
std::vector<double>  interpolateVec(  std::vector<double> &xData,  std::vector<double> &yData,   std::vector<double> &x ){
	int nPoints   = x.size() ;
	 std::vector<double>  outputVec( nPoints ) ;
	for( int iPoint = 0 ; iPoint < nPoints ; iPoint++ ){
		double xI            = x[ iPoint ] ;
		double resultI       = interpolate( xData , yData  , xI ) ;
		outputVec[ iPoint ]  = resultI ;
	}
	return( outputVec );
}



// criterion calculation with Rcpp armadillo

// calculating default scheme statistics (AUC and cMax )

// [[Rcpp::export]]
arma::colvec getFixedSchemePerformanceCpp( arma::mat  popAvCurves , arma::colvec times  ){
	int nTimes ;
	nTimes           =  times.size();
	int nCurves ;
//	nCurves          =  popAvCurves.n_rows() ;
	int NCRITERIA ;
	NCRITERIA        = 2 ;
	arma::mat resultMat ;
	arma::rowvec zeroRow(  popAvCurves.n_cols ) ;
	zeroRow.fill( 0.0 ) ;
	arma::colvec zeroTime( 1 ) ;
	zeroTime.fill( 0.0 ) ;
	arma::colvec timesW0 ;
	timesW0           =  join_cols( zeroTime , times ) ;
	arma::mat popAvCurvesW0 ;
//		Rcpp::Rcout << "size zeroRow " << zeroRow.size() << std::endl;
//		Rcpp::Rcout << "nrows popAvCurves " << size( popAvCurves , 0 )  << std::endl;
//		Rcpp::Rcout << "ncols popAvCurves " << size( popAvCurves , 1 ) << std::endl;

	popAvCurvesW0     =  join_cols( zeroRow , popAvCurves ) ;
	arma::rowvec AUC(  popAvCurves.n_cols ) ;
//	Rcpp::Rcout << "size X " << timesW0.size() << std::endl;
//	Rcpp::Rcout << "size Y " << popAvCurvesW0.size() << std::endl;
	AUC               =  trapz( timesW0 , popAvCurvesW0 , 0 ) ; // dims 0 means over rows
	arma::rowvec cMax(  popAvCurves.n_cols ) ;
    cMax              = max( popAvCurves , 0 ) ;
	resultMat         = join_cols( AUC, cMax ) ;
	arma::colvec varResult( NCRITERIA ) ;
    varResult         = var( resultMat , 0 , 1 ) ; // variance over columns
	return( varResult ) ;
	}


