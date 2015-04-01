//# VanVleck.h: Class of static functions to aid with vanVleck corrections.
//# Copyright (C) 2002
//# Associated Universities, Inc. Washington DC, USA.
//#
//# This library is free software; you can redistribute it and/or modify it
//# under the terms of the GNU Library General Public License as published by
//# the Free Software Foundation; either version 2 of the License, or (at your
//# option) any later version.
//#
//# This library is distributed in the hope that it will be useful, but WITHOUT
//# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
//# FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Library General Public
//# License for more details.
//#
//# You should have received a copy of the GNU Library General Public License
//# along with this library; if not, write to the Free Software Foundation,
//# Inc., 675 Massachusetts Ave, Cambridge, MA 02139, USA.
//#
//# Correspondence concerning AIPS++ should be addressed as follows:
//#        Internet email: aips2-request@nrao.edu.
//#        Postal address: AIPS++ Project Office
//#                        National Radio Astronomy Observatory
//#                        520 Edgemont Road
//#                        Charlottesville, VA 22903-2475 USA
//#
//# $Id$

#ifndef SCIMATH_VANVLECK_H
#define SCIMATH_VANVLECK_H

//#! Includes go here
#include <casacore/casa/aips.h>
#include <casacore/casa/Arrays/Matrix.h>
#include <casacore/scimath/Functionals/Interpolate1D.h>
#include <casacore/casa/BasicSL/Constants.h>
#include <casacore/casa/OS/Mutex.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations

// <summary>
// A class of static functions to aid with vanVleck corrections of lag data.
// </summary>

// <use visibility=export>

// <reviewed reviewer="" date="yyyy/mm/dd" tests="" demos="">
// </reviewed>

// <prerequisite>
//   <li> Familiarity with the issues involved in turning digitally 
//        sampled lag data from a correlator into spectral data.
// </prerequisite>
//
// <etymology>
// This provides the functions necessary to determine the van Vleck correction
// for a general n-level by m-level correlator.
// </etymology>
//
// <synopsis>
// This provides the functions necessary to determine the van Vleck correction
// for a general n-level by m-level correlator.
// </synopsis>
//
// <example>
// </example>
//
// <motivation>
// The GBT spectrometer provides the measured auto-correlation and
// cross-correlation lags.  The gbt MeasurementSet filler (gbtmsfiller)
// needs to convert those lags to the spectral domain.  These functions
// allow the filler to calculate the van Vleck correction appropriate
// for each measured set of lags.  They are of general and hence are
// not specific to the GBT spectrometer.
//
// The functions here are static because of the nature of the underlying
// numerical quadrature fortran code used to integrate the 
// drbyrho function.
// </motivation>
//
// <thrown>
//    <li>
//    <li>
// </thrown>
//
// <todo asof="2002/07/19">
//   <li> The inverse error functions may be more generally useful.
//        It exists here only as a private member function to be
//        used internally.
// </todo>

class VanVleck
{
public:
    // Set the interpolation table size.
    // Should be an odd number.  The default size is 65.
    static void size(uInt npts);

    // get the current size.
    static uInt getsize();
    
    // Set the x and y quantization functions.
    // Each matrix should have dimensions (2,n)
    // where n is the number of levels.  The first
    // row (0,...) is the (n-1) threshold levels and 
    // the second row is the n quantizations based 
    // on those thresholds.  The thresholds may 
    // include a DC offset. The (0,(n-1)) element is
    // never used and need not be set.
    static void setQuantization(const Matrix<Double> &qx,
				const Matrix<Double> &qy);

    // Set the x and y quantization levels for the case
    // of equi-spaced levels with a possible non-zero
    // offset.  The total number of levels is given by n,
    // which must be 3 or 9.  If n is not 3 or 9, False
    // will be returned and no quantization will have been
    // set.  For the 3- and 9- level cases a bivarate normal
    // integral calculation will be used.  That is much faster
    // than the more general numerical integration used 
    // by setQuantization.
    static Bool setEquiSpaced(Double xlev, Double ylev,
			      Double xmean, Double ymean,
			      Int n);
    
    // Get the data used in setting up the interpolation
    static void getTable(Vector<Double> &rs, Vector<Double> &rhos);
    
    // Given a rho return the corresponding corrected r
    // Returns 0.0 if no quantization has been set yet.
    static Double r(const Double rho);
    
    // Given a measured zero-lag autocorrelation and number of
    // levels (n>=3) return the first positive quantizer input
    // threshold level.  This can be used to set the up the
    // matrix arguments used in setQuantization.
    static Double thresh(Int n, Double zerolag)
    { return ( (n>3) ? threshNgt3(n,zerolag) : threshN3(zerolag) ); }
    
    // Predict a given zero-lag given n and a threshold.  This
    // is included here to be used as a check against the output
    // of thresh.
    static Double predict(Int n, Double threshhold)
    { return ( (n>3) ? predictNgt3(n,threshhold) : predictN3(threshhold));}

    // Compute an approximation to the mean signal level (DC offset)
    // and quantizer threshold setting (both in terms of the r.m.s.
    // signal input level) given the observed positive bias (the
    // asymptotic limit of the measured autocorrelation at large
    // lags) and the zero-lag autocorrelation.
    // dcoffset is the mean signal level, threshold is the quantizer
    // setting, n is the number of levels, zerolag is the zero-lag
    // value and bias is the asymptotic bias.
    // Currently, this is only available for the n==3 level case,
    // all other cases set the returned dcoffset to 0 and use thresh()
    // to set the returned value of threshold.  A return value of F
    // indicates that the zerolag and bias values are inconsistent
    // and the dcoffset can not be determined.  In that case,
    // the returned dcoffset is 0 and thresh() is used to set
    // the threshold level.
    static Bool dcoff(Double &dcoffset, Double &threshold,
		      Int n, Double zerolag, Double bias);
		      
    
private:
    // the number of points to use in setting up the interpolator
    static uInt itsSize, itsNx, itsNy;

    static Bool itsEquiSpaced;

    static Double itsXlev, itsYlev, itsXmean, itsYmean;
    
    // The interpolator
    static Interpolate1D<Double, Double> *itsInterp;
    
    // the quantization functions
    static Vector<Double> itsQx0, itsQx1, itsQy0, itsQy1;
    
    // Useful combinations of the above - to speed up drbydrho
    // these are -1/2*(Qx0*Qx0) and -1/2*(Qy0*Qy0)
    // These are only used for i up to (itsQx0.nelements() and
    // for j up to (itsQy0.nelements()).
    static Vector<Double> itsQx0Qx0, itsQy0Qy0;
    // This is Qx0[i]*Qy0[j]
    static Matrix<Double> itsQx0Qy0;
    // This is (Qx1[i+1]-Qx1[i])*(Qy1[j+1]*Qy1[j])
    static Matrix<Double> itsQx1Qy1diffs;
    // The mutex to make the functions thread-safe.
    static Mutex theirMutex;


    // The fortran numerical integration function will call this.
    // For a given rho and quantization functions, this computes,
    // via Price's theorem, the value dr/drho of the derivative,
    // with respect to rho, of the expected value of the correlator
    // output.
    static Double drbydrho(Double *rho);
    
    // For a given rhoi, rhof, this produces a high-accuracy numerical
    // approximation to the integral of drbydrho over the range
    // rhoi to rhof.  It calls the standard QUADPACK adaptive Gaussian quadrature
    // procedure, dqags, to do the numerical integration.
    static Double rinc(Double &rhoi, Double &rhof);
    
    // initialize the interpolator
    static void initInterpolator();
    
    // compute first threshhold for a given zerolag for n>3
    static Double threshNgt3(Int n, Double zerolag);
    
    // compute first threshhold for a given zerolag for n==3
    static Double threshN3(Double zerolag)
    { return sqrt(2.0)*invErfc(zerolag);}
    
    // inverse err fn - used by invErfc
    static Double invErf(Double x);
    
    // inverse complementary err fn - used by threshN3
    static Double invErfc(Double x);
    
    // Predict a zero-lag value given the indicated first threshold level
    // for n>3.
    static Double predictNgt3(Int n, Double threshhold);
    
    // Predict a zero-lag value given the indicated first threshold level
    // for n=3.
    static Double predictN3(Double threshhold)
    { return ::erfc(threshhold/sqrt(2.0));}

    // implementation of dcoff for the 3-level case
    static Bool dcoff3(Double &dcoffset, Double &threshold,
		       Double zerolag, Double bias);
};


} //# NAMESPACE CASACORE - END

#endif
