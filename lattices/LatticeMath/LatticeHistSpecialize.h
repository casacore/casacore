//# LatticeHistSpecialize.h:  specialized functions for LatticeHistograms
//# Copyright (C) 1996,1997,1999,2000
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

#ifndef LATTICES_LATTICEHISTSPECIALIZE_H
#define LATTICES_LATTICEHISTSPECIALIZE_H


//# Includes
#include <casacore/casa/aips.h>
#include <casacore/casa/BasicSL/Complex.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations
template <class T> class Vector;
template <class T> class Block;
class PGPlotter;

// <summary> Specialized functions for LatticeHistograms</summary>
// <use visibility=export>
//
// <reviewed reviewer="" date="yyyy/mm/dd" tests="" demos="">
// </reviewed>
//
// <prerequisite>
//   <li> <linkto class=LatticeHistograms>LatticeHistograms</linkto>
// </prerequisite>
//
// <etymology>
//  Specialized code is type specific.  This code is for LatticeHistograms.
// </etymology>
//
// <synopsis>
// This class provides specialized static functions to handle Type
// dependent (Float, Complex) processing for LatticeHistograms.
// I couldn't do it all with templated functions.
// </synopsis>
//
// <motivation>
// </motivation>
//
// <todo asof="1999/10/18">
// </todo>
  

class LatticeHistSpecialize
{
public:
// Make historgam cumulative
   static void makeCumulative (Vector<Float>& counts,
                               Float& yMax, uInt nBins,
                               Float scale);
   static void makeCumulative (Vector<Complex>& counts,
                               Complex& yMax, uInt nBins,
                               Float scale);

// Make histogram logarithmic
   static void makeLogarithmic (Vector<Float>& counts,
                                Float& yMax,
                                uInt nBins);
   static void makeLogarithmic (Vector<Complex>& counts,
                                Complex& yMax,
                                uInt nBins);

// Multiply.  Real and imaginary treated as independent
// C1*C2 = (r1*r2,i1*i2)
   static Float mul(Float v1, Float v2);
   static Complex mul(Complex v1, Complex v2);

// Plot histograms
   static void plot(PGPlotter& plot, Bool doGauss, Bool doCumu, Bool doLog,
                    Float linearSum, Float yMax, Float binWidth, 
                    const Vector<Float>& values,
                    const Vector<Float>& counts, const Vector<Float>& stats,
                    uInt whereLabel, uInt ci, Bool page);
   static void plot(PGPlotter& plot, Bool doGauss, Bool doCumu, Bool doLog,
                    Complex linearSum, Complex yMax, Complex binWidth, 
                    const Vector<Complex>& values,
                    const Vector<Complex>& counts, const Vector<Complex>& stats,
                    uInt whereLabel, uInt ci, Bool page);

// Process data chunk creating histogram.
   static void process(
		   const Float* pInData, const Bool* pInMask,
		   Block<Float>* pHist, const Vector<Float>& clip,
		   Float binWidth, uInt offset, uInt nrval,
		   uInt nBins, uInt dataIncr, uInt maskIncr
   );
//
   static void process (
		   const Complex* pInData, const Bool* pInMask,
		   Block<Complex>* pHist, const Vector<Complex>& clip,
		   Complex binWidth, uInt offset, uInt nrval,
		   uInt nBins, uInt dataIncr, uInt maskIncr
   );

// Set bin width.  For complex, real and imaginary treated separately
   static Float setBinWidth (Float dmin, Float dmax, uInt nBins);
//
   static Complex setBinWidth(Complex dmin, Complex dmax, uInt nBins);

private:
   static uInt bin(Float datum, Float min, Float width, uInt nBins);
//
   static void makeGauss(uInt& nGPts, Float& gMax,
                         Vector<Float>& gX, Vector<Float>& gY,
                         Float dMean, Float dSigma,
                         Float dSum, Float xMin,
                         Float xMax, Float binWidth,
                         Bool doCumu, Bool doLog);
//
   static void plotHist (const Vector<Float>& x,
                         const Vector<Float>& y,
                         PGPlotter& plotter);
};


} //# NAMESPACE CASACORE - END

#endif
