//# LattStatsSpecialize.h: specialized functions for LatticeStatistics
//# Copyright (C) 1996,1997,1998,1999,2000
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

#if !defined(AIPS_LATTSTATSSPECIALIZE_H)
#define AIPS_LATTSTATSSPECIALIZE_H


//# Includes
#include <aips/aips.h>
#include <aips/Mathematics/Complex.h>
template <class T> class Vector;
template <class T> class Lattice;
class LatticeExprNode;
class String;
class IPosition;



// <summary>  </summary>
// <use visibility=export>
//
// <reviewed reviewer="" date="yyyy/mm/dd" tests="" demos="">
// </reviewed>
//
// <prerequisite>
// </prerequisite>
//
// <etymology>
// </etymology>
//
// <synopsis>
// </synopsis>
// 
// <motivation>
// </motivation>
// 
// <todo asof="1998/01/10">
// </todo>
 

class LattStatsSpecialize
{
public:
//
   static void accumulate (Float& nPts, Double& sum,
                                  Double& sumSq, Float& dataMin,
                                  Float& dataMax, Int& minPos,
                                  Int& maxPos, Bool& minMaxInit,
                                  Bool fixedMinMax, Float datum,
                                  uInt& pos, Float useIt);
   static void accumulate (Complex& nPts, DComplex& sum,
                                  DComplex& sumSq, Complex& dataMin,
                                  Complex& dataMax, Int& minPos,
                                  Int& maxPos, Bool& minMaxInit,
                                  Bool fixedMinMax, Complex datum,
                                  uInt& pos, Complex useIt);
//
   static Bool hasSomePoints (Float npts);
   static Bool hasSomePoints (Complex npts);
//
   static uInt maxPts (const Vector<Float>& nPts);
   static uInt maxPts (const Vector<Complex>& nPts);
//
   static void setUseItTrue (Float& useIt);
   static void setUseItTrue (Complex& useIt);
//
   static Float usePixelInc (Float dMin, Float dMax, Float datum);
   static Complex usePixelInc (Complex dMin, Complex dMax, Complex datum);
//
   static Float usePixelExc (Float dMin, Float dMax, Float datum);
   static Complex usePixelExc (Complex dMin, Complex dMax, Complex datum);
//
   static Float getMean (Float sum, Float n);
   static Complex getMean (Complex sum, Complex n);
//
   static Float getVariance (Float sum, Float sumsq, Float n);
   static Complex getVariance (Complex sum, Complex sumsq, Complex n);
//
   static Float getSigma (Float sum, Float sumsq, Float n);
   static Complex getSigma (Complex sum, Complex sumsq, Complex n);
//
   static Float getSigma (Float var);
   static Complex getSigma (Complex var);
//
   static Float getRms (Float sumsq, Float n);
   static Complex getRms (Complex sumsq, Complex n);
//
   static Float min(Float v1, Float v2);
   static Complex min(Complex v1, Complex v2);
//
   static Float max(Float v1, Float v2);
   static Complex max(Complex v1, Complex v2);
//
   static void putNodeInStorageLattice(Lattice<Float>& lat,
                                       const LatticeExprNode& node,
                                       const IPosition& where);
   static void putNodeInStorageLattice(Lattice<Complex>& lat,
                                       const LatticeExprNode& node,
                                       const IPosition& where);
//
   static Bool setIncludeExclude (String& errorMessage,
                                  Vector<Float>& range,
                                  Bool& noInclude, Bool& noExclude,
                                  const Vector<Float>& include,  
                                  const Vector<Float>& exclude);
   static Bool setIncludeExclude (String& errorMessage,
                                  Vector<Complex>& range,
                                  Bool& noInclude, Bool& noExclude,
                                  const Vector<Complex>& include,  
                                  const Vector<Complex>& exclude);
};

#endif

