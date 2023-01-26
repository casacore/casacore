//# LattStatsSpecialize.h: specialized functions for LatticeStatistics
//# Copyright (C) 1996,1997,1998,1999,2000,2001,2002,2003
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

#ifndef LATTICES_LATTSTATSSPECIALIZE_H
#define LATTICES_LATTSTATSSPECIALIZE_H


//# Includes
#include <casacore/casa/aips.h>
#include <casacore/casa/Arrays/ArrayFwd.h>
#include <casacore/casa/BasicSL/Complex.h>
namespace casacore { //# NAMESPACE CASACORE - BEGIN

template <class T> class Lattice;
template <class T> class MaskedLattice;
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
   static bool hasSomePoints (double npts);
   static bool hasSomePoints (DComplex npts);
//
   static void setUseItTrue (float& useIt);
   static void setUseItTrue (Complex& useIt);
//
   static float usePixelInc (float dMin, float dMax, float datum);
   static Complex usePixelInc (Complex dMin, Complex dMax, Complex datum);
//
   static float usePixelExc (float dMin, float dMax, float datum);
   static Complex usePixelExc (Complex dMin, Complex dMax, Complex datum);
//
   static double getMean (double sum, double n);
   static DComplex getMean (DComplex sum, DComplex n);
//
   static double getVariance (double sum, double sumsq, double n);
   static DComplex getVariance (DComplex sum, DComplex sumsq, DComplex n);
//
   static double getSigma (double sum, double sumsq, double n);
   static DComplex getSigma (DComplex sum, DComplex sumsq, DComplex n);
//
   static double getSigma (double var);
   static DComplex getSigma (DComplex var);
//
   static double getRms (double sumsq, double n);
   static DComplex getRms (DComplex sumsq, DComplex n);
//
   static float min(float v1, float v2);
   static Complex min(Complex v1, Complex v2);
//
   static float max(float v1, float v2);
   static Complex max(Complex v1, Complex v2);
//
   static float getNodeScalarValue(const LatticeExprNode& node, float);
   static Complex getNodeScalarValue(const LatticeExprNode& node, Complex);

   template <class T> static bool setIncludeExclude (String& errorMessage,
                                  Vector<T>& range,
                                  bool& noInclude, bool& noExclude,
                                  const Vector<T>& include,
                                  const Vector<T>& exclude);
   static bool setIncludeExclude (String& errorMessage,
                                  Vector<Complex>& range,
                                  bool& noInclude, bool& noExclude,
                                  const Vector<Complex>& include,  
                                  const Vector<Complex>& exclude);
//
   static bool minMax (float& dataMin, float& dataMax, const MaskedLattice<float>* pLattice,
                       const Vector<float>& range, bool noInclude, bool noExclude);
   static bool minMax (Complex& dataMin, Complex& dataMax, const MaskedLattice<Complex>* pLattice,
                       const Vector<Complex>& range, bool noInclude, bool noExclude);
};


} //# NAMESPACE CASACORE - END

#ifndef CASACORE_NO_AUTO_TEMPLATES
#include <casacore/lattices/LatticeMath/LattStatsSpecialize2.tcc>
#endif //# CASACORE_NO_AUTO_TEMPLATES

#endif

