//# LattStatsSpecialize.cc:  
//# Copyright (C) 1995,1996,1997,1998,1999,2000
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
//

#include <trial/Lattices/LattStatsSpecialize.h>
#include <trial/Lattices/LatticeStatsBase.h>

#include <aips/Arrays/IPosition.h>
#include <aips/Arrays/Vector.h>
#include <aips/Arrays/ArrayMath.h>
#include <aips/Exceptions/Error.h>
#include <aips/Lattices/Lattice.h>
#include <trial/Lattices/LatticeExprNode.h>
#include <aips/Mathematics/Complex.h>
#include <aips/Mathematics/Math.h>
#include <aips/Utilities/String.h>




void LattStatsSpecialize::accumulate (Float& nPts, Double& sum,
                                          Double& sumSq, Float& dataMin,
                                          Float& dataMax, Int& minPos,
                                          Int& maxPos, Bool& minMaxInit,
                                          Bool fixedMinMax, Float datum,
                                          uInt& pos, Float useIt)
{
   if (useIt < 0) return;
//
   nPts += 1;
   sum += datum;
   sumSq += datum*datum;
    
// If fixedMinMax, then the min and max will always
// be given by the inclusion range that the user specified.
// This will be set outside of here.  We have no
// more work to do if so.

   if (fixedMinMax) return;
                                
// Set min and max

   if (minMaxInit) {
      dataMin = datum;
      dataMax = datum;
      minPos = pos;
      maxPos = pos;
      minMaxInit = False;
   } else {  
      if (datum < dataMin) {
         dataMin = datum;
         minPos = pos;
      }
      if (datum > dataMax) {
         dataMax = datum;
         maxPos = pos;
      }
   }
}

void LattStatsSpecialize::accumulate (Complex& nPts, DComplex& sum,
                                          DComplex& sumSq, Complex& dataMin,
                                          Complex& dataMax, Int& minPos,
                                          Int& maxPos, Bool& minMaxInit,
                                          Bool fixedMinMax, Complex datum,
                                          uInt& pos, Complex useIt)
//
// minPos and maxPos currently useless for Complex.  Don't
// try to access.
//
{
   const Float& rd = real(datum);
   const Float& id = imag(datum);
//
   if (real(useIt) > 0.5) {
      nPts.real() += 1;
      sum.real() += rd;
      sumSq.real() += rd*rd;
   }
   if (imag(useIt) > 0.5) {
      nPts.imag() += 1;
      sum.imag() += id;
      sumSq.imag() += id*id;
   }

//
// If fixedMinMax, then the min and max will always
// be given by the inclusion range that the user specified.
// This will be set outside of here.  We have no
// more work to do if so.

   if (fixedMinMax) return;
                                
// Set min and max

   if (minMaxInit) {
      dataMin = datum;
      dataMax = datum;
      minMaxInit = False;
   } else {  
      if (real(useIt) > 0.5) {
         if (rd < real(dataMin)) {
            dataMin.real() = rd;
         }
         if (rd > real(dataMax)) {
            dataMax.real() = rd;
         }
      }
      if (imag(useIt) > 0.5) {
         if (id < imag(dataMin)) {
            dataMin.imag() = id;
         }
         if (id > imag(dataMax)) {
            dataMax.imag() = id;
         }
      }
   }
}


Float LattStatsSpecialize::getMean (Float sum, Float n)
{
   Float tmp = 0.0;
   if (n > 0.5) tmp = sum / n;
   return tmp;
}

Float LattStatsSpecialize::getVariance (Float sum, Float sumsq, Float n)
{
   Double tmp = 0.0;
   if (n > 1.5) tmp = (sumsq - (sum*sum/n)) / (n-1);
   return tmp;
}

Float LattStatsSpecialize::getSigma (Float sum, Float sumsq, Float n)
{
   Double var = getVariance (sum, sumsq, n);
   if (var>0) {
      return sqrt(var);
   } else {
      return 0.0;
   }
   return 0.0;
}

Float LattStatsSpecialize::getSigma (Float var)
{
   if (var>0) {
      return sqrt(var);
   } else {
      return 0.0;
   }
   return 0.0;
}

Float LattStatsSpecialize::getRms (Float sumsq, Float n)
{
   Float tmp = 0.0;
   if (n > 0.5) tmp = sqrt(sumsq/n);
   return tmp;
}
 

Complex LattStatsSpecialize::getMean (Complex sum, Complex n)
{
   Complex tmp(0.0,0.0);
   if (real(n) > 0.5) tmp.real() = real(sum)/real(n);
   if (imag(n) > 0.5) tmp.imag() = imag(sum)/imag(n);
   return tmp;
}

Complex LattStatsSpecialize::getVariance (Complex sum, Complex sumsq, Complex n)
{
   return Complex(getVariance(real(sum), real(sumsq), real(n)),
                  getVariance(imag(sum), imag(sumsq), imag(n)));
}

Complex LattStatsSpecialize::getSigma (Complex sum, Complex sumsq, Complex n)
{
   return Complex(getVariance(real(sum), real(sumsq), real(n)),
                  getVariance(imag(sum), imag(sumsq), imag(n)));
}

Complex LattStatsSpecialize::getSigma (Complex var)
{
   return Complex(getSigma(real(var)), getSigma(imag(var)));
}

Complex LattStatsSpecialize::getRms (Complex sumsq, Complex n)
{
   return Complex(getRms(real(sumsq),real(n)), getRms(imag(sumsq), imag(n)));
}
 

Float LattStatsSpecialize::min(Float v1, Float v2)
{
   return ::min(v1, v2);
}

Complex LattStatsSpecialize::min(Complex v1, Complex v2)
{
   return Complex(::min(real(v1),real(v2)),::min(imag(v1),imag(v2)));
}

Float LattStatsSpecialize::max(Float v1, Float v2)
{
   return ::max(v1, v2);
}

Complex LattStatsSpecialize::max(Complex v1, Complex v2)
{
   return Complex(::max(real(v1),real(v2)),::max(imag(v1),imag(v2)));
}

void LattStatsSpecialize::putNodeInStorageLattice(Lattice<Float>& lat, 
                                                  const LatticeExprNode& node, 
                                                  const IPosition& where)
{
   lat.putAt(node.getFloat(), where);      
}

void LattStatsSpecialize::putNodeInStorageLattice(Lattice<Complex>& lat, 
                                                  const LatticeExprNode& node, 
                                                  const IPosition& where)
{
   lat.putAt(node.getComplex(), where);      
}



Float LattStatsSpecialize::usePixelInc (Float dMin, Float dMax, Float datum)
{
   return ( (datum >= dMin && datum <= dMax) ? 1.0 : -1.0 );
}

Complex LattStatsSpecialize::usePixelInc (Complex dMin, Complex dMax, Complex datum)
{
   return Complex(usePixelInc(real(dMin), real(dMax), real(datum)),
                  usePixelInc(imag(dMin), imag(dMax), imag(datum)));
}

Float LattStatsSpecialize::usePixelExc (Float dMin, Float dMax, Float datum)
{
   return ( (datum < dMin || datum > dMax) ? 1.0 : -1.0 );
}


Complex LattStatsSpecialize::usePixelExc (Complex dMin, Complex dMax, Complex datum)
{
   return Complex(usePixelExc(real(dMin), real(dMax), real(datum)),
                  usePixelExc(imag(dMin), imag(dMax), imag(datum)));
}

void LattStatsSpecialize::setUseItTrue (Float& useIt)
{
   useIt = 1.0;
}

void LattStatsSpecialize::setUseItTrue (Complex& useIt)
{
   useIt.real() = 1.0;
   useIt.imag() = 1.0;
}


Bool LattStatsSpecialize::hasSomePoints (Float npts)
{
   return (npts > 0.5);
}

Bool LattStatsSpecialize::hasSomePoints (Complex npts)
{
   return (real(npts) > 0.5 || imag(npts)>0.5);
}


uInt LattStatsSpecialize::maxPts (const Vector<Float>& npts)
{
   uInt nMax = 0;
   for (uInt j=0; j<npts.nelements(); j++) {
      nMax = ::max(nMax, uInt(npts(j)+0.1));
   }
   return nMax;
}

uInt LattStatsSpecialize::maxPts (const Vector<Complex>& npts)
{
   uInt nMax = 0;
   for (uInt j=0; j<npts.nelements(); j++) {
      nMax = ::max(nMax, uInt(real(npts(j))+0.1));
      nMax = ::max(nMax, uInt(imag(npts(j))+0.1));
   }
   return nMax;
}


Bool LattStatsSpecialize::setIncludeExclude (String& errorMessage, 
                                             Vector<Float>& range,
                                             Bool& noInclude,
                                             Bool& noExclude,
                                             const Vector<Float>& include,
                                             const Vector<Float>& exclude)

//  
// Take the user's data inclusion and exclusion data ranges and
// generate the range and Booleans to say what sort it is
//
// Inputs:
//   include   Include range given by user. Zero length indicates
//             no include range   
//   exclude   Exclude range given by user. As above.
// Outputs:
//   noInclude If True user did not give an include range
//   noExclude If True user did not give an exclude range
//   range     A pixel value selection range.  Will be resized to
//             zero length if both noInclude and noExclude are True
//   Bool      True if successfull, will fail if user tries to give too
//             many values for includeB or excludeB, or tries to give
//             values for both
{      
   noInclude = True;
   range.resize(0);
   if (include.nelements() == 0) {
     ;
   } else if (include.nelements() == 1) {
      range.resize(2);
      range(0) = -abs(include(0));
      range(1) =  abs(include(0));
      noInclude = False;
   } else if (include.nelements() == 2) {
      range.resize(2);
      range(0) = min(include(0),include(1));
      range(1) = max(include(0),include(1));
      noInclude = False;
   } else {
      errorMessage = String("Too many elements for argument include");
      return False;
   }
//   
   noExclude = True;
   if (exclude.nelements() == 0) {
      ;
   } else if (exclude.nelements() == 1) {
      range.resize(2);
      range(0) = -abs(exclude(0));
      range(1) =  abs(exclude(0));
      noExclude = False;
   } else if (exclude.nelements() == 2) {
      range.resize(2);
      range(0) = min(exclude(0),exclude(1));
      range(1) = max(exclude(0),exclude(1));
      noExclude = False; 
   } else {
      errorMessage = String("Too many elements for argument exclude");
      return False;
   }
   if (!noInclude && !noExclude) {
      errorMessage = String("You can only give one of arguments include or exclude");
      return False;
   }  
   return True;   
}
 


Bool LattStatsSpecialize::setIncludeExclude (String& errorMessage, 
                                             Vector<Complex>& range,
                                             Bool& noInclude,
                                             Bool& noExclude,
                                             const Vector<Complex>& include,
                                             const Vector<Complex>& exclude)

{      
   Vector<Float> rangeReal;
   Bool okReal = LattStatsSpecialize::setIncludeExclude (errorMessage, rangeReal, noInclude, noExclude,
                                                         real(include), real(exclude));
   if (!okReal) return False;
//
   Vector<Float> rangeImag;
   Bool okImag = LattStatsSpecialize::setIncludeExclude (errorMessage, rangeImag, noInclude, noExclude,
                                                         imag(include), imag(exclude));
   if (!okImag) return False;
//
   if (rangeReal.nelements() != rangeImag.nelements()) {
      throw (AipsError("Internal error in LattStatsSpecialize"));
   }
// 
   range.resize(rangeReal.nelements());
   for (uInt i=0; i<range.nelements(); i++) {
      range(i).real() = rangeReal(i);
      range(i).imag() = rangeImag(i);
   }
//
   return True;
}
 
