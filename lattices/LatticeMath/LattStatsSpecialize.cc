//# LattStatsSpecialize.cc:  
//# Copyright (C) 1995,1996,1997,1998,1999,2000,2001,2002,2003
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
//

#include <casacore/lattices/LatticeMath/LattStatsSpecialize.h>
#include <casacore/lattices/LatticeMath/LatticeStatsBase.h>

#include <casacore/casa/Arrays/IPosition.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Arrays/Array.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/lattices/Lattices/Lattice.h>
#include <casacore/lattices/Lattices/LatticeIterator.h>
#include <casacore/lattices/LEL/LatticeExprNode.h>
#include <casacore/lattices/LEL/LatticeExpr.h>
#include <casacore/lattices/Lattices/MaskedLattice.h>
#include <casacore/casa/BasicSL/Complex.h>
#include <casacore/casa/BasicMath/Math.h>
#include <casacore/casa/BasicSL/String.h>

#include <casacore/casa/OS/Timer.h>




namespace casacore { //# NAMESPACE CASACORE - BEGIN

double LattStatsSpecialize::getMean (double sum, double n)
{
   double tmp = 0.0;
   if (n > 0.5) tmp = sum / n;
   return tmp;
}

DComplex LattStatsSpecialize::getMean (DComplex sum, DComplex n)
{
   double vR = 0.0; 
   double vI = 0.0;
//
   if (real(n) > 0.5) vR = real(sum)/real(n);
   if (imag(n) > 0.5) vI  = imag(sum)/imag(n);
//
   return DComplex(vR, vI);
}


double LattStatsSpecialize::getVariance (double sum, double sumsq, double n)
{
   double tmp = 0.0;
   if (n > 1.5) tmp = (sumsq - (sum*sum/n)) / (n-1);
   return tmp;
}


DComplex LattStatsSpecialize::getVariance (DComplex sum, DComplex sumsq, DComplex n)
{
   return DComplex(getVariance(real(sum), real(sumsq), real(n)),
                   getVariance(imag(sum), imag(sumsq), imag(n)));
}



double LattStatsSpecialize::getSigma (double sum, double sumsq, double n)
{
   double var = getVariance (sum, sumsq, n);
   if (var>0) {
      return sqrt(var);
   } else {
      return 0.0;
   }
   return 0.0;
}

double LattStatsSpecialize::getSigma (double var)
{
   if (var>0) {
      return sqrt(var);
   } else {
      return 0.0;
   }
   return 0.0;
}

double LattStatsSpecialize::getRms (double sumsq, double n)
{
   float tmp = 0.0;
   if (n > 0.5) tmp = sqrt(sumsq/n);
   return tmp;
}
 


DComplex LattStatsSpecialize::getSigma (DComplex sum, DComplex sumsq, DComplex n)
{
   return DComplex(getVariance(real(sum), real(sumsq), real(n)),
                   getVariance(imag(sum), imag(sumsq), imag(n)));
}

DComplex LattStatsSpecialize::getSigma (DComplex var)
{
   return DComplex(getSigma(real(var)), getSigma(imag(var)));
}

DComplex LattStatsSpecialize::getRms (DComplex sumsq, DComplex n)
{
   return DComplex(getRms(real(sumsq),real(n)), getRms(imag(sumsq), imag(n)));
}

float LattStatsSpecialize::min(float v1, float v2)
{
   return std::min(v1, v2);
}

Complex LattStatsSpecialize::min(Complex v1, Complex v2)
{
   return Complex(std::min(real(v1),real(v2)),std::min(imag(v1),imag(v2)));
}

float LattStatsSpecialize::max(float v1, float v2)
{
   return std::max(v1, v2);
}

Complex LattStatsSpecialize::max(Complex v1, Complex v2)
{
   return Complex(std::max(real(v1),real(v2)),std::max(imag(v1),imag(v2)));
}

float LattStatsSpecialize::getNodeScalarValue(const LatticeExprNode& node, float)
{
   return node.getFloat();
}

Complex LattStatsSpecialize::getNodeScalarValue(const LatticeExprNode& node, Complex)
{
   return node.getComplex();
}


float LattStatsSpecialize::usePixelInc (float dMin, float dMax, float datum)
{
   return ( (datum >= dMin && datum <= dMax) ? 1.0 : -1.0 );
}

Complex LattStatsSpecialize::usePixelInc (Complex dMin, Complex dMax, Complex datum)
{
   return Complex(usePixelInc(real(dMin), real(dMax), real(datum)),
                  usePixelInc(imag(dMin), imag(dMax), imag(datum)));
}

float LattStatsSpecialize::usePixelExc (float dMin, float dMax, float datum)
{
   return ( (datum < dMin || datum > dMax) ? 1.0 : -1.0 );
}


Complex LattStatsSpecialize::usePixelExc (Complex dMin, Complex dMax, Complex datum)
{
   return Complex(usePixelExc(real(dMin), real(dMax), real(datum)),
                  usePixelExc(imag(dMin), imag(dMax), imag(datum)));
}

void LattStatsSpecialize::setUseItTrue (float& useIt)
{
   useIt = 1.0;
}

void LattStatsSpecialize::setUseItTrue (Complex& useIt)
{
  ///   useIt.real() = 1.0;
  ///   useIt.imag() = 1.0;
  useIt = Complex(1.0, 1.0);
}


bool LattStatsSpecialize::hasSomePoints (double npts)
{
   return (npts > 0.5);
}

bool LattStatsSpecialize::hasSomePoints (DComplex npts)
{
   return (real(npts) > 0.5 || imag(npts)>0.5);
}

bool LattStatsSpecialize::setIncludeExclude (String& errorMessage, 
                                             Vector<Complex>& range,
                                             bool& noInclude,
                                             bool& noExclude,
                                             const Vector<Complex>& include,
                                             const Vector<Complex>& exclude)

{      
   Vector<float> rangeReal;
   bool okReal = LattStatsSpecialize::setIncludeExclude<float> (errorMessage, rangeReal, noInclude, noExclude,
                                                         real(include), real(exclude));
   if (!okReal) return false;
//
   Vector<float> rangeImag;
   bool okImag = LattStatsSpecialize::setIncludeExclude<float> (errorMessage, rangeImag, noInclude, noExclude,
                                                         imag(include), imag(exclude));
   if (!okImag) return false;
//
   if (rangeReal.nelements() != rangeImag.nelements()) {
      throw (AipsError("Internal error in LattStatsSpecialize"));
   }
// 
   range.resize(rangeReal.nelements());
   for (uint32_t i=0; i<range.nelements(); i++) {
     range(i) = Complex(rangeReal(i), rangeImag(i));
   }
//
   return true;
}
 

bool LattStatsSpecialize::minMax(float& dataMin, float& dataMax, const MaskedLattice<float>* pLattice,
                                 const Vector<float>& range, bool noInclude, bool noExclude)

{  
  RO_LatticeIterator<float> it(*pLattice);
//  
  dataMin = 1.e30;
  dataMax = -1.0e30;
//
  const float* pData = 0;
  bool deleteData;
//
  if (pLattice->isMasked()) {
     const bool* pMask = 0;
     bool deleteMask;
//
     for (it.reset(); !it.atEnd(); it++) {  
        const Array<float>& data = it.cursor();
        const Array<bool>& mask = pLattice->getMaskSlice(it.position(), it.cursor().shape(), false);
        pData = data.getStorage(deleteData);
        pMask = mask.getStorage(deleteMask);
        uint32_t n = data.nelements();
        if (!noInclude) {
           for (uint32_t i=0; i<n; i++) {
              if (pMask[i] &&
                  LattStatsSpecialize::usePixelInc (range[0], range[1], pData[i]) > 0) {
                 dataMin = (dataMin < (pData[i])) ? dataMin : (pData[i]);
                 dataMax = (dataMax > (pData[i])) ? dataMax : (pData[i]);
              }
           }
        } else if (!noExclude) {
           for (uint32_t i=0; i<n; i++) {
              if (pMask[i] &&
                  LattStatsSpecialize::usePixelExc (range[0], range[1], pData[i]) > 0) {
                 dataMin = (dataMin < (pData[i])) ? dataMin : (pData[i]);
                 dataMax = (dataMax > (pData[i])) ? dataMax : (pData[i]);
              }
           }
        } else {
           for (uint32_t i=0; i<n; i++) {
              if (pMask[i]) {
                 dataMin = (dataMin < (pData[i])) ? dataMin : (pData[i]);
                 dataMax = (dataMax > (pData[i])) ? dataMax : (pData[i]);
              }
           }
        }
//
        data.freeStorage(pData, deleteData);
        mask.freeStorage(pMask, deleteMask);
     }
  } else {
     for (it.reset(); !it.atEnd(); it++) {  
        const Array<float>& data = it.cursor();
        pData = data.getStorage(deleteData);
        uint32_t n = data.nelements();
        if (!noInclude) {
           for (uint32_t i=0; i<n; i++) {
              if (LattStatsSpecialize::usePixelInc (range[0], range[1], pData[i]) > 0) {
                 dataMin = (dataMin < (pData[i])) ? dataMin : (pData[i]);
                 dataMax = (dataMax > (pData[i])) ? dataMax : (pData[i]);
              }
           }
        } else if (!noExclude) {
           for (uint32_t i=0; i<n; i++) {
              if (LattStatsSpecialize::usePixelExc (range[0], range[1], pData[i]) > 0) {
                 dataMin = (dataMin < (pData[i])) ? dataMin : (pData[i]);
                 dataMax = (dataMax > (pData[i])) ? dataMax : (pData[i]);
              }
           }
        } else {
           for (uint32_t i=0; i<n; i++) {
              dataMin = (dataMin < (pData[i])) ? dataMin : (pData[i]);
              dataMax = (dataMax > (pData[i])) ? dataMax : (pData[i]);
           }
        }
        data.freeStorage(pData, deleteData);
     }

  }
//
  return (dataMax > dataMin);
}

bool LattStatsSpecialize::minMax(Complex& dataMin, Complex& dataMax, const MaskedLattice<Complex>* pLattice,
                                 const Vector<Complex>& range, bool noInclude, bool noExclude)
{
   LatticeExprNode nodeR(real(*pLattice));
   LatticeExprNode nodeI(imag(*pLattice));
   LatticeExpr<float> latR(nodeR);   
   LatticeExpr<float> latI(nodeR);   
//
   Vector<float> realRange, imagRange;
   if (!noInclude && !noExclude) {
      realRange.resize(2);
      imagRange.resize(2);
//
      realRange[0] = real(range[0]);
      realRange[1] = real(range[1]);
      imagRange[0] = imag(range[0]);
      imagRange[1] = imag(range[1]);
   }   
//
   float realMin, realMax, imagMin, imagMax;
   bool ok = LattStatsSpecialize::minMax(realMin, realMax, &latR, realRange, noInclude, noExclude);
   if (ok) {
      ok = LattStatsSpecialize::minMax(imagMin, imagMax, &latI, imagRange, noInclude, noExclude);
   }
//
   if (ok) {
      dataMin = Complex(realMin, imagMin);
      dataMax = Complex(realMax, imagMax);
   }
   return ok;
}


} //# NAMESPACE CASACORE - END

