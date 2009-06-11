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
//#
//# $Id$
//

#include <lattices/Lattices/LattStatsSpecialize.h>
#include <lattices/Lattices/LatticeStatsBase.h>

#include <casa/Arrays/IPosition.h>
#include <casa/Arrays/Vector.h>
#include <casa/Arrays/Array.h>
#include <casa/Arrays/ArrayMath.h>
#include <casa/Exceptions/Error.h>
#include <lattices/Lattices/Lattice.h>
#include <lattices/Lattices/LatticeIterator.h>
#include <lattices/Lattices/LatticeExprNode.h>
#include <lattices/Lattices/LatticeExpr.h>
#include <lattices/Lattices/MaskedLattice.h>
#include <casa/BasicSL/Complex.h>
#include <casa/BasicMath/Math.h>
#include <casa/BasicSL/String.h>

#include <casa/OS/Timer.h>




namespace casa { //# NAMESPACE CASA - BEGIN

void LattStatsSpecialize::accumulate (Double& nPts, Double& sum,
                                      Double& sumSq, Float& dataMin,
                                      Float& dataMax, Int& minPos,
                                      Int& maxPos, Bool& minMaxInit,
                                      Bool fixedMinMax, Float datum,
                                      uInt& pos, Float useIt)
{
   if (useIt < 0) return;
//

   nPts += 1.0;
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

void LattStatsSpecialize::accumulate (DComplex& nPts, DComplex& sum,
                                      DComplex& sumSq, Complex& dataMin,
                                      Complex& dataMax, Int&,
                                      Int&, Bool& minMaxInit,
                                      Bool fixedMinMax, Complex datum,
                                      uInt&, Complex useIt)
//
// minPos and maxPos currently useless for Complex.  Don't
// try to access.
//
{
  ///   const Float& rd = real(datum);
  ///   const Float& id = imag(datum);
  const Float rd = real(datum);
  const Float id = imag(datum);
//
   if (real(useIt) > 0.5) {
     nPts += Complex(1.0, 0.0);
     sum += DComplex(rd, 0.0);
     sumSq += DComplex(rd*rd, 0.0);
   }
   if (imag(useIt) > 0.5) {
     nPts += Complex(0.0, 1.0);
     sum += DComplex(0.0, id);
     sumSq += DComplex(0.0, id*id);
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
	   ///            dataMin.real() = rd;
	   dataMin = Complex(rd, dataMin.imag());
         }
         if (rd > real(dataMax)) {
	   ///            dataMax.real() = rd;
	   dataMax = Complex(rd, dataMax.imag());
         }
      }
      if (imag(useIt) > 0.5) {
         if (id < imag(dataMin)) {
	   ///            dataMin.imag() = id;
	   dataMin = Complex(dataMin.real(), id);
         }
         if (id > imag(dataMax)) {
	   ///            dataMax.imag() = id;
	   dataMax = Complex(dataMax.real(), id);
         }
      }
   }
}


Double LattStatsSpecialize::getMean (Double sum, Double n)
{
   Double tmp = 0.0;
   if (n > 0.5) tmp = sum / n;
   return tmp;
}

DComplex LattStatsSpecialize::getMean (DComplex sum, DComplex n)
{
   Double vR = 0.0; 
   Double vI = 0.0;
//
   if (real(n) > 0.5) vR = real(sum)/real(n);
   if (imag(n) > 0.5) vI  = imag(sum)/imag(n);
//
   return DComplex(vR, vI);
}


Double LattStatsSpecialize::getVariance (Double sum, Double sumsq, Double n)
{
   Double tmp = 0.0;
   if (n > 1.5) tmp = (sumsq - (sum*sum/n)) / (n-1);
   return tmp;
}


DComplex LattStatsSpecialize::getVariance (DComplex sum, DComplex sumsq, DComplex n)
{
   return DComplex(getVariance(real(sum), real(sumsq), real(n)),
                   getVariance(imag(sum), imag(sumsq), imag(n)));
}



Double LattStatsSpecialize::getSigma (Double sum, Double sumsq, Double n)
{
   Double var = getVariance (sum, sumsq, n);
   if (var>0) {
      return sqrt(var);
   } else {
      return 0.0;
   }
   return 0.0;
}

Double LattStatsSpecialize::getSigma (Double var)
{
   if (var>0) {
      return sqrt(var);
   } else {
      return 0.0;
   }
   return 0.0;
}

Double LattStatsSpecialize::getRms (Double sumsq, Double n)
{
   Float tmp = 0.0;
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

Float LattStatsSpecialize::min(Float v1, Float v2)
{
   return std::min(v1, v2);
}

Complex LattStatsSpecialize::min(Complex v1, Complex v2)
{
   return Complex(std::min(real(v1),real(v2)),std::min(imag(v1),imag(v2)));
}

Float LattStatsSpecialize::max(Float v1, Float v2)
{
   return std::max(v1, v2);
}

Complex LattStatsSpecialize::max(Complex v1, Complex v2)
{
   return Complex(std::max(real(v1),real(v2)),std::max(imag(v1),imag(v2)));
}

Float LattStatsSpecialize::getNodeScalarValue(const LatticeExprNode& node, Float)
{
   return node.getFloat();
}

Complex LattStatsSpecialize::getNodeScalarValue(const LatticeExprNode& node, Complex)
{
   return node.getComplex();
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
  ///   useIt.real() = 1.0;
  ///   useIt.imag() = 1.0;
  useIt = Complex(1.0, 1.0);
}


Bool LattStatsSpecialize::hasSomePoints (Double npts)
{
   return (npts > 0.5);
}

Bool LattStatsSpecialize::hasSomePoints (DComplex npts)
{
   return (real(npts) > 0.5 || imag(npts)>0.5);
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
     range(i) = Complex(rangeReal(i), rangeImag(i));
   }
//
   return True;
}
 

Bool LattStatsSpecialize::minMax(Float& dataMin, Float& dataMax, const MaskedLattice<Float>* pLattice,
                                 const Vector<Float>& range, Bool noInclude, Bool noExclude)

{  
  RO_LatticeIterator<Float> it(*pLattice);
//  
  dataMin = 1.e30;
  dataMax = -1.0e30;
//
  const Float* pData = 0;
  Bool deleteData;
//
  if (pLattice->isMasked()) {
     const Bool* pMask = 0;
     Bool deleteMask;
//
     for (it.reset(); !it.atEnd(); it++) {  
        const Array<Float>& data = it.cursor();
        const Array<Bool>& mask = pLattice->getMaskSlice(it.position(), it.cursor().shape(), False);
        pData = data.getStorage(deleteData);
        pMask = mask.getStorage(deleteMask);
        uInt n = data.nelements();
        if (!noInclude) {
           for (uInt i=0; i<n; i++) {
              if (pMask[i] &&
                  LattStatsSpecialize::usePixelInc (range[0], range[1], pData[i]) > 0) {
                 dataMin = (dataMin < (pData[i])) ? dataMin : (pData[i]);
                 dataMax = (dataMax > (pData[i])) ? dataMax : (pData[i]);
              }
           }
        } else if (!noExclude) {
           for (uInt i=0; i<n; i++) {
              if (pMask[i] &&
                  LattStatsSpecialize::usePixelExc (range[0], range[1], pData[i]) > 0) {
                 dataMin = (dataMin < (pData[i])) ? dataMin : (pData[i]);
                 dataMax = (dataMax > (pData[i])) ? dataMax : (pData[i]);
              }
           }
        } else {
           for (uInt i=0; i<n; i++) {
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
        const Array<Float>& data = it.cursor();
        pData = data.getStorage(deleteData);
        uInt n = data.nelements();
        if (!noInclude) {
           for (uInt i=0; i<n; i++) {
              if (LattStatsSpecialize::usePixelInc (range[0], range[1], pData[i]) > 0) {
                 dataMin = (dataMin < (pData[i])) ? dataMin : (pData[i]);
                 dataMax = (dataMax > (pData[i])) ? dataMax : (pData[i]);
              }
           }
        } else if (!noExclude) {
           for (uInt i=0; i<n; i++) {
              if (LattStatsSpecialize::usePixelExc (range[0], range[1], pData[i]) > 0) {
                 dataMin = (dataMin < (pData[i])) ? dataMin : (pData[i]);
                 dataMax = (dataMax > (pData[i])) ? dataMax : (pData[i]);
              }
           }
        } else {
           for (uInt i=0; i<n; i++) {
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

Bool LattStatsSpecialize::minMax(Complex& dataMin, Complex& dataMax, const MaskedLattice<Complex>* pLattice,
                                 const Vector<Complex>& range, Bool noInclude, Bool noExclude)
{
   LatticeExprNode nodeR(real(*pLattice));
   LatticeExprNode nodeI(imag(*pLattice));
   LatticeExpr<Float> latR(nodeR);   
   LatticeExpr<Float> latI(nodeR);   
//
   Vector<Float> realRange, imagRange;
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
   Float realMin, realMax, imagMin, imagMax;
   Bool ok = LattStatsSpecialize::minMax(realMin, realMax, &latR, realRange, noInclude, noExclude);
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


} //# NAMESPACE CASA - END

