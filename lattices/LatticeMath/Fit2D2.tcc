//# Fit2D2.tcc: Class to fit 2D objects to a Lattice or Array
//# Copyright (C) 1997,1998,1999,2000,2001,2002,2003
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
//#   $Id$

#ifndef LATTICES_FIT2D2_TCC
#define LATTICES_FIT2D2_TCC

#include <casacore/lattices/LatticeMath/Fit2D.h>

#include <casacore/casa/Arrays/Array.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/lattices/Lattices/MaskedLattice.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

template <class T> Vector<Double> Fit2D::estimate(Fit2D::Types type,
                               const MaskedLattice<T>& data)
{
   if (data.shape().nelements() !=2) {
      itsLogger << "Fit2D::estimate - Lattice must be 2-dimensional" <<
	LogIO::EXCEPTION;
   }
   auto pixels = data.get(True);
   auto mask = data.getMask(True);
   return estimate(type, pixels, mask);
}

template <class T> Vector<Double> Fit2D::estimate(Fit2D::Types type,
                               const Lattice<T>& data)
{
   if (data.shape().nelements() !=2) {
      itsLogger << "Fit2D::estimate - Lattice must be 2-dimensional" <<
	LogIO::EXCEPTION;
   }
   auto pixels = data.get(True);
   Array<Bool> mask(pixels.shape(),True);
   return estimate(type, pixels, mask);
}

template <class T> Vector<Double> Fit2D::estimate(Fit2D::Types type,
                               const Array<T>& data)
{
   if (data.shape().nelements() !=2) {
      itsLogger << "Fit2D::estimate - Array must be 2-dimensional" <<
	LogIO::EXCEPTION;
   }
   Array<Bool> mask(data.shape(),True);
   return estimate(type, data, mask);
}

template <class T> Vector<Double> Fit2D::estimate(Fit2D::Types type,
                               const Array<T>& data,
                               const Array<Bool>& mask) 
// 
// Work out an initial estimate to the solution using Bob Sault's 
// probabilistic approach from Miriad imfit.for   Only works
// for single models.  Honours and inclusion/exclusion pixel range
//
// PA sign convention in pixel coordinate is +x -> +y is positive
//
{
   if (type!=Fit2D::GAUSSIAN  && type==Fit2D::DISK) {
      itsLogger << "Only Gaussian and disk models are currently supported" <<
	LogIO::EXCEPTION;
   }
//
   Vector<Double> parameters;
   auto shape = data.shape();
   if (shape.nelements() !=2) {
      itsLogger << "Fit2D::estimate - Array must be 2-dimensional" <<
	LogIO::EXCEPTION;
   }
   if (mask.shape().nelements() !=2) {
      itsLogger << "Fit2D::estimate - Mask must be 2-dimensional" << 
	LogIO::EXCEPTION;
   }
// 
// Find min and max
//
   MaskedArray<T> pixels(data, mask);
   T minVal, maxVal;
   IPosition minPos(2), maxPos(2);
   minMax(minVal, maxVal, minPos, maxPos, pixels);
//
// For the purposed of the estimate, chuck away pixels
// below abs(5%) of the peak
//
   T clip = 0.05 * max(abs(minVal), abs(maxVal));
//
// Accumulate sums.  Array indexing is not fast.
//
   Int includeThem = 0;
   if (itsPixelRange.nelements()==2) {
     if (itsInclude) {
        includeThem = 1;
     } else {
        includeThem = 2;
     }
   }
//
   Double P, XP, YP, XYP, XXP, YYP;
   Double t, fac, SP;
   P = XP = YP = XYP = XXP = YYP = 0.0;
   SP = 0.0;
//
   IPosition pos(2);
   Double ri, rj;
   uInt nPts = 0;
   for (Int j=0; j<shape(1); j++) {
     for (Int i=0; i<shape(0); i++) {
        pos(0) = i; pos(1) = j;
//
        const T& val = data(pos);
        t = abs(val);
        if (mask(pos) && includeIt(val, itsPixelRange,
				   includeThem) && t>clip) {
           ri = i; rj = j;
//
           SP += val;
           P  += t;                        
           XP += t*ri;
           YP += t*rj;
           XYP += t*ri*rj;
           XXP += t*ri*ri;
           YYP += t*rj*rj;
           nPts++;
         }
      }
   }
   if (nPts==0) {
      itsLogger << LogIO::WARN <<
	"There are not enough good points in the array for a good estimate" <<
	LogIO::POST;
      return parameters;
   }
//
   Double t2;
   if (type==Fit2D::GAUSSIAN || type==Fit2D::DISK) {
      parameters.resize(6);
//
      fac = 4*log(2.0);
      XP  = XP / P;
      YP  = YP / P;
      XYP = XYP / P - XP*YP;
      XXP = XXP / P - XP*XP;
      YYP = YYP / P - YP*YP;
//
      parameters(1) = XP;
      parameters(2) = YP;
//    
      parameters(3)  = sqrt(fac*(XXP + YYP +
                        sqrt( square(XXP-YYP) + 4*square(XYP) )));
      parameters(4) = sqrt(fac*(XXP + YYP -
                       sqrt( square(XXP-YYP) + 4*square(XYP) )));

      t2 = 0.5*atan2(2*XYP,YYP-XXP);
      parameters(5) = paFromGauss2D(-t2);
      piRange(parameters(5));
//
      Double sn = 1.0;
      if (SP<0) sn = -1.0;
      parameters(0) = sn * fac * P / ( C::pi * parameters(3) * parameters(4));
   } else if (type==Fit2D::LEVEL) {
      itsLogger << "Level models are not currently supported" <<
	LogIO::EXCEPTION;
   }
// 
   parameters(3) *= 0.95;   // In case estimate is circular
//
   return parameters;
}

template <class T> Fit2D::ErrorTypes Fit2D::fit(
    const MaskedLattice<T>& data, const Lattice<T>& sigma
) {
   if (!itsValid) {
      itsErrorMessage = "No models have been set - use function addModel";
      return Fit2D::NOMODELS;
   }
// Get data
   auto pixels = data.get(True);
   auto shape = pixels.shape();
   if (shape.nelements() !=2) {
      itsLogger << "Fit2D::fit - Region must be 2-dimensional" <<
    LogIO::EXCEPTION;
   }
   auto mask = data.getMask(True);
//
// Do fit
//
   if (sigma.ndim()==0) {
      Array<T> sigma2;
      return fit(pixels, mask, sigma2);
   } else {
      auto sigma2 = sigma.get(True);
      return fit(pixels, mask, sigma2);
   }
}

template <class T> Fit2D::ErrorTypes Fit2D::fit(const Lattice<T>& data,
                             const Lattice<T>& sigma)
{
   if (!itsValid) {
      itsErrorMessage = "No models have been set - use function addModel";
      return Fit2D::NOMODELS;
   }
   auto pixels = data.get(True);
   IPosition shape = pixels.shape();
   if (shape.nelements() !=2) {
      itsLogger << "Fit2D::fit - Region must be 2-dimensional" <<
    LogIO::EXCEPTION;
   }
   Array<Bool> mask;
   if (sigma.ndim()==0) {
      Array<T> sigma2;
      return fit(pixels, mask, sigma2);
   } else {
      auto sigma2 = sigma.get(True);
      return fit(pixels, mask, sigma2);
   }
}

template <class T> Fit2D::ErrorTypes Fit2D::fit(const Array<T>& data,
                             const Array<T>& sigma)
{
   if (!itsValid) {
      itsErrorMessage = "No models have been set - use function addModel";
      return Fit2D::NOMODELS;
   }
   if (data.ndim() !=2) {
      itsLogger << "Fit2D::fit - Array must be 2-dimensional" <<
    LogIO::EXCEPTION;
   }
   if (sigma.nelements() !=0) {
      if (!data.shape().isEqual(sigma.shape())) {
         itsLogger << "Fit2D::fit - Sigma and pixel arrays must "
       "have the same shape" << LogIO::EXCEPTION;
      }
   }
//
   Matrix<Double> pos;
   Vector<Double> values;
   Vector<Double> weights;
   Array<Bool> mask;
   if (!selectData (pos, values, weights, data, mask, sigma)) {
      itsErrorMessage = String("There were no selected data points");
      return Fit2D::NOGOOD;
   }
//
   return fitData(values, pos, weights);
}


template <class T> Fit2D::ErrorTypes Fit2D::fit(const Array<T>& data,
                             const Array<Bool>& mask,
                             const Array<T>& sigma)
{
   if (!itsValid) {
      itsErrorMessage = "No models have been set - use function addModel";
      return Fit2D::NOMODELS;
   }
   if (data.ndim() !=2) {
      itsLogger << "Fit2D::fit - Array must be 2-dimensional" <<
    LogIO::EXCEPTION;
   }
   if (mask.nelements() !=0) {
      if (!data.shape().isEqual(mask.shape())) {
         itsLogger << "Fit2D::fit - Mask and pixel arrays must "
       "have the same shape" << LogIO::EXCEPTION;
      }
   }
   if (sigma.nelements() !=0) {
      if (!data.shape().isEqual(sigma.shape())) {
         itsLogger << "Fit2D::fit - Sigma and pixel arrays must "
       "have the same shape" << LogIO::EXCEPTION;
      }
   }

   Matrix<Double> pos;
   Vector<Double> values;
   Vector<Double> weights;
   if (!selectData (pos, values, weights, data, mask, sigma)) {
      itsErrorMessage = String("There were no selected data points");
      return Fit2D::NOGOOD;
   }

   return fitData(values, pos, weights);

}

template <class T> Fit2D::ErrorTypes Fit2D::residual(
        Array<T>& resid, Array<T>& model,
        const Array<T>& data, Int xOffset, int yOffset
) const {
   ThrowIf(
      ! itsValid,
      "No models have been set - use function addModel"
   );
   if (!itsValidSolution) {
      return Fit2D::FAILED;
   }

   ThrowIf(data.ndim() !=2, "Array must be 2-dimensional");
   IPosition shape = data.shape();

   if (resid.nelements() ==0) {
       resid.resize(shape);
   } else {
       ThrowIf(
          ! shape.isEqual(resid.shape()),
          "Residual and pixel arrays must be the same shape"
       );
   }
   if (model.nelements() ==0) {
       model.resize(shape);
   }
   else {
       ThrowIf(
        !shape.isEqual(model.shape()),
          "Residual and pixel arrays must "
       );
    }

// Create a functional with the solution (no axis conversion
// necessary because functional interface takes axial ratio)

   std::unique_ptr<Function<AutoDiff<Double>>> sumFunction(itsFunction.clone());
   for (uInt i=0; i<itsSolution.nelements(); i++) {
       (*sumFunction)[i] = itsSolution[i];
   }
   IPosition loc(2);
   for (Int j=0; j<shape(1); j++) {
     loc(1) = j;
      for (Int i=0; i<shape(0); i++) {
         loc(0) = i;
         model(loc) = (*sumFunction)(Double(i + xOffset), Double(j + yOffset)).value();
         resid(loc) = data(loc) - model(loc);
      }
   }
   return Fit2D::OK;
}


template <class T> Bool Fit2D::selectData(
    Matrix<Double>& pos, Vector<Double>& values, Vector<Double>& weights,
    const Array<T>& pixels, const Array<Bool>& mask, const Array<T>& sigma
)
//
// Fish out the unmasked data.
//
// If the mask is of zero length all pixels are assumed good.
// If the sigma array is of zero length the weights are given
// the value 1.0
//
// If there are no good pixels returns False
//
{
   auto shape = pixels.shape();
   auto nPoints = shape.product();
//
// Handle pixel ranges
//
   Vector<Double> pixelRange(2);
   Int includeThem = 0;
   if (itsPixelRange.nelements()==2) {
      pixelRange(0) = itsPixelRange(0);
      pixelRange(1) = itsPixelRange(1);
      if (itsInclude) {
         includeThem = 1;
      } else {
         includeThem = -1;
      }
   }
//
// Do we have sigmas ?
//
   itsHasSigma = False;
   if (sigma.nelements() != 0) itsHasSigma = True;
//
// Find first unmasked point
//
   auto hasMask = True;
   if (mask.nelements()==0) hasMask = False;
   Double minVal(0);
   Double maxVal(0);
   if (hasMask) {
      Bool deleteIt1, deleteIt2;
      const auto* p1 = mask.getStorage(deleteIt1);
      const auto* p2 = pixels.getStorage(deleteIt2);
      for (uInt i=0; i<nPoints; i++) {
         if (p1[i]) {
            minVal = p2[i];
            maxVal = p2[i];
            break;
         }
      }
      mask.freeStorage(p1, deleteIt1);
      pixels.freeStorage(p2, deleteIt2);
   } else {
      minVal = pixels(IPosition(2,0,0));
      maxVal = pixels(IPosition(2,0,0));
   }
//
// Find min/max and select data
//
   values.resize(nPoints);
   weights.resize(nPoints);
   Vector<Int> locX(nPoints);
   Vector<Int> locY(nPoints);
   IPosition loc(2);
//
   itsNumberPoints = 0;
   for (Int j=0; j<shape(1); ++j) {
      for (Int i=0; i<shape(0); ++i) {
         loc(0) = i;
         loc(1) = j;
         if (!hasMask || (hasMask && mask(loc))) {
            if (includeIt(pixels(loc), pixelRange, includeThem)) {
               values(itsNumberPoints) = pixels(loc);
               if (itsHasSigma) {
                  weights(itsNumberPoints) = sigma(loc);
               } else {
                  weights(itsNumberPoints) = 1.0;
               }
               locX(itsNumberPoints) = i;
               locY(itsNumberPoints) = j;
               minVal = min(minVal, values(itsNumberPoints));
               maxVal = max(maxVal, values(itsNumberPoints));
//
               itsNumberPoints++;
            }
         }
      }
   }
   if (itsNumberPoints==0) return False;
//
// Resize arrays for actual number of selected points
//
   pos.resize(itsNumberPoints,2);
   values.resize(itsNumberPoints, True);
   weights.resize(itsNumberPoints, True);
   locX.resize(itsNumberPoints, True);
   locY.resize(itsNumberPoints, True);
//
// Just fill in the position matrix
//
   for (uInt k=0; k<itsNumberPoints; k++) {
      pos(k,0) = locX(k);
      pos(k,1) = locY(k);
   }
//   cout << "Data = " << values << endl;
//   cout << "weights = " << weights << endl;
//   cout << "Pos = " << pos << endl;

   return True;
}

} //# NAMESPACE CASACORE - END

#endif
