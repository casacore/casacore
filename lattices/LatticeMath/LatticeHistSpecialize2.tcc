//# LatticeHistSpecialize.cc:  Defines non-templated classes for LatticeHistograms
//# Copyright (C) 1995,1996,1997,1998,1999,2000,2001,2002
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

#ifndef LATTICEMATH_LATTICEHISTSPECIALIZE2_TCC
#define LATTICEMATH_LATTICEHISTSPECIALIZE2_TCC

#include <casacore/lattices/LatticeMath/LatticeHistSpecialize.h>
#include <casacore/lattices/LatticeMath/LattStatsSpecialize.h>

namespace casacore {
      
template <class T> void LatticeHistSpecialize::makeCumulative (Vector<T>& counts,
                                           T& yMax, uInt nBins, 
                                           typename NumericTraits<T>::BaseType scale)
{  
   counts(0) = scale * counts(0);
   for (uInt i=1; i<nBins; i++) {
      counts(i) = counts(i)*scale + counts(i-1);
   }
   yMax = counts(nBins-1);
}  

template <class T> void LatticeHistSpecialize::makeLogarithmic (Vector<T>& counts,
                                            T& yMax,
                                            uInt nBins)
{
   yMax = 0.0;
   for (uInt i=0; i<nBins; i++) {
     if (counts(i) > 0.0) counts(i) = std::log10(counts(i));
     yMax = std::max(yMax, counts(i));
   }
}

template <class T> void LatticeHistSpecialize::process(
    const T* pInData, const Bool* pInMask,
    Block<T>* pHist, const Vector<T>& clip,
    T binWidth, uInt offset,
    uInt nrval, uInt nBins,
    uInt dataIncr, uInt maskIncr
) {
   T datum;
   uInt rBin;
   uInt index;
//
   if (pInMask==0) {
      for (uInt i=0; i<nrval; i++) {
         datum = *pInData;
         if (LattStatsSpecialize::usePixelInc(clip(0), clip(1), datum) > 0.5) {
            rBin = bin(datum, clip(0), binWidth, nBins);
            index = rBin + offset;
            auto& hist = (*pHist)[index];
            hist += 1.0;
         }
         pInData += dataIncr;
      }
   } else {
      for (uInt i=0; i<nrval; i++) {
         datum = *pInData;
         if (*pInMask &&
             (LattStatsSpecialize::usePixelInc(clip(0), clip(1), datum) > 0.5)) {
            rBin = bin(datum, clip(0), binWidth, nBins);
            index = rBin + offset;
            auto& hist = (*pHist)[index];
            hist += 1.0;
         }
         pInData += dataIncr;
         pInMask += maskIncr;
      }
   }
}

}

#endif

