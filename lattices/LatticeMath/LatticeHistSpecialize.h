// # LatticeHistSpecialize.h:  specialized functions for LatticeHistograms
// # Copyright (C) 1996,1997,1999,2000
// # Associated Universities, Inc. Washington DC, USA.
// #
// # This library is free software; you can redistribute it and/or modify it
// # under the terms of the GNU Library General Public License as published by
// # the Free Software Foundation; either version 2 of the License, or (at your
// # option) any later version.
// #
// # This library is distributed in the hope that it will be useful, but WITHOUT
// # ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
// # FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Library General Public
// # License for more details.
// #
// # You should have received a copy of the GNU Library General Public License
// # along with this library; if not, write to the Free Software Foundation,
// # Inc., 675 Massachusetts Ave, Cambridge, MA 02139, USA.
// #
// # Correspondence concerning AIPS++ should be addressed as follows:
// #        Internet email: casa-feedback@nrao.edu.
// #        Postal address: AIPS++ Project Office
// #                        National Radio Astronomy Observatory
// #                        520 Edgemont Road
// #                        Charlottesville, VA 22903-2475 USA

#ifndef LATTICES_LATTICEHISTSPECIALIZE_H
#define LATTICES_LATTICEHISTSPECIALIZE_H

// # Includes
#include <casacore/casa/aips.h>
#include <casacore/casa/Arrays/ArrayFwd.h>
#include <casacore/casa/BasicSL/Complex.h>
#include <casacore/scimath/Mathematics/NumericTraits.h>
#include <casacore/casa/Containers/Block.h>

namespace casacore {  // # NAMESPACE CASACORE - BEGIN

// # Forward Declarations
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

class LatticeHistSpecialize {
 public:
  // Make historgam cumulative
  template <class T>
  static void makeCumulative(Vector<T>& counts, T& yMax, unsigned int nBins,
                             typename NumericTraits<T>::BaseType scale);
  static void makeCumulative(Vector<Complex>& counts, Complex& yMax, unsigned int nBins,
                             float scale);

  // Make histogram logarithmic
  template <class T>
  static void makeLogarithmic(Vector<T>& counts, T& yMax, unsigned int nBins);
  static void makeLogarithmic(Vector<Complex>& counts, Complex& yMax, unsigned int nBins);

  // Multiply.  Real and imaginary treated as independent
  // C1*C2 = (r1*r2,i1*i2)
  static float mul(float v1, float v2);
  static Complex mul(Complex v1, Complex v2);

  // Plot histograms
  static void plot(PGPlotter& plot, bool doGauss, bool doCumu, bool doLog, float linearSum,
                   float yMax, float binWidth, const Vector<float>& values,
                   const Vector<float>& counts, const Vector<float>& stats, unsigned int whereLabel,
                   unsigned int ci, bool page);
  static void plot(PGPlotter& plot, bool doGauss, bool doCumu, bool doLog, Complex linearSum,
                   Complex yMax, Complex binWidth, const Vector<Complex>& values,
                   const Vector<Complex>& counts, const Vector<Complex>& stats,
                   unsigned int whereLabel, unsigned int ci, bool page);

  // Process data chunk creating histogram.
  template <class T>
  static void process(const T* pInData, const bool* pInMask, Block<T>* pHist, const Vector<T>& clip,
                      T binWidth, unsigned int offset, unsigned int nrval, unsigned int nBins,
                      unsigned int dataIncr, unsigned int maskIncr);
  //
  static void process(const Complex* pInData, const bool* pInMask, Block<Complex>* pHist,
                      const Vector<Complex>& clip, Complex binWidth, unsigned int offset,
                      unsigned int nrval, unsigned int nBins, unsigned int dataIncr,
                      unsigned int maskIncr);

  // Set bin width.  For complex, real and imaginary treated separately
  static float setBinWidth(float dmin, float dmax, unsigned int nBins);
  //
  static Complex setBinWidth(Complex dmin, Complex dmax, unsigned int nBins);

 private:
  static unsigned int bin(float datum, float min, float width, unsigned int nBins);
  //
  static void makeGauss(unsigned int& nGPts, float& gMax, Vector<float>& gX, Vector<float>& gY,
                        float dMean, float dSigma, float dSum, float xMin, float xMax,
                        float binWidth, bool doCumu, bool doLog);
  //
  static void plotHist(const Vector<float>& x, const Vector<float>& y, PGPlotter& plotter);
};

}  // namespace casacore

#ifndef CASACORE_NO_AUTO_TEMPLATES
#include <casacore/lattices/LatticeMath/LatticeHistSpecialize2.tcc>
#endif

#endif
