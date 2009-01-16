//# Cleaner.h: this defines Cleaner a class for doing convolution
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
//#
//#
//# $Id$

#ifndef LATTICES_LATTICECLEANER_H
#define LATTICES_LATTICECLEANER_H

//# Includes
#include <casa/aips.h>
#include <casa/Quanta/Quantum.h>
#include <lattices/Lattices/TempLattice.h>
#include <casa/Arrays/IPosition.h>
#include <casa/Arrays/Vector.h>
#include <casa/Containers/Block.h>

namespace casa { //# NAMESPACE CASA - BEGIN

//# Forward Declarations
class LatticeCleanProgress;
template <class T> class TempLattice;

// <summary>Lists the different types of Convolutions that can be done</summary>
// <synopsis>This enumerator is brought out as a separate class because g++
// currently cannot handle enumerators in a templated class. When it can this
// class will go away and this enumerator moved into the Cleaner
// class</synopsis>
class CleanEnums {
public:
  enum CleanType {
    // Hogbom
    HOGBOM,
    // Multi-scale
    MULTISCALE,
    // Clark
    CLARK
  };
};

// <summary>A class for doing multi-dimensional cleaning</summary>

// <use visibility=export>

// <reviewed reviewer="" date="yyyy/mm/dd" tests="tLatticeCleaner">
// </reviewed>

// <prerequisite>
//  <li> The mathematical concept of deconvolution
// </prerequisite>
//
// <etymology>

// The LatticeCleaner class will deconvolve Lattices.

// </etymology>
//
// <synopsis>
// This class will perform various types of Clean deconvolution
// on Lattices.
//
// </synopsis>
//
// <example>
// <srcblock>
// </srcblock> 
// </example>
//
// <motivation>
// </motivation>
//
// <thrown>
// <li> AipsError: if psf has more dimensions than the model. 
// </thrown>
//
// <todo asof="yyyy/mm/dd">
//   <li> Allow the psf to be specified with a
//     <linkto class=Function>Function</linkto>. 
// </todo>

template<class T> class LatticeCleaner
{
public:

  // Create a cleaner : default constructor
  LatticeCleaner();

  // Create a cleaner for a specific dirty image and PSF
  LatticeCleaner(const Lattice<T> & psf, const Lattice<T> & dirty);

  // The copy constructor uses reference semantics
  LatticeCleaner(const LatticeCleaner<T> & other);

  // The assignment operator also uses reference semantics
  LatticeCleaner<T> & operator=(const LatticeCleaner<T> & other); 

  // The destructor does nothing special.
  ~LatticeCleaner();

  // Update the dirty image only
  void update(const Lattice<T> & dirty);

  // Set a number of scale sizes. The units of the scale are pixels.
  Bool setscales(const Int nscales, const Float scaleInc=1.0);

  // Set a specific set of scales
  Bool setscales(const Vector<Float> & scales);

  // Set up control parameters
  Bool setcontrol(CleanEnums::CleanType cleanType, const Int niter,
		  const Float gain, const Quantity& aThreshold,
		  const Quantity& fThreshold,
		  const Bool choose=True);

  Bool setcontrol(CleanEnums::CleanType cleanType, const Int niter,
		  const Float gain, const Quantity& threshold,
		  const Bool choose=True);

  // return how many iterations we did do
  Int iteration() { return itsIteration; }
  Int numberIterations() { return itsIteration; }

  // what iteration number to start on
  void startingIteration(const Int starting = 0) {itsStartingIter = starting; }

  // Clean an image. 
  Bool clean(Lattice<T> & model, LatticeCleanProgress* progress=0);

  // Set the mask
  void setMask(Lattice<T> & mask);

  // Tell the algorithm to NOT clean just the inner quarter
  // (This is useful when multiscale clean is being used
  // inside a major cycle for MF or WF algorithms)
  // if True, the full image deconvolution will be attempted
  void ignoreCenterBox(Bool huh) { itsIgnoreCenterBox = huh; }

  // Consider the case of a point source: 
  // the flux on all scales is the same, and the first scale will be chosen.
  // Now, consider the case of a point source with a *little* bit of extended structure:
  // thats right, the largest scale will be chosen.  In this case, we should provide some
  // bias towards the small scales, or against the large scales.  We do this in
  // an ad hoc manner, multiplying the maxima found at each scale by
  // 1.0 - itsSmallScaleBias * itsScaleSizes(scale)/itsScaleSizes(nScalesToClean-1);
  // Typical bias values range from 0.2 to 1.0.
  void setSmallScaleBias(const Float x=0.5) { itsSmallScaleBias = x; }

  // During early iterations of a cycled MS Clean in mosaicing, it common
  // to come across an ocsilatory pattern going between positive and
  // negative in the large scale.  If this is set, we stop at the first
  // negative in the largest scale.
  void stopAtLargeScaleNegative() {itsStopAtLargeScaleNegative = True; }

  // Some algorithms require that the cycles be terminated when the image
  // is dominated by point sources; if we get nStopPointMode of the
  // smallest scale components in a row, we terminate the cycles
  void stopPointMode(Int nStopPointMode) {itsStopPointMode = nStopPointMode; }

  // After completion of cycle, querry this to find out if we stopped because
  // of stopPointMode
  Bool queryStopPointMode() {return itsDidStopPointMode; }

  // speedup() will speed the clean iteration by raising the
  // threshold.  This may be required if the threshold is
  // accidentally set too low (ie, lower than can be achieved
  // given errors in the approximate PSF).
  //
  // threshold(iteration) = threshold(0) 
  //                        * ( exp( (iteration - startingiteration)/Ndouble )/ 2.718 )
  // If speedup() is NOT invoked, no effect on threshold
  void speedup(const Float Ndouble);

  // Look at what WE think the residuals look like
  // Assumes the first scale is zero-sized
  Lattice<T>*  residual() { return itsDirtyConvScales[0]; }

  // Method to return threshold, including any speedup factors
  Float threshold();

  // Helper function to optimize adding
  void addTo(Lattice<T>& to, const Lattice<T>& add);

protected:
  //# The following functions are used in various places in the code and are
  //# documented in the .cc file. Static functions are used when the functions
  //# do not modify the object state. They ensure that implicit assumptions
  //# about the current state and implicit side-effects are not possible
  //# because all information must be supplied in the input arguments

  CleanEnums::CleanType itsCleanType;

  TempLattice<T>* itsDirty;
  TempLattice<Complex>* itsXfr;
  TempLattice<T>* itsMask;

  Int itsNscales;
  Vector<Float> itsScaleSizes;

  PtrBlock<TempLattice<T>* > itsScales;
  PtrBlock<TempLattice<Complex>* > itsScaleXfrs;
  PtrBlock<TempLattice<T>* > itsPsfConvScales;
  PtrBlock<TempLattice<T>* > itsDirtyConvScales;
  PtrBlock<TempLattice<T>* > itsScaleMasks;

  Bool itsScalesValid;

  Float itsGain;
  Int itsMaxNiter;	// maximum possible number of iterations
  Int itsIteration;	// what iteration did we get to?
  Int itsStartingIter;	// what iteration did we get to?
  Quantum<Double> itsThreshold;
  Quantum<Double> itsFracThreshold;

  Float itsMaximumResidual;


  IPosition itsPositionPeakPsf;

  Vector<Float> itsTotalFluxScale;
  Float itsTotalFlux;

  // Memory to be allocated per TempLattice
  Double itsMemoryMB;

  // Let the user choose whether to stop
  Bool itsChoose;

  // Threshold speedup factors:
  Bool  itsDoSpeedup;  // if false, threshold does not change with iteration
  Float itsNDouble;

  //# Stop now?
  //#//  Bool stopnow();   Removed on 8-Apr-2004 by GvD

  // Make an lattice of the specified scale
  void makeScale(Lattice<T>& scale, const Float& scaleSize);

  // Make Spheroidal function for scale images
  Float spheroidal(Float nu);

  // Calculate index into PsfConvScales
  Int index(const Int scale, const Int otherscale);
  
  Bool destroyScales();
  Bool destroyMasks();

  Bool findMaxAbsLattice(const Lattice<T>& lattice,
			 T& maxAbs, IPosition& posMax);

  Bool findMaxAbsMaskLattice(const Lattice<T>& lattice, const Lattice<T>& mask,
			     T& maxAbs, IPosition& posMax);

  Bool validatePsf(const Lattice<T> & psf);

  // Helper function to reduce the box sizes until the have the same   
  // size keeping the centers intact  
  static void makeBoxesSameSize(IPosition& blc1, IPosition& trc1,IPosition &blc2, IPosition& trc2);


  Bool makeScaleMasks();
  Bool itsIgnoreCenterBox;
  Float itsSmallScaleBias;
  Bool itsStopAtLargeScaleNegative;
  Int itsStopPointMode;
  Bool itsDidStopPointMode;
  Bool itsJustStarting;


};

} //# NAMESPACE CASA - END

#ifndef CASACORE_NO_AUTO_TEMPLATES
#include <lattices/Lattices/LatticeCleaner.tcc>
#endif //# CASACORE_NO_AUTO_TEMPLATES
#endif
