//# FrequencyAligner.cc: implements FrequencyAligner which aligns spectra in frequency space
//# Copyright (C) 1998,1999,2000,2001,2003
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

#include <coordinates/Coordinates/FrequencyAligner.h>

#include <casa/Arrays/ArrayAccessor.h>
#include <casa/Arrays/VectorIter.h>
#include <casa/BasicMath/Math.h>
#include <casa/Quanta/Unit.h>
#include <casa/Utilities/Assert.h>
#include <casa/Exceptions/Error.h>
#include <casa/Logging/LogIO.h>
#include <casa/Logging/LogOrigin.h>
#include <casa/Quanta/Quantum.h>

#include <coordinates/Coordinates/CoordinateUtil.h>
#include <coordinates/Coordinates/SpectralCoordinate.h>

#include <measures/Measures/MDirection.h>
#include <measures/Measures/MPosition.h>

#include <scimath/Mathematics/InterpolateArray1D.h>

namespace casa {

template<class T>
FrequencyAligner<T>::FrequencyAligner()
 : itsRefFreqX(0),
   itsFreqX(0),
   itsDiffTol(0.0)
{}


template<class T>
FrequencyAligner<T>::FrequencyAligner(const SpectralCoordinate& specCoord,
                                    uInt nPixels,
                                    const MEpoch& refEpoch,
                                    const MDirection& dir,
                                    const MPosition& pos,
                                    MFrequency::Types freqSystem)
 : itsSpecCoord(specCoord),
   itsFreqSystem(freqSystem),
   itsRefFreqX(0),
   itsFreqX(0),
   itsDiffTol(0.0)
{

// Reset the conversion machinery so that there are no extra frame
// conversions in the SpectralCoordinate (its machinery is not optimum
// for a lot of conversions)

   itsSpecCoord.setReferenceConversion (itsSpecCoord.frequencySystem(),
                                        refEpoch, pos, dir);

// Generate the Frequency Machine and set the epoch to the reference epoch

   Unit unit(specCoord.worldAxisUnits()(0));
   makeMachine (refEpoch, dir, pos, itsFreqSystem, unit);

// Generate the frequency abcissa at the reference epoch.  The frequency 
// spectrum is of the MFrequency::Types of the SC. 

   itsRefFreqX.resize(nPixels);
   makeAbcissa (itsRefFreqX, False);
//
   itsFreqX.resize(nPixels);
   itsFreqX = 0.0;
}

template<class T>
FrequencyAligner<T>::FrequencyAligner(const FrequencyAligner<T>& other)
 : itsRefFreqX(0),
   itsFreqX(0),
   itsDiffTol(0.0)
{
  copyOther(other);
}


template<class T>
FrequencyAligner<T>& FrequencyAligner<T>::operator=(const FrequencyAligner<T>& other)
{
  if (this != &other) {
     copyOther(other);
  }
//
  return *this;
}

template<class T>
FrequencyAligner<T>::~FrequencyAligner()
{}




template<class T>
Bool FrequencyAligner<T>::align (Vector<T>& yOut, Vector<Bool>& maskOut,
                                const Vector<T>& yIn, const Vector<Bool>& maskIn,
                                const MEpoch& epoch, Bool useCachedAbcissa,
                                typename InterpolateArray1D<Double,T>::InterpolationMethod method,
                                Bool extrapolate)
{
   const uInt nPixels = itsRefFreqX.nelements();
   AlwaysAssert(nPixels>1, AipsError);
   AlwaysAssert(yIn.nelements()==nPixels,AipsError);
   AlwaysAssert(maskIn.nelements()==nPixels,AipsError);

// Update epoch in FrequencyMachine

   itsRefOut.getFrame().resetEpoch(epoch);
   itsMachine.setOut(itsRefOut);

// Generate abcissa at this epoch

   Double maxDiff = -1;
   if (useCachedAbcissa) {
      maxDiff = abs(itsFreqX[0]-itsRefFreqX[0]);
   } else {   
      maxDiff = makeAbcissa (itsFreqX, True);
   }
   maxDiff /= abs(itsRefFreqX[1]-itsRefFreqX[0]);      // Max diff as a fraction of a channel

// Regrid to reference frequency abcissa. 

   Bool ok = regrid (yOut, maskOut, itsRefFreqX, itsFreqX, yIn, maskIn,
                     method, extrapolate, maxDiff);
   return ok;
}


template<class T>
Bool FrequencyAligner<T>::align (Vector<T>& yOut, Vector<Bool>& maskOut,
                                 const Vector<Double>& xIn, const Vector<T>& yIn, const Vector<Bool>& maskIn,
                                 const MEpoch& epoch, Bool useCachedAbcissa,
                                 typename InterpolateArray1D<Double,T>::InterpolationMethod method,
                                 Bool extrapolate)
{
   const uInt nPixels = itsRefFreqX.nelements();
   AlwaysAssert(nPixels>1, AipsError);
   AlwaysAssert(xIn.nelements()==nPixels,AipsError);
   AlwaysAssert(yIn.nelements()==nPixels,AipsError);
   AlwaysAssert(maskIn.nelements()==nPixels,AipsError);

// Update epoch in FrequencyMachine

   itsRefOut.getFrame().resetEpoch(epoch);
   itsMachine.setOut(itsRefOut);

// The user provided abcissa is in the input Frame. Convert it to the output 
// Frame at the specfied Epoch

   Double maxDiff = -1;
   if (useCachedAbcissa) {
      maxDiff = abs(itsFreqX[0]-itsRefFreqX[0]);
   } else {
      for (uInt i=0; i<nPixels; i++) {
         itsFreqX[i] = itsMachine(xIn[i]).getValue().getValue();
         maxDiff = casa::max(casa::abs(itsFreqX[i]-itsRefFreqX[i]),maxDiff);
      }
   }
   maxDiff /= abs(itsRefFreqX[1]-itsRefFreqX[0]);      // Max diff as a fraction of a channel

// Regrid to reference frequency abcissa. 

   Bool ok = regrid (yOut, maskOut, itsRefFreqX, itsFreqX, yIn, maskIn,
                     method, extrapolate, maxDiff);
   return ok;
}



template<class T>
Bool FrequencyAligner<T>::alignMany (Array<T>& yOut, Array<Bool>& maskOut,
                                    const Array<T>& yIn, const Array<Bool>& maskIn,
                                    uInt axis, const MEpoch& epoch, 
                                    typename InterpolateArray1D<Double,T>::InterpolationMethod method,
                                    Bool extrapolate)
{

// Checks

   const IPosition shp = yIn.shape();
   AlwaysAssert(shp.isEqual(maskIn.shape()), AipsError);
   const Int n = itsRefFreqX.nelements();
   AlwaysAssert(n>1, AipsError);
   AlwaysAssert(axis>=0&&axis<shp.nelements(),AipsError);
   AlwaysAssert(shp(axis)==n,AipsError);
//
   yOut.resize(yIn.shape());
   maskOut.resize(maskIn.shape());

// Update epoch in FrequencyMachine

   itsRefOut.getFrame().resetEpoch(epoch);
   itsMachine.setOut(itsRefOut);

// Generate abcissa at this epoch

   Double maxDiff = makeAbcissa (itsFreqX, True);
   maxDiff /= abs(itsRefFreqX[1]-itsRefFreqX[0]);      // Max diff as a fraction of a channel

// Make iterators

   ReadOnlyVectorIterator<T> yItIn(yIn, axis);
   ReadOnlyVectorIterator<Bool> mItIn(maskIn, axis);
   VectorIterator<T> yItOut(yOut, axis);
   VectorIterator<Bool> mItOut(maskOut, axis);

// Iterate through Array and align each vector with the same grid

   Bool ok = True;
   while (!yItIn.pastEnd()) {

// Align

      Bool ok2 = regrid (yItOut.vector(), mItOut.vector(), itsRefFreqX, itsFreqX,
                         yItIn.vector(), mItIn.vector(),
                         method, extrapolate, maxDiff);
      if (!ok2) ok = False;

// Next vector

       yItIn.next();
       mItIn.next();
       yItOut.next();
       mItOut.next();
   }
//
   return ok;
}



template<class T>
void FrequencyAligner<T>::getReferenceAbcissa(Vector<Double>& xOut) const
{
   xOut.resize(itsRefFreqX.nelements());
   xOut = itsRefFreqX;
}

template<class T>
void FrequencyAligner<T>::getAbcissa(Vector<Double>& xOut) const
{
   xOut.resize(itsFreqX.nelements());
   xOut = itsFreqX;
}


// Private functions



template<class T>
Bool FrequencyAligner<T>::regrid (Vector<T>& yOut, Vector<Bool>& maskOut,
                                 const Vector<Double>& xOut, 
                                 const Vector<Double>& xIn,
                                 const Vector<T>& yIn, const Vector<Bool>& maskIn,
                                 typename InterpolateArray1D<Double,T>::InterpolationMethod method,
                                 Bool extrapolate, Double maxDiff) const
{
   Bool ok = False;
   if (maxDiff > itsDiffTol) {
      Int methodInt = static_cast<Int>(method);
      InterpolateArray1D<Double,T>::interpolate (yOut, maskOut, xOut, xIn, yIn, maskIn,
                                                 methodInt, True, extrapolate);
      ok = True;
   } else {
      yOut.resize(yIn.nelements());
      yOut = yIn;
      maskOut.resize(maskIn.nelements());
      maskOut = maskIn;
      ok = False;
   }
//
   return ok;
}



template<class T>
void FrequencyAligner<T>::makeMachine (const MEpoch& refEpoch,
                                      const MDirection& dir,
                                      const MPosition& pos,
                                      MFrequency::Types freqSystem,
                                      const Unit& unit)
{

// Make the Frequency conversion machine.  All we have to do with it in 
// future is update the epoch

   LogIO os(LogOrigin("FrequencyAligner", "makeMachine", WHERE));
   if (!CoordinateUtil::makeFrequencyMachine(os, itsMachine, freqSystem,
                                             itsSpecCoord.frequencySystem(),
                                             dir, dir, refEpoch, refEpoch,
                                             pos, pos, unit)) {
      os << "A trial conversion failed - something wrong with frequency conversion machine" << LogIO::EXCEPTION;
   }
//
   MeasFrame frameOut;
   frameOut.set(dir);
   frameOut.set(refEpoch);
   frameOut.set(pos);
   itsRefOut = MFrequency::Ref(freqSystem, frameOut);
}

template<class T>
Double FrequencyAligner<T>::makeAbcissa (Vector<Double>& freq, Bool doDiff)
{
   const uInt n = freq.nelements();
   Double world;
   Double maxDiff = -1;
   if (doDiff) {
      for (uInt i=0; i<n; i++) {
         itsSpecCoord.toWorld(world,i); 
         freq[i] = itsMachine(world).getValue().getValue();
//
         maxDiff = casa::max(casa::abs(freq[i]-itsRefFreqX[i]),maxDiff);
      }
   } else {
      for (uInt i=0; i<n; i++) {
         itsSpecCoord.toWorld(world,i); 
         freq[i] = itsMachine(world).getValue().getValue();
      }
   }
   return maxDiff;
}


template<class T>
void FrequencyAligner<T>::copyOther(const FrequencyAligner<T>& other)
{
   itsMachine = other.itsMachine;
//
   itsFreqSystem = other.itsFreqSystem;
//
   itsRefFreqX.resize(other.itsRefFreqX.nelements());
   itsRefFreqX = other.itsRefFreqX;
//
   itsFreqX.resize(other.itsFreqX.nelements());
   itsFreqX = other.itsFreqX;
//
   itsDiffTol = other.itsDiffTol;
}



template<class T> 
SpectralCoordinate FrequencyAligner<T>::alignedSpectralCoordinate (Bool doLinear) const
{
   const uInt n = itsRefFreqX.nelements();
   AlwaysAssert(n>0,AipsError);

// Get SpectralCoordinate 

   const Vector<String>& units = itsSpecCoord.worldAxisUnits();
   Unit unit(units(0));
   Quantum<Double> restFreq(itsSpecCoord.restFrequency(), unit);

// Create SC. Units will be Hz

   SpectralCoordinate sC;
   if (doLinear) {
      Double crpix = 0.0;
      Quantum<Double> crval(itsRefFreqX[0], unit);
      Quantum<Double> cdelt((itsRefFreqX[n-1]-itsRefFreqX[0])/Double(n-1), unit);
//
      sC = SpectralCoordinate(itsFreqSystem, crval, cdelt, crpix, restFreq);
   } else {
      Quantum<Vector<Double> > freqs(itsRefFreqX, unit);
      sC = SpectralCoordinate(itsFreqSystem, freqs, restFreq);
   }

// Set back to original unit

   sC.setWorldAxisUnits(units);

// Set rest freq state

   sC.setRestFrequencies(itsSpecCoord.restFrequencies(), False);
   sC.selectRestFrequency(restFreq.getValue());

// We don't want to set the frame conversion state (although possibly
// the user might like to be able to access the reference pos/dir/epoch ?)
// We can set the velocity state though

   MDoppler::Types doppler = itsSpecCoord.velocityDoppler();
   String velUnit = itsSpecCoord.velocityUnit();
   Bool ok = sC.setVelocity (velUnit, doppler);
  
// Axis names

   ok = sC.setWorldAxisNames(itsSpecCoord.worldAxisNames());
//
   return sC;
}

} //# End namespace casa



   
