//# SpectralIndex.h:
//# Copyright (C) 1998
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

#if !defined(AIPS_SPECTRALINDEX_H)
#define AIPS_SPECTRALINDEX_H

#include <aips/aips.h>
#include <aips/Measures/MFrequency.h>
#include <trial/ComponentModels/ComponentType.h>
#include <trial/ComponentModels/SpectralModel.h>

template <class T> class Vector;

// <summary></summary>

// <use visibility=export>

// <reviewed reviewer="" date="yyyy/mm/dd" tests="" demos="">
// </reviewed>

// <prerequisite>
//   <li> SomeClass
//   <li> SomeOtherClass
//   <li> some concept
// </prerequisite>
//
// <etymology>
// </etymology>
//
// <synopsis>
// </synopsis>
//
// <example>
// </example>
//
// <motivation>
// </motivation>
//
// <thrown>
//    <li>
//    <li>
// </thrown>
//
// <todo asof="yyyy/mm/dd">
//   <li> add this feature
//   <li> fix this bug
//   <li> start discussion of this possible extension
// </todo>

class SpectralIndex: public SpectralModel
{
public:
  // The default SpectralIndex has an exponent of zero and a reference
  // frequency of 0 Hz in the LSR frame
  SpectralIndex();

  // Construct a SpectralIndex with specified reference frequency and exponent.
  SpectralIndex(const MFrequency & refFreq, const Double & exponent);

  // The copy constructor uses copy semantics
  SpectralIndex(const SpectralIndex & other);

  // The destructor does nothing.
  virtual ~SpectralIndex();

  // The assignment operator uses copy semantics.
  SpectralIndex & operator=(const SpectralIndex & other);

  // return the actual spectral type.
  virtual ComponentType::SpectralShape spectralShape() const;

  // set/get the reference frequency
  // <group>
  virtual void setRefFrequency(const MFrequency & newRefFreq);
  virtual const MFrequency & refFrequency() const;
  // </group>

  // set/get the spectral index
  // <group>
  virtual const Double & index() const;
  virtual void index(Double & index) const;
  virtual void setIndex(const Double & newIndex);
  // </group>

  // Return the attenuation of the component at the specified frequency. The
  // attenuation is always 1 at the reference frequency.
  virtual Double scale(const MFrequency & centerFrequency) const;

  // return the number of parameters. There is only one parameter for this
  // spectral model. It is the spectral index.
  // <group>
  virtual uInt nSpectralParameters() const;
  virtual void setSpectralParameters(const Vector<Double> & newSpectralParms);
  virtual void spectralParameters(Vector<Double> & spectralParms) const;
  // </group>

  // Function which checks the internal data of this class for correct
  // dimensionality and consistant values. Returns True if everything is fine
  // otherwise returns False.
  virtual Bool SpectralIndex::ok() const;

private:
  MFrequency itsRefFreq;
  Double itsIndex;
  //# The following data members are actually contained in the itsRefFreq data
  //# member but are cached here to improve the speed of the sample(...)
  //# function.
  MFrequency::Types itsRefFrame;
  Double itsNu0;
};
#endif
