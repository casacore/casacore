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
#include <aips/Arrays/Vector.h>
#include <aips/Measures/MFrequency.h>
#include <aips/Measures/Stokes.h>
#include <trial/ComponentModels/ComponentType.h>
#include <trial/ComponentModels/SpectralModel.h>

template <class T> class Flux;
class RecordInterface;
class GlishRecord;
class String;

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

class SpectralIndex: virtual public SpectralModel
{
public:
  // The default SpectralIndex has all exponents set to zero and a reference
  // frequency of 0 Hz in the LSR frame
  SpectralIndex();

  // Construct a SpectralIndex with specified reference frequency and set the
  // specified Stokes::I exponent to the supplied value. The exponent for the
  // other polarisations is zero.
  SpectralIndex(const MFrequency & refFreq, Double exponent);

  // Construct a SpectralIndex with specified reference frequency and set all
  // the exponents to the supplied values. The Vector must have four elements
  // which contain, in order, the exponents for the Stokes::I, Stokes::Q,
  // Stokes::U and Stokes::V polarizations.
  SpectralIndex(const MFrequency & refFreq, const Vector<Double> & exponents);

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

  // set/get the spectral index. The which argument must be either Stokes::I,
  // Stokes::Q, Stokes::U or Stokes::V. No other StokesTypes are allowed.
  // <group>
  const Double & index(const Stokes::StokesTypes which) const;
  void setIndex(const Double & newIndex, const Stokes::StokesTypes which);
  // </group>

  // set/get the all the spectral indices.  The supplied vector must be of
  // length 4 and contains, in order, the Stokes I, Q, U, and V
  // indices. Similarly the returned vector is of length 4 with the indices in
  // the same order.
  // <group>
  const Vector<Double> & indices() const;
  void setIndices(const Vector<Double> & newIndices);
  // </group>

  // Calculate the flux at the specified frequency given the flux at the
  // reference frequency. The flux at the reference frequency must be supplied
  // in the scaledFlux variable and the flux at the specified frequency is
  // returned in the same variable.
  virtual void sample(Flux<Double> & scaledFlux, 
		     const MFrequency & centerFrequency) const;

  // Return a pointer to a copy of this object upcast to a SpectralModel
  // object. The class that uses this function is responsible for deleting the
  // pointer. This is used to implement a virtual copy constructor.
  virtual SpectralModel * cloneSpectrum() const;

  // return the number of parameters. There are four parameters for this
  // spectral model. They are the spectral index on each of the I, Q, U and V
  // polarisations.
  // <group>
  virtual uInt nSpectralParameters() const;
  virtual void setSpectralParameters(const Vector<Double> & newSpectralParms);
  virtual void spectralParameters(Vector<Double> & spectralParms) const;
  // </group>

  // This functions convert between a record and a SpectralModel. This way
  // derived classes can interpret fields in the record in a class specific
  // way. These functions define how a spectral index is represented in glish.
  // They return False if the supplied record is malformed and append an error
  // message to the supplied string giving the reason.
  // <group>
  virtual Bool fromRecord(String & errorMessage,
			  const RecordInterface & record);
  virtual Bool fromRecord(String & errorMessage, const GlishRecord & record);
  virtual Bool toRecord(String & errorMessage, RecordInterface & record) const;
  virtual Bool toRecord(String & errorMessage, GlishRecord & record) const;
  // </group>

  // Function which checks the internal data of this class for correct
  // dimensionality and consistant values. Returns True if everything is fine
  // otherwise returns False.
  virtual Bool ok() const;

private:
  MFrequency itsRefFreq;
  Vector<Double> itsIndex;
  //# The following data member is contained in the itsRefFreq data
  //# member but are cached here to improve the speed of the sample(...)
  //# function.
  MFrequency::Types itsRefFrame;
  //# This contains the actual reference frequency (ie refFreq + offset) in Hz.
  Double itsNu0;
  //# The following data member just caches whether the Q, U & V indices are
  //# zero ie., we have an I only flux variation.
  Bool itsIonly;
  //# the following are just references into the itsIndex vector and are used
  //# to improve the speed of the sample(...) function
  Double & itsIindex;
  Double & itsQindex;
  Double & itsUindex;
  Double & itsVindex;
};
#endif
