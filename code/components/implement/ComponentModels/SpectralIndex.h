//# SpectralIndex.h: Models the spectral variation with a spectral index
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
class String;

// <summary>Models the spectral variation with a spectral index</summary>

// <use visibility=export>

// <reviewed reviewer="" date="yyyy/mm/dd" tests="tSpectralIndex" demos="">
// </reviewed>

// <prerequisite>
//   <li> <linkto class="SpectralModel">SpectralModel</linkto>
// </prerequisite>
//
// <synopsis>
// This class models the spectral variation of a component with a spectral
// index. A spectral index is is the exponent for a power law model that
// mathematically is defined as:
// <srcblock>
// I = I_0 * (\nu / \nu_0)^\alpha
// </srcblock>
// Where:
// <dl compact>
// <dt><src>\nu_0</src><dd> is the reference frequency
// <dt><src>I_0</src><dd> is the reference frequency
// <dt><src>\alpha</src><dd> is the spectral index
// <dt><src>\nu</src><dd> is the user specified frequency
// <dt><src>I</src><dd> is the flux, in the I polarisation, 
//                      at the specified frequency
// <dl>
// In general the flux has four polarisation components and this class has
// seperate indicies for each of the four Stokes parameters, I, Q, U, \& V.

// As with all classes derived from SpectralModel the basic operation of this
// class is to model the flux as a function of frequency. This class does not
// know what the flux is at the reference frequency and this must be supplied
// as an argument to the <src>sample</src> function. This classes will scale
// the supplied flux using the formula shown above after converting the flux to
// a Stokes representation. The returned flux is always in the Stokes
// representation.

// This class has besides the reference frequency, four parameters. These are
// the spectral indices.
// </synopsis>
//
// <example>
// These examples are coded in the tSpectralModel.h file.
// <h4>Example 1:</h4>
// In this example ...
// <srcblock>
// </srcblock>
/// </example>
//
// <motivation> A Spectral Index frequency variation is the  most widely used
// model in radio astronomy. In particular the NFRA package 'newstar' uses it
// extensively.
// </motivation>
//
// <todo asof="1998/05/18">
//   <li> Nothing I hope
// </todo>

// <linkfrom anchor="SpectralIndex" classes="SpectralModel ConstantSpectrum">
//  <here>SpectralIndex</here> - Uses a spectral index to model the spectrum
// </linkfrom>
 
class SpectralIndex: public SpectralModel
{
public:
  // The default SpectralIndex has all indices set to zero and a reference
  // frequency of 0 Hz in the LSR frame. As such it is no different from the
  // ConstantSpectrum class (except slower).
  SpectralIndex();

  // Construct a SpectralIndex with specified reference frequency and set the
  // Stokes::I exponent to the supplied value. The exponent for the other
  // polarisations is zero.
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
  virtual ComponentType::SpectralShape type() const;

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
  virtual SpectralModel * clone() const;

  // return the number of parameters. There are four parameters for this
  // spectral model. They are the spectral index on each of the I, Q, U and V
  // polarisations.
  // <group>
  virtual uInt nParameters() const;
  virtual void setParameters(const Vector<Double> & newSpectralParms);
  virtual void parameters(Vector<Double> & spectralParms) const;
  // </group>

  // This functions convert between a record and a SpectralModel. This way
  // derived classes can interpret fields in the record in a class specific
  // way. These functions define how a spectral index is represented in glish.
  // They return False if the supplied record is malformed and append an error
  // message to the supplied string giving the reason.
  // <group>
  virtual Bool fromRecord(String & errorMessage,
			  const RecordInterface & record);
  virtual Bool toRecord(String & errorMessage, RecordInterface & record) const;
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
