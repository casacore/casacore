//# ConstantSpectrum.h:
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

#if !defined(AIPS_CONSTANTSPECTRUM_H)
#define AIPS_CONSTANTSPECTRUM_H

#include <aips/aips.h>
#include <trial/ComponentModels/ComponentType.h>
#include <trial/ComponentModels/SpectralModel.h>
#include <aips/Measures/MFrequency.h>

template <class T> class Vector;
class RecordInterface;
class GlishRecord;

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

class ConstantSpectrum: public SpectralModel
{
public:
  ConstantSpectrum();

  // The copy constructor uses copy semantics
  ConstantSpectrum(const ConstantSpectrum & other);

  // The destructor does nothing.
  virtual ~ConstantSpectrum();

  // The assignment operator uses copy semantics.
  ConstantSpectrum & operator=(const ConstantSpectrum & other);

  // return the actual spectral type.
  virtual ComponentType::SpectralShape spectralShape() const;

  // set/get the reference frequency
  // <group>
  virtual void setRefFrequency(const MFrequency & newRefFreq);
  virtual const MFrequency & refFrequency() const;
  // </group>

  // Calculate the flux at the specified frequency given the flux at the
  // reference frequency. The flux at the reference frequency must be supplied
  // in the flux variable and the flux at the specified frequency is returned
  // in the same variable. This function does not change the Flux.
  virtual void sample(Flux<Double> & flux,
		      const MFrequency & centerFrequency) const;

  // Return a pointer to a copy of this object upcast to a SpectralModel
  // object. The class that uses this function is responsible for deleting the
  // pointer. This is used to implement a virtual copy constructor.
  virtual SpectralModel * cloneSpectrum() const;

  // return the number of parameters. There are no parameters for this
  // spectral model.
  // <group>
  virtual uInt nSpectralParameters() const;
  virtual void setSpectralParameters(const Vector<Double> & newSpectralParms);
  virtual void spectralParameters(Vector<Double> & spectralParms) const;
  // </group>

  // This functions convert between a glish record and a SpectralModel. This
  // way derived classes can interpret fields in the record in a class specific
  // way. These functions define how a component is represented in glish.  They
  // return False if the glish record is malformed and append an error message
  // to the supplied string giving the reason.
  // <group>
  virtual Bool fromRecord(String & errorMessage,
			  const RecordInterface & record);
  virtual Bool fromRecord(String & errorMessage,
			  const GlishRecord & record);
  virtual Bool toRecord(String & errorMessage, RecordInterface & record) const;
  virtual Bool toRecord(String & errorMessage, GlishRecord & record) const;
  // </group>

  // Function which checks the internal data of this class for correct
  // dimensionality and consistant values. Returns True if everything is fine
  // otherwise returns False.
  virtual Bool ok() const;
private:
  MFrequency itsRefFreq;
};
#endif
