//# ConstantSpectrum.h: Model the spectral variation with a constant
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

class RecordInterface;
class String;
template <class T> class Flux;
template <class T> class Vector;

// <summary>Model the spectral variation with a constant</summary>

// <use visibility=export>

// <reviewed reviewer="" date="yyyy/mm/dd" tests="tConstantSpectrum" demos="">
// </reviewed>

// <prerequisite>
//   <li> <linkto class="SpectralModel">SpectralModel</linkto>
// </prerequisite>
//
// <synopsis>
// This class models the spectral variation of a component as constant,
// ie. unchanging with frequency. It is the simplest possible model for
// spectral variation. 

// Because the flux is constant with frequency the concept of a reference
// frequency is meaningless with this class. But it can still be accessed using
// the <src>refFrequency</src> and <src>setRefFrequency</src>. However changing
// its value will not affect the behaviour of this class. For this reason the
// <src>fromRecord</src> function will not parse the frequency field of the
// supplied record and the <src>toRecord</src> function will not add a
// frequency field to the generated Record.

// This class does not have any parameters and the <src>nParameters</src>
// function will return zero. It is an error that will generate an exception
// (in Debug mode) to call the <src>setParameters</src> and
// <src>parameters</src> functions with anything other than a zero length
// vector.

// The <src>sample</src> function always scales the supplied flux by 1. In
// other words the input flux is always the same as the returned value for all
// frequencies and polarisations.

// </synopsis>
//
// <example>
// Its hard to think of a good example for this class as it is basically does
// nothing! This example is coded in the file tConstantSpectrum.cc
// <h4>Example 1:</h4>
// In this example the spectral variation of a component is set to
// to a constant value.
// <srcblock>
// SkyComponent myComp(...);
// ...
// if (myComp.spectrum().type() != ComponentType::CONSTANT_SPECTRUM) {
//   myComp.spectrum() = ConstantSpectrum();
// }
// </srcblock>
// </example>
//
// <motivation>
// A ConstantSpectrum class is needed for users who are not interested in
// modelling any spectral variation in there components.
// </motivation>
//
// <todo asof="1998/05/18">
//   <li> Nothing I hope!
// </todo>

// <linkfrom anchor="ConstantSpectrum" classes="SpectralModel SpectralIndex">
//  <here>ConstantSpectrum</here> - Models the spectrum as constant
// </linkfrom>

class ConstantSpectrum: public SpectralModel
{
public:
  // The default constructor is the only one you really need as this class has
  // no parameters!
  ConstantSpectrum();

  // The copy constructor uses copy semantics
  ConstantSpectrum(const ConstantSpectrum & other);

  // The destructor does nothing.
  virtual ~ConstantSpectrum();

  // The assignment operator uses copy semantics.
  ConstantSpectrum & operator=(const ConstantSpectrum & other);

  // return the actual spectral type.
  virtual ComponentType::SpectralShape type() const;

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
  virtual SpectralModel * clone() const;

  // return the number of parameters. There are no parameters for this
  // spectral model.
  // <group>
  virtual uInt nParameters() const;
  virtual void setParameters(const Vector<Double> & newSpectralParms);
  virtual void parameters(Vector<Double> & spectralParms) const;
  // </group>

  // These functions convert between a record and a ConstantSpectrum. These
  // functions define how a spectral model is represented in glish.  A
  // ConstantSpectrum class is defined by the record:
  // <src>[type='constantspectrum']</src>. No reference frequency field is
  // necessary.  These functions return False if the record is malformed and
  // append an error message to the supplied string giving the reason.
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
  //# needed so that the refFrequency function can return a reference to
  //# something.
  MFrequency itsRefFreq;
};
#endif
