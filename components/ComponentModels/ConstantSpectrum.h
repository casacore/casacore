//# ConstantSpectrum.h: Model the spectral variation with a constant
//# Copyright (C) 1998,1999,2000,2003
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

#ifndef COMPONENTS_CONSTANTSPECTRUM_H
#define COMPONENTS_CONSTANTSPECTRUM_H

#include <casa/aips.h>
#include <components/ComponentModels/ComponentType.h>
#include <components/ComponentModels/SpectralModel.h>

namespace casa { //# NAMESPACE CASA - BEGIN

class MFrequency;
class RecordInterface;
class String;
template <class T> class Vector;

// <summary>Model the spectral variation with a constant</summary>

// <use visibility=export>

// <reviewed reviewer="" date="yyyy/mm/dd" tests="tConstantSpectrum" demos="dConstantSpectrum">
// </reviewed>

// <prerequisite>
//   <li> <linkto class="SpectralModel">SpectralModel</linkto>
// </prerequisite>
//
// <synopsis>

// This class models the spectral variation of a component as constant,
// ie. unchanging with frequency. It is the simplest possible model for
// spectral variation. 

// This class like the other spectral models becomes more useful when used
// through the <linkto class=SkyComponent>SkyComponent</linkto> class, which
// incorperates the flux and spatial variation of the emission, or through the
// <linkto class=ComponentList>ComponentList</linkto> class, which handles
// groups of SkyComponent objects.

// Because the flux is constant with frequency the concept of a reference
// frequency is meaningless with this class. But it can still be accessed using
// the <src>refFrequency</src> and <src>setRefFrequency</src>. However changing
// its value will not affect the behaviour of this class.

// This class does not have any parameters and the <src>nParameters</src>
// function will return zero. It is an error that will generate an exception
// (in Debug mode) to call the <src>setParameters</src> and
// <src>parameters</src> functions with anything other than a zero length
// vector.

// The <src>sample</src> functions always return 1.0.

// This class also contains functions (<src>toRecord</src> &
// <src>fromRecord</src>) which perform the conversion between Records and
// ConstantSpectrum objects. These functions define how a ConstantSpectrum
// object is represented in glish. The format of the record that is generated
// and accepted by these functions is:
// <srcblock>
// c := [type = 'constant',
//       frequency = [type = 'frequency',
//                    refer = 'lsr',
//                    m0 = [value = 1, unit = 'GHz']
//                   ]
//      ]
// </srcblock>
// The frequency field contains a record representation of a frequency measure
// and its format is defined in the Measures module. Its refer field defines
// the reference frame for the direction and the m0 field defines the value of
// the reference frequency. The parsing of the type field is case
// insensitive.
// </synopsis>

// <example>
// Its hard to think of a good example for this class as it is basically does
// nothing! In this example the spectral variation of a component is set to to
// a constant value.
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
// modelling any spectral variation in their components.
// </motivation>
//
// <todo asof="1999/11/23">
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
  ConstantSpectrum(const ConstantSpectrum& other);

  // The destructor does nothing.
  virtual ~ConstantSpectrum();

  // The assignment operator uses copy semantics.
  ConstantSpectrum& operator=(const ConstantSpectrum& other);

  // return the actual spectral type. This function always returns
  // ComponentType::CONSTANT_SPECTRUM
  virtual ComponentType::SpectralShape type() const;

  // Return the scaling factor that indicates the flux is at the specified
  // frequency assuming the flux at the reference frequency is one. This
  // function always returns one, as the spectrum is constant.
  virtual Double sample(const MFrequency& centerFrequency) const;

  // Same as the previous function except that many frequencies can be sampled
  // at once. The reference frame must be the same for all the specified
  // frequencies. Uses a customised implementation.
  virtual void sample(Vector<Double>& scale, 
                      const Vector<MFrequency::MVType>& frequencies, 
                      const MFrequency::Ref& refFrame) const;

  // Return a pointer to a copy of this object upcast to a SpectralModel
  // object. The class that uses this function is responsible for deleting the
  // pointer. This is used to implement a virtual copy constructor.
  virtual SpectralModel* clone() const;

  // return the number of parameters. There are no parameters for this spectral
  // model. So calling <src>setParameters</src> or <src>parameters</src> with
  // anything other than a zero length Vector will throw an exception (when
  // compiled in debug mode).
  // <group>
  virtual uInt nParameters() const;
  virtual void setParameters(const Vector<Double>& newSpectralParms);
  virtual Vector<Double> parameters() const;
  virtual void setErrors(const Vector<Double>& newSpectralErrs);
  virtual Vector<Double> errors() const;
  // </group>

  // These functions convert between a Record and a ConstantSpectrum. These
  // functions define how a ConstantSpectrum object is represented in glish and
  // this is detailed in the synopsis above. These functions return False if
  // the record is malformed and append an error message to the supplied string
  // giving the reason.
  // <group>
  virtual Bool fromRecord(String& errorMessage,
			  const RecordInterface& record);
  virtual Bool toRecord(String& errorMessage, RecordInterface& record) const;
  // </group>

  // Convert the parameters of the spectrum to the specified units. As a
  // constant spectrum has no parameters this function does nothing and always
  // returns True.
  virtual Bool convertUnit(String& errorMessage,
                           const RecordInterface& record);
 
  // Function which checks the internal data of this class for consistant
  // values. Returns True if everything is fine otherwise returns False.
  virtual Bool ok() const;
};

} //# NAMESPACE CASA - END

#endif
