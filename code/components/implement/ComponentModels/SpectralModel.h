//# SpectralModel.h:
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

#if !defined(AIPS_SPECTRALMODEL_H)
#define AIPS_SPECTRALMODEL_H

#include <aips/aips.h>
#include <trial/ComponentModels/ComponentType.h>
#include <trial/Utilities/RecordTransformable.h>

template <class T> class Vector;
template <class T> class Flux;
class MFrequency;
class RecordInterface;
class String;

// <summary>Base class for Spectral Models</summary>

// <use visibility=export>

// <reviewed reviewer="" date="yyyy/mm/dd" tests="" demos="">
// </reviewed>

// <prerequisite>
//   <li> <linkto class=MDirection>MDirection</linkto>
// </prerequisite>
//
// <synopsis>

// This abstract base class defines the interface for different classes which
// model the spectrum of a component. The most fundamental derived class is the
// <linkto class=ConstantSpectrum>constant</linkto> spectrum class but the
// <linkto class=SpectralIndex>spectral index</linkto> class is also
// available. These classes model the spectral shape of emission from the
// sky. Classes derived from the <linkto
// class=ComponentShape>ComponentShape</linkto> class are used to model the
// spatial characteristics.

// This class parameterises spectral models with two quantities.
// <dl>
// <dt> A reference frequency. 
// <dd> This is specified using an <linkto class=MFrequency>MFrequency</linkto>
//      object and defines the frequency where the scaling factor is
//      one. Usually this frequency corresponds to one near the where the model
//      is interesting.
// <dt> A Vector of parameters.
// <dd> This contains other parameters that the are defined differently for
//      different spectral models. The length of the vector may vary for
//      different spectral models.
// </dl>
// 

// The basic operation of classes using this interface is to model the flux as
// a function of frequency. Classes derived from this one assume that the Flux
// (or integrated intensity) is always one at the reference frequency. The
// scale functions in this class then calculate the relative flux at other
// frequencies. This scale factor can be multiplied by the actual flux at the
// reference frequency to determine the flux at the specified frequency.

// </synopsis>
//
// <example>
// Because SpectralModel is an abstract base class, an actual instance of this
// class cannot be constructed. However the interface it defines can be used
// inside a function. This is always recommended as it allows functions which
// have SpectralModel's as arguments to work for any derived class.
// <srcblock>
// void plotSpectrum(const SpectralModel & theSpectrum, ) {
//   for (uInt 
//   cout << "This is a " 
//        << ComponentType::name(theShape.shape())
//        << " shape with a reference direction of "
//        << theShape.direction() << endl;
// }
// </srcblock>
// </example>
//
// <motivation>
// </motivation>
//
// <todo asof="yyyy/mm/dd">
//   <li> add this feature
//   <li> fix this bug
//   <li> start discussion of this possible extension
// </todo>

class SpectralModel: public RecordTransformable
{
public:
  // a virtual destructor is needed so that the actual destructor in the
  // derived class will be used.
  virtual ~SpectralModel();

  // return the actual spectral shape.
  virtual ComponentType::SpectralShape spectralShape() const = 0;

  // set/get the reference frequency
  // <group>
  virtual void setRefFrequency(const MFrequency & newRefFreq) = 0;
  virtual const MFrequency & refFrequency() const = 0;
  virtual void refFrequency(MFrequency & refFreq) const;
  // </group>

  // Calculate the flux at the specified frequency given the flux at the
  // reference frequency. The flux at the reference frequency must be supplied
  // in the flux variable and the flux at the specified frequency is returned
  // in the same variable.
  virtual void sample(Flux<Double> & flux, 
		      const MFrequency & centerFrequency) const = 0;

  // Return a pointer to a copy of the derived object upcast to a SpectralModel
  // object. The class that uses this function is responsible for deleting the
  // pointer. This is used to implement a virtual copy constructor.
  virtual SpectralModel * cloneSpectrum() const = 0;

  // return the number of parameters in this spectral shape and set/get them.
  // <group>
  virtual uInt nSpectralParameters() const = 0;
  virtual void setSpectralParameters(const Vector<Double> & newParms) = 0;
  virtual void spectralParameters(Vector<Double> & compParms) const = 0;
  // </group>

  // These functions convert between a record and a SpectralModel. This way
  // derived classes can interpret fields in the record in a class specific
  // way. These functions define how a spectral model is represented in glish.
  // They return False if the record is malformed and append an error message
  // to the supplied string giving the reason.
  // <group>
  virtual Bool fromRecord(String & errorMessage, 
			  const RecordInterface & record) = 0;
  virtual Bool toRecord(String & errorMessage,
			RecordInterface & record) const = 0;
  // </group>

  // Return the spectral shape that the supplied record represents. Returns
  // ComponentType::UNKNOWN_SPECTRAL_SHAPE if the record could not be parsed
  // correctly (it then appends an appropriate error message to the
  // errorMessage String).
  static ComponentType::SpectralShape getType(String & errorMessage,
					      const RecordInterface & record);

  // Function which checks the internal data of this class for correct
  // dimensionality and consistant values. Returns True if everything is fine
  // otherwise returns False.
  virtual Bool ok() const = 0;
};
#endif
