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
//   <li> <linkto class=MFrequency>MFrequency</linkto>
// </prerequisite>
//
// <synopsis>

// This abstract base class defines the interface for different classes which
// model the spectrum of a component. The most fundamental derived class is the
// <linkto class=ConstantSpectrum>ConstantSpectrum</linkto> class but the
// <linkto class=SpectralIndex>SpectralIndex</linkto> class is also
// available. These classes model the spectral shape of emission from the
// sky. Classes derived from the <linkto
// class=ComponentShape>ComponentShape</linkto> class are used to model the
// spatial characteristics.

// This class parameterizes spectral models with two quantities.
// <dl>
// <dt><em> A reference frequency.</em>
// <dd> This is specified using an <linkto class=MFrequency>MFrequency</linkto>
//      object and defines a frequency near the where the model
//      is interesting. See the description of derived classes for the
//      specific interpretation of the reference frequency.
// <dt> <em>A Vector of parameters.</em>
// <dd> This contains other parameters that the are defined differently for
//      different spectral models. The length of the vector may vary for
//      different spectral models.
// </dl>
// 
// The basic operation of classes using this interface is to model the flux as
// a function of frequency. Classes derived from this one assume do not know
// what the flux is at the reference frequency, this must be supplied as an
// argument to the <src>sample</src> function. These classes will scale the
// supplied flux to a value at the user specified frequency.  In general this
// scaling may be different for different polarisations.
// </synopsis>
//
// <example>
// Because SpectralModel is an abstract base class, an actual instance of this
// class cannot be constructed. However the interface it defines can be used
// inside a function. This is always recommended as it allows functions which
// have SpectralModel's as arguments to work for any derived class.
// <h4>Example 1:</h4>
// In this example the plotSpectrum function prints out the type of spectral
// model it is working with and the reference frequency of that model. It then
// uses the model to calculate the flux at other frequencies.
// <srcblock>
// void plotSpectrum(const Flux<Double> & refFlux,
//                   const SpectralModel & modelSpectrum) {
//   cout << "This is a " 
//        << ComponentType::name(modelSpectrum.spectralShape())
//        << " spectrum with a reference frequency of "
//        << modelSpectrum.refFrequency().get("GHz")) << endl
//        << modelSpectrum.refFrequency().getRef() 
//        << endl;
//   cout << "Frequency\t Flux\n";
//   const Quantum<Double> step(100.0, "MHz");
//   Quantum<Double> sampleFreq = modelSpectrum.refFrequency().get("GHz");
//   Flux<Double> modelFlux;
//   for (uInt i = 0; i < 11; i++) {
//     modelFlux = refFlux.copy();
//     modelSpectrum.sample(modelFlux, MFrequency(sampleFreq));
//     cout << sampleFreq.get("GHz")
// 	 << "\t\t " << modelFlux.value(0).re << " " 
// 	 << modelFlux.unit().getName() << endl;
//     sampleFreq += step;
//   }
// }
// </srcblock>
// </example>
//
// <motivation>
// There are many different spectral variations possible and the SkyCompRep
// class needed to be able to handle all of them. Hence a base class is
// needed.
// </motivation>
//
// <todo asof="1998/04/04">
//   <li> Nothing I hope!
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

protected:
  //# These functions are used by derived classes implementing concrete
  //# versions of the toRecord and fromRecord member functions.
  Bool addFreq(String & errorMessage, RecordInterface & record) const;
  Bool readFreq(String & errorMessage, const RecordInterface & record);
};
#endif
