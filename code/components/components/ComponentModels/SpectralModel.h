//# SpectralModel.h: Base class for Spectral Models
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

#ifndef COMPONENTS_SPECTRALMODEL_H
#define COMPONENTS_SPECTRALMODEL_H

#include <casa/aips.h>
#include <components/ComponentModels/ComponentType.h>
#include <casa/Utilities/RecordTransformable.h>
#include <measures/Measures/MFrequency.h>
#include <casa/Quanta/Unit.h>
#include <casa/Quanta/Quantum.h>

namespace casa { //# NAMESPACE CASA - BEGIN

class RecordInterface;
class String;
template <class T> class Vector;

// <summary>Base class for spectral models</summary>

// <use visibility=export>

// <reviewed reviewer="" date="yyyy/mm/dd" tests="tConstantSpectrum" demos="dSpectralModel">
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
// available. These classes model the spectrum of emission from the sky.

// Classes derived from the 
// <linkto class=ComponentShape>ComponentShape</linkto> class are used to model
// the shape and the <linkto class=Flux>Flux</linkto> class is used to model
// the flux. The <linkto class=SkyComponent>SkyComponent</linkto> class
// incorporates these three characteristics (flux, shape & spectrum) and the
// <linkto class=ComponentList>ComponentList</linkto> class handles groups of
// SkyComponent objects.

// This class parameterises spectral models with two quantities.
// <dl>
// <dt><em> A reference frequency.</em>
// <dd> This is specified using an <linkto class=MFrequency>MFrequency</linkto>
//      object and defines a frequency where the model
//      is interesting. See the description of derived classes for the
//      specific interpretation of the reference frequency.
// <dt> <em>A Vector of parameters.</em>
// <dd> This contains other parameters that the are defined differently for
//      different spectral models. The length of the vector may vary for
//      different spectral models.
// </dl>
// 

// The basic operation of classes using this interface is to model the flux as
// a function of frequency. Classes derived from this one do not know what the
// flux is at the reference frequency. Instead the sample functions return
// factors that are used to scale the flux and calculate the amount of flux at
// a specified frequency.

// Any allowed frequency reference frame can be used. However the reference
// frame must be adequately specified in order to allow conversions to other
// reference frames. For example if the reference frame code for the frequency
// is MFrequency::TOPO then the reference frame must also contain the time,
// position on the earth, and direction of the observation that corresponds to
// the specified frequency. This way the sample functions can convert the
// frequency to a value in the LSR reference frame (if you specify the sample
// frequency in the LSR frame).

// </synopsis>
//
// <example>
// Because this is an abstract base class, an actual instance of this class
// cannot be constructed. However the interface it defines can be used inside a
// function. This is always recommended as it allows functions which have
// SpectralModels as arguments to work for any derived class.

// In this example the plotSpectrum function prints out the type of spectral
// model it is working with and the reference frequency of that model. It then
// uses the model to calculate the proportion of the flux at other
// frequencies. This example is coded in the dSpectralModel.cc file.

// <srcblock>
// void plotSpectrum(const SpectralModel& modelSpectrum) {
//   cout << "This is a "
//        << ComponentType::name(modelSpectrum.type())
//        << " spectrum with a reference frequency of: "
//        << setprecision(4) << modelSpectrum.refFrequency().get("GHz") << " ("
//        << modelSpectrum.refFrequency().getRefString() << ")"
//        << endl;
//   const MVFrequency step(Quantity(100.0, "MHz"));
//   MVFrequency sampleFreq(Quantity(1, "GHz"));
//   MeasFrame obsFrame;
//   {
//     Quantity obsRa; MVAngle::read(obsRa, "19:39:");
//     Quantity obsDec; MVAngle::read(obsDec, "-63.43.");
//     Quantity obsDay; MVTime::read(obsDay, "1996/11/20/5:20");
//     obsFrame.set(MEpoch(obsDay, MEpoch::UTC),
// 		    MDirection(obsRa, obsDec, MDirection::J2000));
//   }
//   MFrequency::Ref obsRef(MFrequency::GEO, obsFrame);
//   cout << "Frequency\t scale\n";
//   for (uInt i = 0; i < 11; i++) {
//      cout << setprecision(7) << sampleFreq.get("GHz")
// 	  << "\t\t " << modelSpectrum.sample(MFrequency(sampleFreq, obsRef))
// 	  << endl;
//      sampleFreq += step;
//   }
// }
// </srcblock>
// </example>
//
// <motivation>
// The SpectralModel base class was seperated from the ComponentShape base
// class so that mixing components with different spatial and spectral shapes
// did not result in a combinatorial explosion in the number of classes
// required.
// </motivation>
//
// <todo asof="1999/11/23">
//   <li> I would not be surprised if the base class will need to be updated
//        when classes modelling spectral lines are written.
// </todo>

class SpectralModel: public RecordTransformable
{
public:
  // a virtual destructor is needed so that the actual destructor in the
  // derived class will be used.
  virtual ~SpectralModel();

  // return the actual spectral type. The ident function returns it as a
  // String. 
  // <group>
  virtual ComponentType::SpectralShape type() const = 0;
  virtual const String& ident() const;
  // </group>

  // set/get the reference frequency
  // <group>
  void setRefFrequency(const MFrequency& newRefFreq);
  const MFrequency& refFrequency() const;
  // </group>

  // get the frequency unit, and change the default frequency unit to the
  // specified one. This will only affect the units used in the Record returned
  // by the toRecord function.
  // <group>
  const Unit& frequencyUnit() const;
  void convertFrequencyUnit(const Unit& freqUnit);
  // </group>

  // set/get the error in the reference frequency. Values must be positive
  // angular quantities otherwise an AipsError exception is thrown. The errors
  // are usually interpreted as the 1-sigma bounds in latitude/longitude and
  // implicitly assume a Gaussian distribution. They must have units with the
  // same dimensions as the Hz.
  // <group>
  void setRefFrequencyError(const Quantum<Double>& newRefFreqErr);
  const Quantum<Double>& refFrequencyError() const;
  // </group>

  // Return the scaling factor that indicates what proportion of the flux is at
  // the specified frequency. ie. if the centreFrequency argument is the
  // reference frequency then this function will always return one. At other
  // frequencies it will return a non-negative number.
  virtual Double sample(const MFrequency& centerFrequency) const = 0;

  // Same as the previous function except that many frequencies can be sampled
  // at once. The reference frame must be the same for all the specified
  // frequencies. A default implementation of this function is available that
  // uses the sample function described above.  However customised versions of
  // this function will be more efficient as intermediate values only need to
  // be computed once.
  virtual void sample(Vector<Double>& scale, 
                      const Vector<MFrequency::MVType>& frequencies, 
                      const MFrequency::Ref& refFrame) const = 0;

  // Return a pointer to a copy of the derived object upcast to a SpectralModel
  // object. The class that uses this function is responsible for deleting the
  // pointer. This is used to implement a virtual copy constructor.
  virtual SpectralModel* clone() const = 0;

  // return the number of parameters in this spectral shape and set/get them.
  // <group>
  virtual uInt nParameters() const = 0;
  virtual void setParameters(const Vector<Double>& newParms) = 0;
  virtual Vector<Double> parameters() const = 0;
  virtual void setErrors(const Vector<Double>& newErrors) = 0;
  virtual Vector<Double> errors() const = 0;
  // </group>

  // These functions convert between a record and a SpectralModel. This way
  // derived classes can interpret fields in the record in a class specific
  // way. They return False if the record is malformed and append an error
  // message to the supplied string giving the reason.  These functions define
  // how a spectral model is represented in glish. All records should have
  // 'type' & 'frequency' fields which contain respectively; a string
  // indicating which spectral model is actually used, and a record
  // representation of a frequency measure.  The interpretation of all other
  // fields depends on the specific spectral model used.
  // <group>
  virtual Bool fromRecord(String& errorMessage, 
			  const RecordInterface& record) = 0;
  virtual Bool toRecord(String& errorMessage,
			RecordInterface& record) const = 0;
  // </group>

  // Convert the parameters of the spectral model to the specified units. The
  // Record must contain the same fields that the to/from Record functions have
  // (with the exception of the frequency & type fields). These fields will
  // contain strings (and not Quantums) that specify the new units for these
  // parameters. The new units must have the same dimensions as the existing
  // ones. If there is any problem parsing the record then an error message is
  // appended to the supplied string and the function returns False. 
  virtual Bool convertUnit(String& errorMessage,
			   const RecordInterface& record) = 0;

  // Return the spectral shape that the supplied record represents. The
  // spectral shape is determined by parsing a 'type' field in the supplied
  // record. Returns ComponentType::UNKNOWN_SPECTRAL_SHAPE if the type field
  // (which contains a string) could not be translated into a known spectral
  // shape. It then appends an appropriate error message to the errorMessage
  // String.
  static ComponentType::SpectralShape getType(String& errorMessage,
					      const RecordInterface& record);

  // Function which checks the internal data of this class for correct
  // dimensionality and consistant values. Returns True if everything is fine
  // otherwise returns False.
  virtual Bool ok() const;

protected:
  // The constructors and assignment operator are protected as only derived
  // classes should use them.
  // <group>
  //# The default reference frequency is at 1 GHz in the LSR frame
  SpectralModel();

  //# Construct a SpectralModel at the specified reference frequency.
  SpectralModel(const MFrequency& refFreq, const Unit& = Unit("GHz"));

  //# The copy constructor uses copy semantics.
  SpectralModel(const SpectralModel& other);

  //# The assignment operator uses copy semantics.
  SpectralModel& operator=(const SpectralModel& other);
  // </group>

  // returns True if the quantum is not a non-negative quantity with units
  // dimensionally identical to the Hz
  static Bool badError(const Quantum<Double>& quantum);

private:
  //# The reference frequency of the spectral model
  MFrequency itsRefFreq;
  //# the units (Hz, GHz etc.) that the record functions should use for the
  //# reference frequency. 
  Unit itsFreqUnit;
  Quantity itsFreqErr;
};

} //# NAMESPACE CASA - END

#endif
