//# SkyCompRep.h: this defines SkyCompRep.h
//# Copyright (C) 1996,1997,1998
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
//#
//# $Id$

#if !defined(AIPS_SKYCOMPREP_H)
#define AIPS_SKYCOMPREP_H

#include <aips/aips.h>
#include <trial/ComponentModels/ComponentType.h>
#include <trial/ComponentModels/SpectralModel.h>

class GlishRecord;
class MDirection;
class MFrequency;
class MVAngle;
class String;
template <class Qtype> class Quantum;
template <class T> class Flux;
template <class T> class ImageInterface;
template <class T> class Vector;

// <summary>A model component of the sky brightness</summary>

// <use visibility=export>
// <reviewed reviewer="" date="yyyy/mm/dd" tests="" demos="">
// </reviewed>

// <prerequisite> 
// <li> <linkto class=MDirection>MDirection</linkto>
// </prerequisite>
//

// <synopsis> 

// This abstract base class defines an interface for a number of classes that
// are used to represent models of the sky brightness. It contains the
// functions common to all components while classes like GaussianCompRep &
// PointCompRep, which are derived from this class, contain functions specific
// to the different component types.

// The functions in this class allow the user to:
// <ul>
// <li> sample the flux of the component by specifying a direction and pixel
//      size.
// <li> project a componet onto an image.
// <li> set and change the reference direction of a component.
// <li> set and change the flux of a component.
// <li> set and change a label associated with a component. The label is a text
//      string that a user may use for any purpose.
// <li> set and change other parameters, whose interpretation depends on the
//      specific component type.
// <li> enquire what specific type of component this is.
// <li> convert the component to and from a glish record so that it can be
//      manipulated within glish.

// </synopsis>

// <example>
// Because this is a virtual base class this example will be inside a
// function. 
// <srcblock>
// void printComponent(SkyCompRep & component){
//   Quantum<Vector<Double> > compFlux(Vector<Double>(4), "Jy");
//   component.flux().value(compFlux);
//   cout << "Component has a total flux [I,Q,U,V] of " << compFlux;
//   MDirection compDir;
//   component.direction(compDir);
//   cout << ", is centred at " << compDir.getValue("deg");
//   Vector<Double> peak(4);
//   component.sample(peak, compDir, MVAngle(Quantity(1, "arcsec")));
//   cout << " and an peak intensity of " << peak << " Jy/arcsec" << endl;
// }
// </srcblock>
// </example>
//
// <motivation>
// Model fitting is an important part of astronomical data
// reduction/interpretation. This class defines a model component. Many
// components can be strung together (using the ComponentList class) to
// construct a model. 
// </motivation>

// <thrown>
// Exceptions are not directly thrown by this class
// </thrown>
//
// <todo asof="1996/09/01">
//   <li> 
// </todo>

class SkyCompRep: public SpectralModel
{
public:
  // a virtual destructor is needed so that the actual destructor in the
  // derived class will be used. The default version of this function does
  // nothing.
  virtual ~SkyCompRep();

  // return a reference to the flux of the component. Because this is a
  // reference, manipulation of the flux values is performed through the
  // functions in the Flux class. eg.,
  // <src>comp.flux().setValue(newVal)</src>. If the component Flux varies with
  // frequency then the flux set using this function is the value at the
  // reference freqeuncy. 
  // <group>
  virtual const Flux<Double> & flux() const = 0;
  virtual Flux<Double> & flux() = 0;
  // </group>

  // get the shape of the component.  The default version of this
  // function returns ComponentType::POINT
  virtual ComponentType::Shape shape() const = 0;

  // set/get the direction of the centre of component. Default versions of
  // these functions do nothing.
  // <group>
  virtual void setDirection(const MDirection & newDirection) = 0;
  virtual void direction(MDirection & compDirection) const = 0;
  // </group>

  // return the number of parameters in the component and set/get them.
  // <group>
  virtual uInt nParameters() const = 0;
  virtual void setParameters(const Vector<Double> & newParms) = 0;
  virtual void parameters(Vector<Double> & compParms) const = 0;
  // </group>

  // get the spectral type of the component. The default version of this
  // function returns ComponentType::CONSTANT_SPECTRUM
  virtual ComponentType::SpectralShape spectralShape() const;

  // set/get the reference frequency. Default versions of these functions
  // do nothing.
  // <group>
  virtual void setRefFrequency(const MFrequency & newRefFreq);
  virtual const MFrequency & refFrequency() const;
  //  virtual void refFrequency(MFrequency & refFreq) const;
  // </group>

  // Return the scaling factor used to scale the flux of the component at the
  // specified frequency. The scaling factor is always 1 at the reference
  // frequency. The default version of this function ignores the sampleFreq
  // argument and always returns 1.
  virtual Double scale(const MFrequency & sampleFreq) const;

  // Return the flux (in dimensions of W/m^2/Hz) of the component at the
  // specified frequency. The default version of this function returns the Flux
  // at the reference frequency and ignores the sampleFreq argument.
  virtual Flux<Double> sample(const MFrequency & sampleFreq) const;

  // return the number of parameters in the spectral shape and set/get them.
  // <group>
  virtual uInt nSpectralParameters() const;
  virtual void setSpectralParameters(const Vector<Double> & newParms);
  virtual void spectralParameters(Vector<Double> & compParms) const;
  // </group>

  // set/get the label associated with this component. Default versions of
  // these functions do nothing.
  // <group>
  virtual void setLabel(const String & newLabel) = 0;
  virtual void label(String & compLabel) const = 0;
  // </group>

  // Return the intensity (in Jy/pixel) of the component at the specified
  // direction. The returned Vector contains the different polarizations of the
  // radiation and the pixel size is assumed to be square.
  virtual void sample(Vector<Double> & result,
		      const MDirection & sampleDir,
		      const MVAngle & pixelSize) const = 0;

  // Project the component onto an Image. The default implementation calls the
  // sample function once for the centre of each pixel. The image needs
  // only have a one (and only one) direction axis. Other axes are optional and
  // if there is no Stokes axes then it is assumed that the polarization is
  // Stokes::I. The component is gridded equally onto all other axes of the
  // image (ie. spectral axes).
  virtual void project(ImageInterface<Float> & plane) const;

  // Return the Fourier transform of the component at the specified point in
  // the spatial frequency domain. The point is specified by a 3 element vector
  // (u,v,w) that has units of meters and the frequency of the observation, in
  // Hertz. These two quantities can be used to derive the required spatial
  // frequency <src>(s = uvw*freq/c)</src>. The w component is not used in
  // these functions.
  //
  // The reference position for the transform is the direction of the
  // component. Hence the transform is always a real value and there is no
  // phase variation due to the spatial structure if the component has a
  // symmetric shape. The returned visibility values may still be Complex if a
  // polarisation representation other than Stokes is used.
  // <group>
  virtual Flux<Double> visibility(const Vector<Double> & uvw,
				  const Double & frequency) const = 0;
  // </group>

  // This functions convert between a glish record and a SkyCompRep. This way
  // derived classes can interpret fields in the record in a class specific
  // way. These functions define how a component is represented in glish.  They
  // return False if the glish record is malformed and append an error message
  // to the supplied string giving the reason.
  // <group>
  virtual Bool fromRecord(String & errorMessage, 
			  const GlishRecord & record) = 0;
  virtual Bool toRecord(String & errorMessage, GlishRecord & record) const = 0;
  // </group>

  // Function which checks the internal data of this class for correct
  // dimensionality and consistant values. Returns True if everything is fine
  // otherwise returns False.
  virtual Bool ok() const;

  // Return a pointer to a copy of the derived object upcast to a SkyComponent
  // object. The class that uses this function is responsible for deleting the
  // pointer. This is used to implement a virtual copy constructor.
  virtual SkyCompRep * clone() const = 0;

  // These functions will at a later stage be moved into the Measures
  // module. But for now they are used by GaussianCompRep for converting
  // Quantum<Double> objects to glish records.
  static void toRecord(GlishRecord & record, const Quantum<Double> & quantity);

protected:
  //# These functions are used by derived classes implementing concrete
  //# versions of the toRecord and fromRecord member functions.
  // <group>
  Bool readFlux(String & errorMessage, const GlishRecord & record);
  Bool addFlux(String & errorMessage, GlishRecord & record) const;

  Bool readDir(String & errorMessage, const GlishRecord & record);
  Bool addDir(String & errorMessage, GlishRecord & record) const;

  Bool readLabel(String & errorMessage, const GlishRecord & record);
  Bool addLabel(String & errorMessage, GlishRecord & record) const;
  // </group>
};

#endif
