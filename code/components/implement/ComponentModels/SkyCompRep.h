//# SkyCompRep.h: A model component of the sky brightness
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
#include <trial/ComponentModels/Flux.h>
#include <trial/ComponentModels/SkyCompBase.h>
#include <aips/Utilities/CountedPtr.h>
#include <aips/Utilities/String.h>

class ComponentShape;
class MDirection;
class MFrequency;
class MVAngle;
class RecordInterface;
class SpectralModel;
template <class T> class ImageInterface;
template <class T> class Vector;

// <summary>A model component of the sky brightness</summary>

// <use visibility=export>
// <reviewed reviewer="" date="yyyy/mm/dd" tests="tSkyCompRep" demos="">
// </reviewed>

// <prerequisite> 
// <li> <linkto class=ComponentShape>ComponentShape</linkto>
// <li> <linkto class=SpectralModel>SpectralModel</linkto>
// </prerequisite>
//
// <synopsis> 
// A SkyComp
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
// <li> convert the component to and from a record so that it can be
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

class SkyCompRep: public SkyCompBase
{
public:
  // The default SkyCompRep is a point source with a constant spectrum. See
  // the default constructors in the PointShape, ConstantSpectrum and Flux
  // classes for the default values for the flux, shape and spectrum.
  SkyCompRep();
  
  // Construct a SkyCompRep of the specified shape. The resultant component
  // has a constant spectrum and a shape given by the default constructor of
  // the specified ComponentShape class.
  SkyCompRep(const ComponentType::Shape & shape);
  
  // Construct a SkyCompRep with the user specified model for the shape and
  // spectrum. The resultant component has a shape given by the default
  // constructor of the specified ComponentShape class and a spectrum given by
  // the default constructor of the specified SpectralModel class
  SkyCompRep(const ComponentType::Shape & shape,
 	     const ComponentType::SpectralShape & spectrum);

  // Construct a SkyCompRep with a fully specified model for the shape, 
  // spectrum and flux.
  SkyCompRep(const Flux<Double> & flux,
 	     const ComponentShape & shape, 
 	     const SpectralModel & spectrum);
  
  // The copy constructor uses copy semantics
  SkyCompRep(const SkyCompRep & other);
  
  // The destructor does not appear to do much
  virtual ~SkyCompRep();

  // The assignment operator uses copy semantics.
  SkyCompRep & operator=(const SkyCompRep & other);

  // return a reference to the flux of the component. Because this is a
  // reference, manipulation of the flux values is performed through the
  // functions in the Flux class. eg.,
  // <src>comp.flux().setValue(newVal)</src>.
  // <group>
  virtual const Flux<Double> & flux() const;
  virtual Flux<Double> & flux();
  // </group>

  // return a reference to the shape of the component. Because this is a
  // reference, manipulation of the shape of the component is performed through
  // the functions in the ComponentShape (or derived) class. eg.,
  // <src>comp.shape().setRefDirection(newVal)</src>.
  // <group>
  virtual const ComponentShape & shape() const;
  virtual ComponentShape & shape();
  // </group>
  
  // return a reference to the spectrum of the component. Because this is a
  // reference, manipulation of the spectrum of the component is performed
  // through the functions in the SpectralModel (or derived) class. eg.,
  // <src>refFreq = comp.spectrum().refFrequency()</src>.
  // <group>
  virtual const SpectralModel & spectrum() const;
  virtual SpectralModel & spectrum();
  // </group>
  
  // Calculate the flux at the specified direction & frequency, in a pixel of
  // specified size.
  virtual Flux<Double> sample(const MDirection & direction, 
			      const MVAngle & pixelSize, 
			      const MFrequency & centerFrequency) const;

  // Project the component onto an Image. The default implementation calls the
  // sample function once for the centre of each pixel. The image needs to have
  // one (and only one) direction axis. Other axes are optional and if there is
  // no Stokes axes then it is assumed that the polarization is Stokes::I. If
  // there is no frequency axis then the frequency is assumed to be the
  // reference frequency ie., spectrum().refFrequency().
  virtual void project(ImageInterface<Float> & plane) const;

  // Return the Fourier transform of the component at the specified point in
  // the spatial frequency domain. The point is specified by a 3-element vector
  // (u,v,w) that has units of meters and the frequency of the observation, in
  // Hertz. These two quantities can be used to derive the required spatial
  // frequency <src>(s = uvw*freq/c)</src>. The w component is not used in
  // these functions.

  // The "origin" of the transform is the reference direction of the
  // component. This means, for symmetric components where the reference
  // direction is at the centre, that the Fourier transform will always be
  // real.
  virtual Flux<Double> visibility(const Vector<Double> & uvw,
				  const Double & frequency) const;

  // return a reference to the label associated with this component. The label
  // is a text string for general use.
  // <group>
  virtual String & label();
  virtual const String & label() const;
  // </group>

  // This functions convert between a record and a component.  Derived classes
  // can interpret fields in the record in a class specific way. These
  // functions define how a component is represented in glish.  They return
  // False if the record is malformed and append an error message to the
  // supplied string giving the reason.
  // <group>
  virtual Bool fromRecord(String & errorMessage, 
			  const RecordInterface & record);
  virtual Bool toRecord(String & errorMessage, 
			RecordInterface & record) const;
  // </group>

  // Function which checks the internal data of this class for correct
  // dimensionality and consistant values. Returns True if everything is fine
  // otherwise returns False.
  virtual Bool ok() const;

private:
  void initShape(ComponentType::Shape shape);
  void initSpectrum(ComponentType::SpectralShape spectrum);

  CountedPtr<ComponentShape> itsShapePtr;
  CountedPtr<SpectralModel> itsSpectrumPtr;
  Flux<Double> itsFlux;
  String itsLabel;
};
#endif
