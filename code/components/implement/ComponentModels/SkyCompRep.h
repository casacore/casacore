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
#include <trial/ComponentModels/ComponentShape.h>
#include <trial/ComponentModels/SpectralModel.h>
#include <trial/ComponentModels/Flux.h>
#include <aips/Utilities/String.h>
#include <aips/Utilities/CountedPtr.h>

class MDirection;
class MFrequency;
class MVAngle;
class RecordInterface;
template <class T> class Vector;
// template <class T> class ImageInterface;

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

class SkyCompRep: public RecordTransformable
{
public:

  // The default SkyCompRep is a point source with a constant spectrum. See the
  // default constructors in the PointShape, ConstantSpectrum and Flux classes
  // for the default values for the flux, shape and spectrum.
  SkyCompRep();
  
  // Construct a SkyCompRep with the user specified model for the shape and
  // spectrum.
  SkyCompRep(const ComponentType::Shape & shape,
 	     const ComponentType::SpectralShape & spectrum);

  // Construct a SkyCompRep with a fully specified model for the shape and
  // spectrum and Flux.
  SkyCompRep(const Flux<Double> & flux,
 	     const ComponentShape & shape, 
 	     const SpectralModel & spectrum);
  
  // The copy constructor uses copy semantics
  SkyCompRep(const SkyCompRep & other);
  
  ~SkyCompRep();

  // The assignment operator uses copy semantics.
  SkyCompRep & operator=(const SkyCompRep & other);

  // return a reference to the flux of the component. Because this is a
  // reference, manipulation of the flux values is performed through the
  // functions in the Flux class. eg.,
  // <src>comp.flux().setValue(newVal)</src>. If the component flux varies with
  // frequency then the flux set using this function is the value at the
  // reference frequency.
  // <group>
  const Flux<Double> & flux() const;
  Flux<Double> & flux();
  // </group>

  // get the shape of the component.
  ComponentType::Shape shape() const;

  // set/get the reference direction of the component
  // <group>
  void setRefDirection(const MDirection & newDirection);
  const MDirection & refDirection() const;
  // </group>

  // Calculate the flux at the specified direction, in a pixel of specified
  // size.  The frequency is assumed to be the reference frequency.
  // <group>
  Flux<Double> sample(const MDirection & direction,
		      const MVAngle & pixelSize) const;
  // </group>

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
  // <group>
  Flux<Double> visibility(const Vector<Double> & uvw,
			  const Double & frequency) const;
  // </group>

  // return the number of shape parameters in the component and set/get them.
  // <group>
  uInt nShapeParameters() const;
  void setShapeParameters(const Vector<Double> & newParms);
  void shapeParameters(Vector<Double> & compParms) const;
  // </group>

  // get the spectral type of the component.
  ComponentType::SpectralShape spectralShape() const;

  // set/get the reference frequency.
  // <group>
  void setRefFrequency(const MFrequency & newFrequency);
  const MFrequency & refFrequency() const;
  // </group>

  // Calculate the flux at the specified frequency. The direction is assumed to
  // be the reference direction.
  Flux<Double> sample(const MFrequency & centerFrequency) const;

  // return the number of parameters in the spectral shape and set/get them.
  // <group>
  uInt nSpectralParameters() const;
  void setSpectralParameters(const Vector<Double> & newParms);
  void spectralParameters(Vector<Double> & compParms) const;
  // </group>

  // Calculate the flux at the specified direction & frequency, in a pixel of
  // specified size.
  Flux<Double> sample(const MDirection & direction, 
		      const MVAngle & pixelSize, 
		      const MFrequency & centerFrequency) const;

  // set/get the label associated with this component. The label is a simple
  // string for general use.
  // <group>
  void setLabel(const String & newLabel);
  const String & label() const;
  // </group>

  // This functions convert between a record and a SkyCompRep. This way derived
  // classes can interpret fields in the record in a class specific way. These
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

  //# Project the component onto an Image. The default implementation calls the
  //# sample function once for the centre of each pixel. The image needs
  //# only have a one (and only one) direction axis. Other axes are optionaland
  //# if there is no Stokes axes then it is assumed that the polarization is
  //# Stokes::I. The component is gridded equally onto all other axes of the
  //# image (ie. spectral axes).
  //# void project(ImageInterface<Float> & plane) const;

private:
  CountedPtr<ComponentShape> itsShapePtr;
  CountedPtr<SpectralModel> itsSpectrumPtr;
  Flux<Double> itsFlux;
  String itsLabel;
};
#endif
