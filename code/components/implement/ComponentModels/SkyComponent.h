//# SkyComponent.h: this defines SkyComponent.h
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

#if !defined(AIPS_SKYCOMPONENT_H)
#define AIPS_SKYCOMPONENT_H

#include <aips/aips.h>
#include <aips/Utilities/CountedPtr.h>
#include <trial/ComponentModels/ComponentType.h>

// class RecordInterface;
// template<class T> class ImageInterface;
class RecordInterface;
class MDirection;
class MFrequency;
class MVAngle;
class SkyCompRep;
class String;
template<class T> class Vector;
template<class T> class Flux;

// <summary>A component of a model of the sky </summary>

// <use visibility=export>
// <reviewed reviewer="" date="yyyy/mm/dd" tests="" demos="">
// </reviewed>

// <prerequisite> 
// <li> MDirection 
// </prerequisite>
//

// <synopsis> 

// This base class is used by a number of classes that provide
// components used to represent the sky brightness. It abstracts the
// commonality between different components of a model like
// GaussianComponent, PointComponent, and perhaps in the future
// DiskComponent & SpheroidComponent. In particular it allows the user to
// sample the component at any specified direction in the sky as well as grid
// the component onto a specified image.

// The functions in this class basically allow the user to sample the intensity
// of the component by specifying either a direction or an image onto which the
// component should be projected. 

// The type function returns a string stating the actual type of component
// that is used.
// </synopsis>

// <example>
// <srcblock>
// </srcblock>
// </example>
//
// <motivation>
// Model fitting is an important part of astronomical data
// reduction/interpretation. This class defines a model component. Many
// components can be strung together (using the class ComponentList) to
// construct a model. 
// </motivation>

// <thrown>
// Exceptions are not directly thrown by this class
// </thrown>
//
// <todo asof="1997/05/01">
//   <li> 
// </todo>

class SkyComponent
{
public:
  // The default is a PointComponent at the J2000 north pole with a flux of
  // 1Jy in the I polarization only. 
  SkyComponent();

  // Construct a SkyComponent of the specified shape. The default direction is
  // the J2000 north pole and the default flux is 1 Jy in the I polarisation
  // only. Use the setFlux and SetDirection functions to change this after
  // construction. The spectral variation is assumed to be constant.
  SkyComponent(ComponentType::Shape shape);
  
  // Construct a SkyComponent of the specified spatial and spectral shapes. The
  // default direction is the J2000 north pole and the default flux is 1 Jy in
  // the I polarisation only. Use the setFlux and SetDirection functions to
  // change this after construction.
  SkyComponent(ComponentType::Shape shape, 
	       ComponentType::SpectralShape spectralModel);
  
  // The copy Constructor uses reference semantics
  SkyComponent(const SkyComponent & other);

  // the destructor does nothing obvious (its all done by the CountedPtr)
  ~SkyComponent();

  // The assignment operator uses reference semantics
  SkyComponent & operator=(const SkyComponent & other);

  // return a reference to the flux of the component. Because this is a
  // reference, manipulation of the flux values is performed through the
  // functions in the Flux class. eg.,
  // <src>comp.flux().setValue(newVal)</src>. If the component flux varies with
  // frequency then the flux set using this function is the value at the
  // reference frequency.
  // <group>
  Flux<Double> & flux();
  const Flux<Double> & flux() const;
  // </group>

  // get the shape of the component
  ComponentType::Shape shape() const;

  // set/get the direction of (usually the centre) of the component.
  // <group>
  void setRefDirection(const MDirection & newDirection);
  const MDirection & refDirection() const;
  // </group>

  // Calculate the flux at the specified direction, in a pixel of specified
  // size.  The frequency is assumed to be the reference frequency.
  Flux<Double> sample(const MDirection & direction,
		      const MVAngle & pixelSize) const;

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
  Flux<Double> visibility(const Vector<Double> & uvw,
			  const Double & frequency) const;

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
  // <group>
//   Flux<Double> sample(const MDirection & direction, 
// 		      const MVAngle & pixelSize, 
// 		      const MFrequency & centerFrequency) const;
//   void sample(Flux<Double> & flux, const MDirection & direction, 
// 	      const MVAngle & pixelSize, 
// 	      const MFrequency & centerFrequency) const;
  // </group>

  // set/get the label associated with this component. Default versions of
  // these functions do nothing.
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
  Bool fromRecord(String & errorMessage, const RecordInterface & record);
  Bool toRecord(String & errorMessage, RecordInterface & record) const;
  // </group>

  // Return a distinct copy of this component. As both the assignment operator
  // and the copy constructor use reference semantics this is the only way to
  // get a real copy.
  SkyComponent copy() const;

  // Project the component onto an Image. The default implementation calls the
  // sample function once for the centre of each pixel. The image needs
  // only have a one (and only one) direction axis. Other axes are optional and
  // if there is no Stokes axes then it is assumed that the polarization is
  // Stokes::I. The component is gridded equally onto all other axes of the
  // image (ie. spectral axes).
  //    void project(ImageInterface<Float> & plane) const;

  // Function which checks the internal data of this class for correct
  // dimensionality and consistant values. Returns True if everything is fine
  // otherwise returns False.
  Bool ok() const;

private:
  CountedPtr<SkyCompRep> itsCompPtr;
};
#endif
