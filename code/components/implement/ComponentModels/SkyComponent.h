//# SkyComponent.h: this defines SkyComponent.h
//# Copyright (C) 1996,1997
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

#if defined(_AIX)
#pragma implementation ("SkyComponent.cc")
#endif

#include <aips/aips.h>
#include <aips/Utilities/CountedPtr.h>
#include <trial/ComponentModels/ComponentType.h>

class SkyCompRep;
class MDirection;
class MVAngle;
template<class T> class Vector;
template<class T> class ImageInterface;

// <summary> A component of a model of the sky </summary>

// <use visibility=export>
// <reviewed reviewer="" date="yyyy/mm/dd" tests="" demos="">
// </reviewed>

// <prerequisite> 
// <li> MDirection 
// </prerequisite>
//

// <synopsis> 

// This abstract base class is used by a number of classes that provide
// components used to represent the sky brightness. It abstracts the
// commonality between different components of a model like
// GaussianComponent, PointComponent, and perhaps in the future
// DiskComponent & SpheroidComponent. In particular it allows the user to
// sample the component at any specified position in the sky as well as grid
// the component onto a specified image.

// The functions in this class basically allow the user to sample the
// intensity of the component in by specifying either a direction (or a
// Vector of directions), or an image onto which the component should be
// projected (with the possibility of convolving with a specified point
// spread function). While all of these functions are pure virtual (see
// below for why) a default implementation is provided in which these
// functions are defined in terms of the operator()(MDirection)
// function. This is the only function which does not have a default
// implementation.

// Because of function hiding (Meyer, "Effective C++", Item 50) the default
// versions of the operator() functions provided in this class cannot be
// called directly. Hence the writer of a derived class MUST define these
// functions even though they have a default implementation. To enforce this
// at compile time these functions purely abstract (=0). The default
// implementation which can be accessed by function forwarding as is done in
// the GaussianComponent class.

// The type function returns a string stating the actual type of component
// that is used.
// </synopsis>

// <example>
// Because this is a virtual base class this example will be inside a
// function. 
// <srcblock>
// void printComponent(const SkyComponent & component){
// cout << "Component has a total flux of " << component.flux()
//      << ", is centred at " << component.position() 
//      << " and an peak intensity of " 
//      << component(component.position()) << endl;
// }
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
  // The default is a Point Component at the J2000 north pole with a flux of
  // 1Jy in the I polarization only. 
  SkyComponent();

  // Construct a SkyComponent of the specified type. The default position is
  // the J2000 north pole and the default flux is 1 Jy in the I polarisation
  // only. Use the setFlux and SetPosition functions to change this after
  // construction. 
  SkyComponent(ComponentType::Type type);
  
  // The copy Constructor uses reference semantics
  SkyComponent(const SkyComponent & other);

  // a virtual destructor is needed so that the actual destructor in derived
  // classes will be used.
  virtual ~SkyComponent();

  // The assignment operator uses reference semantics
  SkyComponent & operator=(const SkyComponent & other);

  // Return the intensity (in Jy/pixel) of the component at the specified
  // direction. The Vector contains all the polarizations (Stokes I,Q,U,V) of
  // the radiation and must be of length 4. The returned Vector contains the
  // different polarizations of the radiation and the pixel size is assumed to
  // be square.
  virtual void sample(Vector<Double> & result, 
		      const MDirection & samplePos,
		      const MVAngle & pixelSize) const;

  // Project the component onto an Image. The default implementation calls the
  // sample function once for the centre of each pixel. The image needs
  // only have a one (and only one) direction axis. Other axes are optional and
  // if there is no Stokes axes then it is assumed that the polarization is
  // Stokes::I. The component is gridded equally onto all other axes of the
  // image (ie. spectral axes).
  virtual void project(ImageInterface<Float> & plane) const;

  // set/get the integrated flux (in Jy) of the component. The Vector specifies
  // all the polarizations of the radiation.
  // <group>
  virtual void setFlux(const Vector<Double> & newFlux);
  virtual void flux(Vector<Double> & compflux) const;
  // </group>

  // set/get the position (usually the centre) of the component.
  // <group>
  virtual void setPosition(const MDirection & newPos);
  virtual void position(MDirection & compPos) const;
  // </group>

  // return the number of parameters in the component and set/get them.
  // <group>
  virtual uInt nParameters() const;
  virtual void setParameters(const Vector<Double> & newParms);
  virtual void parameters(Vector<Double> & compParms) const;
  // </group>

  // get the actual type of the component 
  // (as an ComponentTypes::ComponentTypes enum)
  virtual ComponentType::Type type() const;

  // Return a distinct copy of this component. As both the assignment operator
  // and the copy constructor use reference semantics this is the only way to
  // get a real copy.
  virtual SkyComponent copy() const;

  // Function which checks the internal data of this class for correct
  // dimensionality and consistant values. Returns True if everything is fine
  // otherwise returns False.
  virtual Bool ok() const;

protected:
  // An easy way for derived classes to initialise the base class is to use
  // this constructor. The pointer is taken over. 
  SkyComponent(SkyCompRep * rawPtr);

  // Return the polymorphic pointer that is being reference counted. This is
  // used by derived classes to cache an downcast pointer.
  SkyCompRep * rawPtr(); 
  const SkyCompRep * rawPtr() const; 

private:
  CountedPtr<SkyCompRep> theCompPtr;
};

#endif
