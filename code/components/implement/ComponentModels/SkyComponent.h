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
#include <trial/MeasurementEquations/StokesVector.h>
#include <aips/Measures/MDirection.h>

class String;
template<class T> class ImageInterface;
template<class T> class Vector;

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
// <todo asof="1996/09/01">
//   <li> Should this class be derived from Functional<MDirection, Double>? 
// </todo>

class SkyComponent
{
public:
  virtual ~SkyComponent();

  // Return the intensity (in Jy/pixel) of the component at the specified
  // direction.
  virtual StokesVector operator()(const MDirection & samplePos) const = 0;

  // Return the intensity (in Jy/pixel) of the component at the specified
  // directions.  Default implementation calls the scalar sample function
  // iteratively.
  virtual Vector<StokesVector> operator()(const Vector<MDirection> & samplePos)
                                   const = 0;

  // Project the component onto an Image. The default implementation calls
  // the operator() function once for the centre of each pixel. The image
  // MUST have at least three axes (although any one can be degenerate), and
  // the third axis must have four or fewer elements as this is the "Stokes"
  // or polarisation axis. If there are fewer elements than four elemnts
  // then only the necessary polarisation terms are sampled (order is
  // I,Q,U,V)
  virtual void operator()(ImageInterface<Float> & plane) const = 0;

  // Project the component onto an Image and convolve with the psf. The
  // default implementation uses the above routine to project the image
  // then does the convolution separately. However this is not satisfactory
  // for PointComponents or other very small components. If dimensionality
  // of the psf is less than the dimensionality of the plane then the
  // convolution will be done successively over all requied axes. Eg. A two
  // dimensional psf will be successively convolved over each of the
  // polarisation components of a three dimensional plane. 
  virtual void operator()(ImageInterface<Float> & plane,
			  const ImageInterface<Float> & psf) const = 0;

  // set/get the integrated flux (in Jy) of the component
  virtual void setFlux( StokesVector newFlux) = 0;
  virtual StokesVector flux() const = 0;

  // set/get the position (usually the centre) of the component. 
  virtual void setPosition(const MDirection & newPos) = 0;
  virtual MDirection position() const = 0;

  // get the actual type of the component (as a string)
  virtual const String & type() const = 0;				   
};

#endif
