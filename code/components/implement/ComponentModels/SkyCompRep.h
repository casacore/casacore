//# SkyCompRep.h: this defines SkyCompRep.h
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

#if !defined(AIPS_SKYCOMPREP_H)
#define AIPS_SKYCOMPREP_H

#if defined(_AIX)
#pragma implementation ("SkyCompRep.cc")
#endif

#include <aips/aips.h>
#include <trial/ComponentModels/ComponentType.h>

class GlishRecord;
class MDirection;
class MVAngle;
class String;
template<class T> class ImageInterface;
template<class T> class Quantum;
template<class T> class Vector;

// <summary>A component of a model of the sky</summary>

// <use visibility=export>
// <reviewed reviewer="" date="yyyy/mm/dd" tests="" demos="">
// </reviewed>

// <prerequisite> 
// <li> <linkto class=MDirection>MDirection</linkto>
// </prerequisite>
//

// <synopsis> 

// This abstract base class is used by a number of classes that provide
// components used to represent the sky brightness. It abstracts the
// commonality between different components of a model like GaussianCompRep,
// PointCompRep, and perhaps in the future DiskCompRep & SpheroidCompRep. In
// particular it allows the user to sample the component at any specified
// direction in the sky as well as grid the component onto a specified image.

// The functions in this class allow the user to sample the intensity of the
// component by specifying either a direction, or an image onto which the
// component should be projected. While most of these functions are pure
// virtual a default implementation is provided for the project function
// uses the sample function to get the intensity at the centre of each pixel.

// </synopsis>

// <example>
// Because this is a virtual base class this example will be inside a
// function. 
// <srcblock>
// void printComponent(const SkyCompRep & component){
//   Vector<Double> compFlux;
//   component.flux(compFlux);
//   cout << "Component has a total flux of " << compFlux;
//   MDirection compDir;
//   component.direction(compDir);
//   cout << ", is centred at " << compDir;
//   Vector<Double> peak;
//   component.sample(peak, compDir);
//   cout << " and an peak intensity of " << peak << " Jy/pixel" << endl;
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

class SkyCompRep
{
public:
  // a virtual destructor is needed so that the actual destructor in the
  // derived class will be used.
  virtual ~SkyCompRep();

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

  // set/get the integrated flux (in Jy) of the component. The Vector specifies
  // all the polarizations of the radiation.
  // <group>
  virtual void setFlux(const Vector<Double> & newFlux) = 0;
  virtual void flux(Vector<Double> & compflux) const = 0;
  // </group>

  // set/get the direction (usually the centre) of the component.
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

  // get the actual type of the component 
  // (as an ComponentTypes::ComponentTypes enum)
  virtual ComponentType::Type type() const = 0;

  // This functions convert between a glish record and a SkyCompRep. This way
  // derived classes can interpret fields in the record in a class specific
  // way. These functions define how a component is represented in glish. The
  // fromRecord function appends a message to the errorMessage string if the
  // conversion failed for any reason.
  // <group>
  virtual void fromRecord(String & errorMessage, 
			  const GlishRecord & record) = 0;
  virtual void toRecord(GlishRecord & record) const = 0;
  // </group>

  // Function which checks the internal data of this class for correct
  // dimensionality and consistant values. Returns True if everything is fine
  // otherwise returns False.
  virtual Bool ok() const;

  // Return a pointer to a copy of the derived object upcast to a SkyComponent
  // object. The class that uses this function is responsible for deleting the
  // pointer. This is used to implement a virtual copy constructor.
  virtual SkyCompRep * clone() const = 0;

protected:
  // These functions will at a later stage be moved into the Measures
  // module. But for now they are used by derived classes implementing concrete
  // versions of the toRecord and fromRecord member functions.
  // <group>
  static void fromRecord(Quantum<Double> & quantity, String & errorMessage,
			 const GlishRecord & record);
  static void toRecord(GlishRecord & record, const Quantum<Double> & quantity);
  static void fromRecord(MDirection & direction, String & errorMessage, 
			 const GlishRecord & record);
  static void toRecord(GlishRecord & record, const MDirection & direction);
  // </group>

  // These functions are also used by derived classes implementing concrete
  // versions of the toRecord and fromRecord member functions. But they will
  // always remain here as they are quite specific to the components module.
  // <group>
  static void readFlux(Vector<Double> & flux, String & errorMessage,
		       const GlishRecord & record);
  void addFlux(GlishRecord & record) const;

  void readParameters(Vector<Double> & parameters, String & errorMessage,
		      const GlishRecord & record) const;
  void addParameters(GlishRecord & record) const;
  // </group>
};

#endif
