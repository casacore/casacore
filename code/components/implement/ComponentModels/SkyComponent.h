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
#include <trial/ComponentModels/SkyCompBase.h>

class ComponentShape;
class MDirection;
class MFrequency;
class MVAngle;
class RecordInterface;
class SkyCompRep;
class SpectralModel;
class String;
template<class T> class ImageInterface;
template<class T> class Flux;
template<class T> class Vector;

// <summary>A component of a model of the sky </summary>

// <use visibility=export>
// <reviewed reviewer="" date="yyyy/mm/dd" tests="" demos="">
// </reviewed>

// <prerequisite> 
// <li> <linkto class=Flux>Flux</linkto>
// <li> <linkto class=ComponentShape>ComponentShape</linkto>
// <li> <linkto class=SpectralModel>SpectralModel</linkto>
// </prerequisite>

// <synopsis> 

// This class is concrete implementation of a class that represents a component
// of a model of the sky brightness. 

// The base class (<linkto class="SkyCompBase">SkyCompBase</linkto>) contains a
// description of components and all the member functions used to manipulate
// them and hence will not be discussed here. But the base class does not
// include any constructors or a description of the copy semantics. This will
// be discussed below.

// A SkyComponent is an "envelope" class in the sense that it can contain one
// of a variety of different component shapes and spectral models. It is
// necessary to specify the which shape and spectral model you want at
// construction time. This can be done either with enumerators or by
// constructing the classes derived from ComponentShape & SpectralModel and
// supplying them as construction arguments.

// This class uses reference semantics for both the copy constructor and the
// assignment operator. Becuase of this the only way to make a true copy of a
// SkyComponent is to use the <src>copy</src> member function.

// </synopsis>

// <example>
// These examples are coded in the tSkyCompRep.h file.
// <h4>Example 1:</h4>
// In this example a SkyCompRep object is created and used to calculate the
// ...
// <srcblock>
// </srcblock>
// </example>
//
// <motivation>
// Model fitting is an important part of astronomical data
// reduction/interpretation. This class defines a model component. Many
// components can be strung together (using the ComponentList class) to
// construct a model. It is expected that this class will eventually allow you
// to solve for parameters of the model.
// </motivation>

// <thrown>
// <li> AipsError - If an internal inconsistancy is detected, when compiled in 
// debug mode only.
// </thrown>
//
// <todo asof="1998/05/22">
//   <li> Add time variability
//   <li> Add the ability to solve for component parameters.
// </todo>

// <linkfrom anchor="SkyComponent" classes="SkyCompRep SkyCompBase">
//  <here>SkyComponent</here> - Models the sky brightness (reference semantics)
// </linkfrom>
 
class SkyComponent: public SkyCompBase
{
public:
  // The default SkyComponent is a point source with a constant spectrum. See
  // the default constructors in the PointShape, ConstantSpectrum and Flux
  // classes for the default values for the flux, shape and spectrum.
  SkyComponent();

  // Construct a SkyCompRep of the specified shape. The resultant component
  // has a constant spectrum and a shape given by the default constructor of
  // the specified ComponentShape class.
  SkyComponent(const ComponentType::Shape & shape);
  
  // Construct a SkyCompRep with the user specified model for the shape and
  // spectrum. The resultant component has a shape given by the default
  // constructor of the specified ComponentShape class and a spectrum given by
  // the default constructor of the specified SpectralModel class
  SkyComponent(const ComponentType::Shape & shape, 
	       const ComponentType::SpectralShape & spectralModel);
  
  // Construct a SkyComponent with a fully specified model for the shape, 
  // spectrum and flux.
  SkyComponent(const Flux<Double> & flux,
	       const ComponentShape & shape, 
	       const SpectralModel & spectrum);

  // The copy Constructor uses reference semantics
  SkyComponent(const SkyComponent & other);

  // the destructor does nothing obvious (its all done by the CountedPtr)
  virtual ~SkyComponent();

  // The assignment operator uses reference semantics
  SkyComponent & operator=(const SkyComponent & other);

  // See the corresponding functions in the
  // <linkto class="SkyCompBase">SkyCompBase</linkto>
  // class for a description of these functions.
  // <group>
  virtual Flux<Double> & flux();
  virtual const Flux<Double> & flux() const;
  // </group>

  // See the corresponding functions in the
  // <linkto class="SkyCompBase">SkyCompBase</linkto>
  // class for a description of these functions.
  // <group>
  virtual const ComponentShape & shape() const;
  virtual ComponentShape & shape();
  virtual void setShape(const ComponentShape & newShape);
  // </group>
  
  // See the corresponding functions in the
  // <linkto class="SkyCompBase">SkyCompBase</linkto>
  // class for a description of these functions.
  // <group>
  virtual const SpectralModel & spectrum() const;
  virtual SpectralModel & spectrum();
  virtual void setSpectrum(const SpectralModel & newSpectrum);
  // </group>
  
  // See the corresponding functions in the
  // <linkto class="SkyCompBase">SkyCompBase</linkto>
  // class for a description of these functions.
  // <group>
  virtual String & label();
  virtual const String & label() const;
  // </group>

  // See the corresponding functions in the
  // <linkto class="SkyCompBase">SkyCompBase</linkto>
  // class for a description of this function.
  virtual Flux<Double> sample(const MDirection & direction, 
			      const MVAngle & pixelSize, 
			      const MFrequency & centerFrequency) const;

  // See the corresponding functions in the
  // <linkto class="SkyCompBase">SkyCompBase</linkto>
  // class for a description of this function.
  virtual void project(ImageInterface<Float> & plane) const;

  // See the corresponding functions in the
  // <linkto class="SkyCompBase">SkyCompBase</linkto>
  // class for a description of this function.
  virtual Flux<Double> visibility(const Vector<Double> & uvw,
				  const Double & frequency) const;

  // See the corresponding functions in the
  // <linkto class="SkyCompBase">SkyCompBase</linkto>
  // class for a description of these functions.
  // <group>
  virtual Bool fromRecord(String & errorMessage, 
			  const RecordInterface & record);
  virtual Bool toRecord(String & errorMessage, 
			RecordInterface & record) const;
  // </group>

  // Return a distinct copy of this component. As both the assignment operator
  // and the copy constructor use reference semantics this is the only way to
  // get a real copy.
  SkyComponent copy() const;

  // See the corresponding functions in the
  // <linkto class="SkyCompBase">SkyCompBase</linkto>
  // class for a description of this function.
  Bool ok() const;

private:
  CountedPtr<SkyCompRep> itsCompPtr;
};
#endif
