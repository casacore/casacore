//# ComponentType.h: Enumerators common to the Components Module
//# Copyright (C) 1997,1998
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

#if !defined(AIPS_COMPONENTTYPE_H)
#define AIPS_COMPONENTTYPE_H

#include <aips/aips.h>
class String;
class SkyCompRep;
class ComponentShape;
class SpectralModel;
class componentlist;

// <summary>Enumerators common to the ComponentsModels module</summary>

// <use visibility=export>

// <reviewed reviewer="" date="yyyy/mm/dd" tests="tComponentType" demos="">
// </reviewed>

// <etymology>
// This class contains the type definitions in the ComponentModels module 
// </etymology>

// <synopsis> 
// This class does nothing.  It is merely a container for the enumerations used
// by the ComponentModels module.  These enumerations define the standard
// component types. It also contains:
// <ul>
// <li> static functions which convert between these enumerators and strings.
// <li> static functions which construct the appropriate derived object given
//      an enumerator. 
// </ul>
// The first element in the enumerator must be represented by zero and every
// enumerator must contain as the second last and last elements an UNKNOWN and
// NUMBER element.
// </synopsis>

// <example>
// See the documentation for the ComponentModels classes for examples on the
// use of these enumerations.
// </example>
// <todo asof="1997/06/17">
// <li> Nothing I can think of.
// </todo>
//

class ComponentType {
public:
  // Declare which classes access the private functions which convert
  // enumerators to objects. The private construct functions can only be
  // accessed by functions in the friend classes specified below.
  friend class SkyCompRep;
  friend class componentlist;

  // The shapes of all the components
  enum Shape {
    // A simple point component
    POINT = 0,
    // A elliptical Gaussian component
    GAUSSIAN,
    // A elliptical disk component
    DISK,
    // An unknown Component
    UNKNOWN_SHAPE,
    // The number of types in this enumerator
    NUMBER_SHAPES
  };
  // The ways the Flux polarisation can be represented
  enum Polarisation {
    // The four Stokes parameters, ie I,Q,U,V 
    STOKES = 0,
    // Linear polarisation feeds ie., XX,XY,YX,YY with zero parrallactic angle
    LINEAR,
    // Circular polarisation feeds ie., RR, RL, LR, LL
    CIRCULAR,
    // An unknown Component
    UNKNOWN_POLARISATION,
    // The number of types in this enumerator
    NUMBER_POLARISATIONS
  };
  // The different functional forms for the spectral variation.
  enum SpectralShape {
    // The flux is constant at different frequencies
    CONSTANT_SPECTRUM = 0,
    // The flux varies as I = I_0 * (\nu / \nu_0)^\alpha
    SPECTRAL_INDEX,
    // an unknown spectral types
    UNKNOWN_SPECTRAL_SHAPE,
    // The number of spectral types in this enumerator
    NUMBER_SPECTRAL_SHAPES
  };
  // Convert the Shape enumerator to a string
  static String name(ComponentType::Shape shapeEnum);
  // Convert a given String to a Shape enumerator
  static ComponentType::Shape shape(const String & shapeName);

  // Convert the Polarisation enumerator to a string
  static String name(ComponentType::Polarisation fluxEnum);
  // Convert a given String to a Polarisation enumerator
  static ComponentType::Polarisation polarisation(const String & 
						  polarisationName);
  // Convert the SpectralShape enumerator to a string
  static String name(ComponentType::SpectralShape spectralEnum);
  // Convert a given String to a SpectralShape enumerator
  static ComponentType::SpectralShape spectralShape(const String &
						    spectralName);
private:

  // Convert the Shape enumerator to a shape object (upcast to the base
  // object). Returns a null pointer if the object could not be
  // constructed. This will occur is the enumerator is UNKNOWN_SHAPE or
  // NUMBER_SHAPES or there is insufficient memory. The caller of this function
  // is responsible for deleting the pointer. This function is only accessible
  // by the friend classes and should be considered an implementation detail.
  static ComponentShape * construct(ComponentType::Shape shapeEnum);

  // Convert the SpectralShape enumerator to a spectral model object (upcast to
  // the base object). Returns a null pointer if the object could not be
  // constructed. This will occur is the enumerator is UNKNOWN_SPECTRAL_SHAPE
  // or NUMBER_SPECTRAL_SHAPES or there is insufficient memory. The caller of
  // this function is responsible for deleting the pointer. This function is
  // only accessible by the friend classes and should be considered an
  // implementation detail.
  static SpectralModel * construct(ComponentType::SpectralShape spectralEnum);
};
#endif
