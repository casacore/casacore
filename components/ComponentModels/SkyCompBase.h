//# SkyCompBase.h: Base class for model components of the sky brightness
//# Copyright (C) 1996,1997,1998,1999,2000,2003
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

#ifndef COMPONENTS_SKYCOMPBASE_H
#define COMPONENTS_SKYCOMPBASE_H

#include <casa/aips.h>
#include <components/ComponentModels/ComponentType.h>
#include <casa/Utilities/RecordTransformable.h>
#include <casa/BasicSL/Complexfwd.h>

namespace casa { //# NAMESPACE CASA - BEGIN

class MFrequency;
class MVFrequency;
class MDirection;
class MVDirection;
class MVAngle;
class RecordInterface;
class String;
class ComponentShape;
class SpectralModel;
class Unit;
template <class T> class Flux;
template <class T> class Vector;
template <class T> class Matrix;
template <class T> class Cube;
template <class Ms> class MeasRef;

// <summary>Base class for model components of the sky brightness</summary>

// <use visibility=export>
// <reviewed reviewer="" date="yyyy/mm/dd" tests="" demos="">
// </reviewed>

// <prerequisite> 
// <li> <linkto class=Flux>Flux</linkto>
// <li> <linkto class=ComponentShape>ComponentShape</linkto>
// <li> <linkto class=SpectralModel>SpectralModel</linkto>
// </prerequisite>
//

// <synopsis> 
// This abstract base class defines the interface for classes that model the
// sky brightness.

// A model of the sky brightness is defined by three properties.
// <dl>
// <dt><em>A Flux</em>
// <dd> This is the integrated brightness of the component.
// <dt><em>A Shape</em>
// <dd> This defines how the sky brightness varies as a function of position on
//      the sky. Currently two shapes are supported, 
//      <linkto class=PointShape>points</linkto> and
//      <linkto class=GaussianShape>Gaussians</linkto>.
// <dt><em>A Spectrum</em>
// <dd> This defines how the component flux varies as a function of frequency.
//      Currently two spectral models are supported. The simplest assumes the
//      spectrum is <linkto class=ConstantSpectrum>constant</linkto> with 
//      frequency. Alternatively a 
//      <linkto class=SpectralIndex>spectral index</linkto> model can be used. 
// </dl>

// These three properties of a component can be obtained using the
// <src>flux</src>, <src>shape</src> or <src>spectrum</src> functions defined
// in this interface. Each of these properties is represented by an object that
// contains functions for manipulating the parameters associated with that
// property. eg. to set the direction of the component you would use:
// <srcblock> SkyComponent comp; comp.shape().setRefDirection(newDirection);
// </srcblock> See the <linkto class=Flux>Flux</linkto>, 
// <linkto class=ComponentShape>ComponentShape</linkto> or 
// <linkto class=SpectralModel>SpectralModel</linkto> classes for more
// information on these properties and how to manipulate them.

// Besides these three properties the <src>label</src> functions are provided
// to associate a text string with the component.

// A model of the sky brightness is by itself not very useful unless you can do
// something with it. This class contains functions for deriving information
// from the components. These functions are:
// <dl>
// <dt><src>sample</src>
// <dd> This function will return the the flux in an specified pixel, at a
//      specified direction at a specified frequency.
// <dt><src>project</src>
// <dd> This function will generate an image of the component, given a user
//      specified ImageInterface object.
// <dt><src>visibility</src>
// <dd> This function will return the visibility (spatial coherence) that would
//      be measured if the component was at the field centre of an
//      interferometer with a specified (u,v,w) coordinates and observation
//      frequency. 
// </dl>

// The <src>toRecord</src> & <src>fromRecord</src> functions are used to
// convert between a SkyCompBase object and a record representation. This is
// primarily so that a component can be represented in Glish. 

// </synopsis>

// <example>
// Because SpectralModel is an abstract base class, an actual instance of this
// class cannot be constructed. However the interface it defines can be used
// inside a function. This is always recommended as it allows functions which
// have SkyCompBase's as arguments to work for any derived class.
// These examples are coded in the dSkyCompBase.cc file.
// <h4>Example 1:</h4>
// In this example the printComp function prints out the some basic information
// on the spatial, spectral and flux properties of the component.
// <srcblock>
// void printComponent(const SkyCompBase & comp) {
//  cout << "This component has a flux of " 
//       << comp.flux().value() 
//       << " " << comp.flux().unit().getName() << endl;
//  cout << "and a " << ComponentType::name(comp.flux().pol()) 
//       << " polarisation" << endl;
//  cout << "This component has a " 
//       << ComponentType::name(comp.shape().type()) << " shape" << endl;
//  cout << "with a reference direction of " 
//       << comp.shape().refDirection().getAngle("deg") << endl;
//  cout << "This component has a " 
//       << ComponentType::name(comp.spectrum().type()) << " spectrum" << endl;
//  cout << "with a reference frequency of " 
//       << comp.spectrum().refFrequency().get("GHz") << endl;
// }
// </srcblock>
// </example>
//
// <motivation>
// I wanted to force the interfaces of the SkyCompRep and the SkyComponent
// classes to be the same. The best way I found was the introduction of a
// base class that these other classes would derive from. 
// </motivation>

// <todo asof="1998/05/20">
//   <li> Nothing I hope!
// </todo>

class SkyCompBase: public RecordTransformable
{
public:

  // The destructor does not anything
  virtual ~SkyCompBase();

  // return a reference to the flux of the component. Because this is a
  // reference, manipulation of the flux values is performed through the
  // functions in the Flux class. eg.,
  // <src>comp.flux().setValue(newVal)</src>. If the component flux varies with
  // frequency then the flux set using this function is the value at the
  // reference frequency.
  // <group>
  virtual const Flux<Double>& flux() const = 0;
  virtual Flux<Double>& flux() = 0;
  // </group>

  // return a reference to the shape of the component. Because this is a
  // reference, manipulation of the shape of the component is performed through
  // the functions in the ComponentShape (or derived) class. eg.,
  // <src>comp.shape().setRefDirection(newVal)</src>. To change the shape to a
  // different type you must use the <src>setShape</src> function.
  // <group>
  virtual const ComponentShape& shape() const = 0;
  virtual ComponentShape& shape() = 0;
  virtual void setShape(const ComponentShape& newShape) = 0;
  // </group>
  
  // return a reference to the spectrum of the component. Because this is a
  // reference, manipulation of the spectrum of the component is performed
  // through the functions in the SpectralModel (or derived) class. eg.,
  // <src>refFreq = comp.spectrum().refFrequency()</src>. Touse a different
  // spectral model you must use the <src>setSpectrum</src> function.
  // <group>
  virtual const SpectralModel& spectrum() const = 0;
  virtual SpectralModel& spectrum() = 0;
  virtual void setSpectrum(const SpectralModel& newSpectrum) = 0;
  // </group>
  
  // return a reference to the label associated with this component. The label
  // is a text string for general use.
  // <group>
  virtual String& label() = 0;
  virtual const String& label() const = 0;
  // </group>

  // Return True if the component parameters are physically plausable. This
  // checks that I, Q, U, & V are all real numbers and if 
  // I^2 >= Q^2 + U^2 + U^2
  virtual Bool isPhysical() const = 0;
  
  // Calculate the flux at the specified direction & frequency, in a pixel of
  // specified x & y size.
  virtual Flux<Double> sample(const MDirection& direction, 
			      const MVAngle& pixelLatSize, 
			      const MVAngle& pixelLongSize, 
			      const MFrequency& centerFrequency) const = 0;

  // Same as the previous function except that many directions & frequencies
  // are done at once.  The flux is added into the values supplied in the
  // samples argument and this cube must have dimensions of [4, nDirs,
  // nFreqs]. The polarisations are always [I, Q, U, V] and units of the flux
  // added are specified with the reqUnits arguments.
  virtual void sample(Cube<Double>& samples,
		      const Unit& reqUnit,
		      const Vector<MVDirection>& directions, 
		      const MeasRef<MDirection>& dirRef, 
		      const MVAngle& pixelLatSize, 
		      const MVAngle& pixelLongSize, 
		      const Vector<MVFrequency>& frequencies,
		      const MeasRef<MFrequency>& freqRef) const = 0;

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
  virtual Flux<Double> visibility(const Vector<Double>& uvw,
				  const Double& frequency) const = 0;

  // Same as the previous function except that many (u,v,w) points are done at
  // once. The visibilities are returned in the first argument which must have
  // dimensions of [4, nChan, nVis]. The points to sample are specified in the
  // second argument which must have dimensions of [3, nVis], and the
  // frequencies to sample are specified by the third argument which must have
  // a length of nChan. The units and polarisation of the returned visibilities
  // are the same as the flux of this object, and can be queried using the
  // <src>flux().units()</src> & <src>flux().pol()</src> functions.
  virtual void visibility(Cube<DComplex>& visibilities,
			  const Matrix<Double>& uvws,
			  const Vector<Double>& frequencies) const = 0;

  // This functions convert between a record and a component.  Derived classes
  // can interpret fields in the record in a class specific way. These
  // functions define how a component is represented in glish.  They return
  // False if the record is malformed and append an error message to the
  // supplied string giving the reason.
  // <group>
  virtual Bool fromRecord(String& errorMessage,
			  const RecordInterface& record) = 0;
  virtual Bool toRecord(String& errorMessage, 
			RecordInterface& record) const = 0;
  // </group>

  // Function which checks the internal data of this class for correct
  // dimensionality and consistant values. Returns True if everything is fine
  // otherwise returns False.
  virtual Bool ok() const = 0;
};

} //# NAMESPACE CASA - END

#endif
