//# SkyCompBase.h: this defines SkyCompBase.h
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

#if !defined(AIPS_SKYCOMPBASE_H)
#define AIPS_SKYCOMPBASE_H

#include <aips/aips.h>
#include <trial/ComponentModels/ComponentType.h>
#include <aips/Utilities/RecordTransformable.h>

class MDirection;
class MFrequency;
class MVAngle;
class RecordInterface;
class String;
class ComponentShape;
class SpectralModel;
template <class T> class Flux;
template <class T> class Vector;
template <class T> class ImageInterface;

// <summary>A model component of the sky brightness</summary>

// <use visibility=export>
// <reviewed reviewer="" date="yyyy/mm/dd" tests="" demos="">
// </reviewed>

// <prerequisite> 
// <li> <linkto class=MDirection>MDirection</linkto>
// </prerequisite>
//

// <synopsis> 


// </synopsis>

// <example>
// <srcblock>
// </srcblock>
// </example>
//
// <motivation>
// </motivation>

// <thrown>
// </thrown>
//
// <todo asof="yyyy/mm/dd">
//   <li> 
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
  virtual const Flux<Double> & flux() const = 0;
  virtual Flux<Double> & flux() = 0;
  // </group>

  // return a reference to the shape of the component. Because this is a
  // reference, manipulation of the shape of the component is performed through
  // the functions in the ComponentShape (or derived) class. eg.,
  // <src>comp.shape().setRefDirection(newVal)</src>.
  // <group>
  virtual const ComponentShape & shape() const = 0;
  virtual ComponentShape & shape() = 0;
  // </group>
  
  // return a reference to the spectrum of the component. Because this is a
  // reference, manipulation of the spectrum of the component is performed
  // through the functions in the SpectralModel (or derived) class. eg.,
  // <src>refFreq = comp.spectrum().refFrequency()</src>.
  // <group>
  virtual const SpectralModel & spectrum() const = 0;
  virtual SpectralModel & spectrum() = 0;
  // </group>
  
  // Calculate the flux at the specified direction & frequency, in a pixel of
  // specified size.
  virtual Flux<Double> sample(const MDirection & direction, 
			      const MVAngle & pixelSize, 
			      const MFrequency & centerFrequency) const = 0;

  // Project the component onto an Image. The default implementation calls the
  // sample function once for the centre of each pixel. The image needs to have
  // one (and only one) direction axis. Other axes are optional and if there is
  // no Stokes axes then it is assumed that the polarization is Stokes::I. If
  // there is no frequency axis then the frequency is assumed to be the
  // reference frequency ie., spectrum().refFrequency().
  virtual void project(ImageInterface<Float> & plane) const = 0;

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
				  const Double & frequency) const = 0;

  // set/get the label associated with this component. The label is a simple
  // string for general use.
  // <group>
  virtual void setLabel(const String & newLabel) = 0;
  virtual const String & label() const = 0;
  // </group>

  // This functions convert between a record and a component.  Derived classes
  // can interpret fields in the record in a class specific way. These
  // functions define how a component is represented in glish.  They return
  // False if the record is malformed and append an error message to the
  // supplied string giving the reason.
  // <group>
  virtual Bool fromRecord(String & errorMessage, 
			  const RecordInterface & record) = 0;
  virtual Bool toRecord(String & errorMessage, 
			RecordInterface & record) const = 0;
  // </group>

  // Function which checks the internal data of this class for correct
  // dimensionality and consistant values. Returns True if everything is fine
  // otherwise returns False.
  virtual Bool ok() const = 0;
};
#endif
