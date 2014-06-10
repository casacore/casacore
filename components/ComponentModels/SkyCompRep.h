//# SkyCompRep.h: A model component of the sky brightness
//# Copyright (C) 1996,1997,1998,1999,2000,2001,2002
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

#ifndef COMPONENTS_SKYCOMPREP_H
#define COMPONENTS_SKYCOMPREP_H

#include <casa/aips.h>
#include <components/ComponentModels/ComponentType.h>
#include <components/ComponentModels/Flux.h>
#include <components/ComponentModels/SkyCompBase.h>
#include <casa/Utilities/CountedPtr.h>
#include <casa/BasicSL/String.h>
#include <measures/Measures/Stokes.h>

namespace casa { //# NAMESPACE CASA - BEGIN

class ComponentShape;
class CoordinateSystem;
class DirectionCoordinate;
class LogIO;
class MDirection;
class MFrequency;
class MVAngle;
class MVDirection;
class MVFrequency;
class RecordInterface;
class SpectralModel;
class TwoSidedShape;
class Unit;
class GaussianBeam;
template <class Ms> class MeasRef;
template <class T> class Cube;
template <class T> class Vector;
template <class T> class Quantum;



// <summary>A model component of the sky brightness</summary>

// <use visibility=export>
// <reviewed reviewer="" date="yyyy/mm/dd" tests="tSkyCompRep" demos="">
// </reviewed>

// <prerequisite> 
// <li> <linkto class=Flux>Flux</linkto>
// <li> <linkto class=ComponentShape>ComponentShape</linkto>
// <li> <linkto class=SpectralModel>SpectralModel</linkto>
// </prerequisite>
//
// <synopsis> 

// This class is concrete implementation of a class that represents a component
// of a model of the sky brightness. 

// The base class (<linkto class="SkyCompBase">SkyCompBase</linkto>) contains a
// description of components and all the member functions used to manipulate
// them and hence will not be discussed here. But the base class does not
// include any constructors or a description of the copy semantics. This will
// be discussed below.

// A SkyCompRep is an "envelope" class in the sense that it can contain one of
// a variety of different component shapes and spectral models. It is necessary
// to specify the which shape and spectral model you want at construction
// time. This can be done either with enumerators or by constructing the
// classes derived from ComponentShape & SpectralModel and supplying them as
// construction arguments.

// This class uses copy semantics for both the copy constructor and the
// assignment operator. 

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

// <linkfrom anchor="SkyCompRep" classes="SkyComponent SkyCompBase">
//  <here>SkyCompRep</here> - Models the sky brightness (copy semantics)
// </linkfrom>
 
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
  // <thrown>
  // <li> AipsError - if the shape is UNKNOWN_SHAPE or NUMBER_SHAPES
  // </thrown>
  SkyCompRep(const ComponentType::Shape& shape);
  
  // Construct a SkyCompRep with the user specified model for the shape and
  // spectrum. The resultant component has a shape given by the default
  // constructor of the specified ComponentShape class and a spectrum given by
  // the default constructor of the specified SpectralModel class
  // <thrown>
  // <li> AipsError - if the shape is UNKNOWN_SHAPE or NUMBER_SHAPES
  // <li> AipsError - if the spectrum is UNKNOWN_SPECTRAL_SHAPE or
  //                  NUMBER_SPECTRAL_SHAPES
  // </thrown>
  SkyCompRep(const ComponentType::Shape& shape,
 	     const ComponentType::SpectralShape& spectrum);

  // Construct a SkyCompRep with a fully specified model for the shape, 
  // spectrum and flux.
  SkyCompRep(const Flux<Double>& flux,
 	     const ComponentShape& shape, 
 	     const SpectralModel& spectrum);
  
  // The copy constructor uses copy semantics
  SkyCompRep(const SkyCompRep& other);
  
  // The destructor does not appear to do much
  virtual ~SkyCompRep();

  // The assignment operator uses copy semantics.
  SkyCompRep& operator=(const SkyCompRep& other);

  // See the corresponding functions in the
  // <linkto class="SkyCompBase">SkyCompBase</linkto>
  // class for a description of these functions.
  // <group>
  virtual const Flux<Double>& flux() const;
  virtual Flux<Double>& flux();
  // </group>

  // See the corresponding functions in the
  // <linkto class="SkyCompBase">SkyCompBase</linkto>
  // class for a description of these functions.
  // <group>
  virtual const ComponentShape& shape() const;
  virtual ComponentShape& shape();
  virtual void setShape(const ComponentShape& newShape);
  // </group>
  
  // See the corresponding functions in the
  // <linkto class="SkyCompBase">SkyCompBase</linkto>
  // class for a description of these functions.
  // <group>
  virtual const SpectralModel& spectrum() const;
  virtual SpectralModel& spectrum();
  virtual void setSpectrum(const SpectralModel& newSpectrum);
  // </group>
  
  // See the corresponding functions in the
  // <linkto class="SkyCompBase">SkyCompBase</linkto>
  // class for a description of these functions.
  // <group>
  virtual String& label();
  virtual const String& label() const;
  // </group>

  // See the corresponding functions in the
  // <linkto class="SkyCompBase">SkyCompBase</linkto>
  // class for a description of these functions.
  // <group>
  virtual Vector<Double>& optionalParameters();
  virtual const Vector<Double>& optionalParameters() const;
  // </group>

  // See the corresponding function in the
  // <linkto class="SkyCompBase">SkyCompBase</linkto>
  // class for a description of this function.
  virtual Bool isPhysical() const;
  
  // See the corresponding function in the
  // <linkto class="SkyCompBase">SkyCompBase</linkto>
  // class for a description of this function.
  virtual Flux<Double> sample(const MDirection& direction, 
			      const MVAngle& pixelLatSize,
			      const MVAngle& pixelLongSize,
			      const MFrequency& centerFrequency) const;

  // See the corresponding function in the
  // <linkto class="SkyCompBase">SkyCompBase</linkto>
  // class for a description of this function.
  virtual void sample(Cube<Double>& samples,
		      const Unit& reqUnit,
		      const Vector<MVDirection>& directions, 
		      const MeasRef<MDirection>& dirRef, 
		      const MVAngle& pixelLatSize, 
		      const MVAngle& pixelLongSize, 
		      const Vector<MVFrequency>& frequencies,
		      const MeasRef<MFrequency>& freqRef) const;

  // See the corresponding function in the
  // <linkto class="SkyCompBase">SkyCompBase</linkto>
  // class for a description of this function.
  virtual Flux<Double> visibility(const Vector<Double>& uvw,
				  const Double& frequency) const;

  // See the corresponding function in the
  // <linkto class="SkyCompBase">SkyCompBase</linkto>
  // class for a description of this function.
  virtual void visibility(Cube<DComplex>& visibilities,
			  const Matrix<Double>& uvws,
			  const Vector<Double>& frequencies) const;

  // See the corresponding functions in the
  // <linkto class="SkyCompBase">SkyCompBase</linkto>
  // class for a description of these functions.
  // <group>
  virtual Bool fromRecord(String& errorMessage, 
			  const RecordInterface& record);
  virtual Bool toRecord(String& errorMessage, 
			RecordInterface& record) const;
  // </group>

  // Convert the SkyComponent to a vector of Doubles 
  // for the specified Stokes type (others are lost).
  // The first three elements of the returned vector are : flux for given 
  // Stokes (in the units you specify), longitude location (absolute pixels), and
  // latitude location (absolute pixels).  For DISK and GAUSSIAN shapes,
  // the next three elements are major axis (absolute pixels)
  // minor axis (absolute pixels), and position angle (N->E; radians).
  // You must specify the brightness units to which the integral flux stored
  // in the SkyComponent should be converted.  So as to be able to handle
  // /beam units, the restoring beam must also be suppluied.  It can be obtained 
  // from the ImageInfo class.  It should be of length 3 or 0 (no beam).  
  //  A constant spectrum is used so any spectral index information in
  // the component is lost.
  Vector<Double> toPixel (
		  const Unit& brightnessUnitOut,
          const GaussianBeam& restoringBeam,
          const CoordinateSystem& cSys,
          Stokes::StokesTypes stokes
  ) const;

  // Take a vector Doubles and fill the SkyComponent from the values.
  // The first three elements of the given vector are : flux for given 
  // Stokes (in the units you specify), longitude location (absolute pixels), and
  // latitude location (absolute pixels).  For DISK and GAUSSIAN shapes,
  // the next three elements are major axis (absolute pixels)
  // minor axis (absolute pixels), and position angle (N->E; radians).
  // You must specify the brightness units in which the flux is stored
  // in the vector.  It will be converted to an integral reprentation
  // internally for the SkyComponent.  So as to be able to handle
  // /beam units, the restoring beam must also be supplied.  It can be obtained 
  // from the ImageInfo class.  It should be of length 3 or 0 (no beam).  
  // Multiplying by fluxRatio converts the brightness units to Jy/whatever (e.g. mJy/beam
  // to Jy/beam). You must specify the type of shape to convert to.
  // The SkyComponent is given a  constant spectrum.
  void fromPixel (Double& fluxRatio, const Vector<Double>& parameters,
                  const Unit& brightnessUnitIn,
                  const GaussianBeam& restoringBeam,
                  const CoordinateSystem& cSys,
                  ComponentType::Shape componentShape,
                  Stokes::StokesTypes stokes);

  // See the corresponding function in the
  // <linkto class="SkyCompBase">SkyCompBase</linkto>
  // class for a description of this function.
  virtual Bool ok() const;


// Find the factor that converts whatever per whatevers (e.g. mJy per beam)
// to Jy per whatevers (e.g. Jy per beam)
   static Double convertToJy (const Unit& brightnessUnit);

   // Convert a peak flux density to integral flux density
	static Quantity peakToIntegralFlux (
		const DirectionCoordinate& dirCoord,
		const ComponentType::Shape componentShape,
		const Quantum<Double>& peakFlux,
		const Quantum<Double>& majorAxis,
		const Quantum<Double>& minorAxis,
		const GaussianBeam& restoringBeam
	);

	// Convert an integral flux density to peak flux density.  The brightness unit
	// of the output quantum (e.g. mJy/beam) is specified by <src>brightnessUnit</src>
	// Throws an exception if the units of <src>integralFlux</src> do not conform to Jy.
	static Quantity integralToPeakFlux (
		const DirectionCoordinate& dirCoord,
		const ComponentType::Shape componentShape,
		const Quantity& integralFlux,
		const Unit& brightnessUnit,
		const Quantity& majorAxis,
		const Quantity& minorAxis,
		const GaussianBeam& restoringBeam
	);

private:
  CountedPtr<ComponentShape> itsShapePtr;
  CountedPtr<SpectralModel> itsSpectrumPtr;
  Flux<Double> itsFlux;
  String itsLabel;
  Vector<Double> itsOptParms;


  // Make definitions to handle "/beam" and "/pixel" units.   The restoring beam
  // is provided in a vector of quanta (major, minor, position angle).  Should
  // be length 0 or 3. It can be obtained from class ImageInfo
  static Unit defineBrightnessUnits (
		  LogIO& os, const Unit& brightnessUnitIn,
		  const DirectionCoordinate& dirCoord,
		  const GaussianBeam& restoringBeam,
		  const Bool integralIsJy
  );

  // Remove the user defined "/beam" and "/pixel" definitions
     static void undefineBrightnessUnits();

};

} //# NAMESPACE CASA - END

#endif
