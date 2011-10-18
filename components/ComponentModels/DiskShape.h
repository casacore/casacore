//# DiskShape.h:
//# Copyright (C) 1998,1999,2000
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

#ifndef COMPONENTS_DISKSHAPE_H
#define COMPONENTS_DISKSHAPE_H

#include <casa/aips.h>
#include <casa/BasicSL/Complex.h>
#include <components/ComponentModels/ComponentType.h>
#include <components/ComponentModels/TwoSidedShape.h>

namespace casa { //# NAMESPACE CASA - BEGIN

class MDirection;
class MVAngle;
template <class Qtype> class Quantum;
template <class T> class Matrix;
template <class T> class Vector;

// <summary>A disk model for the spatial distribution of emission</summary>

// <use visibility=export> 

// <reviewed reviewer="" date="yyyy/mm/dd" tests="tDiskShape" demos="dTwoSidedShape">
// </reviewed>

// <prerequisite>
//   <li> <linkto class=TwoSidedShape>TwoSidedShape</linkto>
// </prerequisite>

// <synopsis> 

// A DiskShape models the spatial distribution of radiation from the sky as a
// using a uniform brightness elliptical disk with user specified major axis
// width, minor axis width and position angle.

// This class like the other component shapes becomes more useful when used
// through the <linkto class=SkyComponent>SkyComponent</linkto> class, which
// incorperates the flux and spectral variation of the emission, or through the
// <linkto class=ComponentList>ComponentList</linkto> class, which handles
// groups of SkyComponent objects.

// The reference direction is defined in celestial co-ordinates, using a
// <linkto class=MDirection>MDirection</linkto> object. It indicates where the
// centre of the disk is on the sky. The direction can be specified both in
// the constructor or with the <src>setRefDirection</src> function.

// The width of the disk is defined as the angular diameter along the specified
// axis. The major axis has the larger width and is aligned North-South when
// the position angle is zero. A positive position angle moves the Northern
// side of the disk to the East.  The axial ratio is the ratio of the
// minor to major axis widths. The major axis MUST not be smaller than the
// minor axis otherwise an AipsError is thrown.

// These parameters of the disk (width, position angle, direction etc.) can be
// specified at construction time, using the <src>*inRad</src> functions or
// through functions, in the base classes. The base classes also implement
// functions for inter-converting this object into a record representation.

// The flux, or integrated intensity, is always normalised to one. This class
// does not model the actual flux or its variation with frequency. It solely
// models the way the emission varies with position on the sky.

// The <src>sample</src> member function is used to sample the component at any
// point on the sky. The scale factor calculated by this function is the
// proportion of the flux that is within a specified pixel size centered on the
// specified direction. This is not accurate for pixels which are partially
// covered by the disk. Ultimately this function will integrate the emission
// from the disk over the entire pixel but currently the returned flux will be
// either zero or a constant value with the returned value depending on whether
// the centre of the pixel in within the disk or not. This inaccuracy becomes
// more important when the pixel size become large compared to the disk width.

// This class contains functions that return the Fourier transform of the
// disk at a specified spatial frequency. There are described more fully
// in the description of the <src>visibility</src> functions below.
// </synopsis>

// <example>
// Shown below is code to construct a disk shaped model whose direction is
// always centred on the disk of Jupiter. Note that it is necessary to specify
// the observation time in order for the DiskShape class to be able to do the
// conversion into J2000 coordinates. This example is also available in the
// <src>dTwoSidedShape.cc</src> file. 
// <srcblock>
// { // construct a model for Jupiter.
//   Quantity clk_time; MVTime::read(clk_time, "01-10-2000/12:59");
//   MEpoch obs_epoch(clk_time, MEpoch::UTC);
//   MeasFrame obs_frame(obs_epoch);
//   MDirection jupiter_dir(MVDirection(0), 
//                          MDirection::Ref(MDirection::JUPITER, obs_frame));
//   DiskShape jupiter_shape;
//   jupiter_shape.setRefDirection(jupiter_dir);
//   jupiter_shape.setWidth(Quantity(4,"arcmin"), Quantity(3.9,"arcmin"),
//                          Quantity(3, "deg"));
//   printShape(jupiter_shape);
//   MDirection sample_dir(MVDirection(1.218, 0.37), MDirection::J2000);
//   if (jupiter_shape.sample(sample_dir, MVAngle(0.1)) > 0.0) {
//     cout << "The position in J2000 coordinates is near: " 
//          << sample_dir.getAngle("deg") << endl;
//   }
// }
// </srcblock>
// The printShape function is the example shown for the TwoSidedShape class.
// </example>
//
// <todo asof="1999/11/12">
//   <li> Use Measures & Quanta in the interface to the visibility functions.
//   <li> Use a better way of integrating over the pixel area in the sample
//        function. 
//   <li> Ensure that the position angle is always between zero and 180
//        degrees, and that the widths are always positive.
// </todo>

// <linkfrom anchor="GaussianShape" classes="ComponentShape TwoSidedShape PointShape GaussianShape">
//  <here>DiskShape</here> - a uniform brightness disk shape.
// </linkfrom>


class DiskShape: public TwoSidedShape
{
public:
  // The default GaussianShape is at the J2000 North Pole with a width of 1
  // arc-min on both axes.
  DiskShape();

  // Construct a disk shape centred in the specified direction, specifying
  // the widths & position angle.
  // <group>
  DiskShape(const MDirection& direction,
	    const Quantum<Double>& majorAxis,
	    const Quantum<Double>& minorAxis,
	    const Quantum<Double>& positionAngle);
  DiskShape(const MDirection& direction, const Quantum<Double>& width,
	    const Double axialRatio,
	    const Quantum<Double>& positionAngle);
  // </group>

  // The copy constructor uses copy semantics.
  DiskShape(const DiskShape& other);

  // The destructor does nothing special.
  virtual ~DiskShape();

  // The assignment operator uses copy semantics.
  DiskShape& operator=(const DiskShape& other);

  // get the type of the shape. This function always returns
  // ComponentType::DISK.
  virtual ComponentType::Shape type() const;

  // set or return the width and orientation of the disk. All numerical
  // values are in radians. There are also functions in the base class for
  // doing this with other angular units.
  // <group>
  virtual void setWidthInRad(const Double majorAxis,
			     const Double minorAxis, 
			     const Double positionAngle);
  virtual Double majorAxisInRad() const;
  virtual Double minorAxisInRad() const;
  virtual Double positionAngleInRad() const;
  // </group>

  // Calculate the proportion of the flux that is in a pixel of specified size
  // centered in the specified direction. The returned value will always be
  // between zero and one (inclusive).
  virtual Double sample(const MDirection& direction, 
			const MVAngle& pixelLatSize,
			const MVAngle& pixelLongSize) const;

  // Same as the previous function except that many directions can be sampled
  // at once. The reference frame and pixel size must be the same for all the
  // specified directions.
  virtual void sample(Vector<Double>& scale, 
		      const Vector<MDirection::MVType>& directions, 
		      const MDirection::Ref& refFrame,
		      const MVAngle& pixelLatSize,
		      const MVAngle& pixelLongSize) const;

  // Return the Fourier transform of the component at the specified point in
  // the spatial frequency domain. The point is specified by a 3 element vector
  // (u,v,w) that has units of meters and the frequency of the observation, in
  // Hertz. These two quantities can be used to derive the required spatial
  // frequency <src>(s = uvw*freq/c)</src>. The w component is not used in
  // these functions.

  // The reference position for the transform is the direction of the
  // component. As this component is symmetric about this point the transform
  // is always a real value.
  virtual DComplex visibility(const Vector<Double>& uvw,
			      const Double& frequency) const;

  // Same as the previous function except that many (u,v,w) points can be
  // sampled at once. The uvw Matrix must have a first dimension of three, and
  // a second dimension that is the same as the length of the scale
  // Vector. Otherwise and exception is thrown (when compiled in debug mode).
  virtual void visibility(Vector<DComplex>& scale, const Matrix<Double>& uvw,
			  const Double& frequency) const;

  //same as above except with many frequencies
  virtual void visibility(Matrix<DComplex>& scale, const Matrix<Double>& uvw,
   			  const Vector<Double>& frequency) const;

  // Return a pointer to a copy of this object upcast to a ComponentShape
  // object. The class that uses this function is responsible for deleting the
  // pointer. This is used to implement a virtual copy constructor.
  virtual ComponentShape* clone() const;

  // Function which checks the internal data of this class for correct
  // dimensionality and consistent values. Returns True if everything is fine
  // otherwise returns False.
  virtual Bool ok() const;

  // return a pointer to this object.
  virtual const ComponentShape* getPtr() const; 

  virtual String sizeToString() const;

private:
  Double calcSample(const MDirection::MVType& compDirValue, 
		    const MDirection::MVType& dirVal, 
		    const Double majRad, const Double minRad, 
		    const Double pixValue) const;
  Double calcVis(Double u, Double v, const Double factor) const;
  static void rotateVis(Double& u, Double& v, 
			const Double cpa, const Double spa);

  //# The parameters of the disk
  // <group>
  Double itsMajValue;
  Double itsMinValue;
  Double itsPaValue;
  Double itsHeight;
  // </group>
};


} //# NAMESPACE CASA - END

#endif
