//# DiskShape.h:
//# Copyright (C) 1998,1999
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

#if !defined(AIPS_DISKSHAPE_H)
#define AIPS_DISKSHAPE_H

#include <aips/aips.h>
#include <aips/Mathematics/Complex.h>
#include <trial/ComponentModels/ComponentType.h>
#include <trial/ComponentModels/TwoSidedShape.h>

class MDirection;
class MVAngle;
template <class Qtype> class Quantum;
template <class T> class Matrix;
template <class T> class Vector;

// <summary>A disk model for the spatial distribution of emission</summary>

// <use visibility=export> 

// <reviewed reviewer="" date="yyyy/mm/dd" tests="tGaussianShape" demos="dGaussianShape">
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
// side of the component to the East.  The axial ratio is the ratio of the
// minor to major axis widths. The major axis MUST not be smaller than the
// minor axis otherwise an AipsError is thrown.

// These parameters of the Gaussian (width, position angle, direction etc.) can
// be specified at construction time, using the <src>*inRad</src> functions or
// through functions in the base classes (TwoSidedShape & ComponentShape). The
// base classes also implement functions for inter-converting this object into
// a record representation. 

// The flux, or integrated intensity, is always normalised to one. This class
// does not model the actual flux or its variation with frequency. It solely
// models the way the emission varies with position on the sky.

// The <src>scale</src> member function is used to sample the component at any
// point on the sky. The scale factor calculated by this function is the
// proportion of the flux that is within a specified pixel size centered on the
// specified direction. This is not accurate for pixels which are partially
// covered by the disk. Ultimatly this function will integrate the emission
// from the disk over the entire pixel but currently the returned flux will be
// either zero or 1/(pixelSizeInRad^2) with the returned value depending on
// whether the centre of the pixel in within the disk or not. This innacuracy
// becomes more important when the pixel size become large compared to the disk
// width.

// This class contains functions that return the Fourier transform of the
// component at a specified spatial frequency. There are described more fully
// in the description of the <src>visibility</src> functions below.
// </synopsis>

// <example>
// Suppose I had an image of a region of the sky and we wanted to subtract
// a extended source from it. This could be done as follows:
// <li> Construct a SkyComponent with a disk of width that is similar to
//      the extended source.
// <li> Project the component onto an image
// <li> Convolve the image by the point spread function
// <li> subtract the convolved model from the dirty image.
// </li>
// Shown below is the code to perform the first step in this process, ie
// construct the SkyComponent. This example is also available in the
// <src>dDIskShape.cc</src> file.  Note that it is more accurate to do
// subtraction of components in the (u,v) domain.
// <srcblock>
//  MDirection jupiter_dir;
//  { // get the right direction into jupiter_dir
//    Quantity clk_time; MVTime::read(clk_time, "01-10-2000/12:59");
//    MEpoch obs_epoch(clk_time, MEpoch::UTC);
//    MeasFrame obs_frame(obs_epoch);
//    jupiter_dir = MDirection(MVDirection(0), 
//	                       MDirection::Ref(MDirection::JUPITER,obs_frame));
//  }
//  {
//    const Flux<Double> flux(6.28, 0.1, 0.15, 0.01);
//    DiskShape jupiter_shape;
//    jupiter_shape.setRefDirection(jupiter_dir);
//    jupiter_shape.setWidth(Quantity(4,"arcmin"), Quantity(3.9,"arcmin"),
//	                     Quantity(0, "deg"));
//    const ConstantSpectrum spectrum;
//    SkyComponent jupiter_model(flux, jupiter_shape, spectrum);
//    printShape(jupiter_shape);
//  }
// </srcblock>
// The printShape function is the example shown for the TwoSidedShape class.
// </example>
//
// <todo asof="1999/11/12">
//   <li> Use Measures & Quanta in the interface to the visibility functions.
//   <li> Use a better way of integrating over the pixel area in the sample
//   function. 
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

  // set/get the width and orientation of the Disk. The position angle is
  // measured North through East ie a position angle of zero degrees means
  // that the major axis is North-South and a position angle of 10 degrees
  // moves the Northern edge to the East.
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
  //
  // Currently this function does <em>NOT<\em> integrate the Disk over the area
  // of the sky subtended by the pixel. Instead it simply samples the disk at
  // the centre of the pixel and scales by the pixel area. This is satisfactory
  // for pixels which are not near the edge of the disk. However if the disk
  // edge passes through the pixel there will be an error in the value returned
  // by this function.
  virtual Double sample(const MDirection& direction, 
			const MVAngle& pixelSize) const;

  // Calculate the amount of flux that is in the pixels of specified, constant
  // size centered on the specified directions. The returned values will always
  // be between zero and one (inclusive). All the supplied directions must have
  // the same reference frame (that is specified in the refFrame argument). 
  virtual void sample(Vector<Double>& scale, 
		      const Vector<MDirection::MVType>& directions, 
		      const MDirection::Ref& refFrame,
		      const MVAngle& pixelSize) const;

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
  // sampled at once. As with the previous function the returned value is
  // always a constant real vector of one.  The input arguments are ignored
  // except in debug mode where the shape of the uvw Matrix and the scale
  // Vector are checked as is the  sign of the frequency variable.
  virtual void visibility(Vector<DComplex>& scale, const Matrix<Double>& uvw,
			  const Double& frequency) const;

  // Return a pointer to a copy of this object upcast to a ComponentShape
  // object. The class that uses this function is responsible for deleting the
  // pointer. This is used to implement a virtual copy constructor.
  virtual ComponentShape* clone() const;

  // Function which checks the internal data of this class for correct
  // dimensionality and consistent values. Returns True if everything is fine
  // otherwise returns False.
  virtual Bool ok() const;

private:
  //# The parameters of the disk
  // <group>
  Double itsMajValue;
  Double itsMinValue;
  Double itsPaValue;
  Double itsHeight;
  // </group>
};
#endif
