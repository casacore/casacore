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
#include <trial/ComponentModels/ComponentType.h>
#include <trial/ComponentModels/TwoSidedShape.h>

class MDirection;
class MVAngle;
template <class Qtype> class Quantum;
template <class T> class Vector;

// <summary>A disk model for the spatial distribution of emission</summary>

// <use visibility=export> 

// <reviewed reviewer="" date="yyyy/mm/dd" tests="" demos="">
// </reviewed>

// <prerequisite>
//   <li> <linkto class=ComponentShape>ComponentShape</linkto>
// </prerequisite>

// <synopsis> 

// </synopsis>
//
// <example>
// </example>
//
// <todo asof="1997/07/16">
//   <li> Nothing so far
// </todo>

// <linkfrom anchor="GaussianShape" 
//           classes="ComponentShape TwoSidedShape PointShape GaussianShape">
//  <here>GaussianShape</here>
// - a Uniform brightness disk shape.
// </linkfrom>


class DiskShape: public TwoSidedShape
{
public:
  // The default DiskShape is at an RA and Dec of zero in the J2000 Frame. The
  // width on both axes is 1 arc-min.
  DiskShape();

  // Construct a disk shape with specified direction, width and
  // position angle (North through East).
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

  // The destructor does nothing.
  virtual ~DiskShape();

  // The assignment operator uses copy semantics.
  DiskShape& operator=(const DiskShape& other);

  // get the type of the shape. This function always returns
  // ComponentType::DISK.
  virtual ComponentType::Shape type() const;

  // set/get the width and orientation of the Disk. The position angle is
  // measured North through East ie a position angle of zero degrees means
  // that the major axis is North-South and a position angle of 10 degrees
  // moves the Northern edge to the East. The axial ratio is the ratio of the
  // minor to major axes widths. Hence it is always between zero and one.
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
  // of the sky subtended by the pixel. Instead it simply samples the Disk at
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

  // Return the Fourier transform of the component at the specified disk in
  // the spatial frequency domain. The point is specified by a 3 element vector
  // (u,v,w) that has units of meters and the frequency of the observation, in
  // Hertz. These two quantities can be used to derive the required spatial
  // frequency <src>(s = uvw*freq/c)</src>. The w component is not used in
  // these functions.

  // The reference position for the transform is the direction of the
  // component. As this component is symmetric about this point the transform
  // is always a real value.

  // The total flux of the component must be supplied in the flux variable and
  // the corresponding visibility is returned in the same variable.
  virtual DComplex visibility(const Vector<Double>& uvw,
			      const Double& frequency) const;

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
  void updateFT();
  Double itsMajValue;
  Double itsMinValue;
  Double itsPaValue;
  Double itsHeight;
};
#endif
