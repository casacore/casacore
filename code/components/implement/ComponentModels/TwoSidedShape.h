//# TwoSidedShape.h:
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

#if !defined(AIPS_TWOSIDEDSHAPE_H)
#define AIPS_TWOSIDEDSHAPE_H

#include <aips/aips.h>
#include <trial/ComponentModels/ComponentShape.h>
#include <trial/ComponentModels/ComponentType.h>
#include <aips/Quanta/Unit.h>

class MDirection;
class MVAngle;
class RecordInterface;
class String;
template <class Qtype> class Quantum;
template <class T> class Flux;
template <class T> class Vector;

// <summary>A TwoSided model for the spatial distribution of emission</summary>

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

// <linkfrom anchor="TwoSidedCompRep" classes="SkyCompRep TwoSidedComponent">
//  <here>TwoSidedCompRep</here> - a twoSided component with copy semantics
// </linkfrom>

class TwoSidedShape: public ComponentShape
{
public:

  // a virtual destructor is needed so that the actual destructor in the
  // derived class will be used.
  virtual ~TwoSidedShape();

  // return the actual component shape.
  virtual ComponentType::Shape type() const = 0;

  // set/get the width and orientation of the Shape. The width of the major
  // must be larger than the width of the minor axes. The position angle is
  // measured North through East ie a position angle of zero degrees means
  // that the major axis is North-South and a position angle of 10 degrees
  // moves the Northern edge to the East. The axial ratio is the ratio of the
  // minor to major axes widths. Hence it is always between zero and one.
  // <group>
  virtual void setWidth(const Quantum<Double>& majorAxis,
			const Quantum<Double>& minorAxis, 
			const Quantum<Double>& positionAngle);
  virtual void setWidth(const Quantum<Double>& majorAxis,
			const Double axialRatio, 
			const Quantum<Double>& positionAngle);
  virtual Quantum<Double> majorAxis() const;
  virtual Quantum<Double> minorAxis() const;
  virtual Quantum<Double> positionAngle() const;
  virtual Double axialRatio() const;
  // </group>

  // set/get the width and orientation of the Shape. These are the same as the
  // above functions except that all widths are in radians.
  // <group>
  virtual void setWidthInRad(const Double majorAxis,
			     const Double minorAxis, 
			     const Double positionAngle) = 0;
  virtual Double majorAxisInRad() const = 0;
  virtual Double minorAxisInRad() const = 0;
  virtual Double positionAngleInRad() const = 0;
  // </group>

  // Calculate the proportion of the flux that is in a pixel of specified size
  // centered in the specified direction. The returned value will always be
  // between zero and one (inclusive).
  virtual Double sample(const MDirection& direction, 
			const MVAngle& pixelSize) const = 0;

  // Calculate the amount of flux that is in the pixels of specified, constant
  // size centered on the specified directions. The returned values will always
  // be between zero and one (inclusive). All the supplied directions must have
  // the same reference frame (that is specified in the refFrame argument).
  virtual void sample(Vector<Double>& scale, 
		      const Vector<MDirection::MVType>& directions, 
		      const MDirection::Ref& refFrame,
		      const MVAngle& pixelSize) const = 0;

  // Return the Fourier transform of the component at the specified twoSided in
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
  virtual void visibility(Flux<Double>& flux, const Vector<Double>& uvw,
			  const Double& frequency) const = 0;

  // Return a pointer to a copy of this object upcast to a ComponentShape
  // object. The class that uses this function is responsible for deleting the
  // pointer. This is used to implement a virtual copy constructor.
  virtual ComponentShape* clone() const = 0;

  // set/get the shape parameters associated with the TwoSided. There are three
  // these being in order: the major axis, the minor axis and the position
  // angle. All these angular quantities are specified in radians. The Vector
  // supplied to the setParameters function must have three elements and the
  // Vector supplied to and returned by the parameters function will have three
  // elements.
  // <group>
  virtual uInt nParameters() const;
  virtual void setParameters(const Vector<Double>& newParms);
  virtual void parameters(Vector<Double>& compParms) const;
  // </group>

  // This functions convert between a RecordInterface and a
  // TwoSidedShape. These functions define how a TwoSidedShape is represented
  // in glish and this is detailed in the synopsis above.  They return False if
  // the record is malformed and append an error message to the supplied string
  // giving the reason.
  // <group>
  virtual Bool fromRecord(String& errorMessage,
			  const RecordInterface& record);
  virtual Bool toRecord(String& errorMessage,
			RecordInterface& record) const;
  // </group>

  // Convert the parameters of the component to the specified units. The
  // supplied record must have three fields, namely 'majoraxis', 'minoraxis' &
  // 'positionangle'. These fileds must contains strings that are angular
  // units and this function will convert the corresponding parameters to the
  // specified units. This will have no effect on the shape of this class but
  // will affect the format of the record returned by the toRecord function. 
  virtual Bool convertUnit(String& errorMessage,
			   const RecordInterface& record);

  // Function which checks the internal data of this class for correct
  // dimensionality and consistent values. Returns True if everything is fine
  // otherwise returns False.
  virtual Bool ok() const;

protected:
  // The default TwoSidedShape is at the J2000 North Pole.
  TwoSidedShape();

  // Construct a TwoSidedShape at the specified direction.
  TwoSidedShape(const MDirection& direction, const Unit& majorAxisUnit, const
		Unit& minorAxisUnit, const Unit& paUnit);

  // The copy constructor uses copy semantics.
  TwoSidedShape(const TwoSidedShape& other);

  // The assignment operator uses copy semantics.
  TwoSidedShape& operator=(const TwoSidedShape& other);

  // Set the units for the major axis, minor axis and position angle.
  // <group>
  void majorAxisUnit(const Unit& majorAxisUnit);
  void minorAxisUnit(const Unit& minorAxisUnit);
  void paUnit(const Unit& paUnit);
  // </group>

  // Get the current units for the major axis, minor axis or position angle.
  // <group>
  const Unit& majorAxisUnit();
  const Unit& minorAxisUnit();
  const Unit& paUnit();
  // </group>

private:
  Unit itsMajUnit;
  Unit itsMinUnit;
  Unit itsPaUnit;
};
#endif
