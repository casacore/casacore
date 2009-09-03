//# DiskShape.cc:
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
//# $Id$

#include <components/ComponentModels/DiskShape.h>
#include <components/ComponentModels/Flux.h>
#include <casa/Arrays/Vector.h>
#include <casa/Exceptions/Error.h>
#include <casa/Logging/LogIO.h>
#include <casa/Logging/LogOrigin.h>
#include <casa/BasicSL/Constants.h>
#include <casa/BasicMath/Math.h>
#include <measures/Measures/MCDirection.h>
#include <measures/Measures/MDirection.h>
#include <measures/Measures/MeasConvert.h>
#include <measures/Measures/MeasRef.h>
#include <casa/Quanta/MVAngle.h>
#include <casa/Quanta/MVDirection.h>
#include <casa/Quanta/Quantum.h>
#include <casa/Utilities/Assert.h>
#include <casa/BasicSL/String.h>

namespace casa { //# NAMESPACE CASA - BEGIN

DiskShape::DiskShape()
  :TwoSidedShape(),
   itsMajValue(Quantity(1,"'").getValue("rad")),
   itsMinValue(Quantity(1,"'").getValue("rad")),
   itsPaValue(Quantity(0,"deg").getValue("rad")),
   itsHeight(1.0/(C::pi*itsMajValue*itsMinValue))
{
  DebugAssert(ok(), AipsError);
}

DiskShape::DiskShape(const MDirection& direction, 
		     const Quantum<Double>& majorAxis,
		     const Quantum<Double>& minorAxis,
		     const Quantum<Double>& positionAngle)
  :TwoSidedShape(direction, majorAxis.getFullUnit(),
		 minorAxis.getFullUnit(), positionAngle.getFullUnit()),
   itsMajValue(majorAxis.getValue("rad")),
   itsMinValue(minorAxis.getValue("rad")),
   itsPaValue(positionAngle.getValue("rad")),
   itsHeight(1.0/(C::pi*itsMajValue*itsMinValue))
{
  DebugAssert(ok(), AipsError);
}

DiskShape::DiskShape(const MDirection& direction,
		     const Quantum<Double>& width,
		     const Double axialRatio,
		     const Quantum<Double>& positionAngle) 
  :TwoSidedShape(direction, width.getFullUnit(),
		 width.getFullUnit(), positionAngle.getFullUnit()),
   itsMajValue(width.getValue("rad")),
   itsMinValue(itsMajValue*axialRatio),
   itsPaValue(positionAngle.getValue("rad")),
   itsHeight(1.0/(C::pi*itsMajValue*itsMinValue))
{
  DebugAssert(ok(), AipsError);
}

DiskShape::DiskShape(const DiskShape& other) 
  :TwoSidedShape(other),
   itsMajValue(other.itsMajValue),
   itsMinValue(other.itsMinValue),
   itsPaValue(other.itsPaValue),
   itsHeight(other.itsHeight)
{
  DebugAssert(ok(), AipsError);
}

DiskShape::~DiskShape() {
  DebugAssert(ok(), AipsError);
}

DiskShape& DiskShape::operator=(const DiskShape& other) {
  if (this != &other) {
    TwoSidedShape::operator=(other);
    itsMajValue = other.itsMajValue;
    itsMinValue = other.itsMinValue;
    itsPaValue = other.itsPaValue;
    itsHeight = other.itsHeight;
  }
  DebugAssert(ok(), AipsError);
  return *this;
}

ComponentType::Shape DiskShape::type() const {
  DebugAssert(ok(), AipsError);
  return ComponentType::DISK;
}

void DiskShape::setWidthInRad(const Double majorAxis,
			      const Double minorAxis, 
			      const Double positionAngle) {
  itsMajValue = majorAxis;
  itsMinValue = minorAxis;
  itsPaValue = positionAngle;
  AlwaysAssert(itsMajValue > 0 && itsMinValue > 0 && itsMajValue >=itsMinValue,
 	       AipsError);
  itsHeight = 1.0/(C::pi*itsMajValue*itsMinValue);
  DebugAssert(ok(), AipsError);
}

Double DiskShape::majorAxisInRad() const {
  DebugAssert(ok(), AipsError);
  return itsMajValue;
}

Double DiskShape::minorAxisInRad() const {
  DebugAssert(ok(), AipsError);
  return itsMinValue;
}

Double DiskShape::positionAngleInRad() const {
  DebugAssert(ok(), AipsError);
  return itsPaValue;
}

Double DiskShape::sample(const MDirection& direction, 
			 const MVAngle& pixelLatSize,
			 const MVAngle& pixelLongSize) const {
  DebugAssert(ok(), AipsError);
  const MDirection& compDir(refDirection());
  const MDirection::Ref& compDirFrame(compDir.getRef());
  const MDirection::MVType* compDirValue = &(compDir.getValue());
  Bool deleteValue = False;
  // Convert direction to the same frame as the reference direction
  if (ComponentShape::differentRefs(direction.getRef(), compDirFrame)) {
    compDirValue = new MDirection::MVType
      (MDirection::Convert(compDir, direction.getRef())().getValue());
    deleteValue = True;
  }
  Double retVal = calcSample(*compDirValue, direction.getValue(),
			     itsMajValue/2.0, itsMinValue/2.0, 
			     itsHeight*pixelLatSize.radian()*
			     pixelLongSize.radian());
  if (deleteValue) delete compDirValue;
  return retVal;
}

void DiskShape::sample(Vector<Double>& scale, 
		       const Vector<MVDirection>& directions, 
		       const MDirection::Ref& refFrame,
		       const MVAngle& pixelLatSize,
		       const MVAngle& pixelLongSize) const {
  DebugAssert(ok(), AipsError);
  const uInt nSamples = directions.nelements();
  DebugAssert(scale.nelements() == nSamples, AipsError);

  const MDirection& compDir(refDirection());
  const MDirection::Ref& compDirFrame(compDir.getRef());
  const MDirection::MVType* compDirValue = &(compDir.getValue());
  Bool deleteValue = False;
  // Convert direction to the same frame as the reference direction
  if (refFrame != compDirFrame) {
    compDirValue = new MDirection::MVType
      (MDirection::Convert(compDir, refFrame)().getValue());
    deleteValue = True;
  }
  const Double majRad = itsMajValue/2.0; 
  const Double minRad = itsMinValue/2.0; 
  const Double pixValue = itsHeight * 
    pixelLatSize.radian() * pixelLongSize.radian();
  for (uInt i = 0; i < nSamples; i++) {
    scale(i) = calcSample(*compDirValue, directions(i), 
			  majRad, minRad, pixValue);
  }
  if (deleteValue) delete compDirValue;
}

DComplex DiskShape::visibility(const Vector<Double>& uvw,
			       const Double& frequency) const {
  DebugAssert(uvw.nelements() == 3, AipsError);
  DebugAssert(frequency > 0, AipsError);
  DebugAssert(ok(), AipsError);
  Double u = uvw(0);
  Double v = uvw(1);
  if (near(u + v, 0.0)) return DComplex(1.0, 0.0);
  if (!nearAbs(itsPaValue, 0.0)) {
    rotateVis(u, v, cos(itsPaValue), sin(itsPaValue));
  }
  return DComplex(calcVis(u, v, C::pi * frequency/C::c), 0.0);
}

void DiskShape::visibility(Vector<DComplex>& scale,
			   const Matrix<Double>& uvw,
			   const Double& frequency) const {
  DebugAssert(ok(), AipsError);
  const uInt nSamples = scale.nelements();
  DebugAssert(uvw.ncolumn() == nSamples, AipsError);
  DebugAssert(uvw.nrow() == 3, AipsError);
  DebugAssert(frequency > 0, AipsError);
  
  Bool doRotation = False;
  Double cpa = 1.0, spa = 0.0;
  if (!nearAbs(itsPaValue, 0.0)) {
    doRotation = True;
    cpa = cos(itsPaValue);
    spa = sin(itsPaValue);
  }

  const Double factor = C::pi * frequency/C::c;
  Double u, v;
  for (uInt i = 0; i < nSamples; i++) {
    u = uvw(0, i);
    v = uvw(1, i);
    DComplex& thisVis = scale(i);
    ///    thisVis.imag() = 0.0;
    if (near(u + v, 0.0)) {
      ///      thisVis.real() = 1.0; // avoids dividing by zero in calcVis(...)
      thisVis = DComplex(1.0, 0.0); // avoids dividing by zero
      // in calcVis(...)
    } else {
      if (doRotation) rotateVis(u, v, cpa, spa);
      ///      thisVis.real() = calcVis(u, v, factor);
      thisVis = DComplex(calcVis(u, v, factor), 0.0);
    }
  }
}

ComponentShape* DiskShape::clone() const {
  DebugAssert(ok(), AipsError);
  ComponentShape* tmpPtr = new DiskShape(*this);
  AlwaysAssert(tmpPtr != 0, AipsError);
  return tmpPtr;
}

Bool DiskShape::ok() const {
  // The LogIO class is only constructed if an error is detected for
  // performance reasons. Both function static and file static variables
  // where considered and rejected for this purpose.
  if (!TwoSidedShape::ok()) return False; 
  if (itsMajValue <= 0) {
    LogIO logErr(LogOrigin("DiskCompRep", "ok()"));
    logErr << LogIO::SEVERE << "The major axis width is zero or negative"
           << LogIO::POST;
    return False;
  }
  if (itsMinValue <= 0) {
    LogIO logErr(LogOrigin("DiskCompRep", "ok()"));
    logErr << LogIO::SEVERE << "The minor axis width is zero or negative"
           << LogIO::POST;
    return False;
  }
  if (itsMinValue > itsMajValue) {
    LogIO logErr(LogOrigin("DiskCompRep", "ok()"));
    logErr << LogIO::SEVERE << "The minor axis width is larger than "
	   << "the major axis width"
           << LogIO::POST;
    return False;
  }
  if (!near(itsHeight, 1.0/(C::pi*itsMajValue*itsMinValue), C::dbl_epsilon)) {
    LogIO logErr(LogOrigin("DiskCompRep", "ok()"));
    logErr << LogIO::SEVERE << "The disk shape does not have"
	   << " unit area"
           << LogIO::POST;
    return False;
  }
  return True;
}

const ComponentShape* DiskShape::getPtr() const {
    return this;
}


Double DiskShape::calcVis(Double u, Double v, const Double factor) const {
  u *= itsMinValue;
  v *= itsMajValue;
  const Double r = hypot(u, v) * factor;
  return 2.0 * j1(r)/r;
}

void DiskShape::rotateVis(Double& u, Double& v, 
			  const Double cpa, const Double spa) {
  const Double utemp = u;
  u = u * cpa - v * spa;
  v = utemp * spa + v * cpa;
}

Double DiskShape::calcSample(const MDirection::MVType& compDirValue, 
			     const MDirection::MVType& dirVal, 
			     const Double majRad, const Double minRad, 
			     const Double pixValue) const {
  const Double separation = compDirValue.separation(dirVal);
  if (separation <= majRad) {
    const Double pa = compDirValue.positionAngle(dirVal) - itsPaValue;
    const Double x = abs(separation*cos(pa));
    const Double y = abs(separation*sin(pa));
    if ((x <= majRad) && 
	(y <= minRad) && 
	(y <= minRad * sqrt(1 - square(x/majRad)))) {
      return pixValue;
    }
  }
  return 0.0;
}

// Local Variables: 
// compile-command: "gmake DiskShape"
// End: 

} //# NAMESPACE CASA - END

