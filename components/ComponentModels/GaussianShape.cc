//# GaussianShape.cc:
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

#include <components/ComponentModels/GaussianShape.h>
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

GaussianShape::GaussianShape()
  :TwoSidedShape(),
   itsShape(1.0, 0.0, 0.0, Quantity(1,"'").getValue("rad"), 1.0, 0.0),
   itsFT(itsShape)
{
  itsShape.setFlux(1.0);
  updateFT();
  DebugAssert(ok(), AipsError);
}

GaussianShape::GaussianShape(const MDirection& direction, 
			     const Quantum<Double>& majorAxis,
			     const Quantum<Double>& minorAxis,
			     const Quantum<Double>& positionAngle)
  :TwoSidedShape(direction, majorAxis.getFullUnit(),
		 minorAxis.getFullUnit(), positionAngle.getFullUnit()),
   itsShape(1.0, 0.0, 0.0, majorAxis.getValue("rad"),
	    minorAxis.getValue("rad")/majorAxis.getValue("rad"),
	    positionAngle.getValue("rad")),
   itsFT(itsShape)
{
  // Adjust the flux of the Gaussian now that the width is correctly set
  itsShape.setFlux(1.0);
  updateFT();
  DebugAssert(ok(), AipsError);
}

GaussianShape::GaussianShape(const MDirection& direction,
			     const Quantum<Double>& width,
			     const Double axialRatio,
			     const Quantum<Double>& positionAngle) 
  :TwoSidedShape(direction, width.getFullUnit(),
		 width.getFullUnit(), positionAngle.getFullUnit()),
   itsShape(1.0, 0.0, 0.0, width.getValue("rad"), axialRatio,
	    positionAngle.getValue("rad")),
   itsFT(itsShape)
{
  // Adjust the flux of the Gaussian now that the width is correctly set
  itsShape.setFlux(1.0);
  updateFT();
  DebugAssert(ok(), AipsError);
}

GaussianShape::GaussianShape(const GaussianShape& other) 
  :TwoSidedShape(other),
   itsShape(other.itsShape),
   itsFT(other.itsFT)
{
  DebugAssert(ok(), AipsError);
}

GaussianShape::~GaussianShape() {
  DebugAssert(ok(), AipsError);
}

GaussianShape& GaussianShape::operator=(const GaussianShape& other) {
  if (this != &other) {
    TwoSidedShape::operator=(other);
    itsShape = other.itsShape;
    itsFT = other.itsFT;
  }
  DebugAssert(ok(), AipsError);
  return *this;
}

ComponentType::Shape GaussianShape::type() const {
  DebugAssert(ok(), AipsError);
  return ComponentType::GAUSSIAN;
}

void GaussianShape::setWidthInRad(const Double majorAxis,
				  const Double minorAxis, 
				  const Double positionAngle) {
  Vector<Double> angle(2);
  angle(0) = majorAxis;
  angle(1) = minorAxis;
  itsShape.setWidth(angle);
  itsShape.setPA(positionAngle);
  // Adjusting the width normally keeps the height constant and modifies the
  // flux. Modify this behaviour by restoring the flux
  itsShape.setFlux(1.0);
  updateFT();
  DebugAssert(ok(), AipsError);
}

Double GaussianShape::majorAxisInRad() const {
  DebugAssert(ok(), AipsError);
  return itsShape.majorAxis();
}

Double GaussianShape::minorAxisInRad() const {
  DebugAssert(ok(), AipsError);
  return itsShape.minorAxis();
}

Double GaussianShape::axialRatio() const {
  DebugAssert(ok(), AipsError);
  return itsShape.axialRatio();
}

Double GaussianShape::positionAngleInRad() const {
  DebugAssert(ok(), AipsError);
  return itsShape.PA();
}

Double GaussianShape::sample(const MDirection& direction, 
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
  const MDirection::MVType& dirValue = direction.getValue();
  const Double separation = compDirValue->separation(dirValue);
  Double retVal = 0.0;
  if (separation < 4 * itsShape.majorAxis()) {
    const Double pa = - compDirValue->positionAngle(dirValue);
    retVal = pixelLatSize.radian() * pixelLongSize.radian() * 
      itsShape(separation*sin(pa), separation*cos(pa));
  }
  if (deleteValue) delete compDirValue;
  return retVal;
}

void GaussianShape::sample(Vector<Double>& scale, 
			   const Vector<MDirection::MVType>& directions, 
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
  const Double pixArea = pixelLatSize.radian() * pixelLongSize.radian();
  const Double maxSep = 4.0 * itsShape.majorAxis();
  Double separation, pa;
  for (uInt i = 0; i < nSamples; i++) {
    const MDirection::MVType& dirVal = directions(i);
    separation = compDirValue->separation(dirVal);
    if (separation > maxSep) {
      scale(i) = 0.0;
    } else {
      pa = - compDirValue->positionAngle(dirVal);
      scale(i) = pixArea * itsShape(separation*sin(pa), separation*cos(pa));
    }
  }
  if (deleteValue) delete compDirValue;
}

DComplex GaussianShape::visibility(const Vector<Double>& uvw,
				   const Double& frequency) const {
  DebugAssert(uvw.nelements() == 3, AipsError);
  DebugAssert(frequency > 0, AipsError);
  DebugAssert(ok(), AipsError);
  const Double wavenumber = frequency/C::c;
  return DComplex(itsFT(-uvw(0)*wavenumber, uvw(1)*wavenumber), 0.0);
}

void GaussianShape::visibility(Vector<DComplex>& scale,
			       const Matrix<Double>& uvw,
			       const Double& frequency) const {
  ComponentShape::visibility(scale, uvw, frequency);
}

void GaussianShape::visibility(Matrix<DComplex>& scale,
			       const Matrix<Double>& uvw,
			       const Vector<Double>& frequency) const {
  ComponentShape::visibility(scale, uvw, frequency);
}

ComponentShape* GaussianShape::clone() const {
  DebugAssert(ok(), AipsError);
  ComponentShape* tmpPtr = new GaussianShape(*this);
  AlwaysAssert(tmpPtr != 0, AipsError);
  return tmpPtr;
}

Bool GaussianShape::ok() const {
  // The LogIO class is only constructed if an error is detected for
  // performance reasons. Both function static and file static variables
  // where considered and rejected for this purpose.
  if (!TwoSidedShape::ok()) return False;
  if (!near(itsShape.flux(), Double(1.0), 2*C::dbl_epsilon)) {
    LogIO logErr(LogOrigin("GaussianCompRep", "ok()"));
    logErr << LogIO::SEVERE << "The internal Gaussian shape does not have"
	   << " unit area"
           << LogIO::POST;
    return False;
  }
  if (!near(itsFT.height(), 1.0, 2*C::dbl_epsilon)) {
    LogIO logErr(LogOrigin("GaussianCompRep", "ok()"));
    logErr << LogIO::SEVERE << "The cached Fourier Transform of"
	   << " the internal Gaussian shape does not have"
	   << " unit height"
           << LogIO::POST;
    return False;
  }
  return True;
}

const ComponentShape* GaussianShape::getPtr() const {
    return this;
}

Quantity GaussianShape::getArea() const {
	Double majorAxis = itsShape.majorAxis();
	Double minorAxis = itsShape.minorAxis();

	Quantity area(C::pi/(4*C::ln2) * majorAxis * minorAxis, "sr");
	return area;
}

void GaussianShape::updateFT() {
  const Double factor = 4.0*C::ln2/C::pi;
  Vector<Double> width(2);
  width(0) = factor/itsShape.minorAxis();
  width(1) = factor/itsShape.majorAxis();
  itsFT.setWidth(width);
  itsFT.setPA(itsShape.PA() + C::pi_2);
}

String GaussianShape::sizeToString() const {
	return TwoSidedShape::sizeToString(
		Quantity(itsShape.majorAxis(), "rad"),
		Quantity(itsShape.minorAxis(), "rad"),
		Quantity(itsShape.PA(), "rad"), True,
		majorAxisError(), minorAxisError(),
		positionAngleError()
	);
}





// Local Variables: 
// compile-command: "gmake GaussianShape"
// End: 

} //# NAMESPACE CASA - END

