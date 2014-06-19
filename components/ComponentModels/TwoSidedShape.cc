//# TwoSidedShape.cc:
//# Copyright (C) 1999,2000,2001,2002
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

#include <components/ComponentModels/TwoSidedShape.h>
#include <casa/iomanip.h>
#include <casa/Arrays/Vector.h>
#include <casa/Arrays/ArrayLogical.h>
#include <casa/Containers/Record.h>
#include <casa/Containers/RecordFieldId.h>
#include <casa/Containers/RecordInterface.h>
#include <coordinates/Coordinates/DirectionCoordinate.h>
#include <casa/Exceptions/Error.h>
#include <casa/Logging/LogIO.h>
#include <casa/Logging/LogOrigin.h>
#include <casa/BasicSL/Constants.h>
#include <casa/BasicMath/Math.h>
#include <casa/Quanta/Quantum.h>
#include <casa/Quanta/QuantumHolder.h>
#include <casa/Quanta/MVAngle.h>
#include <casa/Utilities/Assert.h>
#include <casa/Utilities/Precision.h>
#include <casa/BasicSL/String.h>

namespace casa { //# NAMESPACE CASA - BEGIN

TwoSidedShape::~TwoSidedShape() {
  DebugAssert(ok(), AipsError);
}

TwoSidedShape& TwoSidedShape::operator=(const TwoSidedShape& other) {
  if (this != &other) {
    ComponentShape::operator=(other);
    itsMajUnit = other.itsMajUnit;
    itsMinUnit = other.itsMinUnit;
    itsPaUnit = other.itsPaUnit;
    itsMajErr = other.itsMajErr;
    itsMinErr = other.itsMinErr;
    itsPaErr = other.itsPaErr;
  }
  DebugAssert(ok(), AipsError);
  return *this;
}

void TwoSidedShape::setWidth(const Quantity& majorAxis,
			     const Quantity& minorAxis,
			     const Quantity& positionAngle) {
  itsMajUnit = majorAxis.getFullUnit();
  itsMinUnit = minorAxis.getFullUnit();
  itsPaUnit = positionAngle.getFullUnit();
  const Unit rad("rad");
  setWidthInRad(majorAxis.getValue(rad), minorAxis.getValue(rad), 
		positionAngle.getValue(rad));
  DebugAssert(ok(), AipsError);
}

void TwoSidedShape::setWidth(const Quantum<Double>& majorAxis,
			     const Double axialRatio, 
			     const Quantum<Double>& positionAngle) {
  itsMinUnit = itsMajUnit = majorAxis.getFullUnit();
  itsPaUnit = positionAngle.getFullUnit();
  const Unit rad("rad");
  const Double majWidth = majorAxis.getValue(rad);
  setWidthInRad(majWidth, majWidth*axialRatio, positionAngle.getValue(rad));
  DebugAssert(ok(), AipsError);
}

Quantum<Double> TwoSidedShape::majorAxis() const {
  Quantum<Double> retVal(majorAxisInRad(), Unit("rad"));
  retVal.convert(itsMajUnit);
  return retVal;
}

Quantum<Double> TwoSidedShape::minorAxis() const {
  Quantum<Double> retVal(minorAxisInRad(), Unit("rad"));
  retVal.convert(itsMinUnit);
  return retVal;
}

Quantum<Double> TwoSidedShape::positionAngle() const {
  Quantum<Double> retVal(positionAngleInRad(), Unit("rad"));
  retVal.convert(itsPaUnit);
  return retVal;
}

Double TwoSidedShape::axialRatio() const {
  return minorAxisInRad()/majorAxisInRad();
}

void TwoSidedShape::setErrors(const Quantum<Double>& majorAxisError,
			      const Quantum<Double>& minorAxisError, 
			      const Quantum<Double>& positionAngleError) {
  if (ComponentShape::badError(majorAxisError) || 
      ComponentShape::badError(minorAxisError) || 
      ComponentShape::badError(positionAngleError)) {
    LogIO logErr(LogOrigin("TwoSidedShape", "setErrors(...)"));
    logErr << "The errors must be non-negative angular quantities."
	   << LogIO::EXCEPTION;
  }
  itsMajErr = majorAxisError;
  itsMinErr = minorAxisError;
  itsPaErr = positionAngleError;
}

const Quantum<Double>& TwoSidedShape::majorAxisError() const {
  return itsMajErr;
}

const Quantum<Double>& TwoSidedShape::minorAxisError() const {
  return itsMinErr;
}

const Quantum<Double>& TwoSidedShape::positionAngleError() const {
  return itsPaErr;
}

Double TwoSidedShape::axialRatioError() const {
  const Unit rad("rad");
  const Double relErr = itsMajErr.getValue(rad)/majorAxisInRad() + 
    itsMinErr.getValue(rad)/minorAxisInRad();
  return axialRatio() * relErr;
}

void TwoSidedShape::sample(Vector<Double>& scale, 
			   const Vector<MDirection::MVType>& directions, 
			   const MDirection::Ref& refFrame,
			   const MVAngle& pixelLatSize,
			   const MVAngle& pixelLongSize) const {
  ComponentShape::sample(scale, directions, refFrame, pixelLatSize,
			 pixelLongSize);
}

void TwoSidedShape::visibility(Vector<DComplex>& scale,
			       const Matrix<Double>& uvw,
			       const Double& frequency) const {
  ComponentShape::visibility(scale, uvw, frequency);
}

void TwoSidedShape::visibility(Matrix<DComplex>& scale,
			       const Matrix<Double>& uvw,
			       const Vector<Double>& frequency) const {
  ComponentShape::visibility(scale, uvw, frequency);
}

Bool TwoSidedShape::isSymmetric() const {
  DebugAssert(ok(), AipsError);
  return True;
}

uInt TwoSidedShape::nParameters() const {
  DebugAssert(ok(), AipsError);
  return 3;
}

void TwoSidedShape::setParameters(const Vector<Double>& newParms) {
  DebugAssert(newParms.nelements() == nParameters(), AipsError);
  DebugAssert(newParms(0) >= newParms(1), AipsError);
  DebugAssert(abs(newParms(2)) <= C::_2pi, AipsError);
  setWidthInRad(newParms(0), newParms(1), newParms(2));
  DebugAssert(ok(), AipsError);
}

Vector<Double> TwoSidedShape::parameters() const {
  DebugAssert(ok(), AipsError);
  Vector<Double> compParms(3);
  compParms(0) = majorAxisInRad();
  compParms(1) = minorAxisInRad();
  compParms(2) = positionAngleInRad();
  return compParms;
}

void TwoSidedShape::setErrors(const Vector<Double>& newErrors) {
  DebugAssert(newErrors.nelements() == nParameters(), AipsError);
  DebugAssert(allGE(newErrors, 0.0), AipsError);
  const Unit rad("rad");
  itsMajErr.setValue(newErrors(0));
  itsMajErr.setUnit(rad);
  itsMinErr.setValue(newErrors(1));
  itsMinErr.setUnit(rad);
  itsPaErr.setValue(newErrors(2));
  itsPaErr.setUnit(rad);
  DebugAssert(ok(), AipsError);
}

Vector<Double> TwoSidedShape::errors() const {
  DebugAssert(ok(), AipsError);
  Vector<Double> compErrors(3);
  compErrors(0) = itsMajErr.getBaseValue();
  compErrors(1) = itsMinErr.getBaseValue();
  compErrors(2) = itsPaErr.getBaseValue();
  return compErrors;
}

Vector<Double> TwoSidedShape::optParameters() const {
  DebugAssert(ok(), AipsError);
  return Vector<Double>(0);
}

void TwoSidedShape::setOptParameters(const Vector<Double>& newOptParms){
  DebugAssert(ok(), AipsError);
  if (&newOptParms == 0) {};
}

Bool TwoSidedShape::fromRecord(String& errorMessage,
			       const RecordInterface& record) {
  if (!ComponentShape::fromRecord(errorMessage, record)) return False;
  Quantum<Double> majorAxis, minorAxis, pa;
  if (!fromAngQRecord(majorAxis, errorMessage, "majoraxis", record) ||
      !fromAngQRecord(minorAxis, errorMessage, "minoraxis", record) ||
      !fromAngQRecord(pa, errorMessage, "positionangle", record)) {
    errorMessage += "Shape not changed\n";
    return False;
  }
  const Unit rad("rad");
  const Double majorAxisInRad = majorAxis.getValue(rad);
  const Double minorAxisInRad = minorAxis.getValue(rad);
//   // The near function is necessary for Intel processors (and doesn't hurt for
//   // other architectures) because of the extra precision that floating point
//   // variables have when returned in floating point registers. See
//   // http://aips2.nrao.edu/mail/aips2-lib/1101 for a discussion of this. The
//   // near function was added here and in the setMinorAxis function to fix
//   // defect AOCso00071
  if (majorAxisInRad < minorAxisInRad && 
      !near(minorAxisInRad, minorAxisInRad, 2*C::dbl_epsilon)) {
    errorMessage += "The major axis cannot be smaller than the minor axis\n";
    return False;
  }
  setWidth(majorAxis, minorAxis, pa);
  if (!fromAngQRecord(majorAxis, errorMessage, "majoraxiserror", record) ||
      !fromAngQRecord(minorAxis, errorMessage, "minoraxiserror", record) ||
      !fromAngQRecord(pa, errorMessage, "positionangleerror", record)) {
    errorMessage += "Shape errors not changed\n";
    return False;
  }
  setErrors(majorAxis, minorAxis, pa);
  DebugAssert(ok(), AipsError);
  return True;
}

Bool TwoSidedShape::toRecord(String& errorMessage,
			     RecordInterface& record) const {
  DebugAssert(ok(), AipsError);
  if (!ComponentShape::toRecord(errorMessage, record)) return False;
  {
    const QuantumHolder qHolder(majorAxis());
    Record qRecord;
    if (!qHolder.toRecord(errorMessage, qRecord)) {
      errorMessage += "Cannot convert the major axis to a record\n";
      return False;
    }
    record.defineRecord(RecordFieldId("majoraxis"), qRecord);
  }
  {
    const QuantumHolder qHolder(minorAxis());
    Record qRecord;
    if (!qHolder.toRecord(errorMessage, qRecord)) {
      errorMessage += "Cannot convert the minor axis to a record\n";
      return False;
    }
    record.defineRecord(RecordFieldId("minoraxis"), qRecord);
  }
  {
    const QuantumHolder qHolder(positionAngle());
    Record qRecord;
    if (!qHolder.toRecord(errorMessage, qRecord)) {
      errorMessage += "Cannot convert the position angle to a record\n";
      return False;
    }
    record.defineRecord(RecordFieldId("positionangle"), qRecord);
  }
  {
    const QuantumHolder qHolder(majorAxisError());
    Record qRecord;
    if (!qHolder.toRecord(errorMessage, qRecord)) {
      errorMessage += "Cannot convert the major axis error to a record\n";
      return False;
    }
    record.defineRecord(RecordFieldId("majoraxiserror"), qRecord);
  }
  {
    const QuantumHolder qHolder(minorAxisError());
    Record qRecord;
    if (!qHolder.toRecord(errorMessage, qRecord)) {
      errorMessage += "Cannot convert the minor axis error to a record\n";
      return False;
    }
    record.defineRecord(RecordFieldId("minoraxiserror"), qRecord);
  }
  {
    const QuantumHolder qHolder(positionAngleError());
    Record qRecord;
    if (!qHolder.toRecord(errorMessage, qRecord)) {
      errorMessage += "Cannot convert the position angle error to a record\n";
      return False;
    }
    record.defineRecord(RecordFieldId("positionangleerror"), qRecord);
  }
  return True;
}

Bool TwoSidedShape::convertUnit(String& errorMessage,
 				const RecordInterface& record) {
  const Unit deg("deg");
  {
    const String fieldString("majoraxis");
    if (!record.isDefined(fieldString)) {
      errorMessage += "The 'majoraxis' field does not exist\n";
      return False;
    }
    const RecordFieldId field(fieldString);
    if (!((record.dataType(field) == TpString) && 
	  (record.shape(field) == IPosition(1,1)))) {
      errorMessage += "The 'majoraxis' field must be a string\n";
      errorMessage += "(but not a vector of strings)\n";
      return False;
    }
    const Unit unit = Unit(record.asString(field));
    if (unit != deg) {
      errorMessage += 
	"Cannot convert the major axis width to a non angular unit";
      return False;
    }
    itsMajUnit = unit;
  }
  {
    const String fieldString("minoraxis");
    if (!record.isDefined(fieldString)) {
      errorMessage += "The 'minoraxis' field does not exist\n";
      return False;
    }
    const RecordFieldId field(fieldString);
    if (!((record.dataType(field) == TpString) && 
	  (record.shape(field) == IPosition(1,1)))) {
      errorMessage += "The 'minoraxis' field must be a string\n";
      errorMessage += "(but not a vector of strings)\n";
      return False;
    }
    const Unit unit = Unit(record.asString(field));
    if (unit != deg) {
      errorMessage += 
	"Cannot convert the minor axis width to a non angular unit";
      return False;
    }
    itsMinUnit = unit;
  }
  {
    const String fieldString("positionangle");
    if (!record.isDefined(fieldString)) {
      errorMessage += "The 'positionangle' field does not exist\n";
      return False;
    }
    const RecordFieldId field(fieldString);
    if (!((record.dataType(field) == TpString) && 
	  (record.shape(field) == IPosition(1,1)))) {
      errorMessage += "The 'positionangle' field must be a string\n";
      errorMessage += "(but not a vector of strings)\n";
      return False;
    }
    const Unit unit = Unit(record.asString(field));
    if (unit != deg) {
      errorMessage += 
	"Cannot convert the position angle to a non angular unit";
      return False;
    }
    itsPaUnit = unit;
  }
  DebugAssert(ok(), AipsError);
  return True;
}

Bool TwoSidedShape::ok() const {
  // The LogIO class is only constructed if an error is detected for
  // performance reasons. Both function static and file static variables
  // where considered and rejected for this purpose.
  if (!ComponentShape::ok()) return False;
  const Unit deg("deg");
  if (itsMajUnit != deg) {
    LogIO logErr(LogOrigin("TwoSidedCompRep", "ok()"));
    logErr << LogIO::SEVERE << "The major axis does not have angular units."
           << LogIO::POST;
    return False;
  }
  if (itsMinUnit != deg) {
    LogIO logErr(LogOrigin("TwoSidedCompRep", "ok()"));
    logErr << LogIO::SEVERE << "The minor axis does not have angular units."
           << LogIO::POST;
    return False;
  }
  if (itsPaUnit != deg) {
    LogIO logErr(LogOrigin("TwoSidedCompRep", "ok()"));
    logErr << LogIO::SEVERE <<"The position angle does not have angular units."
           << LogIO::POST;
    return False;
  }
  return True;
}

TwoSidedShape::TwoSidedShape()
  :ComponentShape(),
   itsMajUnit("arcmin"),
   itsMinUnit("arcmin"),
   itsPaUnit("deg"),
   itsMajErr(0, "arcmin"),
   itsMinErr(0, "arcmin"),
   itsPaErr(0, "deg")
{
  DebugAssert(ok(), AipsError);
}

TwoSidedShape::TwoSidedShape(const MDirection& direction, 
			     const Unit& majorAxisUnit,
			     const Unit& minorAxisUnit, 
			     const Unit& paUnit) 
  :ComponentShape(direction),
   itsMajUnit(majorAxisUnit),
   itsMinUnit(minorAxisUnit),
   itsPaUnit(paUnit),
   itsMajErr(0, "arcmin"),
   itsMinErr(0, "arcmin"),
   itsPaErr(0, "deg")
{
}

TwoSidedShape::TwoSidedShape(const TwoSidedShape& other) 
  :ComponentShape(other),
   itsMajUnit(other.itsMajUnit),
   itsMinUnit(other.itsMinUnit),
   itsPaUnit(other.itsPaUnit),
   itsMajErr(other.itsMajErr),
   itsMinErr(other.itsMinErr),
   itsPaErr(other.itsPaErr)
{
  DebugAssert(ok(), AipsError);
}


Vector<Double> TwoSidedShape::toPixel (const DirectionCoordinate& dirCoord)  const
//
// pars(0) = long cen   abs pix
// pars(1) = lat  cen   abs pix
// pars(2) = major   pix
// pars(3) = minor   pix
// pars(4) = pa radians;  pos +x (long) -> +y (lat)
//
{
   LogIO os(LogOrigin("TwoSidedShape", "toPixel"));
   Vector<Double> parameters(5);

// Do locations

   Vector<Double> pixelCen = ComponentShape::toPixel (dirCoord);
   parameters(0) = pixelCen(0);
   parameters(1) = pixelCen(1);

// Now convert the tip of the major axis to x/y pixel coordinates

   const MDirection dirRef = refDirection();
   Quantum<Double> majorWorld = majorAxis();
   Quantum<Double> paMajor = positionAngle();
   majorWorld.scale(0.5);
   Vector<Double> majorCart = widthToCartesian (majorWorld, paMajor, dirRef, dirCoord, pixelCen);

// Position angle of major axis.  
// atan2 gives pos +x (long) -> +y (lat).   put in range +/- pi
                                        
   MVAngle pa(atan2(majorCart(1), majorCart(0)));
   pa();

// I cannot just add 90deg to the world position angle. It is 90deg in the
// pixel coordinate frame, not the world frame.    So I have to work
// my way along the minor axis in pixel coordinates and locate
// the tip of the minor axis iteratively.  The algorithm
// below could be much smarter/faster with a binary search.

   Quantum<Double> minorWorld = minorAxis();
   Quantum<Double> paMinor = paMajor + Quantum<Double>(C::pi/2.0, Unit("rad"));
   minorWorld.scale(0.5);
//
   Double dX = sin(pa.radian());
   Double dY = cos(pa.radian());
//
   Vector<Double> posPix = pixelCen.copy();
   MDirection posWorld;
   MVDirection mvdRef = dirRef.getValue();
   Vector<Double> prevPosPix(2);
//
   Double minorWorldRad = minorWorld.getValue(Unit("rad"));
   Double sep = 0.0;
   Double prevSep = 0.0;
   Bool more = True;
   while (more) {
      dirCoord.toWorld(posWorld, posPix);
      MVDirection mvd = posWorld.getValue();
      sep = mvdRef.separation(mvd);
      if (sep > minorWorldRad) break;
//  
      prevPosPix = posPix;
      prevSep = sep;
//
      posPix(0) += dX;
      posPix(1) += dY;
   }
   Double frac = (minorWorldRad - prevSep) / (sep - prevSep);
   Double fracX = dX * frac;
   Double fracY = dY * frac;
//
   Vector<Double> minorCart(2);
   minorCart(0) = prevPosPix(0) + fracX - pixelCen(0);
   minorCart(1) = prevPosPix(1) + fracY - pixelCen(1);
//
   Double tmp1 =  2.0 * hypot(majorCart(0), majorCart(1));
   Double tmp2 =  2.0 * hypot(minorCart(0), minorCart(1));
//
   parameters(2) = max(tmp1,tmp2);
   parameters(3) = min(tmp1,tmp2);
   parameters(4) = pa.radian();
//
   return parameters;
}
 
Bool TwoSidedShape::fromPixel (const Vector<Double>& parameters,
                               const DirectionCoordinate& dirCoord)
//
// pars(0) = long cen   abs pix
// pars(1) = lat  cen   abs pix
// pars(2) = major   pix
// pars(3) = minor   pix
// pars(4) = pa radians; pos +x (long) -> +y (lat)
//
{
// Direction first

   Vector<Double> pixelCen(2);
   pixelCen(0) = parameters(0);
   pixelCen(1) = parameters(1);
   ComponentShape::fromPixel (pixelCen, dirCoord);
// Shape.  First put x/y p.a. into +y -> -x system

   Double pa0 = parameters(4) - C::pi_2; 
   MDirection tipMajor = directionFromCartesian (parameters(2), pa0, dirCoord, pixelCen);
//
   pa0 += C::pi_2;                      // minor axis position angle
   MDirection tipMinor = directionFromCartesian (parameters(3), pa0, dirCoord, pixelCen);

// Find tip directions
   const MDirection& directionRef = refDirection();       
   MVDirection mvdRef = directionRef.getValue();
   MVDirection mvdMajor = tipMajor.getValue();
   MVDirection mvdMinor = tipMinor.getValue();

// Separations

   Double tmp1 = 2 * mvdRef.separation(mvdMajor) * 3600 * 180.0 / C::pi;
   Double tmp2 = 2 * mvdRef.separation(mvdMinor) * 3600 * 180.0 / C::pi;

   Quantity majorAxis(max(tmp1,tmp2), Unit("arcsec"));
   Quantity minorAxis(min(tmp1,tmp2), Unit("arcsec"));
   Bool flipped = tmp2 > tmp1;
   Quantity pa;
   if (!flipped) {
      pa = mvdRef.positionAngle(mvdMajor, Unit("deg"));
   } else {
      pa = mvdRef.positionAngle(mvdMinor, Unit("deg"));
   }
   setWidth (majorAxis, minorAxis, pa);
   return flipped;
}

Vector<Double> TwoSidedShape::widthToCartesian (const Quantum<Double>& width,
                                                const Quantum<Double>& pa,
                                                const MDirection& dirRef,
                                                const DirectionCoordinate& dirCoord,
                                                const Vector<Double>& pixelCen) const
{

// Find MDirection of tip of axis
  
   MDirection dirTip = dirRef;
   dirTip.shiftAngle(width, pa);

// Convert to pixel 

   Vector<Double> pixelTip(2);
   if (!dirCoord.toPixel(pixelTip, dirTip)) {
      LogIO os(LogOrigin("TwoSidedShape", "widthToCartesian"));
      os << "DirectionCoordinate conversion to pixel failed because "
         << dirCoord.errorMessage() << LogIO::EXCEPTION;
   }

// Find offset cartesian components
   
   Vector<Double> cart(2);
   cart(0) = pixelTip(0) - pixelCen(0);
   cart(1) = pixelTip(1) - pixelCen(1);
   return cart;
}

MDirection TwoSidedShape::directionFromCartesian (Double width, Double pa,
                                                  const DirectionCoordinate& dirCoord,
                                                  const Vector<Double>& pixelCen) const
{

// Now find tips of major and minor axes in pixel coordinates
// and convert to world

   Double z = width / 2.0;
   Double x = -z * sin(pa);
   Double y =  z * cos(pa);
   MDirection dir;
   Vector<Double> pixelTip(2);
   pixelTip(0) = pixelCen(0) + x;
   pixelTip(1) = pixelCen(1) + y;
   ThrowIf(
		   ! dirCoord.toWorld(dir, pixelTip),
      "DirectionCoordinate conversion failed because "
	 + dirCoord.errorMessage()
   );
   return dir;
}

String TwoSidedShape::sizeToString(
		Quantity major, Quantity minor, Quantity posangle,
		Bool includeUncertainties, Quantity majorErr,
		Quantity minorErr, Quantity posanErr
) {
	// Inputs all as angle quanta
	Vector<String> angUnits(5);
	angUnits[0] = "deg";
	angUnits[1] = "arcmin";
	angUnits[2] = "arcsec";
	angUnits[3] = "marcsec";
	angUnits[4] = "uarcsec";
	// First force position angle to be between 0 and 180 deg
	if(posangle.getValue() < 0) {
		posangle += Quantity(180, "deg");
	}

	String prefUnits;
	Quantity vmax(max(fabs(major.getValue("arcsec")), fabs(minor.getValue("arcsec"))), "arcsec");

	for (uInt i=0; i<angUnits.size(); i++) {
		prefUnits = angUnits[i];
		if(vmax.getValue(prefUnits) > 1) {
			break;
		}
	}
	major.convert(prefUnits);
	minor.convert(prefUnits);
	majorErr.convert(prefUnits);
	minorErr.convert(prefUnits);

	Double vmaj = major.getValue();
	Double vmin = minor.getValue();

	// Formatting as "value +/- err" for symmetric errors

	Double dmaj = majorErr.getValue();
	Double dmin = minorErr.getValue();
	// position angle is always in degrees cuz users like that
	Double pa  = posangle.getValue("deg");
	Double dpa = posanErr.getValue("deg");

	Vector<Double> majVec(2), minVec(2), paVec(2);
	majVec[0] = vmaj;
	majVec[1] = dmaj;
	minVec[0] = vmin;
	minVec[1] = dmin;
	paVec[0] = pa;
	paVec[1] = dpa;
	uInt precision1 = precisionForValueErrorPairs(majVec, minVec);
	uInt precision2 = precisionForValueErrorPairs(paVec, Vector<Double>(0));

	ostringstream summary;
	summary << std::fixed << setprecision(precision1);
	summary << "       --- major axis FWHM:     " << major.getValue();
	if (includeUncertainties) {
		if (majorErr.getValue() == 0) {
			summary << " " << prefUnits << " (fixed)" << endl;
		}
		else {
			summary << " +/- " << majorErr.getValue()
				<< " " << prefUnits << endl;
		}
	}
	else {
		summary << " " << prefUnits << endl;
	}
	summary << "       --- minor axis FWHM:     " << minor.getValue();
	if (includeUncertainties) {
		if (minorErr.getValue() == 0) {
			summary << " " << prefUnits << " (fixed)" << endl;
		}
		else {
			summary << " +/- " << minorErr.getValue()
				<< " " << prefUnits << endl;
		}
	}
	else {
		summary << " " << prefUnits << endl;
	}
	summary << setprecision(precision2);
	summary << "       --- position angle: " << pa;
	if (includeUncertainties) {
		if (dpa == 0) {
			summary << "deg (fixed)" << endl;
		}
		else {
			summary << " +/- " << dpa << " deg" << endl;
		}
	}
	else {
		summary << " deg" << endl;
	}
	return summary.str();
}


// Local Variables: 
// compile-command: "gmake OPTLIB=1 TwoSidedShape"
// End: 


} //# NAMESPACE CASA - END

