//# GaussianShape.cc:
//# Copyright (C) 1998
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

#include <trial/ComponentModels/GaussianShape.h>
#include <aips/Arrays/Vector.h>
#include <aips/Logging/LogIO.h>
#include <aips/Logging/LogOrigin.h>
#include <trial/Tasking/QuantumParameterAccessor.h>
#include <trial/Measures/DOmeasures.h>
#include <aips/Containers/RecordInterface.h>
#include <aips/Containers/RecordFieldId.h>
#include <aips/Containers/Record.h>
#include <aips/Exceptions/Error.h>
#include <aips/Glish/GlishRecord.h>
#include <aips/Mathematics/Constants.h>
#include <aips/Mathematics/Complex.h>
#include <aips/Mathematics/Math.h>
#include <aips/Measures/MCDirection.h>
#include <aips/Measures/MVAngle.h>
#include <aips/Measures/MeasConvert.h>
#include <aips/Measures/Quantum.h>
#include <aips/Utilities/Assert.h>
#include <aips/Utilities/String.h>

#ifdef __GNUG__
typedef MeasConvert<MDirection,MVDirection,MCDirection> 
        gpp_measconvert_mdirection_mvdirection_mcdirection;
#endif

GaussianShape::GaussianShape()
  :itsDir(),
   itsDirValue(itsDir.getValue()),
   itsRefFrame((MDirection::Types) itsDir.getRef().getType()),
   itsShape(1.0, 0.0, 0.0, Quantity(1,"'").getValue("rad"), 1.0, 0.0),
   itsFT(itsShape)
{
  itsShape.setFlux(1.0);
  updateFT();
  DebugAssert(ok(), AipsError);
}

GaussianShape::GaussianShape(const MDirection & direction, 
			     const MVAngle & majorAxis,
			     const MVAngle & minorAxis,
			     const MVAngle & positionAngle)
  :itsDir(direction),
   itsDirValue(itsDir.getValue()),
   itsRefFrame((MDirection::Types) itsDir.getRef().getType()),
   itsShape(1.0, 0.0, 0.0, majorAxis.radian(),
	    minorAxis.radian()/majorAxis.radian(), positionAngle.radian()),
   itsFT(itsShape)
{
  // Adjust the flux of the Gaussian now that the width is correctly set
  itsShape.setFlux(1.0);
  updateFT();
  DebugAssert(ok(), AipsError);
}

GaussianShape::GaussianShape(const MDirection & direction,
			     const MVAngle & width,
			     const Double axialRatio,
			     const MVAngle & positionAngle) 
  :itsDir(direction),
   itsDirValue(itsDir.getValue()),
   itsRefFrame((MDirection::Types) itsDir.getRef().getType()),
   itsShape(1.0, 0.0, 0.0, width.radian(), axialRatio, positionAngle.radian()),
   itsFT(itsShape)
{
  // Adjust the flux of the Gaussian now that the width is correctly set
  itsShape.setFlux(1.0);
  updateFT();
  DebugAssert(ok(), AipsError);
}

GaussianShape::GaussianShape(const GaussianShape & other) 
  :itsDir(other.itsDir),
   itsDirValue(other.itsDirValue),
   itsRefFrame(other.itsRefFrame),
   itsShape(other.itsShape),
   itsFT(other.itsFT)
{
  DebugAssert(ok(), AipsError);
}

GaussianShape::~GaussianShape() {
  DebugAssert(ok(), AipsError);
}

GaussianShape & GaussianShape::operator=(const GaussianShape & other) {
  if (this != &other) {
    itsDir = other.itsDir;
    itsDirValue = other.itsDirValue;
    itsRefFrame = other.itsRefFrame;
    itsShape = other.itsShape;
    itsFT = other.itsFT;
  }
  DebugAssert(ok(), AipsError);
  return *this;
}

ComponentType::Shape GaussianShape::shape() const {
  DebugAssert(ok(), AipsError);
  return ComponentType::POINT;
}

void GaussianShape::setRefDirection(const MDirection & newRefDir) {
  itsDir = newRefDir;
  itsDirValue = newRefDir.getValue();
  itsRefFrame = (MDirection::Types) newRefDir.getRef().getType();
  DebugAssert(ok(), AipsError);
}

const MDirection & GaussianShape::refDirection() const {
  DebugAssert(ok(), AipsError);
  return itsDir;
}

void GaussianShape::setWidth(const MVAngle & majorAxis,
			     const MVAngle & minorAxis, 
			     const MVAngle & positionAngle) {
  Vector<Double> angle(2);
  angle(0) = majorAxis.radian();
  angle(1) = minorAxis.radian();
  itsShape.setWidth(angle);
  itsShape.setPA(positionAngle.radian());
  // Adjusting the width normally keeps the height constant and modifies the
  // flux. Modify this behaviour by restoring the flux
  itsShape.setFlux(1.0);
  updateFT();
  DebugAssert(ok(), AipsError);
}

void GaussianShape::setWidth(const MVAngle & majorAxis,
			     const Double axialRatio, 
			     const MVAngle & positionAngle) {
  
  setWidth(majorAxis, MVAngle(majorAxis.radian()*axialRatio), positionAngle);
}

void GaussianShape::width(MVAngle & majorAxis, MVAngle & minorAxis,
	   MVAngle & positionAngle) const {
  DebugAssert(ok(), AipsError);
  majorAxis = MVAngle(itsShape.majorAxis());
  minorAxis = MVAngle(itsShape.minorAxis());
  positionAngle = MVAngle(itsShape.PA());
}

void GaussianShape::width(MVAngle & majorAxis, Double & axialRatio,
	   MVAngle & positionAngle) const {
  DebugAssert(ok(), AipsError);
  majorAxis = MVAngle(itsShape.majorAxis());
  axialRatio = itsShape.axialRatio();
  positionAngle = MVAngle(itsShape.PA());
}

void GaussianShape::majorAxis(MVAngle & majorAxis) const {
  DebugAssert(ok(), AipsError);
  majorAxis = MVAngle(itsShape.majorAxis());
}

MVAngle GaussianShape::majorAxis() const {
  DebugAssert(ok(), AipsError);
  return MVAngle(itsShape.majorAxis());
}

void GaussianShape::minorAxis(MVAngle & minorAxis) const {
  DebugAssert(ok(), AipsError);
  minorAxis = MVAngle(itsShape.minorAxis());
}

MVAngle GaussianShape::minorAxis() const {
  DebugAssert(ok(), AipsError);
  return MVAngle(itsShape.minorAxis());
}

void GaussianShape::axialRatio(Double & axialRatio) const {
  DebugAssert(ok(), AipsError);
  axialRatio = itsShape.axialRatio();
}

Double GaussianShape::axialRatio() const {
  DebugAssert(ok(), AipsError);
  return itsShape.axialRatio();
}

void GaussianShape::positionAngle(MVAngle & positionAngle) const {
  DebugAssert(ok(), AipsError);
  positionAngle = MVAngle(itsShape.PA());
}

MVAngle GaussianShape::positionAngle() const {
  DebugAssert(ok(), AipsError);
  return MVAngle(itsShape.PA());
}

Double GaussianShape::scale(const MDirection & direction, 
			    const MVAngle & pixelSize) const {
  DebugAssert(ok(), AipsError);
  MVDirection dirVal = direction.getValue();
  if (direction.getRef().getType() != itsRefFrame) {
    dirVal = MDirection::Convert(direction, itsRefFrame)().getValue();
  }
  const Double separation = itsDirValue.separation(dirVal);
  const Double pa = itsDirValue.positionAngle(dirVal);
  const Double pixSize = pixelSize.radian();
  return pixSize * pixSize * itsShape(separation*sin(pa), separation*cos(pa));
}

void GaussianShape::visibility(DComplex & result, const Vector<Double> & uvw,
			       const Double & frequency) const {
  DebugAssert(ok(), AipsError);
  result.re = visibility(uvw, frequency);
  result.im = 0.0;
}

Double GaussianShape::visibility(const Vector<Double> & uvw,
				 const Double & frequency) const {
  DebugAssert(uvw.nelements() == 3, AipsError);
  DebugAssert(frequency > 0, AipsError);
  DebugAssert(ok(), AipsError);
  const Double wavenumber = frequency/C::c;
  return itsFT(uvw(0)*wavenumber, uvw(1)*wavenumber);
}

Bool GaussianShape::isSymmetric() const {
  DebugAssert(ok(), AipsError);
  return True;
}

uInt GaussianShape::nShapeParameters() const {
  DebugAssert(ok(), AipsError);
  return 3;
}

void GaussianShape::setShapeParameters(const Vector<Double> & newParms) {
  AlwaysAssert(newParms.nelements() == nShapeParameters(), AipsError);
  DebugAssert(newParms(0) >= newParms(1), AipsError);
  DebugAssert(abs(newParms(2)) <= C::_2pi, AipsError);
  itsShape.setMajorAxis(newParms(0));
  itsShape.setMinorAxis(newParms(1));
  itsShape.setPA(newParms(2));
  // Adjusting the width normally keeps the height constant and modifies the
  // flux. Modify this behaviour by restoring the flux
  itsShape.setFlux(1.0);
  updateFT();
  DebugAssert(ok(), AipsError);
}

void GaussianShape::shapeParameters(Vector<Double> & compParms) const {
  AlwaysAssert(compParms.nelements() == nShapeParameters(), AipsError);
  compParms(0) = itsShape.majorAxis();
  compParms(1) = itsShape.minorAxis();
  compParms(2) = itsShape.PA();
  DebugAssert(ok(), AipsError);
}

Bool GaussianShape::fromRecord(String & errorMessage,
			       const RecordInterface & record) {
  if (!ComponentShape::readDir(errorMessage, record)) return False;

  GlishRecord gRecord;
  gRecord.fromRecord(record);
  MVAngle majorAxis;
  {
    QuantumParameterAccessor<Double> qpa("majoraxis", ParameterSet::In,
					 &gRecord);
    if (!qpa.copyIn(errorMessage)) return False;
    if (!qpa().isConform("deg")) {
      errorMessage += "\nThe 'majoraxis' field does not have angular units";
      return False;
    }
    majorAxis = MVAngle(qpa());
  }
  MVAngle minorAxis;
  {
    QuantumParameterAccessor<Double> qpa("minoraxis", ParameterSet::In,
					 &gRecord);
    if (!qpa.copyIn(errorMessage)) return False;
    if (!qpa().isConform("deg")) {
      errorMessage += "\nThe 'minoraxis' field does not have angular units";
      return False;
    }
    minorAxis = MVAngle(qpa());
  }
  MVAngle pa;
  {
    QuantumParameterAccessor<Double> qpa("positionangle", ParameterSet::In,
					 &gRecord);
    if (!qpa.copyIn(errorMessage)) return False;
    if (!qpa().isConform("deg")) {
      errorMessage +="\nThe 'positionangle' field does not have angular units";
      return False;
    }
    pa = MVAngle(qpa());
  }
  setWidth(majorAxis, minorAxis, pa);
  DebugAssert(ok(), AipsError);
  return True;
}

Bool GaussianShape::toRecord(String & errorMessage,
			     RecordInterface & record) const {
  DebugAssert(ok(), AipsError);

  record.define(RecordFieldId("type"), String("gaussian"));
  if (!ComponentShape::addDir(errorMessage, record)) return False;
  {
    GlishRecord gParmRecord = measures::toRecord(majorAxis().get("'"));
    Record parmRecord;
    gParmRecord.toRecord(parmRecord);
    record.defineRecord(RecordFieldId("majoraxis"), parmRecord);
    gParmRecord = measures::toRecord(minorAxis().get("'"));
    gParmRecord.toRecord(parmRecord);
    record.defineRecord(RecordFieldId("minoraxis"), parmRecord);
    gParmRecord = measures::toRecord(positionAngle().get("'"));
    gParmRecord.toRecord(parmRecord);
    record.defineRecord(RecordFieldId("positionangle"), parmRecord);
  }
  return True;
}

Bool GaussianShape::ok() const {
  // The LogIO class is only constructed if an error is detected for
  // performance reasons. Both function static and file static variables
  // where considered and rejected for this purpose.
  if (!near(itsShape.flux(), 1.0, C::dbl_epsilon)) {
    LogIO logErr(LogOrigin("GaussianCompRep", "ok()"));
    logErr << LogIO::SEVERE << "The internal Gaussian shape does not have"
	   << " unit area"
           << LogIO::POST;
    return False;
  }
  if (!near(itsFT.height(), 1.0, C::dbl_epsilon)) {
    LogIO logErr(LogOrigin("GaussianCompRep", "ok()"));
    logErr << LogIO::SEVERE << "The cached Fourier Transform of"
	   << " the internal Gaussian shape does not have"
	   << " unit height"
           << LogIO::POST;
    return False;
  }
  return True;
}

void GaussianShape::updateFT() {
  const Double factor = 4.0*C::ln2/C::pi;
  Vector<Double> width(2);
  width(0) = factor/itsShape.minorAxis();
  width(1) = factor/itsShape.majorAxis();
  itsFT.setWidth(width);
  Double pa = itsShape.PA();
  pa += C::pi_2;
  itsFT.setPA(pa);
}
// Local Variables: 
// compile-command: "gmake OPTLIB=1 GaussianShape"
// End: 
