//# LimbDarkenedDiskShape.cc: implementation of LimbDarkenedDiskShape.h which defines LimbDarkened Disk shape
//
//#  CASA - Common Astronomy Software Applications (http://casa.nrao.edu/)
//# Copyright (C) 2012
//# Associated Universities, Inc. Washington DC, USA.
//#
//# This library is free software; you can redistribute it and/or modify it
//# under the terms of the GNU Lesser General Public License as published by
//# the Free Software Foundation; either version 2.1 of the License, or (at your
//# option) any later version.
//#
//# This library is distributed in the hope that it will be useful, but WITHOUT
//# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
//# FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public
//# License for more details.
//#
//# You should have received a copy of the GNU Lesser General Public License
//# along with this library; if not, write to the Free Software Foundation,
//# Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
//#
//#
//#
//# $Id: LimbDarkenedDiskShape.cc 23251 2013-03-15 23:57:28Z tak.tsutsumi $

#include <components/ComponentModels/LimbDarkenedDiskShape.h>
//#include <components/ComponentModels/Flux.h>
#include <casa/Arrays/Matrix.h>
#include <casa/Arrays/Vector.h>
#include <casa/Arrays/VectorIter.h>
#include <casa/Arrays/ArrayMath.h>
#include <casa/Arrays/Array.h>
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

#ifdef HAVE_GSL
# include <gsl/gsl_sf_bessel.h>
#endif

namespace casa { //# NAMESPACE CASA - BEGIN

LimbDarkenedDiskShape::LimbDarkenedDiskShape()
  :TwoSidedShape(),
   itsMajValue(Quantity(1,"'").getValue("rad")),
   itsMinValue(Quantity(1,"'").getValue("rad")),
   itsPaValue(Quantity(0,"deg").getValue("rad")),
   itsHeight(1.0/(C::pi*itsMajValue*itsMinValue)),
   itsAttnFactor(0.05)
{
#ifndef HAVE_GSL
  throw AipsError("LimbDarkenedDiskShape cannot be used, because the "
                  "GSL package is not compiled in");
#endif
  DebugAssert(ok(), AipsError);
}

LimbDarkenedDiskShape::LimbDarkenedDiskShape(const MDirection& direction,
                                         const Quantum<Double>& majorAxis,
                                         const Quantum<Double>& minorAxis,
                                         const Quantum<Double>& positionAngle,
                                         const Float& n)
  :TwoSidedShape(direction, majorAxis.getFullUnit(), minorAxis.getFullUnit(), positionAngle.getFullUnit()),
   itsMajValue(majorAxis.getValue("rad")),
   itsMinValue(minorAxis.getValue("rad")),
   itsPaValue(positionAngle.getValue("rad")),
   itsHeight(1.0/(C::pi*itsMajValue*itsMinValue)),
   itsAttnFactor(n)
{
  DebugAssert(ok(), AipsError);
}


LimbDarkenedDiskShape::LimbDarkenedDiskShape(const MDirection& direction, 
                    const Quantum<Double>& width,
                    const Double axialRatio,
                    const Quantum<Double>& positionAngle, 
                    const Float& n)
  :TwoSidedShape(direction, width.getFullUnit(),
		    width.getFullUnit(), positionAngle.getFullUnit()),
   itsMajValue(width.getValue("rad")),
   itsMinValue(itsMajValue*axialRatio),
   itsPaValue(positionAngle.getValue("rad")),
   itsHeight(1.0/(C::pi*itsMajValue*itsMinValue)),
   itsAttnFactor(n)
{
  DebugAssert(ok(), AipsError);
}

LimbDarkenedDiskShape::LimbDarkenedDiskShape(const LimbDarkenedDiskShape& other)
  :TwoSidedShape(other),
   itsMajValue(other.itsMajValue),
   itsMinValue(other.itsMinValue),
   itsPaValue(other.itsPaValue),
   itsHeight(other.itsHeight),
   itsAttnFactor(other.itsAttnFactor)
{
  DebugAssert(ok(), AipsError);
}

  // The destructor
LimbDarkenedDiskShape::~LimbDarkenedDiskShape() {
  DebugAssert(ok(), AipsError);
}
  //#! Operators
  //The assignment operator
LimbDarkenedDiskShape& LimbDarkenedDiskShape::operator=(const LimbDarkenedDiskShape& other) {
  if (this != &other) {
    TwoSidedShape::operator=(other);
    itsMajValue = other.itsMajValue;
    itsMinValue = other.itsMinValue;
    itsPaValue = other.itsPaValue;
    itsHeight = other.itsHeight;
    itsAttnFactor = other.itsAttnFactor;
  }
  DebugAssert(ok(), AipsError);
  return *this;
}

//#! General Member Functions
// get the type of the shape (always returns ComponentType::LimbDakenDisk)
ComponentType::Shape LimbDarkenedDiskShape::type() const {
    DebugAssert(ok(), AipsError);
    return ComponentType::LDISK;
}


// use diskshape ones?
void LimbDarkenedDiskShape::setWidthInRad(const Double majorAxis,
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
//
Double LimbDarkenedDiskShape::majorAxisInRad() const {
  DebugAssert(ok(), AipsError);
  return itsMajValue;
}

Double LimbDarkenedDiskShape::minorAxisInRad() const {
  DebugAssert(ok(), AipsError);
  return itsMinValue;
}

Double LimbDarkenedDiskShape::positionAngleInRad() const {
  DebugAssert(ok(), AipsError);
  return itsPaValue;
}

Float LimbDarkenedDiskShape::getAttnFactor() const {
  DebugAssert(ok(), AipsError);
  return itsAttnFactor;
}
  //set n factor in darkening equation, I=I0(1-rho^2)^n/2
void LimbDarkenedDiskShape::setAttnFactor(const Float attnFactor) {
  itsAttnFactor=attnFactor;  
}

Vector<Double> LimbDarkenedDiskShape::optParameters() const {
  DebugAssert(ok(), AipsError);
  Vector<Double> optparm(1);
  optparm(0) = (Double)getAttnFactor();
  return optparm;
}

void LimbDarkenedDiskShape::setOptParameters(const Vector<Double>& newOptParms) {
  DebugAssert(ok(), AipsError);
  setAttnFactor((Float)newOptParms(0));
}

  // Calculate the proportion of the flux that is in a pixel of specified size
  // centered in the specified direction. The returned value will always be
  // between zero and one (inclusive).
Double LimbDarkenedDiskShape::sample(const MDirection& direction,
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


  // Same as the previous function except that many directions can be sampled
  // at once. The reference frame and pixel size must be the same for all the
  // specified directions.
void LimbDarkenedDiskShape::sample(Vector<Double>& scale,
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

DComplex LimbDarkenedDiskShape::visibility(const Vector<Double>& uvw,
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

void LimbDarkenedDiskShape::visibility(Matrix<DComplex>& scale,
                           const Matrix<Double>& uvw,
                           const Vector<Double>& frequency) const {

  scale.resize(uvw.ncolumn(), frequency.nelements());

  VectorIterator<DComplex> iter(scale, 0);
  for ( uInt k =0 ; k < frequency.nelements() ; ++k){
    visibility(iter.vector(), uvw, frequency(k));
    iter.next();
  }
}

void LimbDarkenedDiskShape::visibility(Vector<DComplex>& scale,
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
    // DComplex& thisVis = scale(i);
    ///    thisVis.imag() = 0.0;
    if (near(u + v, 0.0)) {
      ///      thisVis.real() = 1.0; // avoids dividing by zero in calcVis(...)
      scale[i] = DComplex(1.0, 0.0); // avoids dividing by zero
      // in calcVis(...)
    } else {
      if (doRotation) rotateVis(u, v, cpa, spa);
      ///      thisVis.real() = calcVis(u, v, factor);
      scale[i] = DComplex(calcVis(u, v, factor), 0.0);
    }
  }
}


ComponentShape* LimbDarkenedDiskShape::clone() const {
  DebugAssert(ok(), AipsError);
  ComponentShape* tmpPtr = new LimbDarkenedDiskShape(*this);
  AlwaysAssert(tmpPtr != 0, AipsError);
  return tmpPtr;
}

Bool LimbDarkenedDiskShape::ok() const {
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
  if (!near(itsHeight, 1.0/(C::pi*itsMajValue*itsMinValue), 2*C::dbl_epsilon)) {
    LogIO logErr(LogOrigin("DiskCompRep", "ok()"));
    logErr << LogIO::SEVERE << "The disk shape does not have"
           << " unit area"
           << LogIO::POST;
    return False;
  }
  return True;
}


  // return a pointer to this object.
const ComponentShape* LimbDarkenedDiskShape::getPtr() const {
    return this;
}

String LimbDarkenedDiskShape::sizeToString() const {
        return TwoSidedShape::sizeToString(
                Quantity(itsMajValue, "rad"),
                Quantity(itsMinValue, "rad"),
                Quantity(itsPaValue, "rad"),
                False
        );
}

//need to modify here
Double LimbDarkenedDiskShape::calcSample(const MDirection::MVType& compDirValue,
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

#ifndef HAVE_GSL
Double LimbDarkenedDiskShape::calcVis(Double, Double, const Double) const {
  throw AipsError("LimbDarkenedDiskShape cannot be used, because the "
                  "GSL package is not compiled in");
}
#else
Double LimbDarkenedDiskShape::calcVis(Double u, Double v, const Double factor) const {
  u *= itsMinValue;
  v *= itsMajValue;
  const double r = hypot(u, v) * factor;
  const double eta = 1 + itsAttnFactor/2.0;
  //return 2.0 * j1(r)/r;
  // Vi(u,v) for the limb-darkened disk from ALMA memo #594 
  // assume u, v are != 0.0 (in such case Vi(u,v)=Vo, handled in visibility())
  return pow(C::e,lgamma(eta + 1))*pow(2.0/r,eta)*gsl_sf_bessel_Jnu(eta,r); 
}
#endif


void LimbDarkenedDiskShape::rotateVis(Double& u, Double& v,
                        const Double cpa, const Double spa) {
  const Double utemp = u;
  u = u * cpa - v * spa;
  v = utemp * spa + v * cpa;
}

} //# NAMESPACE CASA - END
