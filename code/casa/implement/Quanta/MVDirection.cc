//# MVDirection.cc: Vector of three direction cosines
//# Copyright (C) 1996,1997
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

//# Includes
#ifdef __GNUG__
#include <aips/Measures/Quantum.h>
typedef Quantum<Double> gpp_mvdirection_bug1;
#endif
#include <aips/Utilities/Assert.h>
#include <aips/Mathematics/Math.h>
#include <aips/Measures/RotMatrix.h>
#include <aips/Measures/MVDirection.h>
#include <aips/Measures/UnitVal.h>
#include <aips/Measures/QMath.h>
#include <aips/Measures/Unit.h>
#include <aips/Arrays/ArrayMath.h>
#include <aips/Arrays/ArrayLogical.h>

// MVDirection class

//# Constructors
MVDirection::MVDirection() :
MVPosition() {
    xyz(2) = Double(1.0);
}

MVDirection::MVDirection(const MVPosition &other) : 
MVPosition(other) {}

MVDirection &MVDirection::operator=(const MVDirection &other) {
    if (this != &other) {
	xyz = other.xyz;
    }
    return *this;
}

MVDirection::MVDirection(Double in0, Double in1, Double in2) : 
MVPosition(in0,in1,in2) {
    adjust();
}

MVDirection::MVDirection(Double in0) :
MVPosition() {
    xyz(0) = cos(in0);
    xyz(1) = sin(in0);
}

MVDirection::MVDirection(Double angle0, Double angle1) : 
MVPosition() {
    Double loc = cos(angle1);
    xyz(0) = cos(angle0)*loc;
    xyz(1) = sin(angle0)*loc;
    xyz(2) = sin(angle1);
}

MVDirection::MVDirection(const Quantity &angle0) :
MVPosition() {
    xyz(0) = ((cos(angle0)).getValue());
    xyz(1) = ((sin(angle0)).getValue());
    xyz(2) = 0;
}

MVDirection::MVDirection(const Quantity &angle0, const Quantity &angle1) : 
MVPosition() {
    Double loc = (cos(angle1)).getValue();
    xyz(0) = ((cos(angle0)).getValue()) * loc;
    xyz(1) = ((sin(angle0)).getValue()) * loc;
    xyz(2) = (sin(angle1)).getValue();
}

MVDirection::MVDirection(const Quantum<Vector<Double> > &angle) :
MVPosition(angle) {
    adjust();
}

MVDirection::MVDirection(const Vector<Double> &angle) :
MVPosition(angle) {
    adjust();
}

MVDirection::MVDirection(const Vector<Quantity> &angle) :
MVPosition() {
    uInt i; i = angle.nelements();
    if (i > 3 ) {
	throw (AipsError("Illegal vector length in MVDirection constructor"));
    } else if (i == 3) {
	angle(0).assert(UnitVal::NODIM);
	angle(1).assert(UnitVal::NODIM);
	angle(2).assert(UnitVal::NODIM);
	Int j;
	for (j = 0; j<i; j++) {
	    xyz(j) = angle(j).getValue();
	};
	adjust();
    } else {
	Vector<Double> tsin(i), tcos(i);
	Int j;
	for (j=0; j < i; j++) {
	    tsin(j) = (sin(angle(j))).getValue(); 
	    tcos(j) = (cos(angle(j))).getValue(); 
	};
	xyz = Double(0.0);
	if (i > 1) {
	    xyz(0) = tcos(0) * tcos(1);
	    xyz(1) = tsin(0) * tcos(1);
	    xyz(2) = tsin(1);
	} else if (i > 0) {
	    xyz(0) = tcos(0);
	    xyz(1) = tsin(0);
	} else {
	    xyz(2)=1.0;
	}
    }
}

//# Destructor
MVDirection::~MVDirection() {}

//# Operators
MVDirection &MVDirection::operator+=(const MVDirection &right) {
    xyz.ac() += right.xyz.ac();
    adjust();
    return *this;
}

MVDirection MVDirection::operator+(const MVDirection &right) const{
    MVDirection tmp = *this;
    tmp += right;
    return tmp;
}

MVDirection &MVDirection::operator-=(const MVDirection &right) {
    xyz.ac() -= right.xyz.ac();
    adjust();
    return *this;
}

MVDirection MVDirection::operator-(const MVDirection &right) const{
    MVDirection tmp = *this;
    tmp -= right;
    return tmp;
}

//# Member functions
void MVDirection::adjust() {
    Double length = sqrt(operator*(*this));
    if (length == 0) {
	xyz(2) = 1.0;
    } else if (length != 1.0) {
	xyz.ac() /= length;
    }
}

void MVDirection::adjust(Double &res) {
    res = sqrt(operator*(*this));
    if (res == 0) {
	xyz(2) = 1.0;
    } else if (res != 1.0) {
	xyz.ac() /= res;
    }
}

Vector<Double> MVDirection::get() const {
    Vector<Double> tmp(2);
    Double loc = xyz(0);
    if (loc == 0) {
	tmp(0) = asin(xyz(1));
    } else {
	tmp(0) = atan2(xyz(1),xyz(0));
    }
    tmp(1) = asin(xyz(2));
    return tmp;
}    

MVDirection MVDirection::crossProduct(const MVDirection &other) const {
  MVDirection res;
  res(0) = xyz(1)*other(2) - xyz(2)*other(1);
  res(1) = xyz(2)*other(0) - xyz(0)*other(2);
  res(2) = xyz(0)*other(1) - xyz(1)*other(0);
  return res;
}

MeasValue *MVDirection::clone() const {
    return (new MVDirection(*this));
}


MVDirection operator*(const RotMatrix &left, const MVDirection &right) {
    MVDirection result;
    for (Int i=0; i<3; i++) {
	result(i) = 0;
	for (Int j=0; j<3; j++) {
	    result(i) += left(i,j) * right(j);
	}
    }
    return result;
}

MVDirection operator*(const MVDirection &left, const RotMatrix &right) {
    MVDirection result(left);
    result *= right;
    return result;
}
