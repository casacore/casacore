//# Euler.cc: Vector of Euler rotation angles
//# Copyright (C) 1995,1996,1997,1998
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
#include <aips/Quanta/Quantum.h>
typedef Quantum<Double> gpp_euler1_bug1;
#endif
#include <aips/Utilities/Assert.h>
#include <aips/Quanta/Euler.h>
#include <aips/Mathematics/Math.h>
#include <aips/Arrays/ArrayMath.h>
#include <aips/Quanta/UnitVal.h>
#include <aips/Quanta/QMath.h>

// Euler class

//# Constructors
Euler::Euler() : euler(3), axes(3) {
    euler = Double(0.0);
    indgen(axes.ac(),1,1);
}

Euler::Euler(const Euler &other) : 
euler(3), axes(3) {
    euler = other.euler;
    axes = other.axes;
}

Euler &Euler::operator=(const Euler &other) {
    if (this != &other) {
	euler = other.euler;
	axes = other.axes;
    }
    return *this;
}

Euler::Euler(Double in0, Double in1, Double in2) : 
euler(3), axes(3){
    euler(0) = in0;
    euler(1) = in1;
    euler(2) = in2;
    indgen(axes.ac(),1,1);
}


Euler::Euler(Double in0, uInt ax0, Double in1, uInt ax1, Double in2,
	     uInt ax2) : 
euler(3), axes(3) {
    DebugAssert(ax0 <= 3 && ax1 <=3 && ax2 <=3, AipsError);
    euler(0) = in0;
    euler(1) = in1;
    euler(2) = in2;
    axes(0) = ax0;
    axes(1) = ax1;
    axes(2) = ax2;
}

Euler::Euler(const Quantity &in0) :
euler(3), axes(3) {
    euler(0) = Euler::makeRad(in0);
    euler(1) = 0;
    euler(2) = 0;
    indgen(axes.ac(),1,1);
}

Euler::Euler(const Quantity &in0, const Quantity &in1) :
euler(3), axes(3) {
    euler(0) = Euler::makeRad(in0);
    euler(1) = Euler::makeRad(in1);
    euler(2) = 0;
    indgen(axes.ac(),1,1);
}

Euler::Euler(const Quantity &in0, const Quantity &in1, const Quantity &in2) :
euler(3), axes(3) {
    euler(0) = Euler::makeRad(in0);
    euler(1) = Euler::makeRad(in1);
    euler(2) = Euler::makeRad(in2);
    indgen(axes.ac(),1,1);
}

Euler::Euler(const Quantity &in0, uInt ax0) :
euler(3), axes(3) {
    DebugAssert(ax0 <= 3, AipsError);
    euler(0) = Euler::makeRad(in0);
    euler(1) = 0;
    euler(2) = 0;
    axes(0) = ax0;
    axes(1) = 0;
    axes(2) = 0;
}
Euler::Euler(const Quantity &in0, uInt ax0, const Quantity &in1, uInt ax1) :
euler(3), axes(3) {
    DebugAssert(ax0 <= 3 && ax1 <=3, AipsError);
    euler(0) = Euler::makeRad(in0);
    euler(1) = Euler::makeRad(in1);
    euler(2) = 0;
    axes(0) = ax0;
    axes(1) = ax1;
    axes(2) = 0;
}
Euler::Euler(const Quantity &in0, uInt ax0, const Quantity &in1, uInt ax1,
	     const Quantity &in2, uInt ax2) :
euler(3), axes(3) {
    DebugAssert(ax0 <= 3 && ax1 <=3 && ax2 <=3, AipsError);
    euler(0) = Euler::makeRad(in0);
    euler(1) = Euler::makeRad(in1);
    euler(2) = Euler::makeRad(in2);
    axes(0) = ax0;
    axes(1) = ax1;
    axes(2) = ax2;
}

Euler::Euler(const Quantum<Vector<Double> > &in) :
euler(3), axes(3) {
    Vector<Double> tmp = Euler::makeRad(in);
    Int j; tmp.shape(j); j=min(j,3);
    for (Int i=0; i<j; i++) {
	euler(i) = tmp(i);
    }
    for (i=j; i<3; i++) {
	euler(i) = 0;
    }
    indgen(axes.ac(),1,1);
}

Euler::Euler(const Quantum<Vector<Double> > &in, const Vector<uInt> &ax) :
euler(3), axes(3) {
    Vector<Double> tmp = Euler::makeRad(in);
    Int j; tmp.shape(j); j=min(j,3); Int i; ax.shape(i); j=min(j,i);
    for (i=0; i<j; i++) {
	DebugAssert(ax(i) <= 3, AipsError);
	euler(i) = tmp(i);
	axes(i) = ax(i);
    }
    for (i=j; i<3; i++) {
	euler(i) = 0;
	axes(i) = 0;
    }
}

//# Destructor
Euler::~Euler() {}

//# Operators
Euler Euler::operator-() const {
    Euler tmp;
    for (Int i=0; i<3; i++) {
	tmp.euler(i) = -euler(2-i);
	tmp.axes(i) = axes(2-i);
    }
    return tmp;
}

Euler &Euler::operator+=(const Euler &right) {
    DebugAssert( axes(0) == right.axes(0) &&
		 axes(1) == right.axes(1) &&
		 axes(2) == right.axes(2), AipsError);
    euler.ac() += right.euler.ac();
    return *this;
}

Euler Euler::operator+(const Euler &right) const {
    Euler tmp = *this;
    tmp += right;
    return tmp;
}

Euler &Euler::operator-=(const Euler &right) {
    DebugAssert( axes(0) == right.axes(0) &&
		 axes(1) == right.axes(1) &&
		 axes(2) == right.axes(2), AipsError);
    euler.ac() -= right.euler.ac();
    return *this;
}

Euler Euler::operator-(const Euler &right) const {
    Euler tmp = *this;
    tmp -= right;
    return tmp;
}

Double &Euler::operator()(uInt which) {
    DebugAssert(which < 3, AipsError);
    return euler(which);
}

const Double &Euler::operator()(uInt which) const{
    DebugAssert(which < 3, AipsError);
    return euler(which);
}
    

//# Member functions

Double Euler::makeRad(const Quantity &in) {
    in.assert(UnitVal::ANGLE);
    return in.get().getValue();
}

Vector<Double> Euler::makeRad(const Quantum<Vector<Double> > &in) {
    in.assert(UnitVal::ANGLE);
    return in.get().getValue();
}

Quantum<Vector<Double> > Euler::getAngle() const {
    return Quantum<Vector<Double> >(euler,"rad");
}

Quantum<Vector<Double> > Euler::getAngle(const Unit &unit) const {
    return Quantum<Vector<Double> >(euler,"rad").get(unit);
}

void Euler::set(uInt which, uInt ax) {
    DebugAssert(which < 3 && ax <=3, AipsError);
    axes(which) = ax;
}

void Euler::set(uInt ax0, uInt ax1, uInt ax2) {
    DebugAssert(ax0 <= 3 && ax1 <=3 && ax2 <= 3, AipsError);
    axes(0) = ax0;
    axes(1) = ax1;
    axes(2) = ax2;
}

Int Euler::get(uInt which) const{
    DebugAssert(which < 3, AipsError);
    return axes(which);
}

ostream &operator<<(ostream &os, const Euler &eul) {
    os << eul.euler.ac();
    return os;
}
