//# Euler.cc: Vector of Euler rotation angles
//# Copyright (C) 1995,1996,1997,1998,1999,2000
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

//# Includes
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Quanta/Euler.h>
#include <casacore/casa/BasicMath/Math.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/IO/ArrayIO.h>
#include <casacore/casa/Quanta/UnitVal.h>
#include <casacore/casa/Quanta/QMath.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

// Euler class

//# Constructors
Euler::Euler() : euler(3), axes(3) {
    euler = double(0.0);
    indgen(axes,1,1);
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

Euler::Euler(double in0, double in1, double in2) : 
euler(3), axes(3) {
    euler(0) = in0;
    euler(1) = in1;
    euler(2) = in2;
    indgen(axes,1,1);
}


Euler::Euler(double in0, uint32_t ax0, double in1, uint32_t ax1, double in2,
	     uint32_t ax2) : 
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
    indgen(axes,1,1);
}

Euler::Euler(const Quantity &in0, const Quantity &in1) :
  euler(3), axes(3) {
    euler(0) = Euler::makeRad(in0);
    euler(1) = Euler::makeRad(in1);
    euler(2) = 0;
    indgen(axes,1,1);
}

Euler::Euler(const Quantity &in0, const Quantity &in1, const Quantity &in2) :
  euler(3), axes(3) {
    euler(0) = Euler::makeRad(in0);
    euler(1) = Euler::makeRad(in1);
    euler(2) = Euler::makeRad(in2);
    indgen(axes,1,1);
}

Euler::Euler(const Quantity &in0, uint32_t ax0) :
  euler(3), axes(3) {
    DebugAssert(ax0 <= 3, AipsError);
    euler(0) = Euler::makeRad(in0);
    euler(1) = 0;
    euler(2) = 0;
    axes(0) = ax0;
    axes(1) = 0;
    axes(2) = 0;
}
Euler::Euler(const Quantity &in0, uint32_t ax0, const Quantity &in1, uint32_t ax1) :
  euler(3), axes(3) {
    DebugAssert(ax0 <= 3 && ax1 <=3, AipsError);
    euler(0) = Euler::makeRad(in0);
    euler(1) = Euler::makeRad(in1);
    euler(2) = 0;
    axes(0) = ax0;
    axes(1) = ax1;
    axes(2) = 0;
}
Euler::Euler(const Quantity &in0, uint32_t ax0, const Quantity &in1, uint32_t ax1,
	     const Quantity &in2, uint32_t ax2) :
  euler(3), axes(3) {
    DebugAssert(ax0 <= 3 && ax1 <=3 && ax2 <=3, AipsError);
    euler(0) = Euler::makeRad(in0);
    euler(1) = Euler::makeRad(in1);
    euler(2) = Euler::makeRad(in2);
    axes(0) = ax0;
    axes(1) = ax1;
    axes(2) = ax2;
}

Euler::Euler(const Quantum<Vector<double> > &in) :
 euler(3), axes(3) {
    int32_t i;
    Vector<double> tmp = Euler::makeRad(in);
    int32_t j=tmp.size(); j=min(j,3);
    for (i=0; i<j; i++) {
	euler(i) = tmp(i);
    }
    for (i=j; i<3; i++) {
	euler(i) = 0;
    }
    indgen(axes,1,1);
}

Euler::Euler(const Quantum<Vector<double> > &in, const Vector<uint32_t> &ax) :
  euler(3), axes(3) {
    Vector<double> tmp = Euler::makeRad(in);
    int32_t j=tmp.size(); j=min(j,3); int32_t i=ax.size(); j=min(j,i);
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
Euler::~Euler() {
}

//# Operators
Euler Euler::operator-() const {
    Euler tmp;
    for (int32_t i=0; i<3; i++) {
	tmp.euler(i) = -euler(2-i);
	tmp.axes(i) = axes(2-i);
    }
    return tmp;
}

Euler &Euler::operator+=(const Euler &right) {
    DebugAssert( axes(0) == right.axes(0) &&
		 axes(1) == right.axes(1) &&
		 axes(2) == right.axes(2), AipsError);
    euler += right.euler;
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
    euler -= right.euler;
    return *this;
}

Euler Euler::operator-(const Euler &right) const {
    Euler tmp = *this;
    tmp -= right;
    return tmp;
}

double &Euler::operator()(uint32_t which) {
    DebugAssert(which < 3, AipsError);
    return euler(which);
}

const double &Euler::operator()(uint32_t which) const{
    DebugAssert(which < 3, AipsError);
    return euler(which);
}
    

//# Member functions

double Euler::makeRad(const Quantity &in) {
    in.assure(UnitVal::ANGLE);
    return in.get().getValue();
}

Vector<double> Euler::makeRad(const Quantum<Vector<double> > &in) {
    in.assure(UnitVal::ANGLE);
    return in.get().getValue();
}

Quantum<Vector<double> > Euler::getAngle() const {
    return Quantum<Vector<double> >(euler,"rad");
}

Quantum<Vector<double> > Euler::getAngle(const Unit &unit) const {
    return Quantum<Vector<double> >(euler,"rad").get(unit);
}

void Euler::set(uint32_t which, uint32_t ax) {
    DebugAssert(which < 3 && ax <=3, AipsError);
    axes(which) = ax;
}

void Euler::set(uint32_t ax0, uint32_t ax1, uint32_t ax2) {
    DebugAssert(ax0 <= 3 && ax1 <=3 && ax2 <= 3, AipsError);
    axes(0) = ax0;
    axes(1) = ax1;
    axes(2) = ax2;
}

int32_t Euler::get(uint32_t which) const{
    DebugAssert(which < 3, AipsError);
    return axes(which);
}

ostream &operator<<(ostream &os, const Euler &eul) {
    os << eul.euler;
    return os;
}

} //# NAMESPACE CASACORE - END

