//# MVEpoch.cc: a class for high precision time
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
typedef Quantum<Double> gpp_mvepoch_bug1;
#endif
#include <iostream.h>
#include <iomanip.h>
#include <aips/Exceptions/Error.h>
#include <aips/Measures/Unit.h>
#include <aips/Measures/MVEpoch.h>
#include <aips/Measures/UnitVal.h>
#include <aips/Mathematics/Math.h>

//# Constants
const Double MVEpoch::secInDay(3600*24);

//# Constructors
MVEpoch::MVEpoch() : 
secFract(0), sec(0), kDday(0) {}

MVEpoch::MVEpoch(const MVEpoch &other) {
    secFract = other.secFract;
    sec = other.sec;
    kDday = other.kDday;
    adjust();
}

MVEpoch::MVEpoch(Double inday, Double infrac) :
secFract(0), sec(0), kDday(0) {
    addTime(inday);
    addTime(infrac);
    adjust();
}

MVEpoch::MVEpoch(const Quantity &in) : 
secFract(0), sec(0), kDday(0) {
    addTime(makeDay(in));
}

MVEpoch::MVEpoch(const Quantity &in1, const Quantity &in2) : 
secFract(0), sec(0), kDday(0) {
    addTime(makeDay(in1));
    addTime(makeDay(in2));
    adjust();
}

MVEpoch::MVEpoch(const Quantum<Vector<Double> > &in) : 
secFract(0), sec(0), kDday(0) {
    for (Int i=0; i<in.getValue().nelements(); i++) {
	addTime(makeDay(Quantity((in.getValue())(i),in.getUnit())));
    }
    adjust();
}

MVEpoch::MVEpoch(const Vector<Double> &inday) :
secFract(0), sec(0), kDday(0) {
    for (Int i=0; i<inday.nelements(); i++) {
	addTime(inday(i));
    }
    adjust();
}

MVEpoch::MVEpoch(const Vector<Quantity> &in) :
secFract(0), sec(0), kDday(0) {
    for (Int i=0; i<in.nelements(); i++) {
	addTime(makeDay(in(i)));
    }
    adjust();
}

//# Destructor
MVEpoch::~MVEpoch() {}

//# Operators
MVEpoch &MVEpoch::operator=(const MVEpoch &other) {
    if (this != &other) {
	secFract = other.secFract;
	sec = other.sec;
	kDday = other.kDday;
    }
    return *this;
}

MVEpoch &MVEpoch::operator+=(const MVEpoch &other) {
    secFract += other.secFract;
    sec += other.sec;
    kDday += other.kDday;
    adjust();
    return *this;
}

MVEpoch MVEpoch::operator+(const MVEpoch &other) const {
    MVEpoch tmp(*this);
    tmp += other;
    return tmp;
}

MVEpoch &MVEpoch::operator-=(const MVEpoch &other) {
    secFract -= other.secFract;
    sec -= other.sec;
    kDday -= other.kDday;
    adjust();
    return *this;
}

MVEpoch MVEpoch::operator-(const MVEpoch &other) const {
    MVEpoch tmp = *this;
    tmp -= other;
    return tmp;
}

Bool MVEpoch::operator==(const MVEpoch &other) const {
    return ToBool(kDday == other.kDday &&
		  sec == other.sec &&
		  secFract == other.secFract);
}

Bool MVEpoch::operator!=(const MVEpoch &other) const {
    return ToBool(!( *this == other));
}

Bool MVEpoch::near(const MVEpoch &other, Double tol) const {
    return ::near(get(), other.get(), tol);
}

Bool MVEpoch::nearAbs(const MVEpoch &other, Double tol) const {
    return ::nearAbs(get(), other.get(), tol);
}

//# Member functions
void MVEpoch::adjust() {
    while (secFract < 0) {
	secFract += 1; sec -= 1;
    }
    while (secFract >= 1) {
	secFract -= 1; sec += 1;
    }
    while (sec < 0) {
	sec += Long(MVEpoch::secInDay * 10000); 
	kDday -= 1;
    }
    while (sec >= MVEpoch::secInDay * 10000) {
	sec -= Long(MVEpoch::secInDay * 10000); 
	kDday += 1;
    }
}

void MVEpoch::adjust(Double &res) {
    adjust();
    res = 1.0;
}

Double MVEpoch::get() const{
    return ((secFract + sec)/MVEpoch::secInDay + kDday * 10000);
}

Quantity MVEpoch::getTime() const{
    return (Quantity(get(), "d"));
}

Quantity MVEpoch::getTime(const Unit &unit) const {
    return (getTime().get(unit));
}

Double MVEpoch::getDay() const{
    return (kDday * 10000 + ifloor(sec/MVEpoch::secInDay));
}

Double MVEpoch::getSecond() const{
    return (fmod(Double(sec),MVEpoch::secInDay));
}

Double MVEpoch::getFraction() const{
    return (secFract);
}

void MVEpoch::print(ostream &os) const {
    Int h = ifloor(getSecond()/3600.);
    Int m = ifloor(getSecond()/60. - h*60.);
    Double s = getSecond() - m*60. - h*3600.;
    Int prec = os.precision();
    Char fill = os.fill();
    os << getDay() << "::";
    os << setfill('0') << setw(2) << h << ":" <<
	setw(2) << m << ':' << 
	    setprecision(max(prec-2,2));
    Long oldb = os.setf(ios::fixed,ios::floatfield);
    os << setw(os.precision()+3) << s+getFraction() <<
	setprecision(prec);
    os.setf(oldb,ios::floatfield);
}

MeasValue *MVEpoch::clone() const {
    return (new MVEpoch(*this));
}

Double MVEpoch::makeDay(const Quantity &in) const {
    in.assert(UnitVal::TIME);
    return in.get("d").getValue();
}

void MVEpoch::addTime(Double in) {
    Long inday = ifloor(in/10000.);
    Double s = (in - inday*10000.) * MVEpoch::secInDay;
    Long insec = ifloor(s);
    kDday += inday;
    sec += insec;
    secFract += (s - insec);
}
