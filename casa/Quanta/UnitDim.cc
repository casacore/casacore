//# UnitDim.cc: defines the (private) class describing basic SI dimensions
//# Copyright (C) 1994,1995,1996,1997,1998
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

#include <casacore/casa/BasicSL/String.h>
#include <casacore/casa/Quanta/UnitDim.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

void UnitDim::init() {
    for (int32_t i=0; i<UNITDIM_DLNUMBER; i++) {
	unitLong[i] = 0;
    }
    unitDim = reinterpret_cast<signed char *>(unitLong);
}

UnitDim::UnitDim(const UnitDim &other) {
    for (int32_t i=0; i<UNITDIM_DLNUMBER; i++) {
	unitLong[i] = other.unitLong[i];
    }
    unitDim = reinterpret_cast<signed char *>(unitLong);
}

void UnitDim::init(int32_t pos) {
    for (int32_t i=0; i<UNITDIM_DLNUMBER; i++) {
	unitLong[i] = 0;
    }
    unitDim = reinterpret_cast<signed char *>(unitLong);
    unitDim[pos]=1;
}

UnitDim::~UnitDim() {}

UnitDim &UnitDim::operator=(const UnitDim &other) {
    if (this != &other) {
	for (int32_t i=0; i<UNITDIM_DLNUMBER; i++) {
	    unitLong[i] = other.unitLong[i];
	}
        unitDim = reinterpret_cast<signed char *>(unitLong);
    }
    return *this;
}

UnitDim &UnitDim::operator*=(const UnitDim &other) {
    for (int32_t i=0; i<Dnumber; i++) {
	unitDim[i] += other.unitDim[i];
    }
    return *this;
}

UnitDim UnitDim::operator*(const UnitDim &other) const {
    UnitDim result = *this;
    result *= other;
    return result;
}

UnitDim &UnitDim::operator/=(const UnitDim &other) {
    for (int32_t i=0; i<Dnumber; i++) {
	unitDim[i] -= other.unitDim[i];
    }
    return *this;
}

UnitDim UnitDim::operator/(const UnitDim &other) const {
    UnitDim result = *this;
    result /= other;
    return result;
}

UnitDim UnitDim::pow(int32_t p) {
    UnitDim loc= *this;
    for (int32_t i=0; i<Dnumber; i++) {
	loc.unitDim[i] *= p;
    }
    return loc;
}

bool UnitDim::operator==(const UnitDim &other) const {
    for (int32_t i=0; i<UNITDIM_DLNUMBER; i++) {
	if (unitLong[i] != other.unitLong[i]) { return false;}
     }
    return true;
}

bool UnitDim::operator!=(const UnitDim &other) const {
    for (int32_t i=0; i<UNITDIM_DLNUMBER; i++) {
	if (unitLong[i] != other.unitLong[i]) { return true;}
     }
    return false;
}

const String& UnitDim::dimName(uint32_t which) {
  static const String Nlist[UnitDim::Dnumber] = {
    "m",
    "kg",
    "s",
    "A",
    "K",
    "cd",
    "mol",
    "rad",
    "sr",
    "_"
  };
  return Nlist[which];
}

const String& UnitDim::dimFull(uint32_t which) {
  static const String Flist[UnitDim::Dnumber] = {
    "metre",
    "kilogram",
    "second",
    "ampere",
    "kelvin",
    "candela",
    "mole",
    "radian",
    "steradian",
    "undimensioned"
  };
  return Flist[which];
}

ostream& operator<< (ostream &os, const UnitDim &du) {
    String chck(" ");
    for (int32_t i=0; i<UnitDim::Dnumber; i++) {
	if (du.unitDim[i] != 0) {
	    if (du.unitDim[i] == 1) {
		os << chck << UnitDim::dimName(i);
	    } else {
		os << chck << UnitDim::dimName(i) << int32_t(du.unitDim[i]);
	    }
	    chck = ".";
	}
    }
    return os;
}

} //# NAMESPACE CASACORE - END
