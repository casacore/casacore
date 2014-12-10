//# HashMap2.cc: this defines HashMap, which is a hashed associative array
//# Copyright (C) 1995,1996
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
#include <casacore/casa/Containers/HashMap.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/casa/Exceptions/Error.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

uInt hashFunc(const String &s) {
    const char *ptr = s.chars();
    uInt hv = 0;
    while (*ptr)
	hv = hv * 33 + *ptr++;
    return hv;
}

uInt hashFunc(const float &f) {
    uInt *iptr = (uInt *) &f;
    if ( sizeof(float) >= (sizeof(uInt) << 1) )
	return *iptr + *(iptr+1);
    else
	return *iptr ;
}

uInt hashFunc(const double &d) {
    uInt *iptr = (uInt *) &d;
    if ( sizeof(double) >= (sizeof(uInt) << 1) )
	return *iptr + *(iptr+1);
    else
	return *iptr ;
}

uInt hashFunc(const int &i) {
    return (uInt) i;
}

uInt hashFunc(const unsigned int &i) {
    return i;
}

const Int &defaultHashValue(const Int *) {
    static Int val = 0;
    return val;
}
const uInt &defaultHashValue(const uInt *) {
    static uInt val = 0;
    return val;
}
const Long &defaultHashValue(const Long *) {
    static Long val = 0;
    return val;
}
const uLong &defaultHashValue(const uLong *) {
    static uLong val = 0;
    return val;
}
const Float &defaultHashValue(const Float *) {
    static Float val = 0.0;
    return val;
}
const Double &defaultHashValue(const Double *) {
    static Double val = 0.0;
    return val;
}
const lDouble &defaultHashValue(const lDouble *) {
    static lDouble val = 0.0;
    return val;
}

void throw_invalid_hashmapiter_error() {
  throw(AipsError("Use of invalid HashMap iterator."));
}

void throw_hashmapiter_init_error() {
  throw(AipsError("Bad initialization of HashMap iterator"));
}

} //# NAMESPACE CASACORE - END

