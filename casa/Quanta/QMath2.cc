//# QMath2.cc: class to manipulate physical, dimensioned quantities
//# Copyright (C) 1994,1995,1996,1998,2000,2004
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
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/Quanta/QMath.h>
#include <casacore/casa/BasicMath/Math.h>
#include <casacore/casa/Arrays/ArrayMath.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

int32_t ceil(const int32_t &val) {
    return (val);
}

int32_t floor(const int32_t &val) {
    return (val);
}

float real(const float &val) {
   return val;
}

double real(const double &val) {
   return val;
}

Array<Complex> operator *(const Array<Complex> &in, double f) {
  return in * Complex(f);
}
Array<Complex> operator /(const Array<Complex> &in, double f) {
  return in / Complex(f);
}

Array<DComplex> operator *(const Array<DComplex> &in, double f) {
  return in * DComplex(f);
}
Array<DComplex> operator /(const Array<DComplex> &in, double f) {
  return in / DComplex(f);
}

Array<float> operator *(const Array<float> &in, double f) {
  return in * float(f);
}
Array<float> operator /(const Array<float> &in, double f) {
  return in / float(f);
}

Array<int32_t> operator *(const Array<int32_t> &in, double f) {
  return in * int32_t(f);
}
Array<int32_t> operator /(const Array<int32_t> &in, double f) {
  return in / int32_t(f);
}

} //# NAMESPACE CASACORE - END

