//# QMath2.cc: class to manipulate physical, dimensioned quantities
//# Copyright (C) 1994,1995,1996,1998
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
#include <aips/Exceptions/Error.h>
#include <aips/Measures/QMath.h>
#include <aips/Mathematics/Math.h>
#include <aips/Arrays/ArrayMath.h>

Complex ceil(const Complex &val) {
    return (Complex(ceil(val.real()),ceil(val.imag())));
}

Complex floor(const Complex &val) {
    return (Complex(floor(val.real()),floor(val.imag())));
}

DComplex ceil(const DComplex &val) {
    return (DComplex(ceil(val.real()),ceil(val.imag())));
}

DComplex floor(const DComplex &val) {
    return (DComplex(floor(val.real()),floor(val.imag())));
}

IComplex ceil(const IComplex &val) {
    return (val);
}

IComplex floor(const IComplex &val) {
    return (val);
}

Int ceil(const Int &val) {
    return (val);
}

Int floor(const Int &val) {
    return (val);
}

Float real(const Float &val) {
   return val;
}

Double real(const Double &val) {
   return val;
}

Array<Complex> operator *(const Array<Complex> &in, Double f) {
  return in * Complex(f);
}
Array<Complex> operator /(const Array<Complex> &in, Double f) {
  return in / Complex(f);
}

Array<DComplex> operator *(const Array<DComplex> &in, Double f) {
  return in * DComplex(f);
}
Array<DComplex> operator /(const Array<DComplex> &in, Double f) {
  return in / DComplex(f);
}

Array<Float> operator *(const Array<Float> &in, Double f) {
  return in * Float(f);
}
Array<Float> operator /(const Array<Float> &in, Double f) {
  return in / Float(f);
}

Array<Int> operator *(const Array<Int> &in, Double f) {
  return in * Int(f);
}
Array<Int> operator /(const Array<Int> &in, Double f) {
  return in / Int(f);
}
