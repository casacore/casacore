//# Complex.cc: Implement Complex, DComplex
//# Copyright (C) 2000,2001,2002
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
#include <aips/Mathematics/Complex.h>
#include <aips/Mathematics/Math.h>
#include <aips/Mathematics/Constants.h>

// Math functions
/// Should be in stl

#if defined(NEEDS_LOG10_COMPLEX)
DComplex log10(const DComplex &val)
{
  return DComplex(log(val)*C::log10e);
}

/// Should be in stl
Complex log10(const Complex &val)
{
  // Need to make log10e a Float for it to compile
  // with picky compilers
  return Complex(log(val)*Float(C::log10e));
}
#endif

// Near functions
// Note: max() cannot be used from Math.h until it is derived from <math>
// Note: abs() not defined in SGI
//
Bool near(const Complex &val1, const Complex &val2, Double tol)
{
  if (tol <= 0) return val1 == val2;
  if (val1 == val2) return True;
  if (abs(val1) == 0) return abs(val2) <= (1+tol)*FLT_MIN;
  else if (abs(val2) == 0) return abs(val1) <= (1+tol)*FLT_MIN;
  Float aval1(abs(val1)), aval2(abs(val2));
  return abs(val1-val2) <= tol * (aval1 < aval2 ? aval2 : aval1);
}

Bool near(const DComplex &val1, const DComplex &val2, Double tol)
{
  if (tol <= 0) return val1 == val2;
  if (val1 == val2) return True;
  if (abs(val1) == 0) return abs(val2) <= (1+tol)*DBL_MIN;
  else if (abs(val2) == 0) return abs(val1) <= (1+tol)*DBL_MIN;
  Double aval1(abs(val1)), aval2(abs(val2));
  return abs(val1-val2) <= tol * (aval1 < aval2 ? aval2 : aval1);
}

Bool nearAbs(const Complex &val1, const Complex &val2, Double tol)
{
  return abs(val2 - val1) <= tol;
}
Bool nearAbs(const DComplex &val1, const DComplex &val2, Double tol)
{
  return abs(val2 - val1) <= tol;
}

// NaN functions

Bool isNaN(const Complex &val)
{
  return isNaN(val.real()) || isNaN(val.imag());
}
Bool isNaN(const DComplex &val)
{
  return isNaN(val.real()) || isNaN(val.imag());
}
void setNaN(Complex &val)
{
  Float x; setNaN(x);
  Float y; setNaN(y);
  val = Complex(x, y);
}
void setNaN(DComplex &val)
{
  Double x; setNaN(x);
  Double y; setNaN(y);
  val = DComplex(x, y);
}

// fmod functions

DComplex fmod(const DComplex &in, const DComplex &f) {
  return DComplex(fmod(real(in), real(f))); }
Complex fmod(const Complex &in, const Complex &f) {
  return Complex(fmod(real(in), real(f))); }



// Temporary for now, likely should go into templates

#if defined(AIPS_SUN_NATIVE)

template Complex std::log10(const Complex&);
template DComplex std::log10(const DComplex&);
template Complex std::conj(const Complex&);
template DComplex std::conj(const DComplex&);

#endif
