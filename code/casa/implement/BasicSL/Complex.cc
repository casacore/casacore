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
#include <aips/BasicSL/Complex.h>
#include <aips/BasicMath/Math.h>
#include <aips/BasicSL/Constants.h>

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
  return DComplex(fmod(real(in), real(f)), imag(in)); }
Complex fmod(const Complex &in, const Complex &f) {
  return Complex(fmod(real(in), real(f)), imag(in)); }

// Inverse trigonometry (see Abromowitz)
DComplex atan(const DComplex &in) {
  const Double n = norm(in);
  return DComplex(0.5*atan(2.0*real(in)/(1.0-n)),
		  0.25*log((1.0+n+2*imag(in))/(1.0+n-2*imag(in))));
}
Complex atan(const Complex &in) {
  const Float n = norm(in);
  return Complex(0.5*atan(2.0*real(in)/(1.0-n)),
		 0.25*log((1.0+n+2*imag(in))/(1.0+n-2*imag(in))));
}
DComplex asin(const DComplex &in) {
  const Double n = norm(in);
  Double a = 0.5*sqrt(1.0+n+2*real(in));
  const Double c = 0.5*sqrt(1.0+n-2*real(in));
  const Double b = a-c;
  a += c;
  return DComplex(asin(b), log(a+sqrt(a*a-1.0)));
}
Complex asin(const Complex &in) {
  const Float n = norm(in);
  Float a = 0.5*sqrt(1.0+n+2*real(in));
  const Float c = 0.5*sqrt(1.0+n-2*real(in));
  const Float b = a-c;
  a += c;
  return Complex(asin(b), log(a+sqrt(a*a-1.0)));
}
DComplex acos(const DComplex &in) {
  const Double n = norm(in);
  Double a = 0.5*sqrt(1.0+n+2*real(in));
  const Double c = 0.5*sqrt(1.0+n-2*real(in));
  const Double b = a-c;
  a += c;
  return DComplex(acos(b), -log(a+sqrt(a*a-1.0)));
}
Complex acos(const Complex &in) {
  const Float n = norm(in);
  Float a = 0.5*sqrt(1.0+n+2*real(in));
  const Float c = 0.5*sqrt(1.0+n-2*real(in));
  const Float b = a-c;
  a += c;
  return Complex(acos(b), -log(a+sqrt(a*a-1.0)));
}
DComplex atan2(const DComplex &in, const DComplex &t2) {
  if (norm(t2) == 0) return DComplex(C::pi_2);
  const DComplex z = atan(in/t2);
  if (real(t2) > 0) return z;
  return (z + DComplex(C::pi));
}
Complex atan2(const Complex &in, const Complex &t2) {
  if (norm(t2) == 0) return Complex(C::pi_2);
  const Complex z = atan(in/t2);
  if (real(t2) > 0) return z;
  return (z + Complex(C::pi));
}
/// Temporary solutions only
DComplex erf(const DComplex &in) {
  return erf(in.real());
}
Complex erf(const Complex &in) {
  return erf(in.real());
}
DComplex erfc(const DComplex &in) {
  return erfc(in.real());
}
Complex erfc(const Complex &in) {
  return erfc(in.real());
}

/// Temporary for now, likely should go into templates (or in a macro
// preferably in define.h)

#if defined(AIPS_SUN_NATIVE)

template Complex std::log10(const Complex&);
template DComplex std::log10(const DComplex&);
template Complex std::conj(const Complex&);
template DComplex std::conj(const DComplex&);

#endif

#if defined(AIPS_GCC3)
template Float std::norm(const Complex&);
template Double std::norm(const DComplex&);
template Float std::arg(const Complex&);
template Double std::arg(const DComplex&);
template Float std::abs(const Complex&);
template Double std::abs(const DComplex&);
template Complex std::polar(const Float&, const Float&);
template DComplex std::polar(const Double&, const Double&);
template Complex std::sqrt(const Complex&);
template DComplex std::sqrt(const DComplex&);
template Complex std::conj(const Complex&);
template DComplex std::conj(const DComplex&);
template Complex std::pow(const Complex&, const Float&);
template DComplex std::pow(const DComplex&, const Double&);
template Complex std::pow(const Complex&, const Complex&);
template DComplex std::pow(const DComplex&, const DComplex&);
template Complex std::log(const Complex&);
template DComplex std::log(const DComplex&);
template Complex std::exp(const Complex&);
template DComplex std::exp(const DComplex&);
template Complex std::cos(const Complex&);
template DComplex std::cos(const DComplex&);
template Complex std::cosh(const Complex&);
template DComplex std::cosh(const DComplex&);
template Complex std::sin(const Complex&);
template DComplex std::sin(const DComplex&);
template Complex std::sinh(const Complex&);
template DComplex std::sinh(const DComplex&);
#endif
