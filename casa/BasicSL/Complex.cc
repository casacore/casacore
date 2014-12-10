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
#include <casacore/casa/BasicSL/Complex.h>
#include <casacore/casa/BasicMath/Math.h>
#include <casacore/casa/BasicSL/Constants.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

// Math functions
/// Should be in stl

#if defined(NEEDS_LOG10_COMPLEX)
DComplex log10(const DComplex &val)
{
  return DComplex(std::log(val)*C::log10e);
}

/// Should be in stl
Complex log10(const Complex &val)
{
  // Need to make log10e a Float for it to compile
  // with picky compilers
  return Complex(std::log(val)*Float(C::log10e));
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
  if (near(val1.real(), val2.real(), tol) &&
      near(val1.imag(), val2.imag(), tol)) return True;
  Float aval1(std::abs(val1)), aval2(std::abs(val2));
  if (aval1 == 0) return aval2 <= (1+tol)*FLT_MIN;
  else if (aval2 == 0) return aval1 <= (1+tol)*FLT_MIN;
  DComplex dval(val1);
  dval -= DComplex(val2);
  return std::abs(dval) <= tol * (aval1 < aval2 ? aval2 : aval1);
}

Bool near(const DComplex &val1, const DComplex &val2, Double tol)
{
  if (tol <= 0) return val1 == val2;
  if (val1 == val2) return True;
  if (std::abs(val1) == 0) return std::abs(val2) <= (1+tol)*DBL_MIN;
  else if (std::abs(val2) == 0) return std::abs(val1) <= (1+tol)*DBL_MIN;
  Double aval1(std::abs(val1)), aval2(std::abs(val2));
  return std::abs(val1-val2) <= tol * (aval1 < aval2 ? aval2 : aval1);
}

Bool nearAbs(const Complex &val1, const Complex &val2, Double tol)
{
  return std::abs(val2 - val1) <= tol;
}
Bool nearAbs(const DComplex &val1, const DComplex &val2, Double tol)
{
  return std::abs(val2 - val1) <= tol;
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

// Inf functions

Bool isInf(const Complex &val)
{
  return isInf(val.real()) || isInf(val.imag());
}
Bool isInf(const DComplex &val)
{
  return isInf(val.real()) || isInf(val.imag());
}
void setInf(Complex &val)
{
  Float x; setInf(x);
  Float y; setInf(y);
  val = Complex(x, y);
}
void setInf(DComplex &val)
{
  Double x; setInf(x);
  Double y; setInf(y);
  val = DComplex(x, y);
}


// Finite functions

Bool isFinite(const Complex &val)
{
  return isFinite(val.real()) || isFinite(val.imag());
}
Bool isFinite(const DComplex &val)
{
  return isFinite(val.real()) || isFinite(val.imag());
}

// fmod functions

DComplex fmod(const DComplex &in, const DComplex &f) {
  return DComplex(std::fmod(real(in), real(f)), imag(in)); }
Complex fmod(const Complex &in, const Complex &f) {
  return Complex(std::fmod(real(in), real(f)), imag(in)); }

// Inverse trigonometry (see Abromowitz)
DComplex atan(const DComplex &in) {
  const Double n = norm(in);
  return DComplex(0.5*std::atan(2.0*real(in)/(1.0-n)),
		  0.25*std::log((1.0+n+2*imag(in))/(1.0+n-2*imag(in))));
}
Complex atan(const Complex &in) {
  const Float n = norm(in);
  return Complex(0.5*std::atan(2.0*real(in)/(1.0-n)),
		 0.25*std::log((1.0+n+2*imag(in))/(1.0+n-2*imag(in))));
}
DComplex asin(const DComplex &in) {
  const Double n = norm(in);
  Double a = 0.5*std::sqrt(1.0+n+2*real(in));
  const Double c = 0.5*std::sqrt(1.0+n-2*real(in));
  const Double b = a-c;
  a += c;
  return DComplex(std::asin(b), std::log(a+std::sqrt(a*a-1.0)));
}
Complex asin(const Complex &in) {
  const Float n = norm(in);
  Float a = 0.5*std::sqrt(1.0+n+2*real(in));
  const Float c = 0.5*sqrt(1.0+n-2*real(in));
  const Float b = a-c;
  a += c;
  return Complex(std::asin(b), std::log(a+std::sqrt(a*a-1.0)));
}
DComplex acos(const DComplex &in) {
  const Double n = norm(in);
  Double a = 0.5*std::sqrt(1.0+n+2*real(in));
  const Double c = 0.5*std::sqrt(1.0+n-2*real(in));
  const Double b = a-c;
  a += c;
  return DComplex(std::acos(b), -std::log(a+std::sqrt(a*a-1.0)));
}
Complex acos(const Complex &in) {
  const Float n = norm(in);
  Float a = 0.5*std::sqrt(1.0+n+2*real(in));
  const Float c = 0.5*std::sqrt(1.0+n-2*real(in));
  const Float b = a-c;
  a += c;
  return Complex(std::acos(b), -std::log(a+std::sqrt(a*a-1.0)));
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
  return ::erf(in.real());
}
Complex erf(const Complex &in) {
  return ::erf(in.real());
}
DComplex erfc(const DComplex &in) {
  return ::erfc(in.real());
}
Complex erfc(const Complex &in) {
  return ::erfc(in.real());
}

} //# NAMESPACE CASACORE - END
