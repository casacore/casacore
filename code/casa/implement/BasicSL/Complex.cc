//# Complex.cc: Implement Complex, DComplex
//# Copyright (C) 2000,2001
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


#if (!defined(AIPS_USE_OLD_COMPLEX))

//# Includes
#include <aips/Mathematics/Complex.h>
#include <aips/Mathematics/Math.h>
#include <aips/Mathematics/Constants.h>

// Math functions
/// Should be in stl
DComplex log10(const DComplex &val)
{
  return DComplex(log(val)*C::log10e);
}

/// Should be in stl
Complex log10(const Complex &val)
{
  return Complex(log(val)*C::log10e);
}

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


#else

#include <aips/Mathematics/Complex.h>
#include <aips/Mathematics/Constants.h>

//
// FROM builtin.h in libg++
//
typedef void (*one_arg_error_handler_t)(const char*);


// error handling

void default_Complex_error_handler(const char* msg)
{
  cerr << "Fatal Complex arithmetic error. " << msg << "\n";
  exit(1);
}

one_arg_error_handler_t Complex_error_handler = default_Complex_error_handler;

one_arg_error_handler_t set_Complex_error_handler(one_arg_error_handler_t f)
{
  one_arg_error_handler_t old = Complex_error_handler;
  Complex_error_handler = f;
  return old;
}

Bool near(G_COMPLEX(float) val1, G_COMPLEX(float) val2, double tol) {
  if (tol <= 0)
    return (val1 == val2);
  if (val1 == val2) return True;
  if (val1 == 0)
    return (abs(val2) <= (1+tol)*FLT_MIN);
  else if (val2 == 0)
    return (abs(val1) <= (1+tol)*FLT_MIN);
  return (abs(val1-val2) <= tol*max(abs(val1),abs(val2)));
}

Bool near(G_COMPLEX(double) val1, G_COMPLEX(double) val2, double tol) {
  if (tol <= 0)
    return (val1 == val2);
  if (val1 == val2) return True;
  if (val1 == 0)
    return (abs(val2) <= (1+tol)*DBL_MIN);
  else if (val2 == 0)
    return (abs(val1) <= (1+tol)*DBL_MIN);
  return (abs(val1-val2) <= tol*max(abs(val1),abs(val2)));
}

Bool nearAbs(G_COMPLEX(float) val1, G_COMPLEX(float) val2, double tol) {
  return (abs(val2 - val1) <= tol);
}

Bool nearAbs(G_COMPLEX(double) val1, G_COMPLEX(double) val2, double tol) {
  return (abs(val2 - val1) <= tol);
}

g_implement2(G_COMPLEX,double,G_COMPLEX_CTOR_OP_IMP(double,float) G_COMPLEX_ASSIGN_OP_IMP(double,float))
g_implement2(G_COMPLEX,float,G_COMPLEX_CTOR_OP_IMP(float,double))


Bool isNaN(const Complex &val)
{
    return (isNaN(val.real()) || isNaN(val.imag()));
}

Bool isNaN(const DComplex &val)
{
    return (isNaN(val.real()) || isNaN(val.imag()));
}

void setNaN(Complex &val)
{
    setNaN(val.real()); setNaN(val.imag());
}

void setNaN(DComplex &val)
{
    setNaN(val.real()); setNaN(val.imag());
}

#endif
