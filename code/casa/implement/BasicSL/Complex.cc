//# complex.cc:
//# Copyright (C) 1999
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

/* 
Copyright (C) 1988 Free Software Foundation
    written by Doug Lea (dl@rocky.oswego.edu)

This file is part of the GNU C++ Library.  This library is free
software; you can redistribute it and/or modify it under the terms of
the GNU Library General Public License as published by the Free
Software Foundation; either version 2 of the License, or (at your
option) any later version.  This library is distributed in the hope
that it will be useful, but WITHOUT ANY WARRANTY; without even the
implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
PURPOSE.  See the GNU Library General Public License for more details.
You should have received a copy of the GNU Library General Public
License along with this library; if not, write to the Free Software
Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
*/

#ifdef __GNUG__
#pragma implementation
#endif
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
    return ToBool(val1 == val2);
  if (val1 == val2) return True;
  if (val1 == 0)
    return ToBool(abs(val2) <= (1+tol)*FLT_MIN);
  else if (val2 == 0)
    return ToBool(abs(val1) <= (1+tol)*FLT_MIN);
  return ToBool(abs(val1-val2) <= tol*max(abs(val1),abs(val2)));
}

Bool near(G_COMPLEX(double) val1, G_COMPLEX(double) val2, double tol) {
  if (tol <= 0)
    return ToBool(val1 == val2);
  if (val1 == val2) return True;
  if (val1 == 0)
    return ToBool(abs(val2) <= (1+tol)*DBL_MIN);
  else if (val2 == 0)
    return ToBool(abs(val1) <= (1+tol)*DBL_MIN);
  return ToBool(abs(val1-val2) <= tol*max(abs(val1),abs(val2)));
}

Bool nearAbs(G_COMPLEX(float) val1, G_COMPLEX(float) val2, double tol) {
  return ToBool(abs(val2 - val1) <= tol);
}

Bool nearAbs(G_COMPLEX(double) val1, G_COMPLEX(double) val2, double tol) {
  return ToBool(abs(val2 - val1) <= tol);
}

g_implement2(G_COMPLEX,double,G_COMPLEX_CTOR_OP_IMP(double,int) G_COMPLEX_CTOR_OP_IMP(double,float) G_COMPLEX_ASSIGN_OP_IMP(double,float) G_COMPLEX_ASSIGN_OP_IMP(double,int) G_COMPLEX_OPEQ_IMP(double,float) G_COMPLEX_OPEQ_IMP(double,int))
g_implement2(G_COMPLEX,float,G_COMPLEX_CTOR_OP_IMP(float,int) G_COMPLEX_CTOR_OP_IMP(float,double) G_COMPLEX_ASSIGN_OP_IMP(float,double) G_COMPLEX_ASSIGN_OP_IMP(float,int) G_COMPLEX_OPEQ_IMP(float,int) G_COMPLEX_OPEQ_IMP(float,double))
g_implement2(G_COMPLEX,int,G_COMPLEX_CTOR_OP_IMP(int,float) G_COMPLEX_CTOR_OP_IMP(int,double) G_COMPLEX_ASSIGN_OP_IMP(int,double) G_COMPLEX_ASSIGN_OP_IMP(int,float) G_COMPLEX_OPEQ_IMP(int,float) G_COMPLEX_OPEQ_IMP(int,double))

#if defined(COMPLEX_STUPID_COMPILER)
//#***************************************************************************
//#********************* Should not be needed, casts *************************
//#********************* should take care of these   *************************
//#***************************************************************************
G_COMPLEX_DO_BIN_OP_IMP
#endif

Bool isNaN(const Complex &val)
{
    return ToBool(isNaN(val.real()) || isNaN(val.imag()));
}

Bool isNaN(const DComplex &val)
{
    return ToBool(isNaN(val.real()) || isNaN(val.imag()));
}

void setNaN(Complex &val)
{
    setNaN(val.real()); setNaN(val.imag());
}

void setNaN(DComplex &val)
{
    setNaN(val.real()); setNaN(val.imag());
}
