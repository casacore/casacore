//# tConvertArray.cc: This program tests the convertArray functions
//# Copyright (C) 2009
//# Associated Universities, Inc. Washington DC, USA.
//#
//# This program is free software; you can redistribute it and/or modify it
//# under the terms of the GNU General Public License as published by the Free
//# Software Foundation; either version 2 of the License, or (at your option)
//# any later version.
//#
//# This program is distributed in the hope that it will be useful, but WITHOUT
//# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
//# FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
//# more details.
//#
//# You should have received a copy of the GNU General Public License along
//# with this program; if not, write to the Free Software Foundation, Inc.,
//# 675 Massachusetts Ave, Cambridge, MA 02139, USA.
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
#include <casa/Arrays/Array.h>
#include <casa/Arrays/ArrayMath.h>
#include <casa/Arrays/ArrayLogical.h>
#include <casa/Utilities/Assert.h>
#include <casa/iostream.h>

#include <casa/namespace.h>


template<typename T, typename F>
void tConvertEQ()
{
  Array<F> arr(IPosition(3,4,5,6));
  Array<T> res(IPosition(3,4,5,6));
  Array<T> exp(IPosition(3,4,5,6));
  indgen (arr, F(0), F(1));
  indgen (exp, T(0), T(1));
  convertArray (res, arr);
  AlwaysAssertExit (allEQ(res, exp));\
}

template<typename T, typename F>
void tConvertNear()
{
  Array<F> arr(IPosition(3,4,5,6));
  Array<T> res(IPosition(3,4,5,6));
  Array<T> exp(IPosition(3,4,5,6));
  indgen (arr, F(0), F(1));
  indgen (exp, T(0), T(1));
  convertArray (res, arr);
  AlwaysAssertExit (allNear(res, exp, 1e-5));        \
}

int main()
{
  try {
    tConvertEQ<Int,Short>();
    tConvertEQ<Short,Int>();
    tConvertNear<Float,Int>();
    tConvertNear<Complex,Float>();
  } catch (AipsError x) {
    cout << "Unexpected exception: " << x.getMesg() << endl;
    return 1;
  }
  cout << "OK" << endl;
  return 0;
}
