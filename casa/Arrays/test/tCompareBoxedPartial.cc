//# tCompareBoxedPartial.cc: Compare performance of partialSums and boxedArrayMath
//# Copyright (C) 2008
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

#include <casacore/casa/Arrays/Array.h>
#include <casacore/casa/Arrays/ArrayPartMath.h>
#include <casacore/casa/BasicSL/Complex.h>
#include <casacore/casa/OS/Timer.h>

namespace casacore {
template<typename Func, typename T, typename Accum>
void partialWorker (Array<Accum>& result,
		    const Array<T>& array,
		    const IPosition& collapseAxes,
		    Func oper)
{
  if (collapseAxes.nelements() == 0) {
    result.resize (array.shape());
    convertArray (result, array);
    return;
  }
  const IPosition& shape = array.shape();
  uInt ndim = shape.nelements();
  if (ndim == 0) {
    result.resize();
    return;
  }
  IPosition resShape, incr;
  Int nelemCont = 0;
  uInt stax = partialFuncHelper (nelemCont, resShape, incr, shape,
				 collapseAxes);
  result.resize (resShape);
  result = 0;
  Bool deleteData, deleteRes;
  const T* arrData = array.getStorage (deleteData);
  const T* data = arrData;
  Accum* resData = result.getStorage (deleteRes);
  Accum* res = resData;
  // Find out how contiguous the data is, i.e. if some contiguous data
  // end up in the same output element.
  // cont tells if any data are contiguous.
  // stax gives the first non-contiguous axis.
  // n0 gives the number of contiguous elements.
  Bool cont = True;
  uInt n0 = nelemCont;
  Int incr0 = incr(0);
  if (nelemCont <= 1) {
    cont = False;
    n0 = shape(0);
    stax = 1;
  }
  // Loop through all data and assemble as needed.
  IPosition pos(ndim, 0);
  while (True) {
    if (cont) {
      Accum tmp = *res;
      for (uInt i=0; i<n0; ++i) {
	oper (tmp, *data++);
      }
      *res = tmp;
    } else {
      for (uInt i=0; i<n0; ++i) {
	*res = oper (*res, *data++);
	res += incr0;
      }
    }
    uInt ax;
    for (ax=stax; ax<ndim; ++ax) {
      res += incr(ax);
      if (++pos(ax) < shape(ax)) {
	break;
      }
      pos(ax) = 0;
    }
    if (ax == ndim) {
      break;
    }
  }
  array.freeStorage (arrData, deleteData);
  result.putStorage (resData, deleteRes);
  return;
}


template<class T> Array<T> new1PartialSums (const Array<T>& array,
					    const IPosition& collapseAxes)
{
  Array<T> result;
  partialWorker (result, array, collapseAxes, std::plus<T>());
  return result;
}

template<class T> Array<T> new1PartialVariances (const Array<T>& array,
						 const IPosition& collapseAxes)
{
  Array<T> means = partialMeans (array, collapseAxes);
  Array<T> result;
  partialWorker (result, array, collapseAxes, std::plus<T>());
  return result;
}
} // end namespace


using namespace casacore;
using namespace std;


template<class T>
void doIt()
{
  Array<T> arr(IPosition(3,100,200,1000));
  arr=1;
  Timer timer;
  {
    timer.mark();
    Array<T> res = boxedArrayMath(arr, IPosition(3,100,1,1), SumFunc<T>());
    timer.show ("boxed 0  ");
    cout << res.shape() << endl;
  }
  {
    timer.mark();
    Array<T> res = partialSums (arr, IPosition(1,0));
    timer.show ("partial 0");
    cout << res.shape() << endl;
  }
  {
    timer.mark();
    Array<T> res = new1PartialSums (arr, IPosition(1,0));
    timer.show ("n1part  0");
    cout << res.shape() << endl;
  }
  {
    timer.mark();
    Array<T> res = boxedArrayMath(arr, IPosition(3,1,200,1), SumFunc<T>());
    timer.show ("boxed 1  ");
    cout << res.shape() << endl;
  }
  {
    timer.mark();
    Array<T> res = partialSums (arr, IPosition(1,1));
    timer.show ("partial 1");
    cout << res.shape() << endl;
  }
  {
    timer.mark();
    Array<T> res = boxedArrayMath(arr, IPosition(3,1,1,1000), SumFunc<T>());
    timer.show ("boxed 2  ");
    cout << res.shape() << endl;
  }
  {
    timer.mark();
    Array<T> res = partialSums (arr, IPosition(1,2));
    timer.show ("partial 2");
    cout << res.shape() << endl;
  }
}

int main()
{
  std::cout << "test int ..." << std::endl;
  doIt<int>();
  std::cout << "test float ..." << std::endl;
  doIt<float>();
  std::cout << "test DComplex ..." << std::endl;
  doIt<DComplex>();
}

/*
Test remarks on MacBook OS-X Tiger g++-4.01. -O2:
1. partial is faster than boxed, especially for axis 0 (1.5x),
   axis 1 (3x), and axis 2 (4x).
*/
