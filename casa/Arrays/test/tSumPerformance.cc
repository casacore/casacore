//# tSumPerformance.cc: Compare the performance of various ways to sum an array
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
#include <casacore/casa/BasicSL/Complex.h>
#include <casacore/casa/OS/Timer.h>
#include <numeric>
#include <functional>

using namespace casacore;

// Inlining this function makes it almost 3 times slower.
template<typename Result, typename Iter>
Result sum(Iter srcBegin, const Iter& srcEnd)
{
  Result tmp = Result();
  for (; srcBegin != srcEnd; ++srcBegin) {
    tmp = tmp + *srcBegin;
  }
  return tmp;
}

template<class T> T sumstl (const Array<T>& a)
{
  return a.contiguousStorage() ?
    std::accumulate(a.cbegin(), a.cend(), T()) :
    std::accumulate(a.begin(),  a.end(),  T());
}

template<class T> T sumiter (const Array<T>& a)
{
  return a.contiguousStorage() ?
    sum<T,typename Array<T>::const_contiter>(a.cbegin(), a.cend()) :
    sum<T,typename Array<T>::const_iterator>(a.begin(),  a.end());
}

template<class T> T sumold(const Array<T> &a)
{
     uInt ntotal = a.nelements();
     Bool deleteIt;
     const T *storage = a.getStorage(deleteIt);
     T sum = *storage;
     // Account for the fact we've seen the first position
     ntotal--;
     const T *ts = storage + 1;
     while (ntotal--) {
       sum += *ts++;
     }
     a.freeStorage(storage, deleteIt);
     return sum;
}

template<class T> T product(const Array<T> &a)
{
  if (a.empty()) {
    return T();
  }
  // Get first element, because T(1) may not work for all types.
  T prod = *a.data();
  if (a.contiguousStorage()) {
    typename Array<T>::const_contiter iter(a.cbegin());
    ++iter;
    return std::accumulate(iter, a.cend(), prod, std::multiplies<T>());
  } else {
    typename Array<T>::const_iterator iter(a.begin());
    ++iter;
    return std::accumulate(iter, a.end(),  prod, std::multiplies<T>());
  }
}

template<class T> T product1(const Array<T> &a)
{
  if (a.contiguousStorage()) {
    return std::accumulate(a.cbegin(), a.cend(), T(1), std::multiplies<T>());
  } else {
    return std::accumulate(a.begin(),  a.end(),  T(1), std::multiplies<T>());
  }
}

template<class T, class ACC>
void doIt(int nx, int ny, int nz)
{
  T s=0;
  Array<T> arr(IPosition(3,nx,ny,nz));
  arr=T(1.);
  arr(IPosition(3,nx-1,ny-1,nz/10)) = -int(arr.nelements()) + 1;  // makes sum 0
  std::cout<< arr(IPosition(3,nz-1,ny-1,nz/10)) << std::endl;
  // Add in old ArrayMath way.
  Timer timer;
  s = sumold (arr);
  timer.show("sumold       ");
  std::cout<< s << std::endl;
  timer.mark();
  // Add using STL.
  s = sumstl (arr);
  timer.show("sumstl       ");
  std::cout << s << std::endl;
  timer.mark();
  // Add using IterMath style.
  s = sumiter (arr);
  timer.show("sumiter      ");
  std::cout<< s << std::endl;
  // Doing the sum in ACC precision gives much higher accuracy.
  timer.mark();
  s = std::accumulate(arr.cbegin(), arr.cend(), ACC(0));
  timer.show("sumstl ACC   ");
  std::cout<< s << std::endl;
  {
    // Test large subset.
    Array<T> subarr (arr(IPosition(3,0), IPosition(3,nx-2,ny-2,nz-2)));
    timer.mark();
    s = sumold (subarr);
    timer.show("sumold big   ");
    std::cout << s << std::endl;
    timer.mark();
    s = sumiter (subarr);
    timer.show("sumiter big  ");
    std::cout << s << std::endl;
  }
  {
    // Test small subset.
    Array<T> subarr (arr(IPosition(3,0), IPosition(3,19)));
    timer.mark();
    for(int i=0; i<nx; ++i) {
      s = sumold (subarr);
    }
    timer.show("sumold small ");
    std::cout << s << std::endl;
    timer.mark();
    for(int i=0; i<nx; ++i) {
      s = sumiter (subarr);
    }
    timer.show("sumiter small");
    std::cout << s << std::endl;
  }
}

int main (int argc, char* argv[])
{
  // Test with 1000 1000 100 to get some real performance numbers.
  int nx = 0;
  int ny = 0;
  int nz = 0;
  if (argc > 1) nx = atoi(argv[1]);
  if (argc > 2) ny = atoi(argv[2]);
  if (argc > 3) nz = atoi(argv[3]);
  if (nx < 20) nx = 20;
  if (ny < 20) ny = 20;
  if (nz < 20) nz = 20;
  std::cout << "tSumPerformance: nx=" << nx << " ny=" << ny << " nz=" << nz
            << std::endl;
  std::cout << "test int ..." << std::endl;
  doIt<int, int> (nx, ny, nz);
  std::cout << "test float ..." << std::endl;
  doIt<float, double> (nx, ny, nz);
  Array<Complex> arr(IPosition(1,5));
  arr=2;
  std::cout << product(arr) << std::endl;
  std::cout << product1(arr) << std::endl;
}

/*
Test remarks on MacBook OS-X Tiger g++-4.01. -O2:
1. Accumulating with double about as fast as with float
   Thus should use double for higher accuracy.
2. Newer ways of calculating sum are 3x faster, both for contiguous
   and non-contiguous arrays.
3. Inlining function sum makes it three times slower (for float).
*/
