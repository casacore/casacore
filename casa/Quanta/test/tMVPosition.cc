//# tMVPosition.cc: test program for MVPosition class
//# Copyright (C) 2015
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

#include <casacore/casa/aips.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/Quanta/MVPosition.h>
#include <casacore/casa/Quanta/RotMatrix.h>

#include <casacore/casa/namespace.h>
#include <list>
#if defined(AIPS_CXX11) && !defined(__APPLE__)
#include <thread>
#include <mutex>
#endif

void test_parallel();
void test_parallel_openmp();

void test_parallel()
{

#if defined(AIPS_CXX11) && !defined(__APPLE__)
  double sum = 0;
  static const size_t thread_max = 50;
  static const size_t loop_max = 50 * 2; 
  std::vector<std::thread> threads(thread_max);
  std::mutex sum_mutex;
  for(size_t i = 0; i < thread_max ; ++i)
  {
     threads[i] = std::thread([&sum, &sum_mutex]()
       {
         std::unique_ptr<MVPosition[]> positions (new MVPosition[loop_max]);
         double part_sum = 0;
         for(size_t j = 0; j < loop_max ; ++j)
         {
           positions[j] = MVPosition();
           positions[j](2) = j;
           part_sum += positions[j](2);
         }
         std::this_thread::sleep_for( std::chrono::milliseconds( 100 ) );
         std::lock_guard<std::mutex> lock(sum_mutex);
         sum += part_sum;
       });
  }

  std::for_each(threads.begin(),threads.end(),[](std::thread& x){x.join();});
  AlwaysAssertExit(sum == thread_max * loop_max*(loop_max-1)/2); //Arithmetic series
#endif
}

void test_parallel_openmp()
{

  double sum = 0;
  static const size_t thread_max = 50;
  static const size_t loop_max = 50 * 2; //50 is the value for max_cache_array inside MVPosition
#ifdef _OPENMP
#pragma omp parallel for reduction(+:sum)
#endif
  for(size_t i = 0; i < thread_max ; ++i)
  {  
    MVPosition positions[loop_max];
    double part_sum = 0;
    for(size_t j = 0; j < loop_max ; ++j)
    { 
      positions[j] = MVPosition();
      (positions[j])(2) = j;
      part_sum += (positions[j])(2);
    }
    sum += part_sum;
  }
  AlwaysAssertExit(sum == thread_max * loop_max*(loop_max-1)/2); //Arithmetic series
}


int main ()
{
  try {
    MVPosition pos, pos2;
    pos(0) = 1;
    pos(1) = 2;
    pos(2) = 3;
    AlwaysAssertExit(pos(0) == 1);
    AlwaysAssertExit(pos(1) == 2);
    AlwaysAssertExit(pos(2) == 3);
    pos2 = pos * 2;
    AlwaysAssertExit(pos2(0) == 2);
    AlwaysAssertExit(pos2(1) == 4);
    AlwaysAssertExit(pos2(2) == 6);
    pos *= 2.;
    AlwaysAssertExit(pos(0) == 2);
    AlwaysAssertExit(pos(1) == 4);
    AlwaysAssertExit(pos(2) == 6);
    AlwaysAssertExit(pos == pos2);
    RotMatrix rot;
    for (int i = 0; i < 9; i++) {
        rot(i / 3, i % 3) = i;
        AlwaysAssertExit(rot(i / 3, i % 3) == i);
    }
    pos2 = pos * rot;
    AlwaysAssertExit(pos2(0) == 48);
    AlwaysAssertExit(pos2(1) == 60);
    AlwaysAssertExit(pos2(2) == 72);
    pos *= rot;
    AlwaysAssertExit(pos(0) == 48);
    AlwaysAssertExit(pos(1) == 60);
    AlwaysAssertExit(pos(2) == 72);
    AlwaysAssertExit(pos2 == pos);

    test_parallel(); 

  } catch (AipsError& x) {
    cout << "Unexpected exception: " << x.getMesg() << endl;
    return 1;
  } 
  return 0;
}
