//# tNutation.cc:  Test program for parallel Nutation calculation
//# Copyright (C) 2013
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
//# $Id: Nutation.cc 21420 2014-03-19 09:18:51Z gervandiepen $

//# Includes
#include <casacore/measures/Measures/Nutation.h>
#include <casacore/casa/Exceptions/Error.h>

using namespace casacore;

void doIt (int nthr, int n)
{
  double incr = 1./nthr;
  // Do nutation a number of times, if possible in parallel.
#ifdef _OPENMP
#pragma omp parallel for
#endif
  for (int i=0; i<nthr; ++i) {
    Double dat = 51116 + i*incr;
    ostringstream ostr;
    ostr << i;
    ofstream os(("tNutation_tmp.out_a" + ostr.str()).c_str());
    Nutation nut;
    for (int j=0; j<n; ++j) {
      os << dat << " Euler=" << nut(dat) << endl;
      os << " derivative=" << nut.derivative(dat) << endl;
      dat += incr;
    }
  }
}

int main(int argc, char* argv[])
{
  int nthr = 4;
  int n = 32;
  if (argc > 1) nthr = atoi(argv[1]);
  if (argc > 2) n    = atoi(argv[2]);
  try {
    doIt (nthr, n);
  } catch (const std::exception& x) {
    cout << "Unexpected exception: " << x.what() << endl;
    return 1;
  }
  return 0;
}
