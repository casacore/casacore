//# tTimer.cc: Test program for the timing facility
//# Copyright (C) 1993,1994,1995,2001
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

#include <casacore/casa/OS/Timer.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/iostream.h>

#include <casacore/casa/namespace.h>
int main(void)
{
  float x[4096];  // data array
  float w[4096];  // work array

  int i, n, niter, nloop;
  double system, user;
  double sum, real;
  Timer tvec, total;

  total.mark();
  sum = 0.0;
  for (n = 128; n < 4096; n*=2) {
    cout << " timing for vector of length "<< n << endl;

    tvec.mark();      // set timer

    niter = 400;
// loop a few times
     for (nloop = 1; nloop < niter; nloop++) {
       for (i=0; i < n; i++) x[i] = 0;
       for (i=0; i < n; i++) w[i] = 0;
       // Use x and w, otherwise the compiler gives a warning.
       AlwaysAssertExit (x[0] == 0  &&  w[0] == 0);
     }

     user = tvec.user() ;
     system = tvec.system() ;
     real = tvec.real();
     sum = sum + user;

     cout << " tvec user " << user << "  system " << system << endl;
     cout << " real time " << real << endl;
  } 
  cout << " total real time "<< sum << endl;
  user = total.user_usec();
  system = total.all_usec();

  total.show ("Total");
  cout << " total user microseconds " << user << "  all " << system << endl;

  return 0;
}
