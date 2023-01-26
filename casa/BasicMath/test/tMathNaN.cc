//# tMathNaN.cc: Test program for NaN
//# Copyright (C) 1999,2001
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

#include <casacore/casa/aips.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/BasicMath/Math.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Inputs/Input.h>
#include <casacore/casa/OS/Timer.h> 
#if defined(AIPS_SOLARIS) || defined(AIPS_IRIX)
#include <ieeefp.h>
#endif
#include <casacore/casa/iostream.h>
#include <casacore/casa/iomanip.h>


#include <casacore/casa/namespace.h>
#define	isnanfmacro(x)	(((*(int32_t *)(x) & 0x7f800000) == 0x7f800000) && \
			    ((*(int32_t *)(x) & 0x007fffff) != 0x00000000))

inline bool isNaN_isnan(float val) {
  return (std::isnan(double(val)));
}

inline bool isNaN_isnanf(const float& val) {
#if defined(AIPS_SOLARIS) || defined(AIPS_IRIX)
  return (isnanf(val));
#else
  return (isnanfmacro(&val));
#endif
}

inline bool isNaN_ref(const float &x)
{
  return (((*(int32_t *)&(x) & 0x7f800000) == 0x7f800000) && \
		((*(int32_t *)&(x) & 0x007fffff) != 0x00000000));
}

inline bool isNaN_val(float x)
{
  float* xp=&x;
  return (((*(int32_t *)xp & 0x7f800000) == 0x7f800000) && \
		((*(int32_t *)xp & 0x007fffff) != 0x00000000));
}


bool doIt (int32_t n, float x, bool nan)
{
   bool ok = true;
   const int32_t narr = 100000;
   // Determine the expected nr of NaN's.
   uint32_t nrnan = 0;
   if (nan) {
     nrnan = n*narr;
   }
   // Initialize the array.
   float arr[narr];
   for (int32_t i=0; i<narr; i++) {
     arr[i] = x;
   }
//
   Timer t; 
   uint32_t nf = 0;
   t.mark();
   for (int32_t i=0; i<n; i++) {
     for (int32_t j=0; j<narr; j++) {
       if ((arr[j] != arr[j])) {
	 nf++;
       }
     }
   }
   if (nf != nrnan) {
     cout << "!= found " << nf << " NaN's; expected " << nrnan << endl;
     cout << "This NaN test should never be used on this platform!!" << endl;
     cout << endl;
   }
   cout << "nf=" << nf << "       != ";
   t.show();
//
   nf = 0;
   t.mark();
   for (int32_t i=0; i<n; i++) {
     for (int32_t j=0; j<narr; j++) {
       if (isNaN_isnan(arr[j])) {
        nf++;
       }
     }
   }
   if (nf != nrnan) {
     cout << "!= found " << nf << " NaN's; expected " << nrnan << endl;
     ok = false;
   }
   cout << "nf=" << nf << "    isnan ";
   t.show();
//
   nf = 0;
   t.mark();
   for (int32_t i=0; i<n; i++) {
     for (int32_t j=0; j<narr; j++) {
       if (isNaN_isnanf(arr[j])) {
	 nf++;
       }
     }
   }
   if (nf != nrnan) {
     cout << "!= found " << nf << " NaN's; expected " << nrnan << endl;
     ok = false;
   }
   cout << "nf=" << nf << "   isnanf ";
   t.show();
//
   nf = 0;
   t.mark();
   for (int32_t i=0; i<n; i++) {
     for (int32_t j=0; j<narr; j++) {
       if (isNaN_ref(arr[j])) {
	 nf++;
       }
     }
   }
   if (nf != nrnan) {
     cout << "!= found " << nf << " NaN's; expected " << nrnan << endl;
     ok = false;
   }
   cout << "nf=" << nf << "   by ref ";
   t.show();
//
// The test does not work properly for gcc-4 if passed by value.
//    nf = 0;
//    t.mark();
//    for (int32_t i=0; i<n; i++) {
//      for (int32_t j=0; j<narr; j++) {
//        if (isNaN_val(arr[j])) {
// 	 nf++;
//        }
//      }
//    }
//    if (nf != nrnan) {
//      cout << "!= found " << nf << " NaN's; expected " << nrnan << endl;
//      ok = false;
//    }
//    cout << "nf=" << nf << "   by val ";
//    t.show();
//
   nf = 0;
   t.mark();
   for (int32_t i=0; i<n; i++) {
     for (int32_t j=0; j<narr; j++) {
       if (isNaN(arr[j])) {
	 nf++;
       }
     }
   }
   if (nf != nrnan) {
     cout << "!= found " << nf << " NaN's; expected " << nrnan << endl;
     ok = false;
   }
   cout << "nf=" << nf << "    isNaN ";
   t.show();
//
   nf = 0;
   t.mark();
   for (int32_t i=0; i<n; i++) {
     for (int32_t j=0; j<narr; j++) {
       if (isnanfmacro(arr+j)) {
	 nf++;
       }
      }
   }
   if (nf != nrnan) {
     cout << "!= found " << nf << " NaN's; expected " << nrnan << endl;
     ok = false;
   }
   cout << "nf=" << nf << "    macro ";
   t.show();

   cout << endl;
   return ok;
}
 

 

int main (int argc, const char* argv[])
{
   Input inputs(1);
   inputs.version ("$Revision$");
   inputs.create("n", "100", "Number of tries");
 
   inputs.readArguments(argc, argv);
   const int32_t n = inputs.getInt("n");
   cout << "n = " << n << endl;
//
   bool ok = true;
   float x = 0;
   if (! doIt (n, x, false)) {
     ok = false;
   }
   setNaN (x);
   if (! doIt (n, x, true)) {
     ok = false;
   }
   // Let the machine generate a NaN.
   // I don't know if this is really portable.
   x = sqrt(double(-1));
   if (! doIt (n, x, true)) {
     ok = false;
   }

   if (ok) {
     cout << "OK" << endl;
     return 0;
   }
   cout << "errors found" << endl;
   return 1;
}
