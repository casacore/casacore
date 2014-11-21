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
//#
//# $Id$

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
#define	isnanfmacro(x)	(((*(Int *)(x) & 0x7f800000) == 0x7f800000) && \
			    ((*(Int *)(x) & 0x007fffff) != 0x00000000))

inline Bool isNaN_isnan(Float val) {
  return (isnan(Double(val)));
}

inline Bool isNaN_isnanf(const Float& val) {
#if defined(AIPS_SOLARIS) || defined(AIPS_IRIX)
  return (isnanf(val));
#else
  return (isnanfmacro(&val));
#endif
}

inline Bool isNaN_ref(const Float &x)
{
  return (((*(Int *)&(x) & 0x7f800000) == 0x7f800000) && \
		((*(Int *)&(x) & 0x007fffff) != 0x00000000));
}

inline Bool isNaN_val(Float x)
{
  Float* xp=&x;
  return (((*(Int *)xp & 0x7f800000) == 0x7f800000) && \
		((*(Int *)xp & 0x007fffff) != 0x00000000));
}


Bool doIt (Int n, Float x, Bool nan)
{
   Bool ok = True;
   const Int narr = 100000;
   // Determine the expected nr of NaN's.
   uInt nrnan = 0;
   if (nan) {
     nrnan = n*narr;
   }
   // Initialize the array.
   Float arr[narr];
   for (Int i=0; i<narr; i++) {
     arr[i] = x;
   }
//
   Timer t; 
   uInt nf = 0;
   t.mark();
   for (Int i=0; i<n; i++) {
     for (Int j=0; j<narr; j++) {
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
   for (Int i=0; i<n; i++) {
     for (Int j=0; j<narr; j++) {
       if (isNaN_isnan(arr[j])) {
        nf++;
       }
     }
   }
   if (nf != nrnan) {
     cout << "!= found " << nf << " NaN's; expected " << nrnan << endl;
     ok = False;
   }
   cout << "nf=" << nf << "    isnan ";
   t.show();
//
   nf = 0;
   t.mark();
   for (Int i=0; i<n; i++) {
     for (Int j=0; j<narr; j++) {
       if (isNaN_isnanf(arr[j])) {
	 nf++;
       }
     }
   }
   if (nf != nrnan) {
     cout << "!= found " << nf << " NaN's; expected " << nrnan << endl;
     ok = False;
   }
   cout << "nf=" << nf << "   isnanf ";
   t.show();
//
   nf = 0;
   t.mark();
   for (Int i=0; i<n; i++) {
     for (Int j=0; j<narr; j++) {
       if (isNaN_ref(arr[j])) {
	 nf++;
       }
     }
   }
   if (nf != nrnan) {
     cout << "!= found " << nf << " NaN's; expected " << nrnan << endl;
     ok = False;
   }
   cout << "nf=" << nf << "   by ref ";
   t.show();
//
// The test does not work properly for gcc-4 if passed by value.
//    nf = 0;
//    t.mark();
//    for (Int i=0; i<n; i++) {
//      for (Int j=0; j<narr; j++) {
//        if (isNaN_val(arr[j])) {
// 	 nf++;
//        }
//      }
//    }
//    if (nf != nrnan) {
//      cout << "!= found " << nf << " NaN's; expected " << nrnan << endl;
//      ok = False;
//    }
//    cout << "nf=" << nf << "   by val ";
//    t.show();
//
   nf = 0;
   t.mark();
   for (Int i=0; i<n; i++) {
     for (Int j=0; j<narr; j++) {
       if (isNaN(arr[j])) {
	 nf++;
       }
     }
   }
   if (nf != nrnan) {
     cout << "!= found " << nf << " NaN's; expected " << nrnan << endl;
     ok = False;
   }
   cout << "nf=" << nf << "    isNaN ";
   t.show();
//
   nf = 0;
   t.mark();
   for (Int i=0; i<n; i++) {
     for (Int j=0; j<narr; j++) {
       if (isnanfmacro(arr+j)) {
	 nf++;
       }
      }
   }
   if (nf != nrnan) {
     cout << "!= found " << nf << " NaN's; expected " << nrnan << endl;
     ok = False;
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
   const Int n = inputs.getInt("n");
   cout << "n = " << n << endl;
//
   Bool ok = True;
   Float x = 0;
   if (! doIt (n, x, False)) {
     ok = False;
   }
   setNaN (x);
   if (! doIt (n, x, True)) {
     ok = False;
   }
   // Let the machine generate a NaN.
   // I don't know if this is really portable.
   x = sqrt(double(-1));
   if (! doIt (n, x, True)) {
     ok = False;
   }

   if (ok) {
     cout << "OK" << endl;
     return 0;
   }
   cout << "errors found" << endl;
   return 1;
}
