//# tMaskArrMath0.cc: Test program for MaskedArrays mathematical operations.
//# Copyright (C) 1993,1994,1995,1996,1999,2000,2001
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

//# If AIPS_DEBUG is not set, the Assert's won't be called.
#if !defined(AIPS_DEBUG)
#define AIPS_DEBUG
#endif

//# For extra debugging
#if !defined(AIPS_ARRAY_INDEX_CHECK)
#define AIPS_ARRAY_INDEX_CHECK
#endif

#include <casacore/casa/iostream.h>

#include <casacore/casa/aips.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/casa/BasicSL/Complex.h>
#include <casacore/casa/BasicMath/Math.h>
#include <casacore/casa/Utilities/Assert.h>

#include <casacore/casa/Arrays/IPosition.h>
#include <casacore/casa/Arrays/Array.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/Arrays/ArrayLogical.h>
#include <casacore/casa/Arrays/ArrayIO.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Arrays/Matrix.h>
#include <casacore/casa/Arrays/Cube.h>
#include <casacore/casa/Arrays/ArrayError.h>
#include <casacore/casa/Arrays/MaskedArray.h>
#include <casacore/casa/Arrays/MaskArrMath.h>


#include <casacore/casa/namespace.h>

int main()
{
    try {
        {
            cout << endl << "Testing MaskArrMath0." << endl;

// Math

            {

            Vector<Int> f(10), g(10), h(10);
            Vector<Bool> b(10);

            indgen (f);
            cout << endl << "f=indgen(f) = " << endl;
            cout << f << endl;


            {

                b = (f<3);
                cout << endl << "b=(f<3) = " << endl;
                cout << b << endl;

                MaskedArray<Int> m(h,b);

                h = 0;
                indgen (m);
                cout << endl << "h=0; indgen( m(h,b) ); h= " << endl;
                cout << h << endl;

            }

            {

                cout << endl << "((f>3) && (f<7)) = " << endl;
                cout << ((f>3) && (f<7)) << endl;

		MaskedArray<Int> m( h( ((f>3) && (f<7)) ) );

                h = 0;
		indgen (m, 10);
                cout << endl << "h=0; indgen( h( ((f>3) && (f<7)) ), 10 ); h= " << endl;
                cout << h << endl;

            }

            {

                b = (f>2);
                cout << endl << "b=(f>2) = " << endl;
                cout << b << endl;

                MaskedArray<Int> m(h,b);

                h = 1;
                m += 5;
                cout << endl << "h=1; m(h,b) += 5; h= " << endl;
                cout << h << endl;

            }

            {

                b = (f>2);
                cout << endl << "b=(f>2) = " << endl;
                cout << b << endl;

                h = 1;
                h(b) += 5;
                cout << endl << "h=1; h(b) += 5; h= " << endl;
                cout << h << endl;

            }

            {

                b = (f>2);
                cout << endl << "b=(f>2) = " << endl;
                cout << b << endl;

                MaskedArray<Int> m(h,b);

                h = -1;
                m += f;
                cout << endl << "h=-1; m(h,b) += f; h= " << endl;
                cout << h << endl;

            }

            {

                b = (f>2);
                cout << endl << "b=(f>2) = " << endl;
                cout << b << endl;

                h = -1;
                h(b) += f;
                cout << endl << "h=-1; h(b) += f; h= " << endl;
                cout << h << endl;

            }

            {

                b = (f>4);
                cout << endl << "b=(f>4) = " << endl;
                cout << b << endl;

                MaskedArray<Int> m(f,b);

                indgen (h);
                cout << endl << "h=indgen(h) = " << endl;
                cout << h << endl;

                h += m;
                cout << endl << "h += m(f,b); h= " << endl;
                cout << h << endl;

            }

            {

                b = (f>3);
                cout << endl << "b=(f>3) = " << endl;
                cout << b << endl;

                Vector<Bool> c(10);
                c = (f<8);
                cout << endl << "c=(f<8) = " << endl;
                cout << c << endl;

                MaskedArray<Int> m(h,b), n(f,c);

                h = -1;
                m += n;
                cout << endl << "h=-1; m(h,b) += n(f,c); h= " << endl;
                cout << h << endl;

            }

            {

                b = (f>3);
                cout << endl << "b=(f>3) = " << endl;
                cout << b << endl;

                Vector<Bool> c(10);
                c = (f<8);
                cout << endl << "c=(f<8) = " << endl;
                cout << c << endl;

                MaskedArray<Int> n(f,c);

                h = -1;
                h(b) += n;
                cout << endl << "h=-1; h(b) += n(f,c); h= " << endl;
                cout << h << endl;

            }


            }

// End Math

            cout << endl << "OK" << endl;
        }
    } catch (AipsError x) {
        cout << "\nCaught an exception: " << x.getMesg() << endl;
    } 

    cout << "OK" << endl;
    return 0;
}
