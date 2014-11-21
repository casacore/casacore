//# tMaskArrMath2.cc: Test program for MaskedArrays mathematical operations.
//# Copyright (C) 1993,1994,1995,1996,1997,1999,2000,2001
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
            cout << endl << "Testing MaskArrMath2." << endl;

// Math

            {

            Vector<Double> df(10), dg(10), dh(10);
            Vector<Bool> b(10);

            indgen (df);
            cout << endl << "df=indgen(df) = " << endl;
            cout << df << endl;

            indgen (dg, 2.0);
            cout << endl << "dg=indgen(dg) = " << endl;
            cout << dg << endl;

            Vector<Double> dk(10);
            indgen (dk);
            dk (0) = 0.0;
            dk (1) = 1.0;
            dk (2) = 5.0;
            dk (3) = 3.0;
            dk (4) = 9.0;
            dk (5) = 2.0;
            dk (6) = 7.0;
            dk (7) = 7.0;
            dk (8) = 4.0;
            dk (9) = 3.0;
            cout << endl << "dk=" << endl << dk << endl;

            {
                cout << endl
                     << "Test min (MaskedArray<Double>)"
                     << endl;

                cout << " min (dk ((dk > 2.5) && (dk < 7.5)))= "
                     <<   min (dk ((dk > 2.5) && (dk < 7.5)))
                     << endl;
            }

            {
                cout << endl
                     << "Test max (MaskedArray<Double>)"
                     << endl;

                cout << " max (dk ((dk > 2.5) && (dk < 7.5)))= "
                     <<   max (dk ((dk > 2.5) && (dk < 7.5)))
                     << endl;
            }

            {
                cout << endl
                     << "Test minMax (minVal, maxVal, minPos, maxPos,"
                     << endl
                     << "             MaskedArray<Double>)"
                     << endl;

                Double minVal;
                Double maxVal;

                IPosition minPos (1);
                IPosition maxPos (1);

                minMax (minVal, maxVal, minPos, maxPos,
                        dk((dk > 2.5) && (dk < 7.5)));

                cout << "minMax (minVal, maxVal, minPos, maxPos,"
                     << endl
                     << "        dk((dk > 2.5) && (dk < 7.5)));"
                     << endl
                     << "minVal= " << minVal << endl
                     << "maxVal= " << maxVal << endl
                     << "minPos= " << minPos << endl
                     << "maxPos= " << maxPos << endl
                     << endl;
            }

            {
                cout << endl
                     << "Test minMax (minVal, maxVal,"
                     << endl
                     << "             MaskedArray<Double>)"
                     << endl;

                Double minVal;
                Double maxVal;

                minMax (minVal, maxVal,
                        dk((dk > 2.5) && (dk < 7.5)));

                cout << "minMax (minVal, maxVal,"
                     << endl
                     << "        dk((dk > 2.5) && (dk < 7.5)));"
                     << endl
                     << "minVal= " << minVal << endl
                     << "maxVal= " << maxVal << endl
                     << endl;
            }

            {
                cout << endl
                     << "Test min (MaskedArray<Double>,"
                     << endl
                     << "          Array<Double>, Array<Double>);"
                     << endl;

                dh = 2.0;
                min (dh((dk > 2.5) && (dk < 7.5)), dk, df);

                cout << endl << "dh=2.0; "
                     << endl
                     << "min (dh((dk > 2.5) && (dk < 7.5)), dk, df);"
                     << endl;
                cout << dh << endl;

            }

            {
                cout << endl
                     << "Test max (MaskedArray<Double>,"
                     << endl
                     << "          Array<Double>, Array<Double>)"
                     << endl;

                dh = 2.0;
                max (dh((dk > 2.5) && (dk < 7.5)), dk, df);

                cout << endl << "dh=2.0; "
                     << endl
                     << "max (dh((dk > 2.5) && (dk < 7.5)), dk, df);"
                     << endl;
                cout << dh << endl;

            }

            {
                cout << endl
                     << "Test min (MaskedArray<Double>, Array<Double>)"
                     << endl;

                dh = 2.0;
                dh = min (dk((dk > 2.5) && (dk < 7.5)), df);

                cout << endl << "dh=2.0; "
                     << endl
                     << "dh = min (dk((dk > 2.5) && (dk < 7.5)), df);"
                     << endl;
                cout << dh << endl;

            }

            {
                cout << endl
                     << "Test max (MaskedArray<Double>, Array<Double>)"
                     << endl;

                dh = 2.0;
                dh = max (dk((dk > 2.5) && (dk < 7.5)), df);

                cout << endl << "dh=2.0; "
                     << endl
                     << "dh = max (dk((dk > 2.5) && (dk < 7.5)), df);"
                     << endl;
                cout << dh << endl;

            }

            {
                cout << endl
                     << "Test min (Array<Double>, MaskedArray<Double>)"
                     << endl;

                dh = 2.0;
                dh = min (dk((dk > 2.5) && (dk < 7.5)), df);

                cout << endl << "dh=2.0; "
                     << endl
                     << "dh = min (dk((dk > 2.5) && (dk < 7.5)), df);"
                     << endl;
                cout << dh << endl;

            }

            {
                cout << endl
                     << "Test max (Array<Double>, MaskedArray<Double>)"
                     << endl;

                dh = 2.0;
                dh = max (df, dk((dk > 2.5) && (dk < 7.5)));

                cout << endl << "dh=2.0; "
                     << endl
                     << "dh = max (df, dk((dk > 2.5) && (dk < 7.5)));"
                     << endl;
                cout << dh << endl;

            }

            {
                cout << endl
                     << "Test min (MaskedArray<Double>, MaskedArray<Double>)"
                     << endl;

                dh = 2.0;
                dh = min (dk((dk > 2.5) && (dk < 7.5)),
                          df((df > 2.5) && (df < 7.5)));

                cout << endl << "dh=2.0; "
                     << endl
                     << "dh = min (dk((dk > 2.5) && (dk < 7.5)),"
                     << endl
                     << "          df((df > 2.5) && (df < 7.5)));"
                     << endl;
                cout << dh << endl;

            }

            {
                cout << endl
                     << "Test max (MaskedArray<Double>, MaskedArray<Double>)"
                     << endl;

                dh = 2.0;
                dh = max (dk((dk > 2.5) && (dk < 7.5)),
                          df((df > 2.5) && (df < 7.5)));

                cout << endl << "dh=2.0; "
                     << endl
                     << "dh = max (dk((dk > 2.5) && (dk < 7.5)),"
                     << endl
                     << "          df((df > 2.5) && (df < 7.5)));"
                     << endl;
                cout << dh << endl;

            }

            {
                cout << endl
                     << "Test min (MaskedArray<Double>, Double)"
                     << endl;

                dh = 2.0;
                dh = min (dk((dk > 2.5) && (dk < 7.5)), 5.0);

                cout << endl << "dh=2.0; "
                     << endl
                     << "dh = min (dk((dk > 2.5) && (dk < 7.5)), 5.0);"
                     << endl;
                cout << dh << endl;

            }

            {
                cout << endl
                     << "Test max (MaskedArray<Double>, Double)"
                     << endl;

                dh = 2.0;
                dh = max (dk((dk > 2.5) && (dk < 7.5)), 5.0);

                cout << endl << "dh=2.0; "
                     << endl
                     << "dh = max (dk((dk > 2.5) && (dk < 7.5)), 5.0);"
                     << endl;
                cout << dh << endl;

            }

            {
                cout << endl
                     << "Test min (Double, MaskedArray<Double>)"
                     << endl;

                dh = 2.0;
                dh = min (5.0, dk((dk > 2.5) && (dk < 7.5)));

                cout << endl << "dh=2.0; "
                     << endl
                     << "dh = min (5.0, dk((dk > 2.5) && (dk < 7.5)));"
                     << endl;
                cout << dh << endl;

            }

            {
                cout << endl
                     << "Test max (Double, MaskedArray<Double>)"
                     << endl;

                dh = 2.0;
                dh = max (5.0, dk((dk > 2.5) && (dk < 7.5)));

                cout << endl << "dh=2.0; "
                     << endl
                     << "dh = max (5.0, dk((dk > 2.5) && (dk < 7.5)));"
                     << endl;
                cout << dh << endl;

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
