//# tMaskArrMath1.cc: Test program for MaskedArrays mathematical operations.
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
            cout << endl << "Testing MaskArrMath1." << endl;

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

            {
                cout << endl
                     << "Test cos (MaskedArray<Double>)"
                     << endl;
                dh = 2.0;
                dh = cos (df ((df > 2.5) && (df < 6.5)));
                cout << endl << "dh=2.0; "
                     << endl
                     << "dh = cos (df ((df > 2.5) && (df < 6.5)));"
                     << endl;
                cout << dh << endl;
                cout << endl << "df= " << endl << df << endl;
            }

            {
                cout << endl
                     << "Test atan2 (MaskedArray<Double>, Array<Double>)"
                     << endl;
                dh = 2.0;
                dh = atan2 (df ((df > 2.5) && (df < 6.5)), dg);
                cout << endl << "dh=2.0; "
                     << endl
                     << " dh = atan2 (df ((df > 2.5) && (df < 6.5)), dg);"
                     << endl;
                cout << dh << endl;
            }

            {
                cout << endl
                     << "Test atan2 (Array<Double>, MaskedArray<Double>)"
                     << endl;
                dh = 2.0;
                dh = atan2 (dg, df ((df > 2.5) && (df < 6.5)));
                cout << endl << "dh=2.0; "
                     << endl
                     << " dh = atan2 (dg, df ((df > 2.5) && (df < 6.5)));"
                     << endl;
                cout << dh << endl;
            }

            {
                cout << endl
                     << "Test atan2 (MaskedArray<Double>, MaskedArray<Double>)"
                     << endl;
                dh = 2.0;
                dh = atan2 (dg ((df > 3.5) && (df < 7.5)),
                            df ((df > 2.5) && (df < 6.5)));
                cout << endl << "dh=2.0; "
                     << endl
                     << " dh = atan2 (dg ((df > 3.5) && (df < 7.5)),"
                     << endl
                     << "             df ((df > 2.5) && (df < 6.5)));"
                     << endl;
                cout << dh << endl;
            }

            {
                cout << endl
                     << "Test atan2 (MaskedArray<Double>, Double)"
                     << endl;
                dh = 2.0;
                dh = atan2 (df ((df > 2.5) && (df < 6.5)), 2.0);
                cout << endl << "dh=2.0; "
                     << endl
                     << " dh = atan2 (df ((df > 2.5) && (df < 6.5)), 2.0);"
                     << endl;
                cout << dh << endl;
            }

            {
                cout << endl
                     << "Test atan2 (Double, MaskedArray<Double>)"
                     << endl;
                dh = 2.0;
                dh = atan2 (2.0, df ((df > 2.5) && (df < 6.5)));
                cout << endl << "dh=2.0; "
                     << endl
                     << " dh = atan2 (2.0, df ((df > 2.5) && (df < 6.5)));"
                     << endl;
                cout << dh << endl;
            }

            {
                cout << endl
                     << "Test pow (MaskedArray<Double>, Array<Double>)"
                     << endl;
                dh = 2.0;
                dh = pow (df ((df > 2.5) && (df < 6.5)), dg);
                cout << endl << "dh=2.0; "
                     << endl
                     << " dh = pow (df ((df > 2.5) && (df < 6.5)), dg);"
                     << endl;
                cout << dh << endl;
            }

            {
                cout << endl
                     << "Test pow (Array<Double>, MaskedArray<Double>)"
                     << endl;
                dh = 2.0;
                dh = pow (dg, df ((df > 2.5) && (df < 6.5)));
                cout << endl << "dh=2.0; "
                     << endl
                     << " dh = pow (dg, df ((df > 2.5) && (df < 6.5)));"
                     << endl;
                cout << dh << endl;
            }

            {
                cout << endl
                     << "Test pow (MaskedArray<Double>, MaskedArray<Double>)"
                     << endl;
                dh = 2.0;
                dh = pow (dg ((df > 3.5) && (df < 7.5)),
                            df ((df > 2.5) && (df < 6.5)));
                cout << endl << "dh=2.0; "
                     << endl
                     << " dh = pow (dg ((df > 3.5) && (df < 7.5)),"
                     << endl
                     << "             df ((df > 2.5) && (df < 6.5)));"
                     << endl;
                cout << dh << endl;
            }

            {
                cout << endl
                     << "Test pow (MaskedArray<Double>, Double)"
                     << endl;
                dh = 2.0;
                dh = pow (df ((df > 2.5) && (df < 6.5)), 2.0);
                cout << endl << "dh=2.0; "
                     << endl
                     << " dh = pow (df ((df > 2.5) && (df < 6.5)), 2.0);"
                     << endl;
                cout << dh << endl;
            }

            {
                cout << endl
                     << "Test sum (MaskedArray<Double>)"
                     << endl;
                Double result (-1.0);
                result = sum (df ((df > 2.5) && (df < 6.5)));
                cout << endl
                     << " result = sum (df ((df > 2.5) && (df < 6.5)));"
                     << endl;
                cout << result << endl;
            }

            {
                cout << endl
                     << "Test sumsquares (MaskedArray<Double>)"
                     << endl;
                Double result (-1.0);
                result = sumsquares (df ((df > 2.5) && (df < 6.5)));
                cout << endl
                     << " result = sumsquares (df ((df > 2.5) && (df < 6.5)));"
                     << endl;
                cout << result << endl;
            }

            {
                cout << endl
                     << "Test product (MaskedArray<Double>)"
                     << endl;
                Double result (-1.0);
                result = product (df ((df > 2.5) && (df < 6.5)));
                cout << endl
                     << " result = product (df ((df > 2.5) && (df < 6.5)));"
                     << endl;
                cout << result << endl;
            }

            {
                cout << endl
                     << "Test mean (MaskedArray<Double>)"
                     << endl;
                Double result (-1.0);
                result = mean (df ((df > 2.5) && (df < 6.5)));
                cout << endl
                     << " result = mean (df ((df > 2.5) && (df < 6.5)));"
                     << endl;
                cout << result << endl;
            }

            {
                cout << endl
                     << "Test variance (MaskedArray<Double>)"
                     << endl;
                Double result (-1.0);
                result = variance (df ((df > 2.5) && (df < 6.5)));
                cout << endl
                     << " result = variance (df ((df > 2.5) && (df < 6.5)));"
                     << endl;
                cout << result << endl;
            }

            {
                cout << endl
                     << "Test stddev (MaskedArray<Double>)"
                     << endl;
                Double result (-1.0);
                result = stddev (df ((df > 2.5) && (df < 6.5)));
                cout << endl
                     << " result = stddev (df ((df > 2.5) && (df < 6.5)));"
                     << endl;
                cout << result << endl;
            }

            {
                cout << endl
                     << "Test avdev (MaskedArray<Double>)"
                     << endl;
                Double result (-1.0);
                result = avdev (df ((df > 2.5) && (df < 6.5)));
                cout << endl
                     << " result = avdev (df ((df > 2.5) && (df < 6.5)));"
                     << endl;
                cout << result << endl;
            }

            {
                cout << endl
                     << "Test rms (MaskedArray<Double>)"
                     << endl;
                Double result (-1.0);
                result = rms (df ((df > 2.5) && (df < 6.5)));
                cout << endl
                     << " result = rms (df ((df > 2.5) && (df < 6.5)));"
                     << endl;
                cout << result << endl;
            }

            {
                cout << endl
                     << "Test even median (MaskedArray<Double>, True)"
                     << endl;
                Double result (-1.0);
                result = median (df ((df > 2.5) && (df < 6.5)), True);
                cout << endl
                     << " result = median (df ((df > 2.5) && (df < 6.5)), True);"
                     << endl;
                cout << result << endl;
            }

            {
                cout << endl
                     << "Test even median (MaskedArray<Double>)"
                     << endl;
                Vector<Double> dfunsort(10);
                dfunsort = df;
                Double tmp;
                tmp = dfunsort (9);
                dfunsort (9) = dfunsort (5);
                dfunsort (5) = tmp;
                Double result (-1.0);
                result = median (dfunsort ((dfunsort > 2.5) &&
                                           (dfunsort < 6.5)));
                cout << endl
                     << " result = median (dfunsort ((dfunsort > 2.5) &&"
                     << endl
                     << "                            (dfunsort < 6.5)));"
                     << endl;
                cout << result << endl;
                cout << "dfunsort= " << endl << dfunsort << endl;
            }

            {
                cout << endl
                     << "Test odd median (MaskedArray<Double>, True)"
                     << endl;
                Double result (-1.0);
                result = median (df ((df > 2.5) && (df < 7.5)), True);
                cout << endl
                     << " result = median (df ((df > 2.5) && (df < 7.5)), True);"
                     << endl;
                cout << result << endl;
            }

            {
                cout << endl
                     << "Test odd  median (MaskedArray<Double>)"
                     << endl;
                Vector<Double> dfunsort(10);
                dfunsort = df;
                Double tmp;
                tmp = dfunsort (9);
                dfunsort (9) = dfunsort (5);
                dfunsort (5) = tmp;
                Double result (-1.0);
                result = median (dfunsort ((dfunsort > 2.5) &&
                                           (dfunsort < 7.5)));
                cout << endl
                     << " result = median (dfunsort ((dfunsort > 2.5) &&"
                     << endl
                     << "                            (dfunsort < 7.5)));"
                     << endl;
                cout << result << endl;
                cout << "dfunsort= " << endl << dfunsort << endl;
            }

            {
                cout << endl
                     << "Test square (MaskedArray<Double>)"
                     << endl;
                dh = 2.0;
                dh = square (df ((df > 2.5) && (df < 6.5)));
                cout << endl << "dh=2.0; "
                     << endl
                     << "dh = square (df ((df > 2.5) && (df < 6.5)));"
                     << endl;
                cout << dh << endl;
                cout << endl << "df= " << endl << df << endl;
            }

            {
                cout << endl
                     << "Test cube (MaskedArray<Double>)"
                     << endl;
                dh = 2.0;
                dh = cube (df ((df > 2.5) && (df < 6.5)));
                cout << endl << "dh=2.0; "
                     << endl
                     << "dh = cube (df ((df > 2.5) && (df < 6.5)));"
                     << endl;
                cout << dh << endl;
                cout << endl << "df= " << endl << df << endl;
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
