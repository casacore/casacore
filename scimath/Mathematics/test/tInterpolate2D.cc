//# Copyright (C) 1996,1997,1998,1999,2000,2001,2002
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
#include <casacore/scimath/Mathematics/Interpolate2D.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Arrays/Matrix.h>
#include <vector>
#include <string>

#include <casacore/casa/namespace.h>


int main() {
    try {
        AlwaysAssert(Interpolate2D::stringToMethod("l") ==
                       Interpolate2D::LINEAR, AipsError);
        AlwaysAssert(Interpolate2D::stringToMethod("linear") ==
                       Interpolate2D::LINEAR, AipsError);
        AlwaysAssert(Interpolate2D::stringToMethod("z") ==
                       Interpolate2D::LANCZOS, AipsError);
        AlwaysAssert(Interpolate2D::stringToMethod("lanczos") ==
                       Interpolate2D::LANCZOS, AipsError);
        AlwaysAssert(Interpolate2D::stringToMethod("c") ==
                       Interpolate2D::CUBIC, AipsError);
        AlwaysAssert(Interpolate2D::stringToMethod("cubic") ==
                       Interpolate2D::CUBIC, AipsError);

        // Set up matrix of input values
        Matrix<Float> matt_f(10,10);
        Matrix<Double> matt_d(10,10);
        for (uInt i=0; i<10; ++i) {
          for (uInt j=0; j<10; ++j) {
            matt_f(i,j) = i+j;
            matt_d(i,j) = i+j;
          }
        }

        // Where to evaluate the interpolation
        Vector<Double> where(2);
        where(0) = 3.452; where(1) = 6.1;

        // Test for all implemented methods
        vector<string> methods(4);
        methods[0] = "linear";
        methods[1] = "cubic";
        methods[2] = "lanczos";
        methods[3] = "nearest";

        vector<Double> results(4);
        results[0] = 9.552; // Linear
        results[1] = 9.552; // Cubic
        results[2] = 9.473654921656; // Lanczos
        results[3] = 9.; // Nearest

        for (uInt method=0; method<methods.size(); ++method) {
          Bool ok;

          Float result_f;
          Interpolate2D myInterp(Interpolate2D::stringToMethod(methods[method]));

          ok = myInterp.interp(result_f, where, matt_f);
          AlwaysAssert(ok==True, AipsError);
          AlwaysAssert(near(result_f, results[method]), AipsError);

          Double result_d;
          ok = myInterp.interp(result_d, where, matt_d);
          AlwaysAssert(ok==True, AipsError);
          AlwaysAssert(near(result_d, results[method], 1.e-9), AipsError);
        }
    }
    catch (AipsError x) {
        cout << x.getMesg() << endl;
        cout << "FAIL" << endl;
        return 1;
    }
    cout << "OK" << endl;
    return 0;
}

