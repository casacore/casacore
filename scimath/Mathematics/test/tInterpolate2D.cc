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
#include <casacore/casa/BasicMath/Math.h>
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
        Matrix<float> matt_f(10,10);
        Matrix<double> matt_d(10,10);
        for (uint32_t i=0; i<10; ++i) {
          for (uint32_t j=0; j<10; ++j) {
            matt_f(i,j) = i+j;
            matt_d(i,j) = i+j;
          }
        }

        // Where to evaluate the interpolation
        Vector<double> where(2);
        where(0) = 3.452; where(1) = 6.1;

        // Test for all implemented methods
        std::vector<string> methods(4);
        methods[0] = "linear";
        methods[1] = "cubic";
        methods[2] = "lanczos";
        methods[3] = "nearest";

        std::vector<double> results(4);
        results[0] = 9.552; // Linear
        results[1] = 9.552; // Cubic
        results[2] = 9.473654921656; // Lanczos
        results[3] = 9.; // Nearest
        bool ok;
        for (uint32_t method=0; method<methods.size(); ++method) {
          float result_f;
          Interpolate2D myInterp(Interpolate2D::stringToMethod(methods[method]));

          ok = myInterp.interp(result_f, where, matt_f);
          AlwaysAssert(ok==true, AipsError);
          AlwaysAssert(near(result_f, results[method]), AipsError);

          double result_d;
          ok = myInterp.interp(result_d, where, matt_d);
          AlwaysAssert(ok==true, AipsError);
          AlwaysAssert(near(result_d, results[method], 1.e-9), AipsError);
        }

        // complex value interpolation, CAS-11375
        Matrix<Complex> matt_c(10,10);
        Matrix<DComplex> matt_dc(10,10);
        std::vector<DComplex> cresults(results.size());
        for (uint32_t i=0; i<results.size(); ++i) {
            cresults[i] = DComplex(results[i], 2*results[i]);
        }
        for (uint32_t i=0; i<10; ++i) {
            for (uint32_t j=0; j<10; ++j) {
                uint32_t v = i + j;
                matt_c(i,j) = Complex(v, 2*v);
                matt_dc(i,j) = DComplex(v, 2*v);
            }
        }
        for (uint32_t method=0; method<methods.size(); ++method) {
            Complex result_c;
            Interpolate2D myInterp(Interpolate2D::stringToMethod(methods[method]));
            ok = myInterp.interp(result_c, where, matt_c);
            AlwaysAssert(ok, AipsError);
            AlwaysAssert(near(result_c, (Complex)cresults[method]), AipsError);
            DComplex result_dc;
            ok = myInterp.interp(result_dc, where, matt_dc);
            AlwaysAssert(ok, AipsError);
            AlwaysAssert(near(result_dc, cresults[method]), AipsError);
        }
    }
    catch (const std::exception& x) {
        cout << x.what() << endl;
        cout << "FAIL" << endl;
        return 1;
    }
    cout << "OK" << endl;
    return 0;
}

