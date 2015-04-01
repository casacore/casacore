//# tMatrixMathLA.cc: Test functions in MatrixMathLA.h
//# Copyright (C) 1995,1996,1999,2001, 2004
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

#include <casacore/scimath/Mathematics/MatrixMathLA.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/Arrays/ArrayLogical.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/iostream.h>

#include <casacore/casa/namespace.h>
int main()
{
    Matrix<Double> ind(3,3);
    ind(0,0) = 2; ind(0,1) = 8; ind(0,2) = 6;
    ind(1,0) = 4; ind(1,1) = 2; ind(1,2) = -2;
    ind(2,0) = 3; ind(2,1) = -1; ind(2,2) = 1;

    Matrix<Double> outd(3,3);
    outd(0,0) = 0; outd(0,1) = 7; outd(0,2) = 14;
    outd(1,0) = 5; outd(1,1) = 8; outd(1,2) = -14;
    outd(2,0) = 5; outd(2,1) = -13; outd(2,2) = 14;
    outd /= 70.0;

    AlwaysAssertExit(allNearAbs(invert(ind), outd, 0.00001));

    // Now test the other types - Float/Complex/DComplex

    Matrix<Float> inf(3,3), outf(3,3);
    convertArray(inf, ind); convertArray(outf, outd);
    AlwaysAssertExit(allNearAbs(invert(inf), outf, 0.00001));

    Matrix<Complex> inc(3,3), outc(3,3);
    convertArray(inc, ind); convertArray(outc, outd);
    AlwaysAssertExit(allNearAbs(invert(inc), outc, 0.00001));

    Matrix<DComplex> indc(3,3), outdc(3,3);
    convertArray(indc, ind); convertArray(outdc, outd);
    AlwaysAssertExit(allNearAbs(invert(indc), outdc, 0.00001));

    cout << "OK" << endl;
    return 0;
}
