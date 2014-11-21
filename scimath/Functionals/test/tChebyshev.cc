//# tChebyshev: test the Chebyshev class
//# Copyright (C) 2000,2001,2002,2003
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

#ifdef DEBUG 
#define DIAGNOSTICS
#endif

#include <casacore/scimath/Functionals/Chebyshev.h>
#include <casacore/scimath/Functionals/Polynomial.h>
#include <casacore/casa/Containers/Record.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/BasicMath/Math.h>
#include <casacore/scimath/Mathematics/AutoDiffA.h>
#include <casacore/scimath/Mathematics/AutoDiffMath.h>
#include <casacore/scimath/Mathematics/AutoDiffIO.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Exceptions.h>
#include <casacore/casa/iostream.h>

#include <casacore/casa/namespace.h>
int main() {
    Chebyshev<Double> cheb;

    Vector<Double> coeffs(4, 0);
    coeffs(3) = 2.0;
    cheb.setCoefficients(coeffs);

#ifdef DIAGNOSTICS
    cout << "Chebyshev " << coeffs;
#endif
    cheb.chebyshevToPower(coeffs);
#ifdef DIAGNOSTICS
    cout << " maps to polynomials coeffs: " << coeffs << endl;
#endif
    AlwaysAssertExit(coeffs(0) == 0 && coeffs(1) == -6 && 
		     coeffs(2) == 0 && coeffs(3) ==  8   );
#ifdef DIAGNOSTICS
    cout << "And power series coeffs " << coeffs;
#endif
    cheb.powerToChebyshev(coeffs);
#ifdef DIAGNOSTICS
    cout << " maps to chebyshev coeffs: " << coeffs << endl;
    cout << "coeffs: " << coeffs << ", " << cheb << endl;
#endif
    AlwaysAssertExit(coeffs(0) == cheb.getCoefficient(0) && 
		     coeffs(1) == cheb.getCoefficient(1) && 
		     coeffs(2) == cheb.getCoefficient(2) && 
		     coeffs(3) == cheb.getCoefficient(3)   );

    coeffs = 2.0;
    coeffs(0) += 1.0;
    cheb.setCoefficients(coeffs);
#ifdef DIAGNOSTICS
    cout << "Chebyshev " << coeffs;
#endif
    cheb.chebyshevToPower(coeffs);
#ifdef DIAGNOSTICS
    cout << " maps to power series coeffs: " << coeffs << endl;
#endif
    AlwaysAssertExit(coeffs(0) == 1 && coeffs(1) == -4 && 
		     coeffs(2) == 4 && coeffs(3) ==  8   );

    Double xmin = 0, xmax = 4, xp;
    cheb.setInterval(xmin, xmax);
    AlwaysAssertExit(xmin == cheb.getIntervalMin());
    AlwaysAssertExit(xmax == cheb.getIntervalMax());

    Polynomial<Double> poly(3);
    poly.setCoefficients(coeffs);
    Chebyshev<Double> chebp = cheb.derivative();

#ifdef DIAGNOSTICS
    Vector<Double> dce;
    dce = chebp.getCoefficients();
    chebp.chebyshevToPower(dce);
    cout << "dcheb: " << dce << endl;
#endif
    Polynomial<Double> polyp = poly.derivative();
    polyp.setCoefficients(polyp.coefficients() * (2/(xmax-xmin)));
#ifdef DIAGNOSTICS
    cout << "dpoly: " << polyp.coefficients() << endl;
#endif

    for (Double x=xmin; x <= xmax; x += 0.1) {
	xp = (2*x-xmin-xmax)/(xmax-xmin);
	AlwaysAssertExit(nearAbs(cheb(x), poly(xp), 1.0e-14));
	AlwaysAssertExit(nearAbs(chebp(x), polyp(xp), 1.0e-14));
    }

    // Test auto differentiation wrt x
    Chebyshev<AutoDiffA<Double> > chebad(cheb.nparameters());
    chebad.setInterval(cheb.getIntervalMin(), cheb.getIntervalMax());
    ///    for (uInt i=0; i<4; ++i) chebad[i] = AutoDiffA<Double>(cheb[i]);
    for (uInt i=0; i<4; ++i) chebad[i] = cheb[i];
    for (AutoDiffA<Double> x(xmin, 1, 0); x <= xmax; x += 0.1) {
      AlwaysAssertExit(nearAbs(chebp(x.value()), chebad(x).deriv(0), 1.0e-14));
    }

    // test out-of-interval modes
    AlwaysAssertExit(0 == cheb.getDefault());
    cheb.setDefault(5);
    AlwaysAssertExit(5 == cheb.getDefault());
    AlwaysAssertExit(cheb(xmin-1) == cheb.getDefault());
    cheb.setOutOfIntervalMode(ChebyshevEnums::EXTRAPOLATE);
    AlwaysAssertExit(cheb(xmin-0.2) != cheb.getDefault());
    xp = (2*(xmin-0.2)-xmin-xmax)/(xmax-xmin);
#ifdef DIAGNOSTICS
    cout << xmin-0.2 << ": cheb-poly=" << cheb(xmin-0.2)-poly(xp) << endl;
#endif
    AlwaysAssertExit(nearAbs(cheb(xmin-0.2), poly(xp), 1.0e-14));
    cheb.setOutOfIntervalMode(ChebyshevEnums::CYCLIC);
#ifdef DIAGNOSTICS
    cout << xmin-1.3 << ": cheb(-1.3)-cheb(2.7)=" 
	 << cheb(xmin-1.3)-cheb(xmin-1.3+(xmax-xmin)) << endl;
#endif
    AlwaysAssertExit(nearAbs(cheb(xmin-1.3), 
			     cheb(xmin-1.3+(xmax-xmin)), 1.0e-15));
#ifdef DIAGNOSTICS
    cout << xmax+1.3 << ": cheb(5.3)-cheb(1.3)=" 
	 << cheb(xmin+1.3)-cheb(xmin+1.3-(xmax-xmin)) << endl;
#endif
    AlwaysAssertExit(nearAbs(cheb(xmax+1.3),
			     cheb(xmax+1.3-(xmax-xmin)), 1.0e-15));
    cheb.setOutOfIntervalMode(ChebyshevEnums::ZEROTH);
    cheb.setCoefficient(0, 1);
    AlwaysAssertExit(cheb(xmax+1) == cheb.getCoefficient(0));

    // Test setMode()
    AlwaysAssertExit(cheb.hasMode());

    Record rec;
    Vector<Double> intv(2);
    intv(0) = -10.0; intv(1) = 10.0;
    rec.define(RecordFieldId("intervalMode"), "cyclic");
    rec.define(RecordFieldId("interval"), intv);
    rec.define(RecordFieldId("default"), 80.0);

    cheb.setMode(rec);

#ifdef DIAGNOSTICS
    cout << "Results from setMode():" << endl
	 << "  Interval Mode: " << cheb.getOutOfIntervalMode() << endl
	 << "  Range: " << cheb.getIntervalMin() << ", " 
	 << cheb.getIntervalMax() << endl
	 << "  Default: " << cheb.getDefault() << endl;
#endif
    AlwaysAssertExit(cheb.getOutOfIntervalMode() == 
		         ChebyshevEnums::CYCLIC  &&
                     cheb.getIntervalMin() == -10.0 &&
		     cheb.getIntervalMax() ==  10.0 &&
	             cheb.getDefault() == 80.0);

    // test setMode() via constructor
    Chebyshev<Double> cheb2(2, rec);
    AlwaysAssertExit(cheb2.getOutOfIntervalMode() == 
		         ChebyshevEnums::CYCLIC  &&
                     cheb2.getIntervalMin() == -10.0 &&
		     cheb2.getIntervalMax() ==  10.0 &&
	             cheb2.getDefault() == 80.0);

    // test getMode() 
    Record rec2;
    cheb.setInterval(-15.0, 15.0);
    cheb.setDefault(70.0);
    cheb.setOutOfIntervalMode(ChebyshevEnums::ZEROTH);
    cheb.getMode(rec2);

    try {
	Vector<Double> tmp(2);
	rec2.get(RecordFieldId("interval"), tmp);

	Double def;
	rec2.get(RecordFieldId("default"), def);

	String mode;
	rec2.get(RecordFieldId("intervalMode"), mode);

	AlwaysAssertExit(mode == String("zeroth") && 
			 tmp(0) == -15.0 && tmp(1) == 15.0 &&
			 def == 70.0);
    }
    catch (AipsError ex) {
	cerr << "Exception: " << ex.getMesg() << endl;
	exit(1);
    }

    cout << "OK" << endl;
    return 0;
}
