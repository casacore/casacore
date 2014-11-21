//# tSPolynomial.cc: Test the one-dimensional scaled polynomial class
//# Copyright (C) 2002
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

#include <casacore/scimath/Functionals/SPolynomial.h>

#include <casacore/scimath/Mathematics/AutoDiff.h>
#include <casacore/scimath/Mathematics/AutoDiffA.h>
#include <casacore/scimath/Mathematics/AutoDiffIO.h>
#include <casacore/scimath/Mathematics/AutoDiffMath.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Arrays/ArrayLogical.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/Utilities/Assert.h>

#include <casacore/casa/iostream.h>

#include <casacore/casa/namespace.h>
int main() {

//     SPolynomial();
    SPolynomial<Float> null;
//     SPolynomial(uInt order);
//     void setCoefficient(uInt which, T value);
//     virtual void setAdjustParameter(uInt which, const T &val);
    SPolynomial<Float> linear(1); linear.setCoefficient(1, 1); // x
    SPolynomial<Float> square(2); square.setCoefficient(2, 1);   // x^2

//     virtual T operator()(const T &x) const;
    AlwaysAssertExit(linear(3.0) == 3.0f && square(3.0f) == 9.0f);

//     virtual uInt nAdjustParameters() const;
//     uInt order() const {return coefficients_p.nelements() - 1;}
//     virtual T getAdjustParameter(uInt which) const;
//     T coefficient(uInt which) const {return coefficients_p[which];}
//     Vector<T> coefficients() const;
//     virtual Vector<T> getAdjustParameters() const;
    AlwaysAssertExit(null.order() == 0 && linear.order() == 1 &&
		     square.order() == 2 &&
		     null[0+3] == 0.0f && linear[0+3] == 0.0f &&
		     linear[1+3] == 1.0f && square[0+3] == 0.0f &&
		     square[1+3] == 0.0f && square.coefficient(2) == 1.0f);
    AlwaysAssertExit(null.nparameters() == 1+3 &&
		     square.nparameters() == 3+3);
    Vector<Float> sqrCoeff1, sqrCoeff2;
    sqrCoeff1 = square.coefficients();
    AlwaysAssertExit(sqrCoeff1.nelements() == 3);
    AlwaysAssertExit(sqrCoeff1(0) == 0.0f);
    AlwaysAssertExit(sqrCoeff1(1) == 0.0f);
    AlwaysAssertExit(sqrCoeff1(2) == 1.0f);

//     SPolynomial(const SPolynomial &other);
//     SPolynomial<T> &operator=(const SPolynomial<T> &other);
    SPolynomial<Float> squareCopy1(square);
    SPolynomial<Float> squareCopy2; squareCopy2 = square;

    AlwaysAssertExit(square == squareCopy1 && square == squareCopy2);

//     void setCoefficients(const Vector<T> &coefficients);
//     virtual void setAdjustParameters(const Vector<T> &val);
    SPolynomial<Float> tmp1(3), tmp2(3);
    Vector<Float> coefficients(4); 
    indgen(coefficients); // x + 2x^2 + 3x^3

    tmp1.setCoefficients(coefficients);

    AlwaysAssertExit(allEQ(coefficients, tmp1.coefficients()));

//     Bool operator==(const SPolynomial<T> &other) const;
//     Bool operator!=(const SPolynomial<T> &other) const;
    AlwaysAssertExit(null != linear && null != square && square != linear &&
    		     null == null && linear == linear && square == square);

    //	clone()
    //     ~SPolynomial();
    Function<Float> *tmp3ptr = tmp2.clone();
    /*    AlwaysAssertExit(tmp3ptr->nparameters() == 4+3 &&
		     (*tmp3ptr)[0+3] == 0.0f &&
		     (*tmp3ptr)[1+3] == 1.0f &&
		     (*tmp3ptr)[2+3] == 2.0f &&
		     (*tmp3ptr)[3+3] == 3.0f);
    */
    delete tmp3ptr;
  
  // Test Auto differentiation // 1 + 2x + 3x^2
    SPolynomial<AutoDiffA<Double> > sq2(2);
    sq2[0] = AutoDiffA<Double>(1.0,3,0);
    sq2[1] = AutoDiffA<Double>(2.0,3,1);
    sq2[2] = AutoDiffA<Double>(3.0,3,2);
    cout << "Generic(3):  " << sq2(AutoDiffA<Double>(3.0)) << endl;
  
  // Test manual differentiation // 1 + 2x + 3x^2
    SPolynomial<AutoDiff<Double> > sq3(2);
    sq3[0] = AutoDiff<Double>(1.0,3,0);
    sq3[1] = AutoDiff<Double>(2.0,3,1);
    sq3[2] = AutoDiff<Double>(3.0,3,2);
    cout << "Specific(3): " << sq3(3.0) << endl;
    AlwaysAssertExit(near(sq2(AutoDiffA<Double>(3.0)).value(),
			  sq3(3.0).value()) &&
		     allNear(sq2(AutoDiffA<Double>(3.0)).derivatives(),
			     sq3(3.0).derivatives(),
			     1e-13));
    cout << "OK" << endl;
    return 0;
}
