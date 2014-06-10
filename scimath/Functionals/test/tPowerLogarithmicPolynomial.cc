//# tPowerLogarithmicPolynomial.cc: Test the one-dimensional PowerLogarithmicPolynomial class
//# Copyright (C) 1995,1996,1999,2001,2002
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
//# $Id: tPowerLogarithmicPolynomial.cc $

#include <scimath/Functionals/PowerLogarithmicPolynomial.h>

#include <scimath/Mathematics/AutoDiff.h>
#include <scimath/Mathematics/AutoDiffA.h>
#include <scimath/Mathematics/AutoDiffIO.h>
#include <scimath/Mathematics/AutoDiffMath.h>
#include <casa/Arrays/Vector.h>
#include <casa/Arrays/ArrayLogical.h>
#include <casa/Arrays/ArrayMath.h>
#include <casa/Utilities/Assert.h>

#include <casa/iostream.h>

#include <casa/namespace.h>
int main() {

//     PowerLogarithmicPolynomial();
	PowerLogarithmicPolynomial<Float> null;
//     PowerLogarithmicPolynomial(uInt order);
//     void setCoefficient(uInt which, T value);
//     virtual void setAdjustParameter(uInt which, const T &val);
    PowerLogarithmicPolynomial<Float> linear(2);
    Vector<Float> coeff(2, 1);
    linear.setCoefficients(coeff);
    PowerLogarithmicPolynomial<Float> square(2);
    square.setCoefficient(0, 1);
    square.setCoefficient(1, 2);
//     virtual T operator()(const T &x) const;
    AlwaysAssertExit(linear(3.0) == 3.0f && square(3.0f) == 9.0f);
    PowerLogarithmicPolynomial<Float> curve(4);
    curve.setCoefficient(0, 1);
    curve.setCoefficient(1, 0.5);
    curve.setCoefficient(2, 1);
    curve.setCoefficient(3, 2);
    cout << "curve " << curve(3.0) << endl;
    AlwaysAssertExit(near(curve(3.0), 82.1209389f));

//     virtual uInt nAdjustParameters() const;
//     uInt order() const {return coefficients_p.nelements() - 1;}
//     virtual T getAdjustParameter(uInt which) const;
//     T coefficient(uInt which) const {return coefficients_p[which];}
//     Vector<T> coefficients() const;
//     virtual Vector<T> getAdjustParameters() const;

    AlwaysAssertExit(linear.nparameters() == 2 &&
		     square.nparameters() == 2 && curve.nparameters() == 4);

    Vector<Float> curveCoeff1, curveCoeff2;
    curveCoeff1 = curve.coefficients();
    curveCoeff2 = curve.parameters().getParameters();
    AlwaysAssertExit(allEQ(curveCoeff1, curveCoeff2));
    AlwaysAssertExit(curveCoeff1.nelements() == 4);
    AlwaysAssertExit(curveCoeff1(0) == 1.0f);
    AlwaysAssertExit(curveCoeff1(1) == 0.5f);
    AlwaysAssertExit(curveCoeff1(2) == 1.0f);
    AlwaysAssertExit(curveCoeff1(3) == 2.0f);


//     PowerLogarithmicPolynomial(const PowerLogarithmicPolynomial &other);
//     PowerLogarithmicPolynomial<T> &operator=(const PowerLogarithmicPolynomial<T> &other);
    PowerLogarithmicPolynomial<Float> curveCopy1(curve);
    PowerLogarithmicPolynomial<Float> curveCopy2;
    curveCopy2 = curve;

    AlwaysAssertExit(curve == curveCopy1 && curve == curveCopy2);

//     void setCoefficients(const Vector<T> &coefficients);
//     virtual void setAdjustParameters(const Vector<T> &val);
    PowerLogarithmicPolynomial<Float> tmp1(4), tmp2(4);
    Vector<Float> coefficients(4); 
    indgen(coefficients);
    tmp1.setCoefficients(coefficients);
    tmp2.parameters().setParameters(coefficients);

    AlwaysAssertExit(allEQ(coefficients, tmp1.coefficients()) &&
		     allEQ(coefficients, tmp1.parameters().getParameters()));


//     Bool operator==(const PowerLogarithmicPolynomial<T> &other) const;
//     Bool operator!=(const PowerLogarithmicPolynomial<T> &other) const;
    AlwaysAssertExit(null == linear && null != square && square != linear &&
    		     null == null && linear == linear && square == square);

    //	clone()
    //     ~PowerLogarithmicPolynomial();
    Function<Float> *tmp3ptr = tmp2.clone();
    AlwaysAssertExit(tmp3ptr->nparameters() == 4 &&
		     (*tmp3ptr)[0] == 0.0f &&
		     (*tmp3ptr)[1] == 1.0f &&
		     (*tmp3ptr)[2] == 2.0f &&
		     (*tmp3ptr)[3] == 3.0f);
    delete tmp3ptr;
  
    // Test Auto differentiation // 0.5 * x**(2 + 3ln(x) + 4ln(x)**2)
    PowerLogarithmicPolynomial<AutoDiffA<Double> > curve2(4);
    curve2[0] = AutoDiffA<Double>(0.5,4,0);
    curve2[1] = AutoDiffA<Double>(2.0,4,1);
    curve2[2] = AutoDiffA<Double>(3.0,4,2);
    curve2[3] = AutoDiffA<Double>(4.0,4,3);
    cout << "Generic(3):  " << curve2(AutoDiffA<Double>(3.0)) << endl;
  
    // Test manual differentiation // 0.5 * x**(2 + 3ln(x) + 4ln(x)**2)
    PowerLogarithmicPolynomial<AutoDiff<Double> > curve3(4);
    curve3[0] = AutoDiff<Double>(0.5,4,0);
    curve3[1] = AutoDiff<Double>(2.0,4,1);
    curve3[2] = AutoDiff<Double>(3.0,4,2);
    curve3[3] = AutoDiff<Double>(4.0,4,3);
    cout << "Specific(3): " << curve3(3.0) << endl;
    AlwaysAssertExit(
    	near(
    		curve2(AutoDiffA<Double>(3.0)).value(),
    		curve3(3.0).value()
    	)
    	&& allNear(
    		curve2(AutoDiffA<Double>(3.0)).derivatives(),
    		curve3(3.0).derivatives(), 1e-13
    	)
    );
    cout << "OK" << endl;
    return 0;
}
