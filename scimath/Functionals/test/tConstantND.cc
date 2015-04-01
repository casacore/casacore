//# tHyperPlane.cc: Test the HyperPlane class
//# Copyright (C) 2001,2002,2004
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

#include <casacore/scimath/Functionals/ConstantND.h>

#include <casacore/scimath/Mathematics/AutoDiff.h>
#include <casacore/scimath/Mathematics/AutoDiffA.h>
#include <casacore/scimath/Mathematics/AutoDiffIO.h>
#include <casacore/scimath/Mathematics/AutoDiffMath.h>
#include <casacore/casa/BasicSL/Constants.h>
#include <casacore/casa/BasicMath/Math.h>
#include <casacore/casa/Arrays/ArrayLogical.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/Utilities/Assert.h>

#include <casacore/casa/iostream.h>

#include <casacore/casa/namespace.h>
int main() {

  try {
    // Construct a constant in m dimensional space.  By

    ConstantND<Double> constant(3);
    
    ConstantND<Double> const2(constant);
    const2[0] = 22;
    
    ConstantND<Double> const3 = const2;
    
    cout << "nparms " << constant.nparameters() << endl;
    AlwaysAssertExit(
    	constant.nparameters() == 1
    	&& const2.nparameters() == 1
    	&& const3.nparameters() == 1
    );
    cout << "ndim " << constant.ndim() << endl;
    cout << "ndim " << const2.ndim() << endl;

    AlwaysAssertExit(
    	constant.ndim() == 3
    	&& const2.ndim() == 3
    	&& const3.ndim() == 3
    );
    cout << "parms " << constant.parameters()[0] << endl;
    cout << "parms " << const2.parameters()[0] << endl;
    cout << "parms " << const3.parameters()[0] << endl;

    AlwaysAssertExit(
    	constant.parameters()[0] == 0
    	&& const2.parameters()[0] == 22
    	&& const3.parameters()[0] == 22
    );

    Vector<Double> v(1, 45.5);
    
    // Set the value of a coefficient. 
    // Get the value of a coefficient.  
    // f(x,y,z) = 45.5
      constant[0] = 45.5;
      AlwaysAssertExit(constant[0] == Double(45.5));

    
    // Set all coefficients at once. 
    // Get all the values of coefficients at once.  
    constant.parameters().setParameters(v);
    AlwaysAssertExit(allEQ(constant.parameters().getParameters(), v));
    
    Vector<Double> x(3);
    x[0] = 20;
    x[1] = 40;
    x[2] = 90;
    // Evaluate the function at <src>x</src>.
    // f(x,y,z) = 45.5
    AlwaysAssertExit((constant(x) - Double(45.5)) < 1.e-6);


      constant.mask(0) = False;
      AlwaysAssertExit(! constant.mask(0));
      AlwaysAssertExit(constant.parameters().nMaskedParameters() == 0);
      constant.mask(0) = True;
      AlwaysAssertExit(constant.parameters().nMaskedParameters() == 1);

      AlwaysAssertExit(constant.parameters().getMaskedParameters()[0] == 45.5);

      // test specialized AutoDiff
      Vector<AutoDiffA<Double> > v6(1, 0);
      ConstantND<AutoDiff<Double> > s5(3);
      v.resize(3);
      for (uInt i=0; i<3; i++) {

    	  s5[0] = AutoDiff<Double>(0,1,0);
          cout << "s5 " << i << " " << s5[0] << endl;

    	  v[i] = i+10;

      }
      //Double y = s5(v).value();
      Vector<Double> z = s5(v).derivatives();
      cout << "AutoDiff " << s5(v) << endl;

/*
  
    // f(x,y,z) = 10x + 11y + 12*z + 13
    Vector<AutoDiffA<Double> > v6(3);
    HyperPlane<AutoDiff<Double> > s5(3);
    for (uInt i=0; i<3; i++) {
      s5[i] = AutoDiff<Double>(i+10,3,i);
      AlwaysAssertExit(s5[i] == Double(i+10));
      v[i] = i+10;
      v6(i) = i+10;
    }
    Double y50 = s5(v).value();
    Vector<Double> y51;
    y51 = s5(v).derivatives();
    cout << "AutoDiff:  " << s5(v) << endl;
    
    // Generic AutoDiff
    HyperPlane<AutoDiffA<Double> > s6(3);
    for (uInt i=0; i<3; i++) {
      s6[i] = AutoDiffA<Double>(i+10,3,i);
      AlwaysAssertExit(s6[i].value() == Double(i+10));
      v6(i) = i+10;
    }
    Double y60 = s6(v6).value();
    Vector<Double> y61;
    y61 = s6(v6).derivatives();
    cout << "AutoDiffA: " << s6(v6) << endl;
    AlwaysAssertExit(near(y60, y50) &&
		     near(y61(0), y51[0]) &&
		     near(y61(1), y51[1]) &&
		     near(y61(2), y51[2]));
		     */
  } catch (AipsError x) {
    cout << "Exception : " << x.getMesg() << endl;
  } 

  cout << "OK" << endl;
  return 0;
}

