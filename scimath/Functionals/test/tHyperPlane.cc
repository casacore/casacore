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

#include <casacore/scimath/Functionals/HyperPlane.h>

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
    // Construct an m dimensional hyper plane which has m coefficients.  By 
    // default, the coefficients are initialized to zero.
    //HyperPlane(uInt m);
    HyperPlane<Double> hyper(3);
    
    // Make this object a copy of other.
    //HyperPlane(const HyperPlane<Type> &other);
    HyperPlane<Double> comb2(hyper);
    
    // Make this object a copy of other.
    //HyperPlane<Type> &operator=(const HyperPlane<Type> &other);
    comb2 = hyper;
    
    // Return the total number of coefficients, which is the dimension of the
    // hyper plane.
    AlwaysAssertExit(hyper.nparameters() == 3 && 
      hyper.nparameters() == comb2.nparameters());
    Vector<Double> v(3);
    
    // Set the value of a coefficient. 
    // Get the value of a coefficient.  
    // f(x,y,z) = 10x + 11y + 12*z
    for (uInt i=0; i<3; i++) {
      hyper[i] = i+10;
      AlwaysAssertExit(hyper[i] == Double(i+10));
      v(i) = i+10;
    }
    
    // Set all coefficients at once. 
    // Get all the values of coefficients at once.  
    hyper.parameters().setParameters(v);
    AlwaysAssertExit(allEQ(hyper.parameters().getParameters(), v));
    
    // Evaluate the function at <src>v</src>. 
    // f(x,y,z) = 10x + 11y + 12*z
    AlwaysAssertExit((hyper(v) - Double(365)) < 1.e-6);
    // Returns the dimension of functions in the linear hyper
    // uInt ndim() const;
    AlwaysAssertExit(hyper.ndim() == 3);
    
    // Set coefficients
    for (uInt i=0; i<hyper.nparameters()-2; i++) { 
      hyper.mask(i) = False;
      AlwaysAssertExit(!hyper.mask(i));
    }
    
    AlwaysAssertExit(hyper.parameters().nMaskedParameters() == 2);
    
    for (uInt i=0; i<hyper.parameters().nMaskedParameters(); i++) { 
      AlwaysAssertExit(hyper.parameters().getMaskedParameters()[i] ==
		       Double(11+i));
    }
  
    // test specialized AutoDiff 
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
  } catch (AipsError x) {
    cout << "Exception : " << x.getMesg() << endl;
  } 

  cout << "OK" << endl;
  return 0;
}

