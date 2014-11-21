//# tInterpolate1D.cc: This program tests the Interpolate1D class
//# Copyright (C) 1996,1997,1998,1999,2000,2001,2004
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

#include <casacore/casa/aips.h>
#include <casacore/casa/Arrays/Array.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/Arrays/ArrayIO.h>
#include <casacore/casa/Arrays/IPosition.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/BasicMath/Math.h>
#include <casacore/casa/BasicSL/Complex.h>
#include <casacore/casa/Containers/Block.h>
#include <casacore/scimath/Functionals/ArraySampledFunctional.h>
#include <casacore/scimath/Functionals/Interpolate1D.h>
#include <casacore/scimath/Functionals/ScalarSampledFunctional.h>

#include <casacore/casa/iostream.h>

#include <casacore/casa/namespace.h>

// Main program to test the Interpolate1D class

int main()
{
  Bool anyFailures = False;
  // Test the Interpolate1D class with Floating point Vectors and linear
  // interpolation
  {
    Bool failed = False;
    Vector<Float> x(5); indgen(x); 
    Vector<Float> y(5); indgen(y); 
    ScalarSampledFunctional<Float> fx(x), fy(y);
    Interpolate1D<Float,Float> value(fx, fy);
    Float xs;
    for (xs = -1; xs < 5; xs += 0.1)
      if (near(value(xs), xs) == False){
	cout << "value(" << xs << ") = " << value(xs) << 
	  " which is not near the expected value of " << xs << endl;
	failed = True;
      }
    // Check the assignment operator and copy constructor use copy symantics
    Interpolate1D<Float,Float> v1(value), v2;
    v2 = v1;
    Vector<Float> y1(5); indgen(y1, 1.0f);
    Vector<Float> y2(5); indgen(y2, 2.0f);
    ScalarSampledFunctional<Float> fy1(y1), fy2(y2);
    v1.setData(fx, fy1);
    v2.setData(fx, fy2);
    for (xs = -1; xs < 5; xs += 0.1) {
      if (near(v1(xs), xs+1.0f, 1.0E-5) == False){
	cout << "v1(" << xs << ") = " << v1(xs) << 
	  " which is not near the expected value of " << xs+1.0f << endl;
	failed = True;
      }
      if (near(v2(xs), xs+2.0f, 1.0E-5) == False){
	cout << "v2(" << xs << ") = " << v2(xs) << 
	  " which is not near the expected value of " << xs+2.0f << endl;
	failed = True;
      }
    }
    if (failed){
      cout << "Failed "; anyFailures = True;
    }
    else
      cout << "Passed ";
    cout << "the Interpolate1D<Float,Float> test with linear interpolation"
	 << endl;
  }
  // Test the Interpolate1D class with Int/Double Vectors and cubic
  // interpolation
  {
    Bool failed = False;
    Vector<Int> x(5); indgen(x);
    Vector<Double> y(5); indgen(y);
    y = y*y*y;
    ScalarSampledFunctional<Int> fx(x);
    ScalarSampledFunctional<Double> fy(y);
    Interpolate1D<Int, Double> value(fx, fy);
    value.setMethod(Interpolate1D<Int,Double>::cubic);
    for (Int xs = -5; xs < 10; xs += 1)
      if (near(value(xs),(Double) xs*xs*xs,1E-6) == False){
	cout << "value(" << xs << ") = " << value(xs) <<
	  " which is not near the expected value of " << xs*xs*xs << endl;
	failed = True;
      }
    if (!failed){
      Vector<Int> xd = value.getX();
      if (xd.nelements() != 5) 
	failed = True;
      if (!failed)
	for (Int i = 0; i < 5; i++)
	  if (x(i) != xd(i))
	    failed = True;
      Vector<Double> yd = value.getY();
      if (yd.nelements() != 5) 
	failed = True;
      if (!failed)
	for (Int j = 0; j < 5; j++)
	  if (y(j) != yd(j))
	    failed = True;
    }      
    if (failed){
      cout << "Failed "; anyFailures = True;
    }
    else
      cout << "Passed ";
    cout << "the Interpolate1D<Int,Double> test with cubic interpolation" 
	 << endl;
  }
  // Test the Interpolate1D class with Double/Complex Blocks and nearest
  // neighbour interpolation
  {
    Bool failed = False;
    Vector<Double> x(5); indgen(x); 
    Vector<DComplex> y(5); indgen(y); 
    const DComplex j(0.,1.);  
    y = y+j*y*y;
    Block<Double> bx; x.toBlock(bx);
    Block<DComplex> by; y.toBlock(by);
    ScalarSampledFunctional<Double> fx(bx);
    ScalarSampledFunctional<DComplex> fy(by);
    Interpolate1D<Double,DComplex> value(fx, fy);
  
    value.setMethod(Interpolate1D<Double,DComplex>::nearestNeighbour);
    Double ev;
    for (Float xs = -5.0000001; xs < 5; xs += .1){
      ev = max(min((Int) (xs+0.5),4),0);
      if (near((value(xs)).real(), ev) == False ||  
	  near((value(xs)).imag(), ev*ev) == False) {
	cout << "value(" << xs << ") = " << value(xs) << 
	  " is not near the expected value of (" <<
	  ev << ", " << ev*ev << ")" << endl;
	failed = True;
      }
    }
    if (failed){
      cout << "Failed "; anyFailures = True;
    }
    else
      cout << "Passed ";
    cout << "the Interpolate1D<Double,DComplex> test with nearest neighbour"
	 << endl << "       "
	 << "                                   interpolation (using Blocks)"
	 << endl;

  }
  // Test the Interpolate1D class with Float / Float Array and spline
  // interpolation
  {
    Bool failed = False;
    Vector<Float> x(5); indgen(x); 
    IPosition shape(3, 3, 5, 1);
    Array<Float>  y(shape); 
    IPosition xshape(3, 1, 5, 1);
    Array<Float>  xa(xshape); indgen(xa);
    IPosition trc(3,0,4,0), blc(3,0,0,0), step(3,1,0,0);

    y(blc,trc) =  xa; trc += step; blc += step;
    y(blc,trc) =  xa*xa; trc += step; blc += step;
    y(blc,trc) =  xa*xa*xa;

    ScalarSampledFunctional<Float> fx(x);
    ArraySampledFunctional<Array<Float> > fy(y);
    Interpolate1D<Float,Array<Float> > value(fx,fy);
    value.setMethod(Interpolate1D<Float,Array<Float> >::spline);
    trc(0) = 2; trc(1) = 0; trc(2) = 0;
    blc(0) = 0; blc(1) = 0; blc(2) = 0;
    step(0) = 0; step(1) = 1; 
    
    Array<Float> iv; 
    for (Float xs = 0; xs < 5; xs += 1){
      iv = value(xs);
      if ((near(iv(IPosition(1, 0)), 
		y(IPosition(3, 0,(uInt) xs, 0))) == False) || 
	  (near(iv(IPosition(1, 1)), 
		y(IPosition(3, 1, (uInt) xs, 0))) == False) ||	  
 	  (near(iv(IPosition(1, 2)), 
 		y(IPosition(3, 2, (uInt) xs, 0))) == False)){
	cout << "value(" << xs << ")" << endl << iv 
	     << " is not near the expected value of " << endl
	     << y(blc, trc) << endl;
	failed = True;
      }
      trc += step; blc += step; 
    }
    iv = value((Float) 5);
    if ((near(iv(IPosition(1, 0)), (Float) 5) == False) ||
	(near(iv(IPosition(1, 1)), (Float) 23) == False) ||
	(near(iv(IPosition(1, 2)), (Float) 101) == False))
      failed = True;
    // Switch out of spline mode back to cubic interpolation
    value.setMethod(Interpolate1D<Float, Array<Float> >::cubic);
    if (value.getMethod() != Interpolate1D<Float, Array<Float> >::cubic) {
      failed = True;
      cout << "Could not change the interpolation method" << endl;
    }
    iv = value(Float(-1));
    if (near(iv(IPosition(1,1)), Float(1)) == False){
      failed = True;
      cout << "Did not really change the interpolation method" << endl;
    }
    if (failed){
      cout << "Failed "; anyFailures = True;
    }
    else
      cout << "Passed ";
    cout << "the Interpolate1D<Float, Array<Float> > test with "
	 << "spline interpolation" << endl;
  }

  // Now test the table system interface. 
  // This requires the construction of a table (vrtually done)
  {
    Vector<Float> time (6);
    Vector<Double> amp (6);

    for (uInt i=0; i < 6; i++) {
      time[i] = i;
      amp[i]  = i*i;
    }
    // Now I have constructed a table with two scalar and one array column
    // Test the Interpolate1D class with the scalar columns
    {
      Bool failed = False;

      Vector<Float> x(time);
      Vector<Double> y(amp);
      ScalarSampledFunctional<Float> fx(x);
      ScalarSampledFunctional<Double> fy(y);
      Interpolate1D<Float,Double> value(fx, fy);
      value.setMethod(Interpolate1D<Float,Double>::cubic);

      for (Float xs = -5; xs < 10; xs += .5)
	if (near(value(xs),(Double) xs*xs) == False){
	  cout << "value(" << xs << ") = " << value(xs) <<
	    " which is not near the expected value of " << xs*xs << endl;
	  failed = True;
	}
      if (failed){
	cout << "Failed "; anyFailures = True;
      }
      else
	cout << "Passed ";
      cout << "the Interpolate1D<Float,Double> test with cubic interpolation" 
	   << endl << "       "
	   << "                         using table scalar columns as inputs" 
	   << endl;
    }
  }
  if (anyFailures) {
    cout << "FAIL" << endl;
    return 1;
  }
  else {
    cout << "OK" << endl;
    return 0;
  }
}
// Local Variables: 
// compile-command: "gmake OPTLIB=1 tInterpolate1D"
// End:
