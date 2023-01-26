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

#include <casacore/casa/aips.h>
#include <casacore/casa/Arrays/Array.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/IO/ArrayIO.h>
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
  bool anyFailures = false;
  // Test the Interpolate1D class with Floating point Vectors and linear
  // interpolation
  {
    bool failed = false;
    Vector<float> x(5); indgen(x); 
    Vector<float> y(5); indgen(y); 
    ScalarSampledFunctional<float> fx(x), fy(y);
    Interpolate1D<float,float> value(fx, fy);
    float xs;
    for (xs = -1; xs < 5; xs += 0.1)
      if (near(value(xs), xs) == false){
	cout << "value(" << xs << ") = " << value(xs) << 
	  " which is not near the expected value of " << xs << endl;
	failed = true;
      }
    // Check the assignment operator and copy constructor use copy symantics
    Interpolate1D<float,float> v1(value), v2;
    v2 = v1;
    Vector<float> y1(5); indgen(y1, 1.0f);
    Vector<float> y2(5); indgen(y2, 2.0f);
    ScalarSampledFunctional<float> fy1(y1), fy2(y2);
    v1.setData(fx, fy1);
    v2.setData(fx, fy2);
    for (xs = -1; xs < 5; xs += 0.1) {
      if (near(v1(xs), xs+1.0f, 1.0E-5) == false){
	cout << "v1(" << xs << ") = " << v1(xs) << 
	  " which is not near the expected value of " << xs+1.0f << endl;
	failed = true;
      }
      if (near(v2(xs), xs+2.0f, 1.0E-5) == false){
	cout << "v2(" << xs << ") = " << v2(xs) << 
	  " which is not near the expected value of " << xs+2.0f << endl;
	failed = true;
      }
    }
    if (failed){
      cout << "Failed "; anyFailures = true;
    }
    else
      cout << "Passed ";
    cout << "the Interpolate1D<float,float> test with linear interpolation"
	 << endl;
  }
  // Test the Interpolate1D class with int32_t/double Vectors and cubic
  // interpolation
  {
    bool failed = false;
    Vector<int32_t> x(5); indgen(x);
    Vector<double> y(5); indgen(y);
    y = y*y*y;
    ScalarSampledFunctional<int32_t> fx(x);
    ScalarSampledFunctional<double> fy(y);
    Interpolate1D<int32_t, double> value(fx, fy);
    value.setMethod(Interpolate1D<int32_t,double>::cubic);
    for (int32_t xs = -5; xs < 10; xs += 1)
      if (near(value(xs),(double) xs*xs*xs,1E-6) == false){
	cout << "value(" << xs << ") = " << value(xs) <<
	  " which is not near the expected value of " << xs*xs*xs << endl;
	failed = true;
      }
    if (!failed){
      Vector<int32_t> xd = value.getX();
      if (xd.nelements() != 5) 
	failed = true;
      if (!failed)
	for (int32_t i = 0; i < 5; i++)
	  if (x(i) != xd(i))
	    failed = true;
      Vector<double> yd = value.getY();
      if (yd.nelements() != 5) 
	failed = true;
      if (!failed)
	for (int32_t j = 0; j < 5; j++)
	  if (y(j) != yd(j))
	    failed = true;
    }      
    if (failed){
      cout << "Failed "; anyFailures = true;
    }
    else
      cout << "Passed ";
    cout << "the Interpolate1D<int32_t,double> test with cubic interpolation" 
	 << endl;
  }
  // Test the Interpolate1D class with double/Complex Blocks and nearest
  // neighbour interpolation
  {
    bool failed = false;
    Vector<double> x(5); indgen(x); 
    Vector<DComplex> y(5); indgen(y); 
    const DComplex j(0.,1.);  
    y = y+j*y*y;
    Block<double> bx = makeBlock(x);
    Block<DComplex> by = makeBlock(y);
    ScalarSampledFunctional<double> fx(bx);
    ScalarSampledFunctional<DComplex> fy(by);
    Interpolate1D<double,DComplex> value(fx, fy);
  
    value.setMethod(Interpolate1D<double,DComplex>::nearestNeighbour);
    double ev;
    for (float xs = -5.0000001; xs < 5; xs += .1){
      ev = max(min((int32_t) (xs+0.5),4),0);
      if (near((value(xs)).real(), ev) == false ||  
	  near((value(xs)).imag(), ev*ev) == false) {
	cout << "value(" << xs << ") = " << value(xs) << 
	  " is not near the expected value of (" <<
	  ev << ", " << ev*ev << ")" << endl;
	failed = true;
      }
    }
    if (failed){
      cout << "Failed "; anyFailures = true;
    }
    else
      cout << "Passed ";
    cout << "the Interpolate1D<double,DComplex> test with nearest neighbour"
	 << endl << "       "
	 << "                                   interpolation (using Blocks)"
	 << endl;

  }
  // Test the Interpolate1D class with float / float Array and spline
  // interpolation
  {
    bool failed = false;
    Vector<float> x(5); indgen(x); 
    IPosition shape(3, 3, 5, 1);
    Array<float>  y(shape); 
    IPosition xshape(3, 1, 5, 1);
    Array<float>  xa(xshape); indgen(xa);
    IPosition trc(3,0,4,0), blc(3,0,0,0), step(3,1,0,0);

    y(blc,trc) =  xa; trc += step; blc += step;
    y(blc,trc) =  xa*xa; trc += step; blc += step;
    y(blc,trc) =  xa*xa*xa;

    ScalarSampledFunctional<float> fx(x);
    ArraySampledFunctional<Array<float> > fy(y);
    Interpolate1D<float,Array<float> > value(fx,fy);
    value.setMethod(Interpolate1D<float,Array<float> >::spline);
    trc(0) = 2; trc(1) = 0; trc(2) = 0;
    blc(0) = 0; blc(1) = 0; blc(2) = 0;
    step(0) = 0; step(1) = 1; 
    
    Array<float> iv; 
    for (float xs = 0; xs < 5; xs += 1){
      iv = value(xs);
      if ((near(iv(IPosition(1, 0)), 
		y(IPosition(3, 0,(uint32_t) xs, 0))) == false) || 
	  (near(iv(IPosition(1, 1)), 
		y(IPosition(3, 1, (uint32_t) xs, 0))) == false) ||	  
 	  (near(iv(IPosition(1, 2)), 
 		y(IPosition(3, 2, (uint32_t) xs, 0))) == false)){
	cout << "value(" << xs << ")" << endl << iv 
	     << " is not near the expected value of " << endl
	     << y(blc, trc) << endl;
	failed = true;
      }
      trc += step; blc += step; 
    }
    iv = value((float) 5);
    if ((near(iv(IPosition(1, 0)), (float) 5) == false) ||
	(near(iv(IPosition(1, 1)), (float) 23) == false) ||
	(near(iv(IPosition(1, 2)), (float) 101) == false))
      failed = true;
    // Switch out of spline mode back to cubic interpolation
    value.setMethod(Interpolate1D<float, Array<float> >::cubic);
    if (value.getMethod() != Interpolate1D<float, Array<float> >::cubic) {
      failed = true;
      cout << "Could not change the interpolation method" << endl;
    }
    iv = value(float(-1));
    if (near(iv(IPosition(1,1)), float(1)) == false){
      failed = true;
      cout << "Did not really change the interpolation method" << endl;
    }
    if (failed){
      cout << "Failed "; anyFailures = true;
    }
    else
      cout << "Passed ";
    cout << "the Interpolate1D<float, Array<float> > test with "
	 << "spline interpolation" << endl;
  }

  // Now test the table system interface. 
  // This requires the construction of a table (vrtually done)
  {
    Vector<float> time (6);
    Vector<double> amp (6);

    for (uint32_t i=0; i < 6; i++) {
      time[i] = i;
      amp[i]  = i*i;
    }
    // Now I have constructed a table with two scalar and one array column
    // Test the Interpolate1D class with the scalar columns
    {
      bool failed = false;

      Vector<float> x(time);
      Vector<double> y(amp);
      ScalarSampledFunctional<float> fx(x);
      ScalarSampledFunctional<double> fy(y);
      Interpolate1D<float,double> value(fx, fy);
      value.setMethod(Interpolate1D<float,double>::cubic);

      for (float xs = -5; xs < 10; xs += .5)
	if (near(value(xs),(double) xs*xs) == false){
	  cout << "value(" << xs << ") = " << value(xs) <<
	    " which is not near the expected value of " << xs*xs << endl;
	  failed = true;
	}
      if (failed){
	cout << "Failed "; anyFailures = true;
      }
      else
	cout << "Passed ";
      cout << "the Interpolate1D<float,double> test with cubic interpolation" 
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
