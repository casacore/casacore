//# tInterpolate1D.cc: This program tests the Interpolate1D class
//# Copyright (C) 1996,1997,1998,1999,2000
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

#include <aips/aips.h>
#include <aips/Arrays/IPosition.h>
#include <aips/Arrays/Array.h>
#include <aips/Arrays/ArrayMath.h>
#include <aips/Arrays/Vector.h>
#include <aips/Containers/Block.h>
#include <aips/Mathematics/Math.h>
#include <aips/Mathematics/Complex.h>
#include <aips/Tables/TableDesc.h>
#include <aips/Tables/SetupNewTab.h>
#include <aips/Tables/Table.h>
#include <aips/Tables/ScaColDesc.h>
#include <aips/Tables/ScalarColumn.h>
#include <aips/Tables/ArrColDesc.h>
#include <aips/Tables/ArrayColumn.h>
#include <aips/Functionals/Interpolate1D.h>
#include <aips/Functionals/ScalarSampledFunctional.h>
#include <aips/Functionals/ArraySampledFunctional.h>
#include <iostream.h>


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

  // Test the Interpolate1D class with Float Vector / Complex Array and spline
  // interpolation 
  {
    Bool failed = False;
    Vector<Float> x(5); indgen(x); 
    IPosition shape(2, 2, 5);
    Array<Complex>  y(shape); 
    IPosition xshape(2, 1, 5);
    Array<Complex>  xa(xshape); indgen(xa);
    const Complex j(0., 1.);
    IPosition trc(2,0,4), blc(2,0,0), step(2,1,0);

    y(blc,trc) =  xa + j*xa*xa*xa*xa ; trc += step; blc += step;
    y(blc,trc) =  xa*xa + j*xa*xa*xa ; trc += step; blc += step;

    ScalarSampledFunctional<Float> fx(x);
    ArraySampledFunctional<Array<Complex> > fy(y);
//     Interpolate1D<Float,Array<Complex> > value(fx,fy);
//     value.setMethod(Interpolate1D<Float,Array<Complex> >::spline);
//     blc(0) = 0; blc(1) = 0;
//     trc(0) = 1; trc(1) = 0;
//     step(0) = 0; step(1) = 1; 
    
//     Array<Complex> iv; 
//     for (Float xs = 0; xs < 5; xs += 1){
//       iv = value(xs);
//       if ((near(iv(IPosition(1, 0)), 
//  		y(IPosition(2, 0,(uInt) xs))) == False) || 
//   	  (near(iv(IPosition(1, 1)), 
//   		y(IPosition(2, 1, (uInt) xs))) == False)){
// 	cout << "value(" << xs << ")" << endl << iv << endl
// 	     << "is not near the expected value of " << endl
// 	     << y(blc, trc) << endl;
// 	failed = True;
//       }
//       trc += step; blc += step; 
//     }
//     iv = value((Float) 5);
//     if ((near(iv(IPosition(1, 0)),  5+j*431) == False) ||
// 	(near(iv(IPosition(1, 1)), 23+j*101) == False))
//       failed = True;

//     if (failed){
//       cout << "Failed "; anyFailures = True;
//     }
//     else
//       cout << "Passed ";
//     cout << "the Interpolate1D<Float, Array<Complex> > test with "
//  	 << "spline interpolation" << endl;
  }
  // Now test the table system interface. 
  // This requires the construction of a table!
  {
    TableDesc td("A Test Table", "1", TableDesc::Scratch);
    td.comment() = 
      "Create a table to test the table interface of the interpolate1D class";
    td.addColumn(ScalarColumnDesc<Float> ("Time"));
    td.addColumn(ScalarColumnDesc<Double> ("Amplitude"));
    td.addColumn(ArrayColumnDesc<Complex> ("Visibility"));
    SetupNewTable newtab("newtab.data", td, Table::Scratch);
    Table tab(newtab);

    ScalarColumn<Float> time (tab, "Time");
    ScalarColumn<Double> amp (tab, "Amplitude");
    ArrayColumn<Complex> vis (tab, "Visibility");
    IPosition shape(2,2,2);
    Array<Complex> cph(shape);
    const Complex j(0., 1.);
    cph(IPosition(2,0,0)) = 0.1 + j * 0.2;
    cph(IPosition(2,1,0)) = 0.3 + j * 0.4;
    cph(IPosition(2,0,1)) = 0.5 + j * 0.6;
    cph(IPosition(2,1,1)) = 0.7 + j * 0.8;

    for (uInt i=0; i < 6; i++) {
      tab.addRow();
      time.put(i, i);
      amp.put(i, i*i);
      vis.put(i, cph);
      cph = cph + (1 + j*2);
    }
    // Now I have constructed a table with two scalar and one array column
    // Test the Interpolate1D class with the scalar columns
    {
      Bool failed = False;

      Vector<Float> x(time.getColumn());
      Vector<Double> y(amp.getColumn());
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
    // Test the Interpolate1D class with the array columns
    { 
      Bool failed = False;
      Vector<Float> x(time.getColumn());
      Array<Complex> y(vis.getColumn());
      ScalarSampledFunctional<Float> fx(x);
      ArraySampledFunctional<Array<Complex> > fy(y);
//       Interpolate1D<Float,Array<Complex> > value(fx,fy);
//       value.setMethod(Interpolate1D<Float,Array<Complex> >::linear);
      
//       Array<Complex> av; 
//       Complex sv00, sv01, sv10, sv11;
//       for (Float xs = -2.1; xs < 7; xs += .5){
//  	av = value(xs);
//   	sv00 = av(IPosition(2, 0, 0));
//   	sv10 = av(IPosition(2, 1, 0));
//   	sv01 = av(IPosition(2, 0, 1));
//   	sv11 = av(IPosition(2, 1, 1));
//  	if ((near(sv00.real(), xs+0.1f) == False) || 
//  	    (near(sv00.imag(), 2*xs+0.2f) == False) ||
//  	    (near(sv10.real(), xs+0.3f) == False) || 
//  	    (near(sv10.imag(), 2*xs+0.4f) == False) ||
//  	    (near(sv01.real(), xs+0.5f) == False) || 
//  	    (near(sv01.imag(), 2*xs+0.6f) == False) ||
//  	    (near(sv11.real(), xs+0.7f) == False) || 
//  	    (near(sv11.imag(), 2*xs+0.8f) == False)){
//  	  failed = True;
//  	  cout << "value(" << xs << "): " << endl
//  	       << av
//  	       << "is not the same as expected value of" << endl;
//  	  av(IPosition(2, 0, 0)) = Complex(xs+0.1, 2*xs+0.2);
//  	  av(IPosition(2, 1, 0)) = Complex(xs+0.3, 2*xs+0.4);
//  	  av(IPosition(2, 0, 1)) = Complex(xs+0.5, 2*xs+0.6);
//  	  av(IPosition(2, 1, 1)) = Complex(xs+0.7, 2*xs+0.8);
//  	  cout << av
//  	       << endl;
//  	}
//        }
//        if (failed){
//  	cout << "Failed "; anyFailures = True;
//        }
//        else
//  	cout << "Passed ";
//        cout << "the Interpolate1D<Float, Array<Complex> >test with "
//  	   << "linear interpolation" 
//  	   << endl << "                 "
//  	   << "                         using table array columns as inputs" 
//  	   << endl;
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
