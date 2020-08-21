//# tInterpolateArray1D: This program tests the InterpolateArray1D class
//# Copyright (C) 1994,1995,1996,1997,1998,1999,2000,2001
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
//# $Id: tInterpolateArray1D.cc 21510 2014-11-21 12:27:56Z gervandiepen $
//# Includes


// This test is not yet complete. Only the linear interpolation 
// with flagging is partially tested so far.

#include <casacore/casa/aips.h>
#include <casacore/scimath/Mathematics/InterpolateArray1D.h>
#include <casacore/casa/Arrays/Array.h>
#include <casacore/casa/Arrays/ArrayLogical.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/IO/ArrayIO.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/Arrays/IPosition.h>
#include <casacore/casa/BasicSL/Complex.h>
#include <casacore/casa/BasicSL/Constants.h>
#include <casacore/casa/BasicMath/Math.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/iostream.h>

#include <casacore/casa/namespace.h>


#define AlwaysTrue(x, y)      \
do {                          \
    ++tests_done;             \
    AlwaysAssert(x, y);       \
} while (0)

unsigned tests_done = 0;
const Bool debug = False;//True;


template <class T, class S>
class TestLinearInterpolation1
{
public:
  TestLinearInterpolation1() 
  {

    Array<S> a(IPosition(2,2,1000));
    Array<Bool> aflags(IPosition(2,2,1000));
    Array<S> expect(IPosition(2,2,1000));
    Array<Bool> expflags(IPosition(2,2,1000));
    
    Vector<T> ingrid(1000);
    
    for(uInt i=0; i<1000; i++){
      a(IPosition(2,0,i)) = Complex(1.,1.);
      a(IPosition(2,1,i)) = Complex(1.,1.);
      aflags(IPosition(2,0,i)) = False;
      aflags(IPosition(2,1,i)) = False;
      expect(IPosition(2,0,i)) = Complex(1.,1.);	
      expect(IPosition(2,1,i)) = Complex(1.,1.); 	 
      expflags(IPosition(2,0,i)) = False;
      expflags(IPosition(2,1,i)) = False;
      ingrid(i) = (T)i;
    }

    

    cout << "--- equidistant output grid ------------------------------------------------------------" << endl;
    cout << "--- change output grid width from identical to double width in steps of 1/iterations ---" << endl;

    Double iterations = 1000.; // change output grid width from identical to double width in steps of 1/iterations
    
    for (Int it = 0; it < (Int)iterations; it++) {

      Array<S> yout; 
      Array<Bool> youtFlags;
      Vector<T> xout(1000);
      Vector<T> xin; 
      Array<S> yin;
      Array<Bool> yinFlags;
      Int method =  InterpolateArray1D<T,S>::linear;
      Bool goodIsTrue=False;
      Bool extrapolate=False;

      xin.assign(ingrid);
      yin.assign(a);
      yinFlags.assign(aflags);

      for(uInt i=0; i<1000; i++){
	xout(i) = xin(i) + (Double)it * (i+1)/iterations;
      }

      InterpolateArray1D<T,S>::interpolate(yout, // the new visibilities
					   youtFlags, // the new flags
					   xout, // the new channel centers
					   xin, // the old channel centers
					   yin, // the old visibilities 
					   yinFlags,// the old flags
					   method, // the interpol method
					   goodIsTrue, // for flagging: good is not true
					   extrapolate // do not extrapolate
					   );

      for(uInt i=0; i<2; i++){
	for(uInt j=0; j<500; j++){
	  Double diffr = yout(IPosition(2,i,j)).real() - expect(IPosition(2,i,j)).real();
	  Double diffi = yout(IPosition(2,i,j)).imag() - expect(IPosition(2,i,j)).imag();
	  if(debug){
	    cout << it << " " << i << " " << j << " " << xin(j) << " " << xout(j) << " " << yout(IPosition(2,i,j)) << " " << expect(IPosition(2,i,j)) << endl;
	    cout << it << " flag " << i << " " << j << " " << youtFlags(IPosition(2,i,j)) << " " << expflags(IPosition(2,i,j)) << endl;
	  }
	  AlwaysAssert(fabs(diffr)<1E-8, AipsError);
	  AlwaysAssert(fabs(diffi)<1E-8, AipsError);
	  AlwaysAssert(youtFlags(IPosition(2,i,j)) == expflags(IPosition(2,i,j)), AipsError);
	}
      }


    } // end for it=0
    ++tests_done;             \

  }
};

template <class T, class S>
class TestLinearInterpolation2
{
public:
  TestLinearInterpolation2() 
  {

    Array<S> a(IPosition(2,2,1000));
    Array<Bool> aflags(IPosition(2,2,1000));
    Array<S> expect(IPosition(2,2,1000));
    Array<Bool> expflags(IPosition(2,2,1000));
    
    Vector<T> ingrid(1000);
    
    for(uInt i=0; i<1000; i++){
      a(IPosition(2,0,i)) = Complex((Float)i,(Float)i);
      a(IPosition(2,1,i)) = Complex((Float)i,(Float)i);
      aflags(IPosition(2,0,i)) = False;
      aflags(IPosition(2,1,i)) = False;
      expect(IPosition(2,0,i)) = Complex(1.,1.);	
      expect(IPosition(2,1,i)) = Complex(1.,1.); 	 
      expflags(IPosition(2,0,i)) = False;
      expflags(IPosition(2,1,i)) = False;
      ingrid(i) = (T)i;
    }

    cout << "--- equidistant output grid, increasing values -----------------------------------------" << endl;
    cout << "--- change output grid width from identical to double width in steps of 1/iterations ---" << endl;

    Double iterations = 1000.; // change output grid width from identical to double width in steps of 1/iterations
    
    for (Int it = 0; it < (Int)iterations; it++) {

      Array<S> yout; 
      Array<Bool> youtFlags;
      Vector<T> xout(1000);
      Vector<T> xin; 
      Array<S> yin;
      Array<Bool> yinFlags;
      Int method =  InterpolateArray1D<T,S>::linear;
      Bool goodIsTrue=False;
      Bool extrapolate=False;

      xin.assign(ingrid);
      yin.assign(a);
      yinFlags.assign(aflags);

      for(uInt i=0; i<1000; i++){
	xout(i) = xin(i) + (Double)it * (i+1)/iterations;
      }

      InterpolateArray1D<T,S>::interpolate(yout, // the new visibilities
					   youtFlags, // the new flags
					   xout, // the new channel centers
					   xin, // the old channel centers
					   yin, // the old visibilities 
					   yinFlags,// the old flags
					   method, // the interpol method
					   goodIsTrue, // for flagging: good is not true
					   extrapolate // do not extrapolate
					   );

      for(uInt i=0; i<2; i++){
	for(uInt j=0; j<500; j++){
	  Double diffr = yout(IPosition(2,i,j)).real() - xout(j) * expect(IPosition(2,i,j)).real();
	  Double diffi = yout(IPosition(2,i,j)).imag() - xout(j) * expect(IPosition(2,i,j)).imag();
	  if(debug){
	    cout << it << " " << i << " " << j << " " << xin(j) << " " << xout(j) << " " 
		 << yout(IPosition(2,i,j)) << " " <<  xout(j) * expect(IPosition(2,i,j)) << " diffs: " << diffr << " " << diffi << endl;
	    cout << it << " flag " << i << " " << j << " " << youtFlags(IPosition(2,i,j)) << " " << expflags(IPosition(2,i,j)) << endl;
	  }
	  AlwaysAssert(fabs(diffr)<(xout(j)+1.)*1E-7, AipsError);
	  AlwaysAssert(fabs(diffi)<(xout(j)+1.)*1E-7, AipsError);
	  AlwaysAssert(youtFlags(IPosition(2,i,j)) == expflags(IPosition(2,i,j)), AipsError);
	}
      }


    } // end for it=0
    ++tests_done;             \

  }
};


template <class T, class S>
class TestNearestInterpolation1
{
public:
  TestNearestInterpolation1() 
  {

    Int N(10);
    Vector<T> Xa(N,0.0), Xd(N,0.0);
    Vector<S> Ya(N,0.0), Yd(N,0.0);
    indgen(Xa);
    indgen(Ya);
    for (Int i=0;i<N;++i) {
      Xd[i]=Xa[N-1-i];
      Yd[i]=Ya[N-1-i];
    }

    Int n(90);
    Vector<T> x(n,0.0);
    Vector<S> ya(n,0.0), yd(n,0.0);
    indgen(x);
    x/=static_cast<T>(10.0);    // tenths

    Int method =  InterpolateArray1D<T,S>::nearestNeighbour;

    InterpolateArray1D<T,S>::interpolate(ya,x,Xa,Ya,method);  // Ascending abscissa
    InterpolateArray1D<T,S>::interpolate(yd,x,Xd,Yd,method);  // Descending abscissa

    Vector<S> yatest(n,0.0),ydtest(n,0.0);
    indgen(yatest);
    yatest/=static_cast<S>(10.0);
    yatest+=static_cast<S>(0.4999); // ensures middle value goes down (matches InterpolateArray1D)
    yatest=floor(yatest);

    indgen(ydtest);
    ydtest/=static_cast<S>(10.0);
    ydtest+=static_cast<S>(0.5001); // ensures middle value goes up (matches InterpolateArray1D)
    ydtest=floor(ydtest);

    AlwaysAssert(allEQ(ya,yatest),AipsError);
    AlwaysAssert(allEQ(yd,ydtest),AipsError);

    ++tests_done;

  }
};





template <class T, class S>
void run_tests()
{

    TestLinearInterpolation1<T, S> ();
    TestLinearInterpolation2<T, S> ();

    return;
}

template <class T, class S>
void run_nearest_tests()
{

    TestNearestInterpolation1<T, S> ();

    return;
}

int main()
{
  tests_done = 0;
  try {

      cout << "Testing 'linear' Float/Complex" << endl;
      run_tests<Float, Complex>();
      cout << "Testing 'linear' Double/Complex" << endl;
      run_tests<Double, Complex>();

      cout << "Testing 'nearestNeighbour' Float/Float, ascending/descending" << endl;
      run_nearest_tests<Float,Float>();
      cout << "Testing 'nearestNeighbour' Double/Float, ascending/descending" << endl;
      run_nearest_tests<Double,Float>();

  }
  catch (std::exception& x) {
    cerr << x.what() << endl;
    cout << "FAIL" << endl;
    return 1;
  } 
  cout << tests_done << " tests OK" << endl;
  return 0;
}
