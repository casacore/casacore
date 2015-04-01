//# tArray.cc: Test program for the Array class
//# Copyright (C) 1993,1994,1995,1996,1997,1998,1999,2000,2001,2002,2003
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

//# If AIPS_DEBUG is not set, the Assert's won't be called.
#if !defined(AIPS_DEBUG)
#define AIPS_DEBUG
#endif

//# For extra debugging
#if !defined(AIPS_ARRAY_INDEX_CHECK)
#define AIPS_ARRAY_INDEX_CHECK
#endif

#include <casacore/casa/iostream.h>

#include <casacore/casa/aips.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/casa/BasicSL/Complex.h>
#include <casacore/casa/BasicMath/Math.h>
#include <casacore/casa/Utilities/Assert.h>

#include <casacore/casa/Arrays/IPosition.h>
#include <casacore/casa/Arrays/Array.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/Arrays/ArrayLogical.h>
#include <casacore/casa/Arrays/ArrayIO.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Arrays/Matrix.h>
#include <casacore/casa/Arrays/Cube.h>
#include <casacore/casa/Arrays/Slice.h>
#include <casacore/casa/Arrays/Slicer.h>
#include <casacore/casa/Arrays/ArrayError.h>
#include <casacore/casa/BasicMath/Functional.h>


#include <casacore/casa/namespace.h>
//# Define a simple functional class doing the square.
class FuncSqr: public Functional<Float,Float>
{
public:
  Float operator()(const Float& x) const { return x*x; }
};


Int zero(Int)
{
    return 0;
}

Int minusone(const Int &)
{
    return -1;
}

void oldArrayTest()
{
    {   
	cout << "Testing multidimensional arrays......";
	IPosition shape(2);
	shape(0) = shape(1) = 5;
	Array<Int> x(shape);
	AlwaysAssertExit(x.ndim() == 2);
	AlwaysAssertExit(x.shape() == shape);
	x.set(-1);
	IPosition index(2);
	index = 2;
	x(index) = 6;
	AlwaysAssertExit(x(index) == 6);
	Array<Int> y(shape+1);
	y.resize(x.shape());
	y = x;
	AlwaysAssertExit(y.shape() == x.shape()); 
	y.resize(shape+3);
	AlwaysAssertExit( y.shape() == shape + 3); 
	Array<Int> y1(shape, 4);
	AlwaysAssertExit (allEQ(y1, 4));

	// Test Array slices
	IPosition i1(3), i2(3);
	i1 = 0; i2 = 3;
	Array<Int> a1(i2);
	a1 = 0;
	i2 = 1;
	a1(i1, i2) = 1;
	AlwaysAssertExit(allEQ (a1(i1, i2), 1));
	AlwaysAssertExit(a1(i1) == 1);
	AlwaysAssertExit(a1(i2) == 1);
	i2 = 2;
	AlwaysAssertExit(a1(i2) == 0);

        //*
        //*  Try to get the Array test coverage up quick and dirty
        //*

        a1.apply(zero);
        AlwaysAssertExit(allEQ (a1, 0));
        a1.apply(minusone);
        AlwaysAssertExit(allEQ (a1, -1));
	{
	    Vector<Float> vi(10);
	    indgen(vi);
	    FuncSqr pi;
	    vi.apply(pi);
	    for (uInt i=0; i < 10; i++) {
	        AlwaysAssertExit(vi(i) == i*i);
	    }
	}

        AlwaysAssertExit(allEQ (a1, a1.copy()));
        Array<Int> a2(a1.copy());
        a1 = a1;
        AlwaysAssertExit(allEQ (a1, a2));
        i2 = 0;
        a1.resize(i2);
        a1 = a2;
        AlwaysAssertExit(allEQ (a1, a2));
        i2.resize(1);
        i2 = 10;
        a2.resize(i2);
        a2 = 10;
        i2 = 0;
        a1.resize(i2);
	a1 = a2;
	AlwaysAssertExit(allEQ (a1, 10));

        i2 = 100;
        Array<Float> *a3 = new Array<Float>(i2);
        *a3 = 11.0;
	Array<Float> a4(a3->operator()(IPosition(a3->ndim(),0), 
					a3->shape()-1, IPosition(1,2)));
        delete a3;
        a4.unique();
	AlwaysAssertExit(allEQ (a4, 11.0F));

	Array<Float> ab1(IPosition(4,5,6,7,8));
	indgen(ab1);
	Array<Float> ab2 (ab1(IPosition(4,1,2,1,3), IPosition(4,2,2,5,7),
			     IPosition(4,1,1,2,3)).reform (IPosition(3,2,3,2)));
	for (uInt i=0; i<2; i++) {
	    for (uInt j=0; j<3; j++) {
		for (uInt k=0; k<2; k++) {
		    AlwaysAssertExit (&(ab2(IPosition(3,i,j,k)))
				  ==  &(ab1(IPosition(4,1+i,2,1+j*2,3+k*3))));
		}
	    }
	}

	{
	  Slicer sl(IPosition(4,1,2,1,3), IPosition(4,2,2,5,5),
		    IPosition(4,1,1,2,3), Slicer::endIsLast);
	  Array<Float> absl = ab1(sl);
	  AlwaysAssertExit (absl.shape() == IPosition(4,2,1,3,1));
	  for (uInt i=0; i<2; i++) {
	    for (uInt j=0; j<3; j++) {
	      for (uInt k=0; k<1; k++) {
		AlwaysAssertExit (&(absl(IPosition(4,i,0,j,k)))
				  ==  &(ab1(IPosition(4,1+i,2,1+j*2,3+k*3))));
	      }
	    }
	  }
	}
	{
	  Slicer sl(IPosition(4,1,2,1,3),
		    IPosition(4,2,2,Slicer::MimicSource,7),
		    IPosition(4,1,1,2,3), Slicer::endIsLast);
	  Array<Float> absl = ab1(sl);
	  AlwaysAssertExit (absl.shape() == IPosition(4,2,1,3,2));
	  for (uInt i=0; i<2; i++) {
	    for (uInt j=0; j<3; j++) {
	      for (uInt k=0; k<2; k++) {
		AlwaysAssertExit (&(absl(IPosition(4,i,0,j,k)))
				  ==  &(ab1(IPosition(4,1+i,2,1+j*2,3+k*3))));
	      }
	    }
	  }
	}

	// Test an empty array slice.
	{
	  Array<Int> a1(IPosition(3,2,3,4));
	  Array<Int> a2 = a1(IPosition(3,0,0,2), IPosition(3,0,0,1));
	  AlwaysAssertExit (a2.shape() == IPosition(3,1,1,0));
	  AlwaysAssertExit (a2.size() == 0);
	}

	cout << "OK\n";
    }
    
    {
	cout << "Testing Vectors......................";
 	Vector<Int> x(10);
	x = 5;     
	for (Int i=0; i < 10; i++)
	    AlwaysAssertExit(x(i) == 5);
	Vector<Int> y(x);
	AlwaysAssertExit(x.nrefs() == y.nrefs() && x.nrefs() > 1);
	for (Int i=0; i < 10; i++)
	    AlwaysAssertExit(y(i) == 5);
	Vector<Int> z(x.copy());
	z = 11;
	for (Int i=0; i < 10; i++)
	    AlwaysAssertExit(z(i) == 11 && x(i) == 5);
	x(Slice(0,5,2)) = z(Slice(0,5,2));
	for(Int i=0; i < 10; i += 2)
	    AlwaysAssertExit(x(i) == 11 && x(i+1) == 5);
	Vector<Int> zz; // default constructor
	AlwaysAssertExit(zz.nelements() == 0);
	AlwaysAssertExit(zz.size() == 0);
	AlwaysAssertExit(zz.empty());
	Vector<Int> zzz(x(Slice(0,5,2)));
	zzz.unique();
	AlwaysAssertExit(zzz.nrefs() == 1 && allEQ(zzz, 11) &&
			 zzz.nelements() == 5);
	AlwaysAssertExit(zzz.size() == 5);
	AlwaysAssertExit(!zzz.empty());
	Vector<Int> y1(5, 4);
	AlwaysAssertExit (allEQ(y1, 4));

	Vector<String> vs(5);
	vs(0) = "Every";vs(1) = "Good";vs(2) = "Boy";vs(3) = "Deserves";
	vs(4) = "Fudge";
	AlwaysAssertExit(vs(0) == "Every");
	AlwaysAssertExit(vs(1) == "Good");
	AlwaysAssertExit(vs(2) == "Boy");
	AlwaysAssertExit(vs(3) == "Deserves");
	AlwaysAssertExit(vs(4) == "Fudge");
	
	zzz.resize(10);
	zzz = 13;
	AlwaysAssertExit(zzz.nelements() == 10);
	Vector<Int> yyy(10);
	yyy = -13;
	zzz = yyy;
	AlwaysAssertExit(allEQ(zzz, yyy) &&
			 allEQ (zzz, -13));
	Block<Int> blk;
	zzz.toBlock(blk);
	Vector<Int>  aaa(blk);
	AlwaysAssertExit(allEQ (aaa, zzz) && 
			 allEQ (aaa, -13));
	cout << "OK\n";
    }

    {
	cout << "Testing math functions ..............";
	Vector<double> x(5), y, z;
	for (Int i = 0; i < 5; i++)
	    x(i) = double(i+1)/10.0;
	
	y = floor(x);
	for (Int i = 0; i < 5; i++)
	    AlwaysAssertExit(y(i) == 0.0);
	
	z = pow(x,1.0);
	AlwaysAssertExit(allNear (z, x, 1.0e-10));
	z = pow(x,2.0);
	AlwaysAssertExit(allNear (z, x*x, 1.0e-10));
	z = pow(x,y);
	for (Int i = 0; i < 5; i++)
	    AlwaysAssertExit(z(i) == 1.0);
	
	AlwaysAssertExit(min(z) == 1.0 && max(z) == 1.0);
	z(4) = -1.0; z(3) = 22.0;
	AlwaysAssertExit(min(z) == -1.0 && max(z) 
			 == 22.0);

	Vector<Int> vi1(5);
	indgen(vi1);
	Vector<Int> vi2(5), vi3;
	vi2 = 0;
	vi2(3) = -3;
	vi3 = casacore::min(vi1, vi2);
	AlwaysAssertExit(vi3(0) == 0 && vi3(1) == 0 && vi3(2) == 0 && vi3(3) == -3 &&
		vi3(4) == 0);
	vi2(3) = 9;
	vi3 = max(-vi1, vi2);
	AlwaysAssertExit(vi3(0) == 0 && vi3(1) == 0 && vi3(2) == 0 && vi3(3) == 9 &&
		vi3(4) == 0);


        {

            Matrix<Float> a(10u,3u);
            Matrix<Bool> mask(10u,3u);

            Float val;
            for (Int j=0; j<3; j++) {
                for (Int i=0; i<10; i++) {
                    a(i, j) = sin( (10*j + i) * 0.6);
                    val = a(i, j);
                    mask(i, j)= (Bool)((val > 0) && (val < 0.5));
                }
            }
            for (Int i=0; i<10; i++) {
                mask(i, 0)= False;
            }

            Float min,max;
            IPosition minPos(2),maxPos(2);


            minMax (min, max, minPos, maxPos, a);

            AlwaysAssertExit(minPos == IPosition(2, 8, 0));
            AlwaysAssertExit(maxPos == IPosition(2, 3, 1));
            AlwaysAssertExit(min == a(minPos));
            AlwaysAssertExit(max == a(maxPos));

            minMax(min, max,a);
            AlwaysAssertExit(min == a(minPos));
            AlwaysAssertExit(max == a(maxPos));

            minMax(min, max, a);
            AlwaysAssertExit(min == a(minPos));
            AlwaysAssertExit(max == a(maxPos));


            minMax (min, max, minPos, maxPos, a, mask);

            AlwaysAssertExit(minPos == IPosition(2, 1, 2));
            AlwaysAssertExit(maxPos == IPosition(2, 5, 1));
            AlwaysAssertExit(min == a(minPos));
            AlwaysAssertExit(max == a(maxPos));

        }


	cout << "OK\n";
    }
 
    {
	cout << "Testing Vector math and logicals.....";
	Vector<Int> x(5), y(5);
	x = 1; y = 2;

	// Not all, but a lot
        AlwaysAssertExit(allEQ (x, 1) && allEQ (y, 2) &&
		 allLE (x, y) &&  allGE(y, x) &&
		 allNE(x, 2) && allNE (x, y) && allEQ (x,
		 x) && allLT (x, y) && allGT (y, x) );

	x += 1;
	AlwaysAssertExit(allEQ (x, y));
	x += y;
	AlwaysAssertExit(allEQ (x, 2*y));
	x = -1 * x;
	AlwaysAssertExit(allEQ (x, -4));
	indgen(x); indgen(y,1);
	AlwaysAssertExit(allEQ (x - y, -1));
	for (Int i = 0; i < 5; i++)
	    AlwaysAssertExit(x(i) == i);

	AlwaysAssertExit(sum(x) == 10);
	AlwaysAssertExit(product(y) == 120);
	AlwaysAssertExit(mean(y) == median(y) && mean(y) == 3);

        AlwaysAssertExit(anyLE(x, y) && anyLE(x, y-1) &&
			 !anyLE(x, y-2));
        AlwaysAssertExit(anyLT(x, y) && !anyLT(x, y-1) &&
			 !anyLT (x, y-2));
        AlwaysAssertExit(!anyGE(x, y) && anyGE (x, y-1) &&
			 anyGE (x, y-2));
        AlwaysAssertExit(!anyGT(x, y) && !anyGT (x, y-1) &&
			 anyGT (x, y-2));
        AlwaysAssertExit(!anyEQ(x, y) && anyEQ(x, y-1) &&
			 !anyEQ(x, y-2));
        AlwaysAssertExit(anyNE(x, y) && !anyNE (x, y-1) &&
			 anyNE(x, y-2));

        AlwaysAssertExit(anyLE(x,1) && anyLE(x,0)&& !anyLE(x,-1));
        AlwaysAssertExit(anyLT(x,1) && !anyLT(x,0)&&!anyLT(x,-1));
        AlwaysAssertExit(anyGE(x,3)&&anyGE(x,4) && !anyGE(x, 5));
        AlwaysAssertExit(anyGT(x,3) && !anyGT(x,4)&&!anyGT(x, 5));
        AlwaysAssertExit(anyEQ(x,3) && anyEQ(x,4) && !anyEQ(x,5));
        AlwaysAssertExit(anyNE(x,3) && anyNE(x,4) && anyNE(x, 5));

        AlwaysAssertExit(anyLE(3,x) && anyLE(4,x) && !anyLE(5,x));
        AlwaysAssertExit(anyLT(3,x) && !anyLT(4,x) &&!anyLT(5,x));
        AlwaysAssertExit(!anyGE(-1,x) && anyGE(0,x) &&anyGE(1,x));
        AlwaysAssertExit(!anyGT(-1,x) && !anyGT(0,x)&&anyGT(1,x));
        AlwaysAssertExit(!anyEQ(-1,x)&& anyEQ(0,x) && anyEQ(1,x));
        AlwaysAssertExit(anyNE(-1,x) && anyNE(0,x) &&anyNE (1,x));

	Vector<Double> vd(5);
	indgen(vd,1.0);
	AlwaysAssertExit(fabs(variance(vd) - 2.5) < 0.0001);
	AlwaysAssertExit(fabs(stddev(vd) - sqrt(2.5)) < 0.0001);
	AlwaysAssertExit(fabs(avdev(vd) - 1.2) < 0.0001);
        {
            Vector<Complex> vc(2);
            Vector<Float> vr;
            vc(0) = Complex(1.0, 2.0);
            vc(1) = Complex(0.0, 1.0);
            vr = real(vc);
            AlwaysAssertExit(vr(1) == 0.0f);
            vr = imag(vc);
            AlwaysAssertExit(vr(0) == 2.0f);
            float pi2 = 3.1415927/2.0;
            vr = phase(vc);
            float pi2out = vr(1);
            AlwaysAssertExit(fabs(pi2 - pi2out) < 0.00001);
            vr = amplitude(vc);
            AlwaysAssertExit(vr(1) == 1.0f);
	}
	{
	    Vector<Int> vf(3);
	    vf = -3;
	    AlwaysAssertExit(allEQ (square(vf), 9));
	    AlwaysAssertExit(allEQ (cube(vf), -27));
	}

	{
	    Vector<Float> x1(2), x2(2);
	    x1 = 10000;
	    x2(0) = 10001; x2(1) = 10002;
	    // Array,Array
	    AlwaysAssertExit(allNear(x1, x2, 2.01e-4));
	    AlwaysAssertExit(!allNear(x1, x2, 1.01e-4));
	    AlwaysAssertExit(anyNear(x1, x2, 1.01e-4));
	    AlwaysAssertExit(!anyNear(x1, x2, 0.99e-4));
	    AlwaysAssertExit(allNearAbs(x1, x2, 2.01));
	    AlwaysAssertExit(!allNearAbs(x1, x2, 1.01));
	    AlwaysAssertExit(anyNearAbs(x1, x2, 1.01));
	    AlwaysAssertExit(!anyNearAbs(x1, x2, 0.99));

	    // Constant,Array
	    AlwaysAssertExit(allNear(10000.0f, x2, 2.01e-4));
	    AlwaysAssertExit(!allNear(10000.0f, x2, 1.01e-4));
	    AlwaysAssertExit(anyNear(10000.0f, x2, 1.01e-4));
	    AlwaysAssertExit(!anyNear(10000.0f, x2, 0.99e-4));
	    AlwaysAssertExit(allNearAbs(10000.0f, x2, 2.01));
	    AlwaysAssertExit(!allNearAbs(10000.0f, x2, 1.01));
	    AlwaysAssertExit(anyNearAbs(10000.0f, x2, 1.01));
	    AlwaysAssertExit(!anyNearAbs(10000.0f, x2, 0.99));

	    // Array,Constant
	    AlwaysAssertExit(allNear(x1, 10002.0f, 2.01e-4));
	    AlwaysAssertExit(!allNear(x1, 10002.0f, 1.01e-4));
	    AlwaysAssertExit(!anyNear(x1, 10002.0f, 1.01e-4));
	    AlwaysAssertExit(!anyNear(x1, 10002.0f, 0.99e-4));
	    AlwaysAssertExit(allNearAbs(x1, 10002.0f, 2.01));
	    AlwaysAssertExit(!allNearAbs(x1, 10002.0f, 1.01));
	    AlwaysAssertExit(anyNearAbs(x1, 10001.0f, 1.01));
	    AlwaysAssertExit(!anyNearAbs(x1, 10002.0f, 0.99));
	}

	cout << "OK\n";
    }
 
    {
        cout << "Testing median and fractile..........";
	Vector<Float> x1(5), x2(10), x3(100), x4(101), x5(101);
	indgen(x1);
	indgen(x2);
	indgen(x3);
	indgen(x4);
	indgen(x5);
	AlwaysAssertExit (median(x1) == 2.);
	AlwaysAssertExit (median(x1, True) == 2.);
	AlwaysAssertExit (median(x1, True, False, False) == 2.);
	AlwaysAssertExit (median(x2) == 4.5);
	AlwaysAssertExit (median(x2, True) == 4.5);
	AlwaysAssertExit (median(x2, True, False, False) == 4.);
	AlwaysAssertExit (median(x3) == 49.5);
	AlwaysAssertExit (median(x3, True) == 49.5);
	AlwaysAssertExit (medianInPlace(x3) == 49.5);
	AlwaysAssertExit (median(x3, True, False, True) == 49.);
	AlwaysAssertExit (median(x4) == 50.);
	AlwaysAssertExit (median(x4, True) == 50.);
	AlwaysAssertExit (median(x4, True, False, False) == 50.);

        cout << endl<<madfm(x1)<<endl;
	AlwaysAssertExit (madfm(x1) == 1.);
	AlwaysAssertExit (madfm(x1, True) == 1.);
	AlwaysAssertExit (madfm(x1, True, False, False) == 1.);
	AlwaysAssertExit (madfm(x1(Slice(0,2,2))) == 1.);
	AlwaysAssertExit (madfm(x4) == 25.);
	// Make sure x4 is not sorted itself.
	AlwaysAssertExit (allEQ (x4, x5));

	AlwaysAssertExit (fractile(x1, 0.0) == 0.);
	AlwaysAssertExit (fractile(x1, 0.25) == 1.);
	AlwaysAssertExit (fractile(x1, 0.5) == 2.);
	AlwaysAssertExit (fractile(x1, 0.75) == 3.);
	AlwaysAssertExit (fractile(x1, 1.0) == 4.);
	AlwaysAssertExit (fractile(x1, 0.75, True) == 3.);
	AlwaysAssertExit (fractile(x2, 0.5) == 4.);
	AlwaysAssertExit (fractile(x3, 0.5, False, True) == 49.);
	AlwaysAssertExit (fractile(x4, 0.0) == 0.);
	AlwaysAssertExit (fractile(x4, 0.5) == 50.);
	AlwaysAssertExit (fractile(x4, 0.05) == 5.);
	AlwaysAssertExit (fractile(x4, 0.951) == 95.);

        AlwaysAssertExit (interQuartileRange(x1) == 2.);
        AlwaysAssertExit (interFractileRange(x1, 0.25) == 2.);
        AlwaysAssertExit (interHexileRange(x4) ==
                          fractile(x4, 5./6.) - fractile(x4, 1./6.));
	// Make sure x4 is not sorted itself.
	AlwaysAssertExit (allEQ (x4, x5));
	cout << "OK\n";
    }

    {
	cout << "Simple matrix tests..................";
	Matrix<Int> a(5u,5u), b;
	a = 3;
	AlwaysAssertExit(a.nrow() == a.ncolumn() && a.nrow() == 5);
	AlwaysAssertExit(allEQ(a, 3));
	AlwaysAssertExit(allLE (a, 3)); 
	AlwaysAssertExit(allEQ (a, a)); 
	AlwaysAssertExit(allNE (a, 1));
	b = 2*a;
	Matrix<Int> c =  a(Slice(2,1),Slice(3,1));
	AlwaysAssertExit(allEQ (b, 6));
	a.row(3) = 6;
	AlwaysAssertExit(allEQ (a.row(3), 6));
	a.column(3) = 1;
	AlwaysAssertExit(allEQ (a.column(3), 1));
	a.diagonal(-1) = 7;
	AlwaysAssertExit(allEQ (a.diagonal(-1), 7));
	
	IPosition l(1);
	l(0) = a.nelements();
	Vector<Int> d(a.reform(l));
	for (Int i = 0; i < 5; i++)
	    for (Int j = 0; j < 5; j++)
		AlwaysAssertExit(a(i,j) == d(i + j*5));
	Matrix<Int> y1(5u,6u, 4u);
	AlwaysAssertExit (allEQ(y1, 4));
	
	
	Vector<Int> v(10);
	indgen(v);
	Matrix<Int> vm(v);
	AlwaysAssertExit(vm.ndim() == 2 && vm.nelements() == v.nelements());
	for (Int i = 0; i < Int(v.nelements()); i++)
	    AlwaysAssertExit(vm(i,0) == v(i) && v(i) == i);
	
	cout << "OK\n";
    }
 
    {
	cout << "Cube tests...........................";
	Cube<Int> c(3,3,3);
	c = 3;
	for (Int k=0; k <= 2; k++)
	    for (Int j=0; j <= 2; j++)
		for (Int i=0; i <= 2; i++)
		    AlwaysAssertExit(c(i,j,k) == 3);
	
	for (Int k=0; k <= 2; k++)
	    AlwaysAssertExit(allEQ (c.xyPlane(k), 3));
	
	// Check copy ctor
	Cube<Int> c2(c);
	c(1,1,1) = -3;
	AlwaysAssertExit(c2(1,1,1) == -3);
	
	// Check assignment
	Cube<Int> c3;
	c3 = c;
	AlwaysAssertExit(allEQ (c3, c));
	Cube<Int> y1(5,6,7, 4);
	AlwaysAssertExit (allEQ(y1, 4));
	
	// slice
	AlwaysAssertExit(allEQ (c3 (Slice(0,2), Slice(1,2), 1),
                       c (Slice(0,2), Slice(1,2), 1)));
	Cube<Int> c4(c3(Slice(0,2),Slice(1,2),1));
	IPosition c4shape(c4.Array<Int>::shape());
	AlwaysAssertExit(c4.nelements() == 4 && c4shape(2) == 1);
	IPosition blc(3), trc(3);
	// middle plane
	blc(0) = 0; blc(1) = 0; blc(2) = 1;
	trc(0) = 2; trc(1) = 2; trc(2) = 1;
        c(blc,trc) = 11;
	AlwaysAssertExit(allEQ (c.xyPlane(1), 11));
	AlwaysAssertExit(allEQ (c.xyPlane(0), 3));
	AlwaysAssertExit(allEQ (c.xyPlane(2), 3));

        // Check index operator.
        Array<Int> cinx (c[1]);
        AlwaysAssertExit (allEQ (cinx, c.xyPlane(1)));
        cinx.reference (cinx[0]);
        AlwaysAssertExit (cinx.shape() == c.shape().getFirst(1));
        cinx.reference (cinx[0]);
        AlwaysAssertExit (cinx.shape() == IPosition(1,1));
        cinx.reference (cinx[0]);
        AlwaysAssertExit (cinx.shape() == IPosition(1,1));
        AlwaysAssertExit (allEQ (cinx, 11));
	
	cout << "OK\n";
    }

    {
	cout << "Raw pointer test.....................";
	Matrix<Int> m(8u,8u);
	m = -1;
	Bool deleteIt;
	Int *storage;
	storage = m.getStorage(deleteIt);
	AlwaysAssertExit(deleteIt == False);
	for (uInt i = 0; i < m.nelements(); i++)
	    storage[i] = +1;
	AlwaysAssertExit(allEQ (m, 1));
	m.putStorage(storage, deleteIt);
	storage = m(Slice(0,2,3), Slice(2,2,4)).getStorage(deleteIt);
	AlwaysAssertExit(deleteIt == True);
	for (Int i=0; i < 4; i++)
	    storage[i] = 0;
	AlwaysAssertExit(m(0,2) == 1 && m(0,6) == 1 && m(3,2) == 1 && m(3,6) == 1);
	m(Slice(0,2,3), Slice(2,2,4)).putStorage(storage,deleteIt);
	AlwaysAssertExit(m(0,2) == 0 && m(0,6) == 0 && m(3,2) == 0 && m(3,6) == 0);
	cout << "OK\n";
    }
 
}

void testVector()
{
  // Test the Vector copy ctor for arrays with !1 dimension.
  {
    Array<Int> arr;
    Array<Int> arr2(arr);
    AlwaysAssertExit (arr2.ndim()==0  &&  arr2.nelements()==0);
    AlwaysAssertExit (arr2.shape() == IPosition());
    Vector<Int> vec(arr);
    AlwaysAssertExit (vec.ndim()==1  &&  vec.nelements()==0);
    AlwaysAssertExit (vec.shape() == IPosition(1,0));
    Matrix<Int> mat(arr);
    AlwaysAssertExit (mat.ndim()==2  &&  mat.nelements()==0);
    AlwaysAssertExit (mat.shape() == IPosition(2,0));
    Cube<Int> cub(arr);
    AlwaysAssertExit (cub.ndim()==3  &&  cub.nelements()==0);
    AlwaysAssertExit (cub.shape() == IPosition(3,0));
  }
  // Test use of a Vector from an Array with > 1 dimensions.
  {
    IPosition shape(4,20,21,22,23);
    Array<Int> arr(shape);
    indgen(arr);
    Array<Int> arr2;
    {
      arr2.reference(arr(IPosition(4,0), IPosition(4,shape(0)-1,0,0,0)));
      Vector<Int> vec(arr2);
      AlwaysAssertExit (vec.ndim()==1  &&  vec.nelements()==uInt(shape(0)));
      AlwaysAssertExit (vec.shape() == IPosition(1,shape(0)));
      AlwaysAssertExit (vec.contiguousStorage());
      for (uInt i=0; i<vec.size(); ++i) {
	AlwaysAssertExit (vec(i) == arr(IPosition(4,i,0,0,0)));
	AlwaysAssertExit (vec(i) == arr2(IPosition(4,i,0,0,0)));
      }
    }
    {
      // Test it for a slice.
      arr2.reference(arr(IPosition(4,0,9,9,9), IPosition(4,shape(0)-1,9,9,9)));
      Vector<Int> vec(arr2);
      AlwaysAssertExit (vec.ndim()==1  &&  vec.nelements()==uInt(shape(0)));
      AlwaysAssertExit (vec.shape() == IPosition(1,shape(0)));
      AlwaysAssertExit (vec.contiguousStorage());
      for (uInt i=0; i<vec.size(); ++i) {
	AlwaysAssertExit (vec(i) == arr(IPosition(4,i,9,9,9)));
	AlwaysAssertExit (vec(i) == arr2(IPosition(4,i,0,0,0)));
      }
    }
    {
      // Test it for a single element.
      arr2.reference(arr(IPosition(4,9,9,9,9), IPosition(4,9,9,9,9)));
      Vector<Int> vec(arr2);
      AlwaysAssertExit (vec.ndim()==1  &&  vec.nelements()==1);
      AlwaysAssertExit (vec.shape() == IPosition(1,1));
      AlwaysAssertExit (vec.contiguousStorage());
      AlwaysAssertExit (vec(0) == arr(IPosition(4,9,9,9,9)));
      AlwaysAssertExit (vec(0) == arr2(IPosition(4,0,0,0,0)));
    }
    {
      // Take a part of the original array.
      arr2.reference(arr(IPosition(4,9,0,9,9), IPosition(4,9,shape(1)-1,9,9)));
      Vector<Int> vec(arr2);
      AlwaysAssertExit (vec.ndim()==1  &&  vec.nelements()==uInt(shape(1)));
      AlwaysAssertExit (vec.shape() == IPosition(1,shape(1)));
      AlwaysAssertExit (!vec.contiguousStorage());
      for (uInt i=0; i<vec.size(); ++i) {
	AlwaysAssertExit (vec(i) == arr(IPosition(4,9,i,9,9)));
	AlwaysAssertExit (vec(i) == arr2(IPosition(4,0,i,0,0)));
      }
    }
    {
      Array<Int> arr3(arr(IPosition(4,1,2,3,4),
			  IPosition(4,19,18,20,22),
			  IPosition(4,2)));
      IPosition shp3 = arr3.shape();
      arr2.reference(arr3(IPosition(4,1,3,1,2),
			  IPosition(4,1,3,shp3(2)-1,2),
			  IPosition(4,1,1,2,1)));
      // Note that elements 1..shp3(2)-1 with step 2 gives shp3(2)/2 elements.
      Vector<Int> vec(arr2);
      AlwaysAssertExit (vec.ndim()==1  &&  vec.size()==uInt(shp3(2))/2);
      AlwaysAssertExit (vec.shape() == IPosition(1,shp3(2)/2));
      AlwaysAssertExit (!vec.contiguousStorage());
      for (uInt i=0; i<vec.size(); ++i) {
	AlwaysAssertExit (vec(i) == arr3(IPosition(4,1,3,1+2*i,2)));
	AlwaysAssertExit (vec(i) == arr2(IPosition(4,0,0,i,0)));
	AlwaysAssertExit (&(vec(i)) == &(arr2(IPosition(4,0,0,i,0))));
	AlwaysAssertExit (&(vec(i)) == &(arr(IPosition(4,3,8,5+4*i,8))));
      }
    }
  }
}

void seeIfWeMakeMemoryLeak()
{
    IPosition ip(5);
    ip = 5;
    Array<Int> ai(ip);
    throw(ArrayError("Just trying to check memory leak"));
}

void testResizeCopy()
{
  cout << "Testing resize with copy" << endl;
  Array<Int> arr1(IPosition(3,4,5,6));
  indgen (arr1);
  Array<Int> arr2;
  arr2 = arr1;
  arr1.resize (IPosition(3,4,5,8), True);
  AlwaysAssertExit (allEQ (arr2, arr1(IPosition(3,0), IPosition(3,3,4,5))));
  arr1.resize (IPosition(3,6,4,2), True);
  AlwaysAssertExit (allEQ (arr2(IPosition(3,0), IPosition(3,3,3,1)),
			   arr1(IPosition(3,0), IPosition(3,3,3,1))));
  arr1.resize();
  arr1 = arr2;
  arr1.resize (IPosition(2,6,4), True);
  Array<Int> arr1ca = arr1.reform(IPosition(3,6,4,1));
  AlwaysAssertExit (allEQ (arr2(IPosition(3,0), IPosition(3,3,3,0)),
			   arr1ca(IPosition(3,0), IPosition(3,3,3,0))));
  arr1.resize (IPosition(4,8,3,2,4), True);
  Array<Int> arr1cb = arr1.reform(IPosition(3,8,3,8));
  AlwaysAssertExit (allEQ (arr2(IPosition(3,0), IPosition(3,3,2,0)),
			   arr1cb(IPosition(3,0), IPosition(3,3,2,0))));
}


void checkRCDVec (const Vector<Int>& v1, const Vector<Int>& v2)
{
  AlwaysAssertExit (allEQ(v1,v2));
  Array<Int>::const_iterator iter1 = v1.begin();
  Array<Int>::const_iterator iter2 = v2.begin();
  for (uInt i=0; i<v1.size(); ++i, ++iter1, ++iter2) {
    AlwaysAssertExit (v1[i] == v2[i]);
    AlwaysAssertExit (iter1 != v1.end());
    AlwaysAssertExit (iter2 != v2.end());
    AlwaysAssertExit (v1[i] == *iter1);
    AlwaysAssertExit (v2[i] == *iter2);
  }
  AlwaysAssertExit (iter1 == v1.end());
  AlwaysAssertExit (iter2 == v2.end());
}

void checkRCD (const Vector<Int>& vn, const Vector<Int>& vc)
{
  Slice sl(1,3,2);  // start=1,n=3,inc=2
  cout << "  check vn,vc" << endl;
  checkRCDVec (vn, vc);
  cout << "  check vn(sl),vc(sl)" << endl;
  checkRCDVec (vn(sl), vc(sl));
  cout << "  check vn(sei),vn(sl)" << endl;
  checkRCDVec (vn(IPosition(1,1), IPosition(1,5), IPosition(1,2)), vn(sl));
  cout << "  check vc(sei),vc(sl)" << endl;
  checkRCDVec (vc(IPosition(1,1), IPosition(1,5), IPosition(1,2)), vc(sl));
}

void doRowColDiag (const Matrix<Int>& m)
{
  // Make contiguous copy of matrix.
  Matrix<Int> cm(m.copy());
  AlwaysAssertExit (cm.contiguousStorage());
  // Check row selection and subsetting.
  Vector<Int> r0(m.row(1));
  Vector<Int> cr0(cm.row(1));
  AlwaysAssertExit (!r0.contiguousStorage() && !cr0.contiguousStorage());
  checkRCD (r0, cr0);
  // Check column selection and subsetting.
  Vector<Int> c0(m.column(1));
  Vector<Int> cc0(cm.column(1));
  AlwaysAssertExit (cc0.contiguousStorage());
  checkRCD (c0, cc0);
  // Check diagonal selection and subsetting.
  Vector<Int> d0(m.diagonal());
  Vector<Int> cd0(cm.diagonal());
  AlwaysAssertExit (!d0.contiguousStorage() && !cd0.contiguousStorage());
  checkRCD (d0, cd0);
}

void testRowColDiag()
{
  Matrix<Int> m(18,18);
  indgen (m);
  cout << "Testing contiguous matrix ..." << endl;
  doRowColDiag (m);
  cout << "Testing non-contiguous matrix ..." << endl;
  doRowColDiag (m(IPosition(2,1,1), IPosition(2,12,12), IPosition(2,1,1)));
  doRowColDiag (m(IPosition(2,1,1), IPosition(2,12,12), IPosition(2,2,2)));
  doRowColDiag (m(IPosition(2,1,2), IPosition(2,17,12), IPosition(2,3,2)));
}


int main()
{
    try {
	oldArrayTest();                          // Used to be ArrayTest.cc
 	{
	    Int i;

	    Array<Int> ai1;                      // Array<T>()
	    AlwaysAssertExit(ai1.ndim() == 0);             // ndim()
	    AlwaysAssertExit(ai1.nelements() == 0);        // nelements()
	    
	    IPosition ip1(5,1,2,3,4,5);
	    Array<Int> ai2(ip1);                 // Array<T>(IPosition)
	    AlwaysAssertExit(ai2.ndim() == 5);
	    AlwaysAssertExit(ai2.nelements() == 120);
	    AlwaysAssertExit(ai2.shape() == ip1);

	    Bool caught;
	    for (i=0; i<10; i++) {               // cleanup() - should
		caught = False;                  // check for leaks
		try {
		    seeIfWeMakeMemoryLeak();
		} catch (ArrayError x) {
		    caught = True;
		} 
		AlwaysAssertExit(caught);
	    }
	    
	    Array<Int> ai3(ip1);
	    IPosition ip3(1,11);
	    Array<Int> ai4(ip3);
	    ai4.set(10);                         // set(T);
	    IPosition ip5(1);
	    for(i=0; i <11; i++) {
		ip5(0) = i;
		AlwaysAssertExit(ai4(ip5) == 10);          // T operator()(IPosition)
	    }

	    ai3.reference(ai4);                  // reference()
	    AlwaysAssertExit(ai4.nrefs() == 2 && ai3.nrefs() == 2);
	    AlwaysAssertExit(ai3.ndim() == 1 && ai3.shape() == 11);
	    ip5(0) = 0;
	    AlwaysAssertExit(&ai3(ip5) == &ai4(ip5));
            // Eventually should carry on with all member functions. Still,
            // The test coverage isn't terrible.
	}

	{
	  // Tests of the pointer->Array functions
	  Int *ip = new Int[100];
	  IPosition shape(2, 5, 20);
	  Array<Int> ai(shape, ip, SHARE);
	  indgen(ai);
	  for (Int i=0; i < 100; i++) {
	    AlwaysAssertExit(ip[i] == i);
	  }
	  Array<Int> ai2(shape, ip, COPY);
	  AlwaysAssertExit(allEQ(ai2, ai));
	  ai2 = 11;
	  AlwaysAssertExit(ip[0] == 0 && ip[99] == 99 && 
			   ai(IPosition(2,4,19)) == 99 && allEQ(ai2, 11));
	  Vector<Int> vi(IPosition(1, 100), ip, SHARE);
	  Matrix<Int> mi(IPosition(2, 10, 10), ip, SHARE);
	  Cube<Int> ci(IPosition(3, 4, 5, 5), ip, SHARE);
	  vi(99) = 66;
	  AlwaysAssertExit(vi(99) == 66 && mi(9,9) == 66 && ci(3,4,4) == 66 &&
			   ai(IPosition(2,4,19)) == 66);
	  delete [] ip;
	}

	{
	  // Test the nonDegenerate() function
	  Array<Int> a1(IPosition(5,1,2,1,3,1));
	  indgen(a1);
  	  AlwaysAssertExit(a1.nonDegenerate().shape() == IPosition(2,2,3));
 	  AlwaysAssertExit(a1.nonDegenerate(1).shape() == IPosition(3,1,2,3));
 	  Cube<Int> c = a1.nonDegenerate(1);
 	  AlwaysAssertExit(c(0,1,2) == 5);
 	  c(0,1,2) = 99;
 	  AlwaysAssertExit(a1(IPosition(5, 0, 1, 0, 2, 0)) == 99);
 	  AlwaysAssertExit(a1.nonDegenerate(4).shape() == IPosition(4,1,2,1,3));
 	  Array<Int> a2(IPosition(3,1,1,1));
  	  AlwaysAssertExit(a2.nonDegenerate().shape() == IPosition(1,1));

	  const Array<Int> a3(a1);
	  AlwaysAssertExit(a3.nonDegenerate().shape() == IPosition(2,2,3));
	  AlwaysAssertExit(a3.nonDegenerate(1).shape() == IPosition(3,1,2,3));
	  AlwaysAssertExit(a3.nonDegenerate()(IPosition(2,0,2)) == 4);
	  AlwaysAssertExit(a3.nonDegenerate()(IPosition(2,1,2)) == 99);

	  Array<Int> a4;
	  a4.nonDegenerate(a1);
	  AlwaysAssertExit(a4.shape() == IPosition(2,2,3));
	  AlwaysAssertExit(a4(IPosition(2,0,2)) == 4);
	  AlwaysAssertExit(a4(IPosition(2,1,2)) == 99);
	  a4.nonDegenerate(a1, 1);
	  AlwaysAssertExit(a4.shape() == IPosition(3,1,2,3));
	  AlwaysAssertExit(a4(IPosition(3,0,0,0)) == 0);
	  AlwaysAssertExit(a4(IPosition(3,0,1,2)) == 99);
 	}
  	{
 	  // Test the addDegenerate() function
 	  Array<Int> a1(IPosition(2,10,10));
 	  indgen(a1);
 	  AlwaysAssertExit(a1.addDegenerate(1u).shape()==IPosition(3,10,10,1));

 	  Matrix<Int> m = a1(IPosition(2,1),IPosition(2,3),IPosition(2,2));
 	  AlwaysAssertExit(m(0,0) == 11);
 	  AlwaysAssertExit(m(1,1) == 33);
 	  Array<Int> md(m.addDegenerate(2u));
  	  AlwaysAssertExit(md.shape() == IPosition(4,2,2,1,1));
   	  AlwaysAssertExit(md(IPosition(4,0)) == 11);
  	  AlwaysAssertExit(md(IPosition(4,1,1,0,0)) == 33);
  	  md(IPosition(4,0)) = 100;
  	  AlwaysAssertExit(m(0,0) == 100);

 	  const Array<Int> a2(m);
 	  AlwaysAssertExit(a2.addDegenerate(1u).shape() == IPosition(3,2,2,1));
 	}
	{
	  // Test 0-dimensioned (not sized) arrays
	  IPosition shape(0);
	  Array<Int> ai(shape);
	  Array<Int> ai2(ai);
	  ai2 = ai;
	  ai = 999;
	  AlwaysAssertExit(ai.ndim() == 0 && ai2.ndim() == 0 && 
			   ai.nelements() == 0);
	}

	{
	  // Test the copying Vector::resize functions
	  Vector<Int> vi(10);
	  indgen(vi);
	  vi.resize(20, True);
	  AlwaysAssertExit(vi(0) == 0 && vi(9) == 9);
	  vi.resize(IPosition(1,5), True);
	  AlwaysAssertExit(vi(0) == 0 && vi(4) == 4);
	  vi.resize(IPosition(1,10)); // All bets are off, nothing to test
	}

	{
	  // Matrix.reference(1-d array)
	  Array<Int> ai(IPosition(1,10));
	  Matrix<Int> mi;
	  mi.reference(ai);
	  AlwaysAssertExit(mi.shape() == IPosition(2,10,1));
	  ai = 11;
	  AlwaysAssertExit(allEQ(mi, 11));
	}

	{
	  // Array assign
	  Array<Int> ai(IPosition(1,10));
	  ai = 1;
	  Matrix<Int> mi(5,3);
	  mi = 2;
	  Bool exc = False;
	  try {
	    mi.assign (ai);
	  } catch (AipsError) {
	    exc = True;
	  }
	  AlwaysAssertExit (exc);
	  AlwaysAssertExit(mi.shape() == IPosition(2,5,3));
	  AlwaysAssertExit(allEQ(mi, 2));
	  ai.assign (mi);
	  AlwaysAssertExit(ai.shape() == IPosition(2,5,3));
	  AlwaysAssertExit(allEQ(ai, 2));
	}

	{
	  // Test nonDegenerate on an Array subsection.
	  IPosition shape0(5,2,3,4,5,6);
	  Array<Float> data(shape0);
	  indgen(data, Float(0.0));
	  IPosition blc(5, 0);
	  IPosition trc = shape0 - 1;
	  for (Int i=0; i<shape0(0); i++) {
	    blc(0) = i;
	    trc(0) = i;
	    for (Int j=0; j<shape0(3); j++) {
	      blc(3) = j;
	      trc(3) = j;
	      Array<Float> data2 = data(blc, trc);
	      IPosition shape1(3, shape0(1), shape0(2), shape0(4));
	      Array<Float> data3 = data2.nonDegenerate();
	      Array<Float> data4 = data2.reform(shape1);
	      AlwaysAssertExit (allEQ(data3, data4));
	      Bool deleteIt;
	      const Float* dataPtr = data2.getStorage (deleteIt);
	      Array<Float> data5 (shape1, dataPtr);
	      AlwaysAssertExit (allEQ(data3, data5));
	      data2.freeStorage (dataPtr, deleteIt);
	    }
	  }
	}
	// Test some special Vector things.
	testVector();
	// Test the resize with copy.
	testResizeCopy();
        // Test getting row, column, diagonal
        testRowColDiag();
        {
        	// tovector tests
        	Vector<Int> x(3);
        	x[0] = 20;
        	x[1] = 33;
        	x[2] = -20;
        	vector<Int> tx;
        	x.tovector(tx);
        	Vector<Int> xx = x.tovector();
        	AlwaysAssertExit(tx.size() == x.size());
        	AlwaysAssertExit(tx.size() == xx.size());

        	for (uInt i=0; i<x.size(); i++) {
        		AlwaysAssertExit(x[i] == tx[i]);
        		AlwaysAssertExit(x[i] == xx[i]);

        	}
        }
        {
          cout << "*** Test std::vector constructor" << endl;
          // Make sure compiler does not find ambiguous constructor.
          Vector<size_t> vs1(3, 2);
          AlwaysAssertExit (allEQ(vs1, size_t(2)));
          Vector<uInt> vs2(3, 2);
          AlwaysAssertExit (allEQ(vs2, uInt(2)));
          // Construct from iterator.
          std::vector<size_t> v(5);
          v[0] = 2;
          v[1] = 3;
          v[2] = 4;
          v[3] = 5;
          v[4] = 6;
          Vector<size_t> myvec(v.begin(), v.size(), 0);
          AlwaysAssertExit(v.size() == myvec.size());
          for (uInt i=0; i<5; i++) {
            AlwaysAssertExit(v[i] == myvec[i]);
          }
          // Construct from std::vector.
          std::vector<int> v2(2);
          v2[0] = 5;
          v2[1] = -2;
          Vector<Int> myvec2(v2);
          AlwaysAssertExit(v2.size() == myvec2.size());
          for (uInt i=0; i<2; i++) {
            AlwaysAssertExit(v2[i] == myvec2[i]);
          }
          // Construct and convert type.
          Vector<Double> myvec3(v2.begin(), v2.size(), 0);
          AlwaysAssertExit(v2.size() == myvec3.size());
          for (uInt i=0; i<2; i++) {
            AlwaysAssertExit(v2[i] == myvec3[i]);
          }
        }
        {
          cout << "*** Test Matrix::identity()" << endl;
          for (uInt i=0; i<20; i++) {
            Matrix<Double> x = Matrix<Double>::identity(i);
            AlwaysAssertExit(x.ncolumn() == i);
            AlwaysAssertExit(x.nrow() == i);
            for (uInt j=0; j<i; j++) {
              for (uInt k=0; k<i; k++) {
                if (j == k) {
                  AlwaysAssertExit(x(j, k) == 1);
                } else {
                  AlwaysAssertExit(x(j, k) == 0);
                }
              }
            }
          }
        }
    } catch (const AipsError& x) {
	cout << "\nCaught an exception: " << x.getMesg() << endl;
	return 1;
    } 

    cout << "OK" << endl;
    return 0;
}
