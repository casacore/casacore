//# tArray.cc: Test program for the Array class
//# Copyright (C) 1993,1994,1995,1996,1997,1998,1999
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

#include <iostream.h>

#include <aips/aips.h>
#include <aips/Utilities/String.h>
#include <aips/Mathematics/Complex.h>
#include <aips/Mathematics/Math.h>
#include <aips/Utilities/Assert.h>

#include <aips/Lattices/IPosition.h>
#include <aips/Arrays/Array.h>
#include <aips/Arrays/ArrayMath.h>
#include <aips/Arrays/ArrayLogical.h>
#include <aips/Arrays/ArrayIO.h>
#include <aips/Arrays/Vector.h>
#include <aips/Arrays/Matrix.h>
#include <aips/Arrays/Cube.h>
#include <aips/Arrays/ArrayError.h>
#include <aips/Functionals/Polynomial.h>

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
	    indgen(vi.ac());
	    Polynomial<Float> pi(2);
	    pi.setCoefficient(2, 1.0f);
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
	Vector<Int> zzz(x(Slice(0,5,2)));
	zzz.unique();
	AlwaysAssertExit(zzz.nrefs() == 1 && allEQ(zzz.ac(), 11) &&
			 zzz.nelements() == 5);
	Vector<Int> y1(5, 4);
	AlwaysAssertExit (allEQ(y1.ac(), 4));

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
	AlwaysAssertExit(allEQ(zzz.ac(), yyy.ac()) &&
			 allEQ (zzz.ac(), -13));
	Block<Int> blk;
	zzz.toBlock(blk);
	Vector<Int>  aaa(blk);
	AlwaysAssertExit(allEQ (aaa.ac(), zzz.ac()) && 
			 allEQ (aaa.ac(), -13));
	cout << "OK\n";
    }

    {
	cout << "Testing math functions ..............";
	Vector<double> x(5), y, z;
	for (Int i = 0; i < 5; i++)
	    x(i) = double(i+1)/10.0;
	
	y = floor(x.ac());
	for (Int i = 0; i < 5; i++)
	    AlwaysAssertExit(y(i) == 0.0);
	
	z = pow(x.ac(),y.ac());
	for (Int i = 0; i < 5; i++)
	    AlwaysAssertExit(z(i) == 1.0);
	
	AlwaysAssertExit(min(z.ac()) == 1.0 && max(z.ac()) == 1.0);
	z(4) = -1.0; z(3) = 22.0;
	AlwaysAssertExit(min(z.ac()) == -1.0 && max(z.ac()) 
			 == 22.0);

	Vector<Int> vi1(5);
	indgen(vi1.ac());
	Vector<Int> vi2(5), vi3;
	vi2 = 0;
	vi2(3) = -3;
	vi3 = min(vi1.ac(), vi2.ac());
	AlwaysAssertExit(vi3(0) == 0 && vi3(1) == 0 && vi3(2) == 0 && vi3(3) == -3 &&
		vi3(4) == 0);
	vi2(3) = 9;
	vi3 = max(-vi1.ac(), vi2.ac());
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


            minMax (min, max, minPos, maxPos, a.ac());

            AlwaysAssertExit(minPos == IPosition(2, 8, 0));
            AlwaysAssertExit(maxPos == IPosition(2, 3, 1));
            AlwaysAssertExit(min == a(minPos));
            AlwaysAssertExit(max == a(maxPos));

            minMax(min, max,a.ac());
            AlwaysAssertExit(min == a(minPos));
            AlwaysAssertExit(max == a(maxPos));

            minMax(min, max, a.ac());
            AlwaysAssertExit(min == a(minPos));
            AlwaysAssertExit(max == a(maxPos));


            minMax (min, max, minPos, maxPos, a.ac(), mask);

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
        AlwaysAssertExit(allEQ (x.ac(), 1) && allEQ (y.ac(), 2) &&
		 allLE (x.ac(), y.ac()) &&  allGE(y.ac(), x.ac()) &&
		 allNE(x.ac(), 2) && allNE (x.ac(), y.ac()) && allEQ (x.ac(),
		 x.ac()) && allLT (x.ac(), y.ac()) && allGT (y.ac(), x.ac()) );

	x.ac() += 1;
	AlwaysAssertExit(allEQ (x.ac(), y.ac()));
	x.ac() += y.ac();
	AlwaysAssertExit(allEQ (x.ac(), 2*y.ac()));
	x = -1 * x.ac();
	AlwaysAssertExit(allEQ (x.ac(), -4));
	indgen(x.ac()); indgen(y.ac(),1);
	AlwaysAssertExit(allEQ (x.ac() - y.ac(), -1));
	for (Int i = 0; i < 5; i++)
	    AlwaysAssertExit(x(i) == i);

	AlwaysAssertExit(sum(x.ac()) == 10);
	AlwaysAssertExit(product(y.ac()) == 120);
	AlwaysAssertExit(mean(y.ac()) == median(y.ac()) && mean(y.ac()) == 3);

        AlwaysAssertExit(anyLE(x.ac(), y.ac()) && anyLE(x.ac(), y.ac()-1) &&
			 !anyLE(x.ac(), y.ac()-2));
        AlwaysAssertExit(anyLT(x.ac(), y.ac()) && !anyLT(x.ac(), y.ac()-1) &&
			 !anyLT (x.ac(), y.ac()-2));
        AlwaysAssertExit(!anyGE(x.ac(), y.ac()) && anyGE (x.ac(), y.ac()-1) &&
			 anyGE (x.ac(), y.ac()-2));
        AlwaysAssertExit(!anyGT(x.ac(), y.ac()) && !anyGT (x.ac(), y.ac()-1) &&
			 anyGT (x.ac(), y.ac()-2));
        AlwaysAssertExit(!anyEQ(x.ac(), y.ac()) && anyEQ(x.ac(), y.ac()-1) &&
			 !anyEQ(x.ac(), y.ac()-2));
        AlwaysAssertExit(anyNE(x.ac(), y.ac()) && !anyNE (x.ac(), y.ac()-1) &&
			 anyNE(x.ac(), y.ac()-2));

        AlwaysAssertExit(anyLE(x.ac(),1) && anyLE(x.ac(),0)&& !anyLE(x.ac(),-1));
        AlwaysAssertExit(anyLT(x.ac(),1) && !anyLT(x.ac(),0)&&!anyLT(x.ac(),-1));
        AlwaysAssertExit(anyGE(x.ac(),3)&&anyGE(x.ac(),4) && !anyGE(x.ac(), 5));
        AlwaysAssertExit(anyGT(x.ac(),3) && !anyGT(x.ac(),4)&&!anyGT(x.ac(), 5));
        AlwaysAssertExit(anyEQ(x.ac(),3) && anyEQ(x.ac(),4) && !anyEQ(x.ac(),5));
        AlwaysAssertExit(anyNE(x.ac(),3) && anyNE(x.ac(),4) && anyNE(x.ac(), 5));

        AlwaysAssertExit(anyLE(3,x.ac()) && anyLE(4,x.ac()) && !anyLE(5,x.ac()));
        AlwaysAssertExit(anyLT(3,x.ac()) && !anyLT(4,x.ac()) &&!anyLT(5,x.ac()));
        AlwaysAssertExit(!anyGE(-1,x.ac()) && anyGE(0,x.ac()) &&anyGE(1,x.ac()));
        AlwaysAssertExit(!anyGT(-1,x.ac()) && !anyGT(0,x.ac())&&anyGT(1,x.ac()));
        AlwaysAssertExit(!anyEQ(-1,x.ac())&& anyEQ(0,x.ac()) && anyEQ(1,x.ac()));
        AlwaysAssertExit(anyNE(-1,x.ac()) && anyNE(0,x.ac()) &&anyNE (1,x.ac()));

	Vector<Double> vd(5);
	indgen(vd.ac(),1.0);
	AlwaysAssertExit(fabs(variance(vd.ac()) - 2.5) < 0.0001);
	AlwaysAssertExit(fabs(stddev(vd.ac()) - sqrt(2.5)) < 0.0001);
	AlwaysAssertExit(fabs(avdev(vd.ac()) - 1.2) < 0.0001);
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
	    AlwaysAssertExit(allEQ (square(vf.ac()), 9));
	    AlwaysAssertExit(allEQ (cube(vf.ac()), -27));
	}

	{
	    Vector<Float> x1(2), x2(2);
	    x1 = 10000;
	    x2(0) = 10001; x2(1) = 10002;
	    // Array,Array
	    AlwaysAssertExit(allNear(x1.ac(), x2.ac(), 2.01e-4));
	    AlwaysAssertExit(!allNear(x1.ac(), x2.ac(), 1.01e-4));
	    AlwaysAssertExit(anyNear(x1.ac(), x2.ac(), 1.01e-4));
	    AlwaysAssertExit(!anyNear(x1.ac(), x2.ac(), 0.99e-4));
	    AlwaysAssertExit(allNearAbs(x1.ac(), x2.ac(), 2.01));
	    AlwaysAssertExit(!allNearAbs(x1.ac(), x2.ac(), 1.01));
	    AlwaysAssertExit(anyNearAbs(x1.ac(), x2.ac(), 1.01));
	    AlwaysAssertExit(!anyNearAbs(x1.ac(), x2.ac(), 0.99));

	    // Constant,Array
	    AlwaysAssertExit(allNear(10000.0f, x2.ac(), 2.01e-4));
	    AlwaysAssertExit(!allNear(10000.0f, x2.ac(), 1.01e-4));
	    AlwaysAssertExit(anyNear(10000.0f, x2.ac(), 1.01e-4));
	    AlwaysAssertExit(!anyNear(10000.0f, x2.ac(), 0.99e-4));
	    AlwaysAssertExit(allNearAbs(10000.0f, x2.ac(), 2.01));
	    AlwaysAssertExit(!allNearAbs(10000.0f, x2.ac(), 1.01));
	    AlwaysAssertExit(anyNearAbs(10000.0f, x2.ac(), 1.01));
	    AlwaysAssertExit(!anyNearAbs(10000.0f, x2.ac(), 0.99));

	    // Array,Constant
	    AlwaysAssertExit(allNear(x1.ac(), 10002.0f, 2.01e-4));
	    AlwaysAssertExit(!allNear(x1.ac(), 10002.0f, 1.01e-4));
	    AlwaysAssertExit(!anyNear(x1.ac(), 10002.0f, 1.01e-4));
	    AlwaysAssertExit(!anyNear(x1.ac(), 10002.0f, 0.99e-4));
	    AlwaysAssertExit(allNearAbs(x1.ac(), 10002.0f, 2.01));
	    AlwaysAssertExit(!allNearAbs(x1.ac(), 10002.0f, 1.01));
	    AlwaysAssertExit(anyNearAbs(x1.ac(), 10001.0f, 1.01));
	    AlwaysAssertExit(!anyNearAbs(x1.ac(), 10002.0f, 0.99));
	}

	cout << "OK\n";
    }
 
    {
	cout << "Simple matrix tests..................";
	Matrix<Int> a(5u,5u), b;
	a = 3;
	AlwaysAssertExit(a.nrow() == a.ncolumn() && a.nrow() == 5);
	AlwaysAssertExit(allEQ(a.ac(), 3));
	AlwaysAssertExit(allLE (a.ac(), 3)); 
	AlwaysAssertExit(allEQ (a.ac(), a.ac())); 
	AlwaysAssertExit(allNE (a.ac(), 1));
	b = 2*a.ac();
	Matrix<Int> c =  a(Slice(2,1),Slice(3,1));
	AlwaysAssertExit(allEQ (b.ac(), 6));
	a.row(3) = 6;
	AlwaysAssertExit(allEQ (a.row(3).ac(), 6));
	a.column(3) = 1;
	AlwaysAssertExit(allEQ (a.column(3).ac(), 1));
	a.diagonal(-1) = 7;
	AlwaysAssertExit(allEQ (a.diagonal(-1).ac(), 7));
	
	IPosition l(1);
	l(0) = a.nelements();
	Vector<Int> d(a.reform(l));
	for (Int i = 0; i < 5; i++)
	    for (Int j = 0; j < 5; j++)
		AlwaysAssertExit(a(i,j) == d(i + j*5));
	Matrix<Int> y1(5u,6u, 4u);
	AlwaysAssertExit (allEQ(y1.ac(), 4));
	
	
	Vector<Int> v(10);
	indgen(v.ac());
	Matrix<Int> vm(v);
	AlwaysAssertExit(vm.ndim() == 2 && vm.nelements() == v.nelements());
	for (Int i = 0; i < v.nelements(); i++)
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
	    AlwaysAssertExit(allEQ (c.xyPlane(k).ac(), 3));
	
	// Check copy ctor
	Cube<Int> c2(c);
	c(1,1,1) = -3;
	AlwaysAssertExit(c2(1,1,1) == -3);
	
	// Check assignment
	Cube<Int> c3;
	c3 = c;
	AlwaysAssertExit(allEQ (c3.ac(), c.ac()));
	Cube<Int> y1(5,6,7, 4);
	AlwaysAssertExit (allEQ(y1.ac(), 4));
	
	// slice
	AlwaysAssertExit(allEQ (c3 (Slice(0,2), Slice(1,2), 1).ac(),
                       c (Slice(0,2), Slice(1,2), 1).ac()));
	Cube<Int> c4(c3(Slice(0,2),Slice(1,2),1));
	IPosition c4shape(c4.Array<Int>::shape());
	AlwaysAssertExit(c4.nelements() == 4 && c4shape(2) == 1);
	IPosition blc(3), trc(3);
	// middle plane
	blc(0) = 0; blc(1) = 0; blc(2) = 1;
	trc(0) = 2; trc(1) = 2; trc(2) = 1;
// Why can't we just write   c(blc,trc) = 11; ?
	c.Array<Int>::operator()(blc,trc) = 11;
	AlwaysAssertExit(allEQ (c.xyPlane(1).ac(), 11));
	AlwaysAssertExit(allEQ (c.xyPlane(0).ac(), 3));
	AlwaysAssertExit(allEQ (c.xyPlane(2).ac(), 3));
	
	
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
	for (Int i = 0; i < m.nelements(); i++)
	    storage[i] = +1;
	AlwaysAssertExit(allEQ (m.ac(), 1));
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

void seeIfWeMakeMemoryLeak()
{
    IPosition ip(5);
    ip = 5;
    Array<Int> ai(ip);
    throw(ArrayError("Just trying to check memory leak"));
}


main()
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
		} end_try;
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
	  for (uInt i=0; i < 100; i++) {
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
	  indgen(vi.ac());
	  vi.resize(20, True);
	  AlwaysAssertExit(vi(0) == 0 && vi(9) == 9);
	  vi.resize(IPosition(1,5), True);
	  AlwaysAssertExit(vi(0) == 0 && vi(4) == 4);
	  vi.resize(IPosition(1,10)); // All bets are off, nothing to test
	}

	{
	  // Matrix.referece(1-d array)
	  Array<Int> ai(IPosition(1,10));
	  Matrix<Int> mi;
	  mi.reference(ai);
	  AlwaysAssertExit(mi.shape() == IPosition(2,10,1));
	  ai = 11;
	  AlwaysAssertExit(allEQ(mi.ac(), 11));
	}

    } catch (AipsError x) {
	cout << "\nCaught an exception: " << x.getMesg() << endl;
    } end_try;

    cout << "OK" << endl;
    return 0;
}
