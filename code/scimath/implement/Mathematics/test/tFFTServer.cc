//# tFFTServer: This program tests the FFTServer and FourierTool classes
//# Copyright (C) 1994,1995,1996
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
//# Includes

#include <iostream.h>
#include <stdlib.h>
#include <aips/aips.h>
#include <aips/Mathematics/FFTServer.h>
#include <aips/Mathematics/Complex.h>
#include <aips/Arrays/Array.h>
#include <aips/Arrays/ArrayIO.h>
#include <aips/Arrays/ArrayIter.h>
#include <aips/Arrays/ArrayPosIter.h>
#include <aips/Arrays/Matrix.h>
#include <aips/Arrays/Vector.h>
#include <aips/Arrays/Cube.h>
#include <aips/Lattices/IPosition.h>

#include <aips/Arrays/ArrayLogical.h>
#include <aips/Arrays/MaskArrLogi.h>
#include <aips/Utilities/Assert.h>
#include <aips/Arrays/ArrayMath.h>


// Some functions to conveniently initialize arrays.

// Sets array to zero, except for the middle element and the
// element at the origin. Assumes that arr has a zero origin.
void initArr1(Array<Float> &arr)
{
    arr = Float(0.0);
    IPosition cursor(arr.shape());
    int ndims = arr.ndim();
    int nelems = arr.nelements();

    cursor /= 2;
    arr(cursor) = 1.0;

    cursor = 0;
    arr(cursor) = 1.0;
}
  
// Sets succesive elements in arr to successive integers starting at i
void initArr2(Array<Float> &arr, int &i)
{
    ArrayPositionIterator iter(arr.shape(), arr.origin(), 0);
    while (!iter.pastEnd()) {
	arr(iter.pos()) = Float(i++);
	iter.next();
    }
}

// Sets the real part of succesive elements in arr to successive integers
// starting at i
void initArr2(Array<Complex> &arr, int &i)
{
    ArrayPositionIterator iter(arr.shape(), arr.origin(), 0);
    while (!iter.pastEnd()) {
	// zero for imaginary part so it looks like initArr2(Array<float>&,...)
	arr(iter.pos()) = Complex(i++,0);
	iter.next();
    }
}

// Sets each element in arr to a random integer between 0 and 99.
void initArr3(Array<Float> &arr)
{
    ArrayPositionIterator iter(arr.shape(), arr.origin(), 0);
    while (!iter.pastEnd()) {
	arr(iter.pos()) = Float(rand() % 100);
	iter.next();
    }
}

// Sets the real part of each element in arr to a random integer
// between 0 and 99. Sets the imaginary part to zero. 
void initArr3(Array<Complex> &arr)
{
    ArrayPositionIterator iter(arr.shape(), arr.origin(), 0);
    while (!iter.pastEnd()) {
	// zero for imaginary part so it looks like initArr2(Array<float>&,...)
	arr(iter.pos()) = Complex(rand()%100,0);
	iter.next();
    }
}

// Sets the real part of each element in arr to a random integer
// between 0 and 99. Sets the imaginary part to zero. 
void initArr3(Array<DComplex> &arr)
{
    ArrayPositionIterator iter(arr.shape(), arr.origin(), 0);
    while (!iter.pastEnd()) {
	// zero for imaginary part so it looks like initArr2(Array<float>&,...)
	arr(iter.pos()) = Complex(rand()%100,0);
	iter.next();
    }
}

// Computes the absolute difference of elements in a and b in a pairwise
// fashion. Sets max to the maximum difference, and average to the
// average difference. Detemplate since it was causing the CFront compiler
// grief for some reason.
void normalStatistics(Array<Float> &a, Array<Float> &b, double &max, 
		      double &average) 
{
  
    double result(0.0);
    Float diff(0.0);
    max = 0;
    average = 0;
    if (!a.conform(b)) {
	cerr << "normalStatistics(...): arrays a and b do not conform" << endl;
	return;
    }
    ArrayPositionIterator iter(a.shape(), a.origin(), 0);
    while (!iter.pastEnd()) {
	diff = a(iter.pos()) - b(iter.pos());
	if (abs(diff) > max) {
	    max = abs(diff);
	}
	result = result + abs(diff);
	iter.next();
    }
    average = result / a.nelements();
}

void normalStatistics(Array<Complex> &a, Array<Complex> &b, double &max, 
		      double &average) 
{
  
    double result(0.0);
    Complex diff(0.0);
    max = 0;
    average = 0;
    if (!a.conform(b)) {
	cerr << "normalStatistics(...): arrays a and b do not conform" << endl;
	return;
    }
    ArrayPositionIterator iter(a.shape(), a.origin(), 0);
    while (!iter.pastEnd()) {
	diff = a(iter.pos()) - b(iter.pos());
	if (abs(diff) > max) {
	    max = abs(diff);
	}
	result = result + abs(diff);
	iter.next();
    }
    average = result / a.nelements();
}

// Tests FourierTool methods with the real array re and the complex
// array im.
void testBaseMethods(Array<Float> &re, Array<Complex> &im)
{
    cout << "----Testing Base Methods----" << endl;

    cout << "Initial real array: " << endl;
    cout << re << endl;
    cout << endl;
    cout << "Initial complex array: " << endl;
    cout << im << endl;

    try {

	FFTServer<Float, Complex> fftserv(re); 	  // initialize fft server;
	Array<Complex> nyquistC;
	Array<Complex> resultC;
	int i = -100;

	// test real unpack & pack
    
	{
	    Array<Float> tempre;


	    tempre = re;
/*      cout << "Resetting real array to initial." << endl; */
/*      cout << "real array before packing:" << endl;
	cout << tempre << endl; */
	    cout << "complex Nyquist before packing: " << endl;
	    nyquistC = fftserv.extractNYC();
	    initArr2(nyquistC, i);
	    fftserv.insertNYC(nyquistC);
	    nyquistC = fftserv.extractNYC();
	    cout << nyquistC << endl;
	    cout << endl;

	    fftserv.pack( tempre );

	    cout << "real array after packing:" << endl;
	    cout << tempre << endl;
/*      cout << "complex Nyquist after packing: " << endl;
	nyquistC = fftserv.extractNYC();
	cout << nyquistC << endl;
	*/
	    cout << endl;

	    cout << "real array before unpacking:" << endl;
	    cout << tempre << endl;
	    cout << "complex Nyquist before unpacking: " << endl;
	    nyquistC = fftserv.extractNYC();
	    cout << nyquistC << endl; 
	    cout << endl;

	    fftserv.unpack( tempre );

	    cout << "real array after unpacking:" << endl;
	    cout << tempre << endl;
	    cout << "complex Nyquist after unpacking: " << endl;
	    nyquistC = fftserv.extractNYC();
	    cout << nyquistC << endl;
	    cout << endl;

	}

	// test complex pack and unpack

	{
	    Array<Complex> tempim(im.shape());

	    tempim = im;
/*      cout << "complex array before packing:" << endl;
	cout << tempim << endl; */
	    cout << "complex Nyquist before packing: " << endl;
	    nyquistC = fftserv.extractNYC();
	    cout << nyquistC << endl;
	    cout << endl;

	    fftserv.pack( tempim );

	    cout << "complex array after packing:" << endl;
	    cout << tempim << endl;
/*      cout << "complex Nyquist after packing: " << endl;
	nyquistC = fftserv.extractNYC();
	cout << nyquistC << endl;
	*/
	    cout << endl;

	    cout << "complex array before unpacking:" << endl;
	    cout << tempim << endl;
	    cout << "complex Nyquist before unpacking: " << endl;
	    nyquistC = fftserv.extractNYC();
	    cout << nyquistC << endl;
	    cout << endl;

	    fftserv.unpack( tempim );

	    cout << "complex array after unpacking:" << endl;
	    cout << tempim << endl;
	    cout << "complex Nyquist after unpacking: " << endl;
	    nyquistC = fftserv.extractNYC();
	    cout << nyquistC << endl;
	    cout << endl;


	}

	// test real shrink & expand
    
	{
	    Array<Float> tempre;

	    tempre = re;
	    cout << "Resetting real array to initial." << endl;
/*      cout << "real array before expanding:" << endl;
	cout << tempre << endl; */
	    cout << "complex Nyquist before expanding: " << endl;
	    nyquistC = fftserv.extractNYC();
	    initArr2(nyquistC, i);
	    fftserv.insertNYC(nyquistC);
	    nyquistC = fftserv.extractNYC();
	    cout << nyquistC << endl;
	    cout << endl;

	    fftserv.expand( tempre );

	    cout << "real array after expanding:" << endl;
	    cout << tempre << endl;
/*      cout << "complex Nyquist after expanding: " << endl;
	nyquistC = fftserv.extractNYC();
	cout << nyquistC << endl; 
	*/
	    cout << endl;

//      tempre = re;
	    cout << "real array before shrinking:" << endl;
	    cout << tempre << endl;
	    cout << "complex Nyquist before shrinking: " << endl;
	    nyquistC = fftserv.extractNYC();
	    cout << nyquistC << endl;
	    cout << endl;

	    fftserv.shrink( tempre );

	    cout << "real array after shrinking:" << endl;
	    cout << tempre << endl;
	    cout << "complex Nyquist after shrinking: " << endl;
	    nyquistC = fftserv.extractNYC();
	    cout << nyquistC << endl;
	    cout << endl;

	}

	// test shrinkby1 & expandby1
    
	{
	    Array<Float> tempre;

	    tempre = re;
	    cout << "Resetting real array to initial." << endl;
/*      cout << "real array before expandby1:" << endl;
	cout << tempre << endl; */
	    cout << "complex Nyquist before expandby1: " << endl;
	    nyquistC = fftserv.extractNYC();
	    initArr2(nyquistC, i);
	    fftserv.insertNYC(nyquistC);
	    nyquistC = fftserv.extractNYC();
	    cout << nyquistC << endl;
	    cout << endl;

	    fftserv.expandby1( tempre );

	    cout << "real array after expandby1:" << endl;
	    cout << tempre << endl;
/*      cout << "complex Nyquist after expandby1: " << endl;
	nyquistC = fftserv.extractNYC();
	cout << nyquistC << endl;
	*/
	    cout << endl;

	    cout << "real array before shrinkby1:" << endl;
	    cout << tempre << endl;
	    cout << "complex Nyquist before shrinkby1: " << endl;
	    nyquistC = fftserv.extractNYC();
	    cout << nyquistC << endl;
	    cout << endl;

	    fftserv.shrinkby1( tempre );

	    cout << "real array after shrinkby1:" << endl;
	    cout << tempre << endl;
	    cout << "complex Nyquist after shrinkby1: " << endl;
	    nyquistC = fftserv.extractNYC();
	    cout << nyquistC << endl;
	    cout << endl;

	}


	// test complex shrink and expand

	{
	    Array<Complex> tempim(im.shape());

	    tempim = im;
	    cout << "Resetting complex array to initial." << endl;
/*      cout << "complex array before expanding:" << endl;
	cout << tempim << endl; */
	    cout << "complex Nyquist before expanding: " << endl;
	    nyquistC = fftserv.extractNYC();
	    cout << nyquistC << endl;
	    cout << endl;

	    fftserv.expand( tempim );

	    cout << "complex array after expanding:" << endl;
	    cout << tempim << endl;
/*      cout << "complex Nyquist after expanding: " << endl;
	nyquistC = fftserv.extractNYC();
	cout << nyquistC << endl; 
	*/
	    cout << endl;


	    cout << "complex array before shrinking:" << endl;
	    cout << tempim << endl;
	    cout << "complex Nyquist before shrinking: " << endl;
	    nyquistC = fftserv.extractNYC();
	    cout << nyquistC << endl;
	    cout << endl;

	    fftserv.shrink( tempim );

	    cout << "complex array after shrinking:" << endl;
	    cout << tempim << endl;
	    cout << "complex Nyquist after shrinking: " << endl;
	    nyquistC = fftserv.extractNYC();
	    cout << nyquistC << endl;
	    cout << endl;
	}
    } catch (AipsError x) {
	cout << "Caught exception at line " << x.thrownLine()
	     << " from file " << x.thrownFile() << endl;
	cout << "Message is: " << x.getMesg() << endl;
    } end_try;


}

// Test flipImage and exchangeUV methods
void testFlipMethods(Array<Float> &re, Array<Complex> &im)
{
    cout << "----Testing Auxiliary Methods----" << endl;

    cout << "Initial real array: " << endl;
    cout << re << endl;
    cout << endl;
    cout << "Initial complex array: " << endl;
    cout << im << endl;

    try {

	FFTServer<Float, Complex> fftserv(re); 	  // initialize fft server;
	Array<Complex> nyquistC;
	Array<Complex> resultC;
	int i = -100;

	// test real flipImage, normal image type
    
	{
	    Array<Float> tempre;

	    tempre = re;

	    fftserv.flipImage( tempre );

	    cout << "real array after flipImage, "
		<< "normal image type, parity==1:" << endl;
	    cout << tempre << endl;
	    cout << endl;

	    fftserv.flipImage( tempre, 0, 0);

	    cout << "real array after flipImage, parity==0," << 
		"expect original array:" << endl;
	    cout << tempre << endl;
	    cout << endl;

	}


	// test complex flipImage, normal image type

	{
	    Array<Complex> tempim(im.shape());

	    tempim = im;

	    fftserv.flipImage( tempim );

	    cout << "complex array after flipImage," <<  
		"normal image type, parity==1:" << endl;
	    cout << tempim << endl;
	    cout << endl;

	    fftserv.flipImage( tempim, 0, 0);

	    cout << "complex array after flipImage, parity==0," << 
		"expect original array:" << endl;
	    cout << tempim << endl;
	    cout << endl;
	}

	// test real flipImage, nyquist image type
    
	{
	    Array<Float> tempre;

	    tempre = re;

	    fftserv.flipImage( tempre, 1);

	    cout << "real array after flipImage, Nyquist image type, "
		<< "parity==1:" << endl;
	    cout << tempre << endl;
	    cout << endl;

	    fftserv.flipImage( tempre, 1, 0);

	    cout << "real array after flipImage, parity==0," << 
		"expect original array:" << endl;
	    cout << tempre << endl;
	    cout << endl;

	}


	// test complex flipImage

	{
	    Array<Complex> tempim(im.shape());

	    tempim = im;

	    fftserv.flipImage( tempim );

	    cout << "complex array after flipImage, parity==1:" << endl;
	    cout << tempim << endl;
	    cout << endl;

	    fftserv.flipImage( tempim, 0, 0);

	    cout << "complex array after flipImage, parity==0," << 
		"expect original array:" << endl;
	    cout << tempim << endl;
	    cout << endl;
	}

	// test real exchangeUV
    
	{
	    Array<Float> tempre;

	    tempre = re;

	    fftserv.exchangeUV( tempre );

	    cout << "real array after exchangeUV, parity==1:" << endl;
	    cout << tempre << endl;
	    cout << endl;

	    fftserv.exchangeUV( tempre, 0);

	    cout << "real array after exchangeUV, parity==0," << 
		"expect original array:" << endl;
	    cout << tempre << endl;
	    cout << endl;

	}

	// test complex exchangeUV

	{
	    Array<Complex> tempim(im.shape());

	    tempim = im;

	    fftserv.exchangeUV( tempim );

	    cout << "complex array after exchangeUV, parity==1:" << endl;
	    cout << tempim << endl;
	    cout << endl;

	    fftserv.exchangeUV( tempim, 0);

	    cout << "complex array after exchangeUV, parity==0," << 
		"expect original array:" << endl;
	    cout << tempim << endl;
	    cout << endl;
	}

    } catch (AipsError x) {
	cout << "Caught exception at line " << x.thrownLine()
	     << " from file " << x.thrownFile() << endl;
	cout << "Message is: " << x.getMesg() << endl;
    } end_try;


}


// Test the transform methods in the FFTServer class, using
// the arrays re and im
void testTransformMethods(Array<Float> &re, Array<Complex> &im)
{
    double max;
    double average;

    //cout << "----Testing Transform Methods----" << endl;
    //cout << "Note: the max difference after two transforms is "
    //<< endl
    //<< "      the maximum difference between corresponding " << endl
    //<< "      elements in the original array and the array " << endl
    //<< "      after a forward and backward transform." << endl;
    
    //cout << endl;
    //cout << "      average difference denotes the average " << endl
    //<< "      difference." << endl;
    //cout << endl;
    
    try {

      Array<Float> initialre;
      Array<Complex> initialim;
      
      initialre = re;
      initialim = im;
      
      FFTServer<Float, Complex> fftserv(re);   // initialize fft server;
      Array<Complex> nyquistC;
      Array<Complex> resultC;
      
      // test fft
      
      //	cout <<"test in-place fft"<<endl;
      //	cout << "initial real array " <<endl;
      //	cout << re << endl;
      //	cout << "initial Nyquist " << endl;

      nyquistC = fftserv.extractNYC();

      //	cout << nyquistC << endl;
      //	cout << endl;

      fftserv.fft(re, 1);            		  // real to complex fft

      //	cout <<"array after forward fft" << endl;
      //	cout << re << endl;
      //	cout <<"Nyquist after forward fft" << endl;
      
      nyquistC = fftserv.extractNYC();

      //	cout << nyquistC << endl;
      //	cout << endl;
      
      fftserv.fft(re, -1);             		  // complex to real fft
      //	cout << "array after reverse fft " << endl;
      //	cout << re << endl;
      //	cout << "Nyquist after reverse fft" << endl;
      
      nyquistC = fftserv.extractNYC();
      
      //      cout << nyquistC << endl;
      //	cout << endl;

      normalStatistics(re, initialre, max, average);
      AlwaysAssertExit(abs(max) < 1.e-4); 
      AlwaysAssertExit(abs(average) < 1.e-4); 	

      //	cout << "max difference after two transforms:" <<  max << endl;
      //	cout << "average difference:" << average << endl;
      //	cout << endl;

      //cout << "resetting array to initial." << endl << endl;
      re = initialre;
      im = initialim;


      // test nyfft
    
      fftserv.expand( re );
      //cout << "array after expanding " << endl;
      //cout << re << endl;
      //cout << "Nyquist after expanding " << endl;
      //nyquistC = fftserv.extractNYC();
      //cout << nyquistC << endl;
      //cout << endl;

      fftserv.nyfft(re, 1);
      //cout << "array after forward nyfft " << endl;
      //cout << re << endl;
      //cout << "Nyquist after forward nyfft" << endl;
      //nyquistC = fftserv.extractNYC();
      //cout << nyquistC << endl;
      //cout << endl;

      fftserv.nyfft(re, -1);

      //cout << "array after backward nyfft " << endl;
      //cout << re << endl;
      //cout << "Nyquist after backward nyfft" << endl;
      //nyquistC = fftserv.extractNYC();
      //cout << nyquistC << endl;
      //cout << endl;

      fftserv.shrink( re );
      //cout << "array after shrinking " << endl;
      //cout << re << endl;

      //cout << "Nyquist after shrinking " << endl;
      nyquistC = fftserv.extractNYC();
      //cout << nyquistC << endl;
      //cout << endl;


      normalStatistics(re, initialre, max, average);

      //cout << "max difference after two transforms:" <<  max << endl;
      //cout << "average difference:" << average << endl;
      //cout << endl;
      AlwaysAssertExit(abs(max) < 1.e-4);
      AlwaysAssertExit(abs(average) < 1.e-4);

      //cout << "resetting array to initial." << endl << endl;
      re = initialre;
      im = initialim;


    
      // test rrfft
    
      fftserv.rrfft(re, 1);
      //cout << "array after forward rrfft " << endl;
      //cout << re << endl;
      //cout << "Nyquist after forward fft" << endl;
      //nyquistC = fftserv.extractNYC();
      //cout << nyquistC << endl;
      //cout << endl;

      fftserv.rrfft(re, -1);
      //cout << "array after backward rrfft " << endl;
      //cout << re << endl;
      //cout << "Nyquist after backward fft" << endl;
      //nyquistC = fftserv.extractNYC();
      //cout << nyquistC << endl;
      //cout << endl;

      normalStatistics(re, initialre, max, average);

      //cout << "max difference after two transforms:" <<  max << endl;
      //cout << "average difference:" << average << endl;
      //cout << endl;
      AlwaysAssertExit(abs(max) < 1.e-4);
      AlwaysAssertExit(abs(average) < 1.e-4);

    
      //cout << "resetting array to initial." << endl << endl;
      re = initialre;
      im = initialim;



      //cout << "inital array" << endl;
      //cout << re << endl;
      //cout << "initial Nyquist" << endl;
      //nyquistC = fftserv.extractNYC();
      //cout << nyquistC << endl;
      //cout << endl;

      // test rndfft

      //cout << "testing rndfft" << endl;
      fftserv.rndfft(re, 1);
      //cout << "array after forward rndfft " << endl;
      //cout << re << endl;
      //cout << "Nyquist after forward rndfft" << endl;
      //nyquistC = fftserv.extractNYC();
      //cout << nyquistC << endl;
      //cout << endl;

      fftserv.rndfft(re, -1, 0);
      //cout << "array after backward rndfft, no scaling " << endl;
      //cout << re << endl;

      //cout << "Nyquist after backward rndfft, no scaling" << endl;
      nyquistC = fftserv.extractNYC();
      //cout << nyquistC << endl;
      //cout << endl;
      
      Float scalefactor = fftserv.scaleFactor();

      //cout << "scaleFactor returns: " << scalefactor << endl;
      
      re = re * Float(scalefactor);
      //nyquistC = nyquistC * Complex(scalefactor);
      
      fftserv.insertNYC(nyquistC);
      
      //cout << "array after scaling " << endl;
      //cout << re << endl;
      //cout << "Nyquist after scaling " << endl;
      //nyquistC = fftserv.extractNYC();
      //cout << nyquistC << endl;
      //cout << endl;
      
      normalStatistics(re, initialre, max, average);
      
      //cout << "max difference after two transforms:" <<  max << endl;
      //cout << "average difference:" << average << endl;
      //cout << endl;

      AlwaysAssertExit(abs(max) < 1.e-4);
      AlwaysAssertExit(abs(average) < 1.e-4);
      
      //cout << "resetting array to initial." << endl << endl;
      re = initialre;
      im = initialim;
      
      {
	// test rcndfft
	
	Array<Float> real;
	Array<Float> imag;
	Array<Float> initimag;
	
	real = re;
	imag = re;
	imag = Float(0.0);
	initimag = imag;
	
	//cout << "Initial real part " << endl;
	//cout << real << endl << endl;
	
	//cout << "Initial imaginary part " << endl;
	//cout << imag << endl << endl;

	fftserv.rcndfft( real, imag, 1 );

	//cout << "real part after forward rcndfft" << endl;
	//cout << real << endl << endl;

	//cout << "imag part after forward rcndfft" << endl;
	//cout << imag << endl << endl;
      
	fftserv.rcndfft( real, imag, -1 );

	//cout << "real part after backward rcndfft" << endl;
	//cout << real << endl << endl;

	//cout << "imag part after backward rcndfft" << endl;
	//cout << imag << endl << endl;

	normalStatistics(real, initialre, max, average);
      
	//cout << "max difference after two transforms on real part:" <<  max << endl;
	//cout << "average difference on real part:" << average << endl;
	//cout << endl;
      
	AlwaysAssertExit(abs(max) < 1.e-4);
	AlwaysAssertExit(abs(average) < 1.e-4);

	normalStatistics(imag, initimag, max, average);
      
	//cout << "max difference after two transforms on imaginary part:" <<  max << endl;
	//cout << "average difference on imaginary part:" << average << endl;
	//cout << endl;
	AlwaysAssertExit(abs(max) < 1.e-4);
	AlwaysAssertExit(abs(average) < 1.e-4);
	
      }

    
      //cout << "resetting array to initial." << endl << endl;
      re = initialre;
      im = initialim;

      // test rcfft & crfft

      {
	int shrinkodd = 0;
	//cout << "test non in-place rcfft & crfft" << endl;
	resultC = fftserv.rcfft(re);      // real to complex fft
	//cout << "fft'd complex array " << endl;
	//cout << resultC << endl;
	//cout << "Nyquist after rcfft" << endl;
	nyquistC = fftserv.extractNYC();
	//cout << nyquistC << endl;
	//cout << endl;
	
	if (re.shape()(0) % 2) {
	  //cout << "First dimension of original array is odd, so calling " << endl
	  //<< "crfft with shrinkodd set to 1 " << endl;
	  shrinkodd=1;
	}
	Array<Float> tempre;
	tempre = fftserv.crfft(resultC, 1, shrinkodd);
	//cout << "real result after crfft " << endl;
	//cout << tempre << endl;
	//cout << "Nyquist after crfft " << endl;
	nyquistC = fftserv.extractNYC();
	//cout << nyquistC << endl;
	//cout << endl;

	normalStatistics(tempre, initialre, max, average);
      
	AlwaysAssertExit(abs(max) < 1.e-4);
	AlwaysAssertExit(abs(average) < 1.e-4);
	//cout << "max difference after two transforms:" <<  max << endl;
	//cout << "average difference:" << average << endl;
	//cout << endl;

      }

      //cout << "resetting array to initial." << endl << endl;
      re = initialre;
      im = initialim;
      
      // test rcnyfft & crnyfft
      {
	int shrinkodd = 0;
	//cout << "test non in-place rcnyfft & crnyfft" << endl;
	//    cout << "initial real array: " << endl;
	//cout << re << endl;
	//cout << "initial nyquist: " << endl;
	Array<Complex> tempNy;
	tempNy = fftserv.extractNYC();
	//cout << tempNy << endl;

	Array<Complex> tempim;
	tempim  = fftserv.rcnyfft(re);
	tempNy = fftserv.extractNYC();
	//cout << "complex result array:" << endl;
	//cout << tempim << endl;
	//cout << "Nyquist array:" << endl;
	//cout << tempNy << endl;
	//cout << endl;


	if (re.shape()(0) % 2) {
	  //cout << "length of first dimension is odd, so calling " << endl
	  //<< "crnyfft with shrinkodd set to 1 " << endl;
	  shrinkodd = 1;
	}

	Array<Float> tempre;
	tempre = fftserv.crnyfft(tempim, 1, shrinkodd);
	//cout << "real result array after crnyfft:" << endl;
	//cout << tempre << endl;
	//cout << "Nyquist after crnyfft:" << endl;
	//tempNy = fftserv.extractNYC();
	//cout << tempNy << endl;
	//cout << endl;

	normalStatistics(tempre, initialre, max, average);
      
	//cout << "max difference after two transforms:" <<  max << endl;
	//cout << "average difference:" << average << endl;
	//cout << endl;

	AlwaysAssertExit(abs(max) < 1.e-4);
	AlwaysAssertExit(abs(average) < 1.e-4);

      }

      //cout << "resetting array to initial." << endl << endl;
      re = initialre;
      im = initialim;    
      
      // test pure complex methods
      {
	//cout << "initial complex array " << endl;
	//cout << im << endl;
	//cout << "initial Nyquist array " << endl;
	nyquistC = fftserv.extractNYC();
	//cout << nyquistC << endl;
	//cout << endl;
	
	fftserv.cxfft( im, 1 );
      
	//cout << "complex array after forward cxfft " << endl;
	//cout << im << endl;
	//cout << "Nyquist array after forward cxfft" << endl;
	nyquistC = fftserv.extractNYC();
	//    cout << nyquistC << endl;
	//cout << endl;

	fftserv.cxfft( im, -1 );

	//    cout << "complex array after backward cxfft " << endl;
	//cout << im << endl;
	//cout << "Nyquist array after backward cxfft" << endl;
	nyquistC = fftserv.extractNYC();
	//cout << nyquistC << endl;
	//cout << endl;

	normalStatistics(im, initialim, max, average);
      
	//cout << "max difference after two transforms:" <<  max << endl;
	//cout << "average difference:" << average << endl;
	//cout << endl;

	AlwaysAssertExit(abs(max) < 1.e-4);
	AlwaysAssertExit(abs(average) < 1.e-4);

	
	fftserv.cndfft( im, 1 );
	
	//cout << "complex array after forward cndfft " << endl;
	//cout << im << endl;
	//cout << "Nyquist array after forward cndft" << endl;
	//nyquistC = fftserv.extractNYC();
	//cout << nyquistC << endl;
	//cout << endl;      

	fftserv.cndfft( im, -1 );
	
	//cout << "complex array after backward cndfft " << endl;
	//cout << im << endl;
	//cout << "Nyquist array after backward cndft" << endl;
	nyquistC = fftserv.extractNYC();
	//cout << nyquistC << endl;
	//cout << endl;

	normalStatistics(im, initialim, max, average);
      
	//cout << "max difference after two transforms:" <<  max << endl;
	//cout << "average difference:" << average << endl;
	//cout << endl;      
	AlwaysAssertExit(abs(max) < 1.e-4);
	AlwaysAssertExit(abs(average) < 1.e-4);

      }

    } catch (AipsError x) {
      cout << "Caught exception at line " << x.thrownLine()
	<< " from file " << x.thrownFile() << endl;
      cout << "Message is: " << x.getMesg() << endl;
    } end_try;
  }



main()
{

// Some redundant test cases have been commented out to reduce the
// volume of output.

// test Auxiliary methods

    {

	// test normalStatistics

	Matrix<float> m(4,4);
	Matrix<float> n(4,4);
	m = 0;
	n = 0;
	m(0,0) = 16;

	double max, average;
	normalStatistics(m, n, max, average);
    
	cout << "Max: should be 16 : " << max << endl;
	cout << "Average: should be 1 : " << average << endl;
    }

    {

	// 1 d even

	int i = 0;

	Vector<float> realVec(16);
	Vector<Complex> complexVec(8);

	initArr2(realVec, i);
	i = 0;
	initArr2(complexVec, i);

	testBaseMethods(realVec, complexVec);

    }



    {

	// 1 d odd

	int i = 0;
	Vector<float> realVec(15);
	Vector<Complex> complexVec(7);
    
	initArr2(realVec, i);
	i = 0;
	initArr2(complexVec, i);

	testBaseMethods(realVec, complexVec);

    }

    {

	// even 2 d

	int i = 0;

	Matrix<float> realMatrix(4,4);
	Matrix<Complex> complexMatrix(4,4);

	initArr2(realMatrix, i);
	i = 0;
	initArr2(complexMatrix, i);

	testBaseMethods(realMatrix, complexMatrix);
    }
/*
  {

  // odd 2 d

  int i = -10;
  Matrix<float> realMatrix(4,5);
  Matrix<Complex> complexMatrix(4,5);

  initArr2(realMatrix, i);
  i = -10;
  initArr2(complexMatrix, i);

  testBaseMethods(realMatrix, complexMatrix);
  }
  */
    {

	// odd 2 d

	int i = -15;
	Matrix<float> realMatrix(5,5);
	Matrix<Complex> complexMatrix(5,5);

	initArr2(realMatrix, i);
	i = -15;
	initArr2(complexMatrix, i);

	testBaseMethods(realMatrix, complexMatrix);
    }

/*  {

    // odd 2 d

    int i = -30;
    Matrix<float> realMatrix(9,8);
    Matrix<Complex> complexMatrix(8,9);

    initArr2(realMatrix, i);
    i = -30;
    initArr2(complexMatrix, i);

    testBaseMethods(realMatrix, complexMatrix);
    }

    {

    // odd 2 d

    int i = -30;
    Matrix<float> realMatrix(9,9);
    Matrix<Complex> complexMatrix(9,9);

    initArr2(realMatrix, i);
    i = -30;
    initArr2(complexMatrix, i);

    testBaseMethods(realMatrix, complexMatrix);
    }
    */
    {

	// even 3 d

	int i = -30;
	Cube<float> realCube(4,4,4);
	Cube<Complex> complexCube(4,4,4);

	initArr2(realCube, i);
	i = -30;
	initArr2(complexCube, i);

	testBaseMethods(realCube, complexCube);
    }
/*
  {
  // various more-d

  // 5 d 

  int i = -100;
  IPosition size(5);
  size(0) = 2;
  size(1) = 3;
  size(2) = 4;
  size(3) = 3;
  size(4) = 3;

  Array<float> realArray(size);
  Array<Complex> complexArray(size);

  initArr2(realArray, i);
  i = -100;
  initArr2(complexArray, i);
  testBaseMethods(realArray, complexArray);
  }
  */



// test Transform methods



    {

	// 1 d even

	Vector<float> realVec(8);
	Vector<Complex> complexVec(8);

	realVec = float(0.0);
	complexVec = Complex(0.0);

	realVec(0) = float(1.0);
	complexVec(0) = Complex(1.0);

	testTransformMethods(realVec, complexVec);

    }
/*
  {

  // 1 d even

  int i = 0;

  Vector<float> realVec(8);
  Vector<Complex> complexVec(8);

  initArr2(realVec, i);
  i = 0;
  initArr2(complexVec, i);

  testTransformMethods(realVec, complexVec);

  }



  {

  // 1 d odd

  int i = 0;
  Vector<float> realVec(15);
  Vector<Complex> complexVec(7);
    
  initArr2(realVec, i);
  i = 0;
  initArr2(complexVec, i);

  testTransformMethods(realVec, complexVec);

  }
  */

    {

	// 1 d odd

	Vector<float> realVec(15);
	Vector<Complex> complexVec(7);
    

	realVec = float(0.0);
	complexVec = Complex(0.0);

	realVec(8) = float(1.0);
	complexVec(3) = Complex(1.0);

	testTransformMethods(realVec, complexVec);

    }



    {

	// even 2 d

	int i = 0;

	Matrix<float> realMatrix(4,4);
	Matrix<Complex> complexMatrix(4,4);

	initArr2(realMatrix, i);
	i = 0;
	initArr2(complexMatrix, i);

	testTransformMethods(realMatrix, complexMatrix);
    }

/*
  {

  // even 2 d

  Matrix<float> realMatrix(4,4);
  Matrix<Complex> complexMatrix(4,4);

  realMatrix = float(0.0);
  realMatrix(0,0) = float(1.0);
  realMatrix(1,3) = float(1.0);
  realMatrix(0,1) = float(2.4); 

  complexMatrix = Complex(0.0);
  complexMatrix(0,0) = Complex(1.0);
  complexMatrix(1,3) = Complex(1.0);
  complexMatrix(0,1) = Complex(2.4);

  testTransformMethods(realMatrix, complexMatrix);
  }

  */

    {

	// odd 2 d

	Matrix<float> realMatrix(4,5);
	Matrix<Complex> complexMatrix(4,5);

	realMatrix = float(0.0);
	realMatrix(0,0) = float(1.0);
	realMatrix(2,2) = float(1.0);

	complexMatrix = Complex(0.0);
	complexMatrix(0,0) = Complex(1.0);
	complexMatrix(2,2) = Complex(1.0);

	testTransformMethods(realMatrix, complexMatrix);
    }
/*
  {

  // odd 2 d

  Matrix<float> realMatrix(5,5);
  Matrix<Complex> complexMatrix(5,5);

  realMatrix = float(0.0);
  realMatrix(0,0) = float(1.0);
  realMatrix(2,2) = float(1.0);

  complexMatrix = Complex(0.0);
  complexMatrix(0,0) = Complex(1.0);
  complexMatrix(2,2) = Complex(1.0);

  testTransformMethods(realMatrix, complexMatrix);
  }

  {

  // odd 2 d

  int i = -100;

  Matrix<float> realMatrix(9,8);
  Matrix<Complex> complexMatrix(8,9);

  initArr2(realMatrix, i);
  i = -100;
  initArr2(complexMatrix, i);
    
  testTransformMethods(realMatrix, complexMatrix);
  }
  */
    {
	// even 3 d

	int i = -30;
	Cube<float> realCube(2,2,2);
	Cube<Complex> complexCube(2,2,2);

	initArr2(realCube, i);
	i = -30;
	initArr2(complexCube, i);

	testTransformMethods(realCube, complexCube);
    }

    {

	int i = -30;
	Cube<float> realCube(2,3,2);
	Cube<Complex> complexCube(2,3,2);
	initArr2(realCube, i);
	i = -30;
	initArr2(complexCube, i);
	testTransformMethods(realCube, complexCube);
    }
/*
  {

  int i = -30;
  Cube<float> realCube(5,5,5);
  Cube<Complex> complexCube(5,5,5);
  initArr2(realCube, i);
  i = -30;
  initArr2(complexCube, i);
  testTransformMethods(realCube, complexCube);
  }
  */
    // various more-d


    {

	// 5 d 

	int i = -100;
	IPosition size(5);
	size(0) = 4;
	size(1) = 3;
	size(2) = 4;
	size(3) = 3;
	size(4) = 3;

	Array<float> realArray(size);
	Array<Complex> complexArray(size);

	initArr2(realArray, i);
	i = -100;
	initArr2(complexArray, i);
	testTransformMethods(realArray, complexArray);
    }
/*  {

    // 4 d 

    int i = -100;
    IPosition size(4);
    size(0) = 5;
    size(1) = 3;
    size(2) = 4;
    size(3) = 3;

    Array<float> realArray(size);
    Array<Complex> complexArray(size);

    initArr2(realArray, i);
    i = -100;
    initArr2(complexArray, i);
    testTransformMethods(realArray, complexArray);
    }
    */

// test flipImage and exchangeUV methods

    {

	// 1 d even

	int i = 0;

	Vector<float> realVec(16);
	Vector<Complex> complexVec(8);

	initArr2(realVec, i);
	i = 0;
	initArr2(complexVec, i);

	testFlipMethods(realVec, complexVec);

    }


    {

	// 1 d odd

	int i = 0;
	Vector<float> realVec(15);
	Vector<Complex> complexVec(7);
    
	initArr2(realVec, i);
	i = 0;
	initArr2(complexVec, i);

	testFlipMethods(realVec, complexVec);

    }

    {

	// even 2 d

	int i = 0;

	Matrix<float> realMatrix(4,4);
	Matrix<Complex> complexMatrix(4,4);

	initArr2(realMatrix, i);
	i = 0;
	initArr2(complexMatrix, i);

	testFlipMethods(realMatrix, complexMatrix);
    }

    {

	// odd 2 d

	int i = -15;
	Matrix<float> realMatrix(5,5);
	Matrix<Complex> complexMatrix(5,5);

	initArr2(realMatrix, i);
	i = -15;
	initArr2(complexMatrix, i);

	testFlipMethods(realMatrix, complexMatrix);
    }

    {

	// even 3 d

	int i = -30;
	Cube<float> realCube(4,4,4);
	Cube<Complex> complexCube(4,4,4);

	initArr2(realCube, i);
	i = -30;
	initArr2(complexCube, i);

	testFlipMethods(realCube, complexCube);
    }


// Test shift methods, by demonstrating the
// equivalence of shifting with transforming.

    try {
	cout << endl;
	cout << "Using single precision values; testing shifting." << endl;
	int i;
	int j;
	int k;
	int l;

	cout << "1-d complex test\n" << endl;
	Vector<Complex> vector(8);
	for ( i = 0; i < 8; ++i) {
	    vector(i) = Complex(Float(i), Float(i));
	}

	FFTServer<Float, Complex> ffts(vector);

	{
	    Vector<Float> shifter(1);
	    shifter(0)=1;
	    Vector<Complex> vc(4);
	    vc = Complex(0.0, 0.0);
	    vc(0) = Complex(1.0);

	    int i, j;

	    cout << "Doing it by shifting:" << endl;
	    for ( i = 0; i < 4; ++i) {
		shifter(0) = i;
		vc = Complex(0.0, 0.0);
		vc(0) = Complex(1.0);
		cout <<"Inital vector: " << endl << vc.ac() << endl;
		ffts.cxfft(vc, 1, 0);
		ffts.shift(vc, Complex(1.0), 1, shifter);
		cout <<"After shift by " << i << " " << vc.ac() << endl;
		cout << endl;
	    }
	    cout << "Doing it by FFT:" << endl;
	    for (i = 0; i < 4; ++i) {
		vc = Complex(0.0, 0.0);
		vc(i) = Complex(1.0);
		cout <<"Inital vector: " << endl << vc.ac() << endl;
		ffts.cxfft(vc, 1, 0);
		cout <<"After fft: " << endl << vc.ac() << endl;
		cout << endl;
	    }
	}

	{
	    Vector<Float> shifter(2);
	    shifter = 0;
	    Matrix<Complex> mc(3,4);
	    mc = Complex(0,0);


	    cout << "Matrix is: " << mc.ac() << endl;

	    mc(0,0) = Complex(1.0);
	    shifter(0) = 2;
	    shifter(1) = 3;
	    cout << "Doing it by shifting " <<shifter.ac() << endl;
	    cout <<"Inital vector: " << endl << mc.ac() << endl;
	    ffts.cxfft(mc, 1, 0);
	    ffts.shift(mc, Complex(1.0), 1, shifter);
	    cout <<"After shift by " << shifter.ac() << " " << mc.ac() << endl;
	    cout << endl;

	    cout << "Doing it by FFT:" << endl;
	    mc = Complex(0.0, 0.0);
	    mc(2,3) = Complex(1.0,0.0);
	    cout <<"Inital vector: " << endl;
	    cout << mc.ac() << endl;
	    ffts.cxfft(mc, 1, 0);
	    cout <<"After fft: " << endl << mc.ac() << endl;
	    cout << endl;

	    mc = Complex(0.0);
	    mc(0,0) = Complex(1.0);
	    shifter(0) = 1;
	    shifter(1) = 0;
	    cout << "Doing it by shifting " <<shifter.ac() << endl;
	    cout <<"Inital vector: " << endl << mc.ac() << endl;
	    ffts.cxfft(mc, 1, 0);
	    ffts.shift(mc, Complex(1.0), 1, shifter);
	    cout <<"After shift by " << shifter.ac() << " " << mc.ac() << endl;
	    cout << endl;

	    cout << "Doing it by FFT:" << endl;
	    mc = Complex(0.0, 0.0);
	    mc(1,0) = Complex(1.0,0.0);
	    cout <<"Inital vector: " << endl << mc.ac() << endl;
	    ffts.cxfft(mc, 1, 0);
	    cout <<"After fft: " << endl << mc.ac() << endl;
	    cout << endl;
	}
/*    
      cout <<"2-d complex test\n"<<endl;
      Matrix<Complex> matrix(4,4); 
      for ( i = 0; i < 4; ++i) {
      for ( j = 0; j < 4; ++j) {
      matrix(i, j) = Complex(i, -j);
      }
      }

      cout <<"3-d complex test\n"<<endl;
      Cube<Complex> cube(2,2,2); 
      {
      ArrayIterator<Complex> iterator(cube);
      int i = 0;
      while (!iterator.pastEnd()) {
      cube(iterator.pos()) = Complex(i++, i++);
      iterator.next();
      }
      }
    
      cout <<"4-d case\n";
      IPosition  fourd(4);
      for(i = 0; i < 4; ++i) {
      fourd(i) = 2;
      }
    
      Array<Complex> hypercube(fourd);

      {
      ArrayIterator<Complex> iterator(cube);
      int i = 0;
      while (!iterator.pastEnd()) {
      cube(iterator.pos()) = Complex(i++, i++);
      iterator.next();
      }
      }
      */    
    } catch (AipsError x) {
	cout << "Caught exception at line " << x.thrownLine()
	     << " from file " << x.thrownFile() << endl;
	cout << "Message is: " << x.getMesg() << endl;
    } end_try;

    return 0;
}
