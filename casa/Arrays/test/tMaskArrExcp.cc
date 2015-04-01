//# tMaskArrExcp.cc: Test program for MaskedArray Exceptions
//# Copyright (C) 1994,1995,1996,1999,2000,2001
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
#include <casacore/casa/Arrays/MaskedArray.h>
#include <casacore/casa/Arrays/MaskArrIO.h>
#include <casacore/casa/Arrays/MaskArrLogi.h>
#include <casacore/casa/Arrays/MaskArrMath.h>
#include <casacore/casa/Arrays/Array.h>
#include <casacore/casa/Arrays/ArrayError.h>
#include <casacore/casa/Arrays/ArrayIO.h>
#include <casacore/casa/Arrays/ArrayLogical.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Arrays/Matrix.h>
#include <casacore/casa/Arrays/Cube.h>
#include <casacore/casa/Arrays/LogiArray.h>
#include <casacore/casa/Arrays/LogiVector.h>

#include <casacore/casa/namespace.h>
int main ()
{
  try {
    cout << endl << "\n\nTesting MaskedArray Exceptions." << endl;

    {
      Vector<Int> a(10);
      LogicalArray b(IPosition(1,11));
      a = 0;
      b = True;

      try {
	cout << "\nTest conformance, MaskedArray (Array, LogicalArray)";
	MaskedArray<Int> ma (a,b);
	cout << "\nFAILED" << endl;
      } catch (ArrayConformanceError e) {
	cout << "\nCaught an ArrayConformanceError:\n";
	cout << e.getMesg() << "\n";
      } 
    }

    {
      Vector<Int> a(10);
      LogicalArray ab(IPosition(1,10));
      LogicalArray b(IPosition(1,11));
      a = 0;
      ab = True;
      b = True;
      MaskedArray<Int> mab (a,ab);

      try {
	cout << "\nTest conformance, MaskedArray (MaskedArray,"
	     << " LogicalArray)";
	MaskedArray<Int> ma (mab,b);
	cout << "\nFAILED" << endl;
      } catch (ArrayConformanceError e) {
	cout << "\nCaught an ArrayConformanceError:\n";
	cout << e.getMesg() << "\n";
      } 
    }

    {
      Vector<Int> a(10);
      Vector<Int> ba(11);
      LogicalArray b(IPosition(1,11));
      a = 0;
      ba = 1;
      b = True;

      try {
	cout << "\nTest conformance, MaskedArray::operator= (Array)";
	ba(b) = a;
	cout << "\nFAILED" << endl;
      } catch (ArrayConformanceError e) {
	cout << "\nCaught an ArrayConformanceError:\n";
	cout << e.getMesg() << "\n";
      } 
    }

    {
      Vector<Int> a(10);
      LogicalArray ab(IPosition(1,10));
      Vector<Int> ba(11);
      LogicalArray b(IPosition(1,11));
      a = 0;
      ab = True;
      ba = 1;
      b = True;
      MaskedArray<Int> mbab (ba,b);

      try {
	cout << "\nTest conformance, MaskedArray::operator= (MaskedArray)";
	a(ab) = mbab;
	cout << "\nFAILED" << endl;
      } catch (ArrayConformanceError e) {
	cout << "\nCaught an ArrayConformanceError:\n";
	cout << e.getMesg() << "\n";
      } 
    }

    {
      Vector<Int> a(10);
      Vector<Int> ba(11);
      LogicalArray b(IPosition(1,11));
      a = 0;
      ba = 1;
      b = True;
      MaskedArray<Int> mbab (ba,b);

      try {
	cout << "\nTest conformance, Array::operator= (MaskedArray)";
	a = mbab;
	cout << "\nFAILED" << endl;
      } catch (ArrayConformanceError e) {
	cout << "\nCaught an ArrayConformanceError:\n";
	cout << e.getMesg() << "\n";
      } 
    }

    {
      Vector<Int> a(10);
      LogicalArray b(IPosition(1,10));
      a = 0;
      b = True;

      try {
	cout << "\nTest readonly MaskedArray, operator=(value)";
	MaskedArray<Int> ma (a,b,True);
	ma = 1;
	cout << "\nFAILED" << endl;
      } catch (ArrayError e) {
	cout << "\nCaught an ArrayError:\n";
	cout << e.getMesg() << "\n";
      } 
    }

    {
      Vector<Int> aa(10);
      aa = 0;
      const Vector<Int> a(aa);
      LogicalArray b(IPosition(1,10));
      b = True;

      try {
	cout << "\nTest readonly MaskedArray, operator=(Array)";
	MaskedArray<Int> ma (a,b,True);
	a(b) = a;
	cout << "\nFAILED" << endl;
      } catch (ArrayError e) {
	cout << "\nCaught an ArrayError:\n";
	cout << e.getMesg() << "\n";
      } 
    }

    {
      Vector<Int> a(10);
      LogicalArray b(IPosition(1,10));
      a = 0;
      b = True;

      try {
	cout << "\nTest readonly MaskedArray, operator=(MaskedArray)";
	MaskedArray<Int> ma (a,b);
	MaskedArray<Int> mma (a,b);
	ma.setReadOnly();
	ma = mma;
	cout << "\nFAILED" << endl;
      } catch (ArrayError e) {
	cout << "\nCaught an ArrayError:\n";
	cout << e.getMesg() << "\n";
      } 
    }

    {
      Vector<Int> a(10);
      LogicalArray b(IPosition(1,10));
      a = 0;
      b = True;

      try {
	cout << "\nTest readonly MaskedArray, getRWArray()";
	MaskedArray<Int> ma (a,b,True);
	Vector<Int> aa (ma.getRWArray());
	cout << "\nFAILED" << endl;
      } catch (ArrayError e) {
	cout << "\nCaught an ArrayError:\n";
	cout << e.getMesg() << "\n";
      } 
    }

    {
      Vector<Int> a(10);
      LogicalArray b(IPosition(1,10));
      a = 0;
      b = True;

      try {
	cout << "\nTest readonly MaskedArray, getRWArrayStorage()";
	MaskedArray<Int> ma (a,b,True);
	Bool deleteIt;
	ma.getRWArrayStorage(deleteIt);
	cout << "\nFAILED" << endl;
      } catch (ArrayError e) {
	cout << "\nCaught an ArrayError:\n";
	cout << e.getMesg() << "\n";
      } 
    }

    {
      Vector<Int> a(10);
      LogicalArray b(IPosition(1,10));
      a = 0;
      b = True;

      try {
	cout << "\nTest readonly MaskedArray, putArrayStorage()";
	MaskedArray<Int> ma (a,b,True);
	Bool deleteIt;
	const Int *arrS (ma.getArrayStorage(deleteIt));
	Int *arrRWS ((Int *) arrS);
	ma.putArrayStorage (arrRWS, deleteIt);
	cout << "\nFAILED" << endl;
      } catch (ArrayError e) {
	cout << "\nCaught an ArrayError:\n";
	cout << e.getMesg() << "\n";
      } 
    }

    {
      Vector<Int> a(10);
      LogicalVector b(10);
      a = 0;
      b = True;
      b(5) = False;

      try {
	cout << "\nTest getCompressedArray (shape)";
	MaskedArray<Int> ma (a,b,True);
	IPosition shape (2,2,5);
	Matrix<Int> c (ma.getCompressedArray(shape));
	cout << "\nFAILED" << endl;
      } catch (ArrayError e) {
	cout << "\nCaught an ArrayError:\n";
	cout << e.getMesg() << "\n";
      } 

    }

    {
      Vector<Int> a(10);
      LogicalVector b(10);
      a = 0;
      b = True;
      b(5) = False;

      try {
	cout << "\nTest getCompressedArray (Array)";
	MaskedArray<Int> ma (a,b,True);
	Matrix<Int> c (IPosition (2,2,5));
	ma.getCompressedArray(c);
	cout << "\nFAILED" << endl;
      } catch (ArrayError e) {
	cout << "\nCaught an ArrayError:\n";
	cout << e.getMesg() << "\n";
      } 
    }

    {
      Vector<Int> a(10);
      LogicalVector b(10);
      a = 0;
      b = True;
      b(5) = False;

      try {
	cout << "\nTest setCompressedArray (Array)";
	MaskedArray<Int> ma (a,b,True);
	Matrix<Int> c (IPosition (2,2,5));
	c = 1;
	ma.setCompressedArray(c);
	cout << "\nFAILED" << endl;
      } catch (ArrayError e) {
	cout << "\nCaught an ArrayError:\n";
	cout << e.getMesg() << "\n";
      } 
    }

  } catch (AipsError x) {
    cout << "\nERROR.  Caught an uncaught exception:\n";
    cout << x.getMesg() << "\n";
  } 



  try {
    cout << endl << "\n\nTesting MaskArrMath Exceptions." << endl;

    {
      Vector<Int> a(10);
      Vector<Int> ba(11);
      LogicalArray b(IPosition(1,11));
      a = 0;
      ba = 1;
      b = True;

      try {
	cout << "\nTest conformance, ::operator+= (MaskedArray, Array)";
	ba(b) += a;
	cout << "\nFAILED" << endl;
      } catch (ArrayConformanceError e) {
	cout << "\nCaught an ArrayConformanceError:\n";
	cout << e.getMesg() << "\n";
      } 
    }

    {
      Vector<Int> a(10);
      Vector<Int> ba(10);
      LogicalArray b(IPosition(1,10));
      a = 0;
      ba = 1;
      b = True;

      try {
	cout << "\nTest readonly, ::operator+= (MaskedArray, Array)";
	MaskedArray<Int> bab (ba,b,True);
	bab += a;
	cout << "\nFAILED" << endl;
      } catch (ArrayError e) {
	cout << "\nCaught an ArrayError:\n";
	cout << e.getMesg() << "\n";
      } 
    }

    {
      Vector<Int> a(10);
      Vector<Int> ba(11);
      LogicalArray b(IPosition(1,11));
      a = 0;
      ba = 1;
      b = True;
      MaskedArray<Int> mbab (ba,b);

      try {
	cout << "\nTest conformance, ::operator+= (Array, MaskedArray)";
	a += mbab;
	cout << "\nFAILED" << endl;
      } catch (ArrayConformanceError e) {
	cout << "\nCaught an ArrayConformanceError:\n";
	cout << e.getMesg() << "\n";
      } 
    }

    {
      Vector<Int> a(10);
      LogicalArray ab(IPosition(1,10));
      Vector<Int> ba(11);
      LogicalArray b(IPosition(1,11));
      a = 0;
      ab = True;
      ba = 1;
      b = True;
      MaskedArray<Int> mbab (ba,b);

      try {
	cout << "\nTest conformance, ::operator+= (MaskedArray,"
	     << " MaskedArray)";
	a(ab) += mbab;
	cout << "\nFAILED" << endl;
      } catch (ArrayConformanceError e) {
	cout << "\nCaught an ArrayConformanceError:\n";
	cout << e.getMesg() << "\n";
      } 
    }

    {
      Vector<Int> a(10);
      Vector<Int> ba(10);
      LogicalArray b(IPosition(1,10));
      a = 0;
      ba = 1;
      b = True;

      try {
	cout << "\nTest readonly, ::operator+= (MaskedArray, MaskedArray)";
	MaskedArray<Int> bab (ba,b,True);
	bab += bab;
	cout << "\nFAILED" << endl;
      } catch (ArrayError e) {
	cout << "\nCaught an ArrayError:\n";
	cout << e.getMesg() << "\n";
      } 
    }

    {
      Vector<Int> a(10);
      Vector<Int> ba(11);
      LogicalArray b(IPosition(1,11));
      Vector<Int> c(10);
      a = 0;
      ba = 1;
      b = True;
      c = 0;
      MaskedArray<Int> mbab (ba,b);

      try {
	cout << "\nTest conformance, ::operator+ (MaskedArray, Array)";
	c = mbab + a;
	cout << "\nFAILED" << endl;
      } catch (ArrayConformanceError e) {
	cout << "\nCaught an ArrayConformanceError:\n";
	cout << e.getMesg() << "\n";
      } 
    }

    {
      Vector<Int> a(10);
      Vector<Int> ba(11);
      LogicalArray b(IPosition(1,11));
      Vector<Int> c(10);
      a = 0;
      ba = 1;
      b = True;
      c = 0;
      MaskedArray<Int> mbab (ba,b);

      try {
	cout << "\nTest conformance, ::operator+ (Array, MaskedArray)";
	c = a + mbab;
	cout << "\nFAILED" << endl;
      } catch (ArrayConformanceError e) {
	cout << "\nCaught an ArrayConformanceError:\n";
	cout << e.getMesg() << "\n";
      } 
    }

    {
      Vector<Int> a(10);
      LogicalArray ab(IPosition(1,10));
      Vector<Int> ba(11);
      LogicalArray b(IPosition(1,11));
      Vector<Int> c(10);
      a = 0;
      ab = True;
      ba = 1;
      b = True;
      c = 0;
      MaskedArray<Int> maab (a,ab);
      MaskedArray<Int> mbab (ba,b);

      try {
	cout << "\nTest conformance, ::operator+ (MaskedArray,"
	     << " MaskedArray)";
	c = maab + mbab;
	cout << "\nFAILED" << endl;
      } catch (ArrayConformanceError e) {
	cout << "\nCaught an ArrayConformanceError:\n";
	cout << e.getMesg() << "\n";
      } 
    }

    {
      Vector<Int> a(10);
      Vector<Int> ba(11);
      LogicalArray b(IPosition(1,11));
      Vector<Int> c(10);
      a = 0;
      ba = 1;
      b = True;
      c = 0;
      MaskedArray<Int> mbab (ba,b);

      try {
	cout << "\nTest conformance, ::pow (MaskedArray, Array)";
	c = pow (mbab, a);
	cout << "\nFAILED" << endl;
      } catch (ArrayConformanceError e) {
	cout << "\nCaught an ArrayConformanceError:\n";
	cout << e.getMesg() << "\n";
      } 
    }

    {
      Vector<Int> a(10);
      Vector<Int> ba(11);
      LogicalArray b(IPosition(1,11));
      Vector<Int> c(10);
      a = 0;
      ba = 1;
      b = True;
      c = 0;
      MaskedArray<Int> mbab (ba,b);

      try {
	cout << "\nTest conformance, ::pow (Array, MaskedArray)";
	c = pow (a, mbab);
	cout << "\nFAILED" << endl;
      } catch (ArrayConformanceError e) {
	cout << "\nCaught an ArrayConformanceError:\n";
	cout << e.getMesg() << "\n";
      } 
    }

    {
      Vector<Int> a(10);
      LogicalArray ab(IPosition(1,10));
      Vector<Int> ba(11);
      LogicalArray b(IPosition(1,11));
      Vector<Int> c(10);
      a = 0;
      ab = True;
      ba = 1;
      b = True;
      c = 0;
      MaskedArray<Int> maab (a,ab);
      MaskedArray<Int> mbab (ba,b);

      try {
	cout << "\nTest conformance, ::pow (MaskedArray,"
	     << " MaskedArray)";
	c = pow (maab, mbab);
	cout << "\nFAILED" << endl;
      } catch (ArrayConformanceError e) {
	cout << "\nCaught an ArrayConformanceError:\n";
	cout << e.getMesg() << "\n";
      } 
    }

    {
      Vector<Int> a(10);
      Vector<Int> ba(11);
      LogicalArray b(IPosition(1,11));
      Vector<Int> c(10);
      a = 0;
      ba = 1;
      b = True;
      c = 0;
      MaskedArray<Int> mbab (ba,b);

      try {
	cout << "\nTest conformance, ::atan2 (MaskedArray, Array)";
	c = atan2 (mbab, a);
	cout << "\nFAILED" << endl;
      } catch (ArrayConformanceError e) {
	cout << "\nCaught an ArrayConformanceError:\n";
	cout << e.getMesg() << "\n";
      } 
    }

    {
      Vector<Int> a(10);
      Vector<Int> ba(11);
      LogicalArray b(IPosition(1,11));
      Vector<Int> c(10);
      a = 0;
      ba = 1;
      b = True;
      c = 0;
      MaskedArray<Int> mbab (ba,b);

      try {
	cout << "\nTest conformance, ::atan2 (Array, MaskedArray)";
	c = atan2 (a, mbab);
	cout << "\nFAILED" << endl;
      } catch (ArrayConformanceError e) {
	cout << "\nCaught an ArrayConformanceError:\n";
	cout << e.getMesg() << "\n";
      } 
    }

    {
      Vector<Int> a(10);
      LogicalArray ab(IPosition(1,10));
      Vector<Int> ba(11);
      LogicalArray b(IPosition(1,11));
      Vector<Int> c(10);
      a = 0;
      ab = True;
      ba = 1;
      b = True;
      c = 0;
      MaskedArray<Int> maab (a,ab);
      MaskedArray<Int> mbab (ba,b);

      try {
	cout << "\nTest conformance, ::atan2 (MaskedArray,"
	     << " MaskedArray)";
	c = atan2 (maab, mbab);
	cout << "\nFAILED" << endl;
      } catch (ArrayConformanceError e) {
	cout << "\nCaught an ArrayConformanceError:\n";
	cout << e.getMesg() << "\n";
      } 
    }

    {
      Vector<Int> a(10);
      LogicalArray b(IPosition(1,10));
      indgen (a);
      b = False;
      MaskedArray<Int> ma (a,b);
      Int minVal, maxVal;
      IPosition minPos (1,1), maxPos (1,1);

      try {
	cout << "\nTest insufficient elements,"
	     << " ::minMax (T &minVal, T &maxVal,"
	     << " IPosition &minPos, IPosition &maxPos,"
	     << " const MaskedArray &)";
	minMax (minVal, maxVal, minPos, maxPos, ma);
	cout << "\nFAILED" << endl;
      } catch (ArrayError e) {
	cout << "\nCaught an ArrayError:\n";
	cout << e.getMesg() << "\n";
      } 
    }

    {
      Vector<Int> ba(11);
      LogicalArray b(IPosition(1,11));
      indgen (ba);
      b = False;
      MaskedArray<Int> mbab (ba,b);

      try {
	cout << "\nTest insufficient elements, ::min (MaskedArray)";
	min (mbab);
	cout << "\nFAILED" << endl;
      } catch (ArrayError e) {
	cout << "\nCaught an ArrayError:\n";
	cout << e.getMesg() << "\n";
      } 
    }

    {
      Vector<Int> a(10);
      Vector<Int> ba(11);
      LogicalArray b(IPosition(1,11));
      Vector<Int> c(10);
      a = 0;
      ba = 1;
      b = True;
      c = 0;
      MaskedArray<Int> mbab (ba,b);

      try {
	cout << "\nTest conformance,"
	     << " ::min (MaskedArray, Array)";
	c = min (mbab, a);
	cout << "\nFAILED" << endl;
      } catch (ArrayConformanceError e) {
	cout << "\nCaught an ArrayConformanceError:\n";
	cout << e.getMesg() << "\n";
      } 
    }

    {
      Vector<Int> a(10);
      Vector<Int> ba(11);
      LogicalArray b(IPosition(1,11));
      Vector<Int> c(10);
      a = 0;
      ba = 1;
      b = True;
      c = 0;
      MaskedArray<Int> mbab (ba,b);

      try {
	cout << "\nTest conformance, ::min (Array, MaskedArray)";
	c = min (a, mbab);
	cout << "\nFAILED" << endl;
      } catch (ArrayConformanceError e) {
	cout << "\nCaught an ArrayConformanceError:\n";
	cout << e.getMesg() << "\n";
      } 
    }

    {
      Vector<Int> a(10);
      LogicalArray ab(IPosition(1,10));
      Vector<Int> ba(11);
      LogicalArray b(IPosition(1,11));
      Vector<Int> c(10);
      a = 0;
      ab = True;
      ba = 1;
      b = True;
      c = 0;
      MaskedArray<Int> maab (a,ab);
      MaskedArray<Int> mbab (ba,b);

      try {
	cout << "\nTest conformance, ::min (MaskedArray,"
	     << " MaskedArray)";
	c = min (maab, mbab);
	cout << "\nFAILED" << endl;
      } catch (ArrayConformanceError e) {
	cout << "\nCaught an ArrayConformanceError:\n";
	cout << e.getMesg() << "\n";
      } 
    }

    {
      Vector<Int> a(10);
      Vector<Int> ba(11);
      Vector<Int> c(10);
      LogicalArray cb(IPosition(1,10));
      a = 0;
      ba = 1;
      c = 0;
      cb = True;

      try {
	cout << "\nTest conformance, ::min (Array, Array,"
	     << " MaskedArray)";
	::min (a, ba, c);
	cout << "\nFAILED" << endl;
      } catch (ArrayConformanceError e) {
	cout << "\nCaught an ArrayConformanceError:\n";
	cout << e.getMesg() << "\n";
      } 
    }

    {
      Vector<Int> a(10);
      Vector<Int> ba(11);
      Vector<Int> c(10);
      LogicalArray cb(IPosition(1,10));
      a = 0;
      ba = 1;
      c = 0;
      cb = True;

      try {
	cout << "\nTest conformance, ::min (Array, Array,"
	     << " MaskedArray)";
	::min (ba, a, c);
	cout << "\nFAILED" << endl;
      } catch (ArrayConformanceError e) {
	cout << "\nCaught an ArrayConformanceError:\n";
	cout << e.getMesg() << "\n";
      } 
    }

    {
      Vector<Int> a(10);
      LogicalArray b(IPosition(1,10));
      indgen (a);
      b = False;
      MaskedArray<Int> ma (a,b);

      try {
	cout << "\nTest insufficient elements,"
	     << " ::sum (const MaskedArray &)";
        sum (ma);
	cout << "\nFAILED" << endl;
      } catch (ArrayError e) {
	cout << "\nCaught an ArrayError:\n";
	cout << e.getMesg() << "\n";
      } 
    }

    {
      Vector<Int> a(10);
      LogicalArray b(IPosition(1,10));
      indgen (a);
      b = False;
      MaskedArray<Int> ma (a,b);

      try {
	cout << "\nTest insufficient elements,"
	     << " ::sumsquares (const MaskedArray &)";
	sumsquares (ma);
	cout << "\nFAILED" << endl;
      } catch (ArrayError e) {
	cout << "\nCaught an ArrayError:\n";
	cout << e.getMesg() << "\n";
      } 
    }

    {
      Vector<Int> a(10);
      LogicalArray b(IPosition(1,10));
      indgen (a);
      b = False;
      MaskedArray<Int> ma (a,b);

      try {
	cout << "\nTest insufficient elements,"
	     << " ::product (const MaskedArray &)";
	product (ma);
	cout << "\nFAILED" << endl;
      } catch (ArrayError e) {
	cout << "\nCaught an ArrayError:\n";
	cout << e.getMesg() << "\n";
      } 
    }

    {
      Vector<Int> a(10);
      LogicalArray b(IPosition(1,10));
      indgen (a);
      b = False;
      MaskedArray<Int> ma (a,b);

      try {
	cout << "\nTest insufficient elements,"
	     << " ::mean (const MaskedArray &)";
	mean (ma);
	cout << "\nFAILED" << endl;
      } catch (ArrayError e) {
	cout << "\nCaught an ArrayError:\n";
	cout << e.getMesg() << "\n";
      } 
    }

    {
      Vector<Int> a(10);
      LogicalArray b(IPosition(1,10));
      Int mean (0);
      indgen (a);
      b = False;
      b(IPosition(1,0)) = True;
      MaskedArray<Int> ma (a,b);

      try {
	cout << "\nTest insufficient elements,"
	     << " ::variance (const MaskedArray &, T)";
	variance (ma, mean);
	cout << "\nFAILED" << endl;
      } catch (ArrayError e) {
	cout << "\nCaught an ArrayError:\n";
	cout << e.getMesg() << "\n";
      } 
    }

    {
      Vector<Int> a(10);
      LogicalArray b(IPosition(1,10));
      Int mean (0);
      indgen (a);
      b = False;
      MaskedArray<Int> ma (a,b);

      try {
	cout << "\nTest insufficient elements,"
	     << " ::avdev (const MaskedArray &, T)";
	avdev (ma, mean);
	cout << "\nFAILED" << endl;
      } catch (ArrayError e) {
	cout << "\nCaught an ArrayError:\n";
	cout << e.getMesg() << "\n";
      } 
    }

    {
      Vector<Int> a(10);
      LogicalArray b(IPosition(1,10));
      indgen (a);
      b = False;
      MaskedArray<Int> ma (a,b);

      try {
	cout << "\nTest insufficient elements,"
	     << " ::median (const MaskedArray &, Bool)";
        median (ma, False);
	cout << "\nFAILED" << endl;
      } catch (ArrayError e) {
	cout << "\nCaught an ArrayError:\n";
	cout << e.getMesg() << "\n";
      } 
    }

  } catch (AipsError x) {
    cout << "\nERROR.  Caught an uncaught exception:\n";
    cout << x.getMesg() << "\n";
  }



  try {
    cout << endl << "\n\nTesting MaskArrLogi Exceptions." << endl;

    {
      Vector<Int> a(10);
      Vector<Int> ba(11);
      LogicalArray b(IPosition(1,11));
      a = 0;
      ba = 1;
      b = True;
      MaskedArray<Int> mbab (ba,b);

      try {
	cout << "\nTest conformance, ::allLE (MaskedArray, Array)";
	allLE (mbab, a);
	cout << "\nFAILED" << endl;
      } catch (ArrayConformanceError e) {
	cout << "\nCaught an ArrayConformanceError:\n";
	cout << e.getMesg() << "\n";
      } 
    }

    {
      Vector<Int> a(10);
      Vector<Int> ba(10);
      LogicalArray b(IPosition(1,10));
      a = 0;
      ba = 1;
      b = False;
      MaskedArray<Int> mbab (ba,b);

      try {
	cout << "\nTest insufficient elements,"
	  " ::allLE (MaskedArray, Array)";
	allLE (mbab, a);
	cout << "\nFAILED" << endl;
      } catch (ArrayError e) {
	cout << "\nCaught an ArrayError:\n";
	cout << e.getMesg() << "\n";
      } 
    }

    {
      Vector<Int> a(10);
      Vector<Int> ba(11);
      LogicalArray b(IPosition(1,11));
      a = 0;
      ba = 1;
      b = True;
      MaskedArray<Int> mbab (ba,b);

      try {
	cout << "\nTest conformance, ::allLE (Array, MaskedArray)";
        allLE (a, mbab);
	cout << "\nFAILED" << endl;
      } catch (ArrayConformanceError e) {
	cout << "\nCaught an ArrayConformanceError:\n";
	cout << e.getMesg() << "\n";
      } 
    }

    {
      Vector<Int> a(10);
      Vector<Int> ba(10);
      LogicalArray b(IPosition(1,10));
      a = 0;
      ba = 1;
      b = False;
      MaskedArray<Int> mbab (ba,b);

      try {
	cout << "\nTest insuficient elements,"
	  " ::allLE (Array, MaskedArray)";
	allLE (a, mbab);
	cout << "\nFAILED" << endl;
      } catch (ArrayError e) {
	cout << "\nCaught an ArrayError:\n";
	cout << e.getMesg() << "\n";
      } 
    }

    {
      Vector<Int> a(10);
      LogicalArray ab(IPosition(1,10));
      Vector<Int> ba(11);
      LogicalArray b(IPosition(1,11));
      a = 0;
      ab = True;
      ba = 1;
      b = True;
      MaskedArray<Int> maab (a,ab);
      MaskedArray<Int> mbab (ba,b);

      try {
	cout << "\nTest conformance, ::allLE (MaskedArray,"
	     << " MaskedArray)";
	allLE (maab, mbab);
	cout << "\nFAILED" << endl;
      } catch (ArrayConformanceError e) {
	cout << "\nCaught an ArrayConformanceError:\n";
	cout << e.getMesg() << "\n";
      } 
    }

    {
      Vector<Int> a(10);
      LogicalArray ab(IPosition(1,10));
      Vector<Int> ba(10);
      LogicalArray b(IPosition(1,10));
      a = 0;
      ab = False;
      ba = 1;
      b = True;
      MaskedArray<Int> maab (a,ab);
      MaskedArray<Int> mbab (ba,b);

      try {
	cout << "\nTest insufficient elements,"
	  " ::allLE (MaskedArray, MaskedArray)";
	allLE (maab, mbab);
	cout << "\nFAILED" << endl;
      } catch (ArrayError e) {
	cout << "\nCaught an ArrayError:\n";
	cout << e.getMesg() << "\n";
      } 
    }

    {
      LogicalArray a(IPosition(1,10));
      LogicalArray c(IPosition(1,11));
      LogicalArray b(IPosition(1,11));
      a = True;
      c = False;
      b = True;
      MaskedLogicalArray mcb (c,b);

      try {
	cout << "\nTest conformance, ::anyAND (MaskedArray, Array)";
	anyAND (mcb, a);
	cout << "\nFAILED" << endl;
      } catch (ArrayConformanceError e) {
	cout << "\nCaught an ArrayConformanceError:\n";
	cout << e.getMesg() << "\n";
      } 
    }

    {
      LogicalArray a(IPosition(1,10));
      LogicalArray c(IPosition(1,10));
      LogicalArray b(IPosition(1,10));
      a = True;
      c = False;
      b = False;
      MaskedLogicalArray mcb (c,b);

      try {
	cout << "\nTest insufficient elements,"
	  " ::anyAND (MaskedArray, Array)";
	anyAND (mcb, a);
	cout << "\nFAILED" << endl;
      } catch (ArrayError e) {
	cout << "\nCaught an ArrayError:\n";
	cout << e.getMesg() << "\n";
      } 
    }

    {
      LogicalArray a(IPosition(1,10));
      LogicalArray c(IPosition(1,11));
      LogicalArray b(IPosition(1,11));
      a = True;
      c = False;
      b = True;
      MaskedLogicalArray mcb (c,b);

      try {
	cout << "\nTest conformance, ::anyAND (Array, MaskedArray)";
	anyAND (a, mcb);
	cout << "\nFAILED" << endl;
      } catch (ArrayConformanceError e) {
	cout << "\nCaught an ArrayConformanceError:\n";
	cout << e.getMesg() << "\n";
      } 
    }

    {
      LogicalArray a(IPosition(1,10));
      LogicalArray c(IPosition(1,10));
      LogicalArray b(IPosition(1,10));
      a = False;
      c = True;
      b = False;
      MaskedLogicalArray mcb (c,b);

      try {
	cout << "\nTest insufficient elements,"
	  " ::anyAND (Array, MaskedArray)";
	anyAND (a, mcb);
	cout << "\nFAILED" << endl;
      } catch (ArrayError e) {
	cout << "\nCaught an ArrayError:\n";
	cout << e.getMesg() << "\n";
      } 
    }

    {
      LogicalArray a(IPosition(1,10));
      LogicalArray ab(IPosition(1,10));
      LogicalArray c(IPosition(1,11));
      LogicalArray b(IPosition(1,11));
      a = False;
      ab = True;
      c = True;
      b = True;
      MaskedLogicalArray maab (a,ab);
      MaskedLogicalArray mcb (c,b);

      try {
	cout << "\nTest conformance, ::anyAND (MaskedArray,"
	     << " MaskedArray)";
	anyAND (maab, mcb);
	cout << "\nFAILED" << endl;
      } catch (ArrayConformanceError e) {
	cout << "\nCaught an ArrayConformanceError:\n";
	cout << e.getMesg() << "\n";
      } 
    }

    {
      LogicalArray a(IPosition(1,10));
      LogicalArray ab(IPosition(1,10));
      LogicalArray c(IPosition(1,10));
      LogicalArray b(IPosition(1,10));
      a = False;
      ab = True;
      c = True;
      b = False;
      MaskedLogicalArray maab (a,ab);
      MaskedLogicalArray mcb (c,b);

      try {
	cout << "\nTest insufficient elements,"
	  " ::anyAND (MaskedArray, MaskedArray)";
	anyAND (maab, mcb);
	cout << "\nFAILED" << endl;
      } catch (ArrayError e) {
	cout << "\nCaught an ArrayError:\n";
	cout << e.getMesg() << "\n";
      } 
    }

    {
      LogicalArray a(IPosition(1,10));
      LogicalArray c(IPosition(1,11));
      LogicalArray b(IPosition(1,11));
      a = False;
      c = True;
      b = True;
      MaskedLogicalArray mcb (c,b);

      try {
	cout << "\nTest conformance, ::anyOR (MaskedArray, Array)";
        anyOR (mcb, a);
	cout << "\nFAILED" << endl;
      } catch (ArrayConformanceError e) {
	cout << "\nCaught an ArrayConformanceError:\n";
	cout << e.getMesg() << "\n";
      } 
    }

    {
      LogicalArray a(IPosition(1,10));
      LogicalArray c(IPosition(1,10));
      LogicalArray b(IPosition(1,10));
      a = False;
      c = True;
      b = False;
      MaskedLogicalArray mcb (c,b);

      try {
	cout << "\nTest insufficient elements,"
	  " ::anyOR (MaskedArray, Array)";
        anyOR (mcb, a);
	cout << "\nFAILED" << endl;
      } catch (ArrayError e) {
	cout << "\nCaught an ArrayError:\n";
	cout << e.getMesg() << "\n";
      } 
    }

    {
      LogicalArray a(IPosition(1,10));
      LogicalArray c(IPosition(1,11));
      LogicalArray b(IPosition(1,11));
      a = False;
      c = True;
      b = True;
      MaskedLogicalArray mcb (c,b);

      try {
	cout << "\nTest conformance, ::anyOR (Array, MaskedArray)";
	anyOR (a, mcb);
	cout << "\nFAILED" << endl;
      } catch (ArrayConformanceError e) {
	cout << "\nCaught an ArrayConformanceError:\n";
	cout << e.getMesg() << "\n";
      } 
    }

    {
      LogicalArray a(IPosition(1,10));
      LogicalArray c(IPosition(1,10));
      LogicalArray b(IPosition(1,10));
      a = False;
      c = True;
      b = False;
      MaskedLogicalArray mcb (c,b);

      try {
	cout << "\nTest insufficient elements,"
	  " ::anyOR (Array, MaskedArray)";
	anyOR (a, mcb);
	cout << "\nFAILED" << endl;
      } catch (ArrayError e) {
	cout << "\nCaught an ArrayError:\n";
	cout << e.getMesg() << "\n";
      } 
    }

    {
      LogicalArray a(IPosition(1,10));
      LogicalArray ab(IPosition(1,10));
      LogicalArray c(IPosition(1,11));
      LogicalArray b(IPosition(1,11));
      a = False;
      ab = True;
      c = True;
      b = True;
      MaskedLogicalArray maab (a,ab);
      MaskedLogicalArray mcb (c,b);

      try {
	cout << "\nTest conformance, ::anyOR (MaskedArray,"
	     << " MaskedArray)";
	anyOR (maab, mcb);
	cout << "\nFAILED" << endl;
      } catch (ArrayConformanceError e) {
	cout << "\nCaught an ArrayConformanceError:\n";
	cout << e.getMesg() << "\n";
      } 
    }

    {
      LogicalArray a(IPosition(1,10));
      LogicalArray ab(IPosition(1,10));
      LogicalArray c(IPosition(1,10));
      LogicalArray b(IPosition(1,10));
      a = False;
      ab = False;
      c = True;
      b = True;
      MaskedLogicalArray maab (a,ab);
      MaskedLogicalArray mcb (c,b);

      try {
	cout << "\nTest insufficient elements"
	  " ::anyOR (MaskedArray, MaskedArray)";
	anyOR (maab, mcb);
	cout << "\nFAILED" << endl;
      } catch (ArrayError e) {
	cout << "\nCaught an ArrayError:\n";
	cout << e.getMesg() << "\n";
      } 
    }

    {
      Vector<Int> a(10);
      Vector<Int> ba(11);
      LogicalArray b(IPosition(1,11));
      LogicalArray cb(IPosition(1,10));
      a = 0;
      ba = 1;
      b = True;
      cb = False;
      MaskedArray<Int> mbab (ba,b);

      try {
	cout << "\nTest conformance, ::operator<= (MaskedArray, Array)";
	cb = (mbab <= a);
	cout << "\nFAILED" << endl;
      } catch (ArrayConformanceError e) {
	cout << "\nCaught an ArrayConformanceError:\n";
	cout << e.getMesg() << "\n";
      } 
    }

    {
      Vector<Int> a(10);
      Vector<Int> ba(11);
      LogicalArray b(IPosition(1,11));
      LogicalArray cb(IPosition(1,10));
      a = 0;
      ba = 1;
      b = True;
      cb = False;
      MaskedArray<Int> mbab (ba,b);

      try {
	cout << "\nTest conformance, ::operator<= (Array, MaskedArray)";
	cb = (a <= mbab);
	cout << "\nFAILED" << endl;
      } catch (ArrayConformanceError e) {
	cout << "\nCaught an ArrayConformanceError:\n";
	cout << e.getMesg() << "\n";
      } 
    }

    {
      Vector<Int> a(10);
      LogicalArray ab(IPosition(1,10));
      Vector<Int> ba(11);
      LogicalArray b(IPosition(1,11));
      LogicalArray cb(IPosition(1,10));
      a = 0;
      ab = True;
      ba = 1;
      b = True;
      cb = False;
      MaskedArray<Int> maab (a,ab);
      MaskedArray<Int> mbab (ba,b);

      try {
	cout << "\nTest conformance, ::operator<= (MaskedArray,"
	     << " MaskedArray)";
	cb = (maab <= mbab);
	cout << "\nFAILED" << endl;
      } catch (ArrayConformanceError e) {
	cout << "\nCaught an ArrayConformanceError:\n";
	cout << e.getMesg() << "\n";
      } 
    }

    {
      Vector<Int> a(10);
      LogicalArray c(IPosition(1,10));
      LogicalArray b(IPosition(1,10));
      indgen (a);
      c = (a<5);
      b = False;
      MaskedLogicalArray mcb (c,b);

      try {
	cout << "\nTest insufficient elements,"
	  " ::allAND (MaskedArray, scalar)";
        allAND (mcb, True);
	cout << "\nFAILED" << endl;
      } catch (ArrayError e) {
	cout << "\nCaught an ArrayError:\n";
	cout << e.getMesg() << "\n";
      } 
    }

    {
      Vector<Int> a(10);
      LogicalArray c(IPosition(1,10));
      LogicalArray b(IPosition(1,10));
      indgen (a);
      c = (a<5);
      b = False;
      MaskedLogicalArray mcb (c,b);

      try {
	cout << "\nTest insufficient elements,"
	  " ::allAND (scalar, MaskedArray)";
	allAND (True, mcb);
	cout << "\nFAILED" << endl;
      } catch (ArrayError e) {
	cout << "\nCaught an ArrayError:\n";
	cout << e.getMesg() << "\n";
      } 
    }
      
    {
      Vector<Int> a(10);
      LogicalArray c(IPosition(1,10));
      LogicalArray b(IPosition(1,10));
      indgen (a);
      c = (a<5);
      b = False;
      MaskedLogicalArray mcb (c,b);

      try {
	cout << "\nTest insufficient elements,"
	  " ::allOR (MaskedArray, scalar)";
        allOR (mcb, False);
	cout << "\nFAILED" << endl;
      } catch (ArrayError e) {
	cout << "\nCaught an ArrayError:\n";
	cout << e.getMesg() << "\n";
      } 
    }

    {
      Vector<Int> a(10);
      LogicalArray c(IPosition(1,10));
      LogicalArray b(IPosition(1,10));
      indgen (a);
      c = (a<5);
      b = False;
      MaskedLogicalArray mcb (c,b);

      try {
	cout << "\nTest insufficient elements,"
	  " ::allOR (scalar, MaskedArray)";
        allOR (False, mcb);
	cout << "\nFAILED" << endl;
      } catch (ArrayError e) {
	cout << "\nCaught an ArrayError:\n";
	cout << e.getMesg() << "\n";
      } 
    }

    {
      Vector<Int> a(10);
      Vector<Int> ba(10);
      LogicalArray b(IPosition(1,10));
      a = 0;
      ba = 1;
      b = False;
      MaskedArray<Int> mbab (ba,b);

      try {
	cout << "\nTest insufficient elements,"
	  " ::allLE (MaskedArray, scalar)";
        allLE (mbab, 7);
	cout << "\nFAILED" << endl;
      } catch (ArrayError e) {
	cout << "\nCaught an ArrayError:\n";
	cout << e.getMesg() << "\n";
      } 
    }

    {
      Vector<Int> a(10);
      Vector<Int> ba(10);
      LogicalArray b(IPosition(1,10));
      a = 0;
      ba = 1;
      b = False;
      MaskedArray<Int> mbab (ba,b);

      try {
	cout << "\nTest insuficient elements,"
	  " ::allLE (scalar, MaskedArray)";
        allLE (7, mbab);
	cout << "\nFAILED" << endl;
      } catch (ArrayError e) {
	cout << "\nCaught an ArrayError:\n";
	cout << e.getMesg() << "\n";
      } 
    }

    {
      Vector<Int> a(10);
      LogicalArray c(IPosition(1,10));
      LogicalArray b(IPosition(1,10));
      indgen (a);
      c = (a<5);
      b = False;
      MaskedLogicalArray mcb (c,b);

      try {
	cout << "\nTest insufficient elements,"
	  " ::anyAND (MaskedArray, scalar)";
        anyAND (mcb, True);
	cout << "\nFAILED" << endl;
      } catch (ArrayError e) {
	cout << "\nCaught an ArrayError:\n";
	cout << e.getMesg() << "\n";
      } 
    }

    {
      Vector<Int> a(10);
      LogicalArray c(IPosition(1,10));
      LogicalArray b(IPosition(1,10));
      indgen (a);
      c = (a<5);
      b = False;
      MaskedLogicalArray mcb (c,b);

      try {
	cout << "\nTest insufficient elements,"
	  " ::anyAND (scalar, MaskedArray)";
        anyAND (True, mcb);
	cout << "\nFAILED" << endl;
      } catch (ArrayError e) {
	cout << "\nCaught an ArrayError:\n";
	cout << e.getMesg() << "\n";
      } 
    }

    {
      Vector<Int> a(10);
      LogicalArray c(IPosition(1,10));
      LogicalArray b(IPosition(1,10));
      indgen (a);
      c = (a<5);
      b = False;
      MaskedLogicalArray mcb (c,b);

      try {
	cout << "\nTest insufficient elements,"
	  " ::anyOR (MaskedArray, scalar)";
        anyOR (mcb, False);
	cout << "\nFAILED" << endl;
      } catch (ArrayError e) {
	cout << "\nCaught an ArrayError:\n";
	cout << e.getMesg() << "\n";
      } 
    }

    {
      Vector<Int> a(10);
      LogicalArray c(IPosition(1,10));
      LogicalArray b(IPosition(1,10));
      indgen (a);
      c = (a<5);
      b = False;
      MaskedLogicalArray mcb (c,b);

      try {
	cout << "\nTest insufficient elements,"
	  " ::anyOR (scalar, MaskedArray)";
        anyOR (False, mcb);
	cout << "\nFAILED" << endl;
      } catch (ArrayError e) {
	cout << "\nCaught an ArrayError:\n";
	cout << e.getMesg() << "\n";
      } 
    }

  } catch (AipsError x) {
    cout << "\nERROR.  Caught an uncaught exception:\n";
    cout << x.getMesg() << "\n";
  } 


  cout << endl << "OK" << endl;
  return 0;
}
