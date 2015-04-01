//# tByteSinkSource.cc: Test program for class ByteSinkSource
//# Copyright (C) 1996,1999,2000,2001
//# Associated Universities, Inc. Washington DC, USA.
//#
//# This library is free software; you can redistribute it and/or modify it
//# under the terms of the GNU Library General Public License as published by
//# the Free Software Foundation; either version 2 of the License, or (at your
//# option) any later version.
//#
//# This library is distributed in the hope that it will be useful, but WITHOUT
//# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
//# FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Library General Public
//# License for more details.
//#
//# You should have received a copy of the GNU Library General Public License
//# along with this library; if not, write to the Free Software Foundation,
//# Inc., 675 Massachusetts Ave, Cambridge, MA 02139, USA.
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
#include <casacore/casa/iostream.h>
#include <casacore/casa/IO/ByteSinkSource.h>
#include <casacore/casa/IO/CanonicalIO.h>
#include <casacore/casa/IO/RegularFileIO.h>
#include <casacore/casa/OS/RegularFile.h>
#include <casacore/casa/OS/Path.h>
#include <casacore/casa/Exceptions.h>

#include <casacore/casa/namespace.h>
const Int nrOfTests = 1;

int main ()
{
    {
	RegularFileIO regularFileIO (Path("tByteSinkSource_tmp.dat"),
				     ByteIO::New);
    }
    {
	RegularFileIO regularFileIO (Path("tByteSinkSource_tmp.dat1"),
				     ByteIO::New);
	CanonicalIO canonicalIO(&regularFileIO);
	CanonicalIO canonicalIO1(canonicalIO);
	canonicalIO1 = canonicalIO;

	ByteSinkSource sinkSource(&canonicalIO);
	ByteSinkSource sinkSource1(sinkSource);
	sinkSource1 = sinkSource; 
	ByteSinkSource sinkSource2(&(sinkSource.typeIO()));
    }
    {
	Bool     testBool = True;
	Short    testShort = -30;
	uShort   testuShort = 10;
	Int      testInt = -20;
	uInt     testuInt = 80;
	Int64    testInt64 = -100000;
	uInt64   testuInt64 = 100000;
	Float    testFloat = 18.45;
	Double   testDouble = 23.987; 
	Complex  testComplex(2,3);
	DComplex testDComplex(2.5,3.8);
	Char     testChar = 'A';
	uChar    testuChar = 'B';
	String   testString("This is a teststring");

	RegularFileIO regularFileIO (Path("tByteSinkSource_tmp.dat1"),
				     ByteIO::New);
	CanonicalIO canonicalIO(&regularFileIO);
	ByteSinkSource  sinkSource(&canonicalIO);

	cout << sinkSource.isReadable() << endl;
	cout << sinkSource.isWritable() << endl;
	cout << sinkSource.isSeekable() << endl;

	sinkSource << testBool;
	sinkSource << testShort;
	sinkSource << testuShort;
	sinkSource << testInt;
	sinkSource << testuInt;
	sinkSource << testInt64;
	sinkSource << testuInt64;
	sinkSource << testFloat;
	sinkSource << testDouble;
	sinkSource << testComplex;
	sinkSource << testDComplex;
	sinkSource << testChar;
	sinkSource << testuChar;
	sinkSource << testString;

	sinkSource.write (nrOfTests, &testBool);
	sinkSource.write (nrOfTests, &testShort);
	sinkSource.write (nrOfTests, &testuShort);
	sinkSource.write (nrOfTests, &testInt);
	sinkSource.write (nrOfTests, &testuInt);
	sinkSource.write (nrOfTests, &testInt64);
	sinkSource.write (nrOfTests, &testuInt64);
	sinkSource.write (nrOfTests, &testFloat);
	sinkSource.write (nrOfTests, &testDouble);
	sinkSource.write (nrOfTests, &testComplex);
	sinkSource.write (nrOfTests, &testDComplex);
	sinkSource.write (nrOfTests, &testChar);
	sinkSource.write (nrOfTests, &testuChar);
	sinkSource.write (nrOfTests, &testString);

	sinkSource.seek (0);

	sinkSource >> testBool;
	sinkSource >> testShort;
	sinkSource >> testuShort;
	sinkSource >> testInt;
	sinkSource >> testuInt;
	sinkSource >> testInt64;
	sinkSource >> testuInt64;
	sinkSource >> testFloat;
	sinkSource >> testDouble;
	sinkSource >> testComplex;
	sinkSource >> testDComplex;
	sinkSource >> testChar;
	sinkSource >> testuChar;
	sinkSource >> testString;

	cout << testBool      << endl;
	cout << testShort     << endl;
	cout << testuShort    << endl;
	cout << testInt       << endl;
	cout << testuInt      << endl;
	cout << testInt64     << endl;
	cout << testuInt64    << endl;
	cout << testFloat     << endl;
	cout << testDouble    << endl;
	cout << testComplex   << endl;
	cout << testDComplex  << endl;
	cout << testChar      << endl;
	cout << testuChar     << endl;
	cout << testString    << endl;

	sinkSource.read (nrOfTests, &testBool);
	sinkSource.read (nrOfTests, &testShort);
	sinkSource.read (nrOfTests, &testuShort);
	sinkSource.read (nrOfTests, &testInt);
	sinkSource.read (nrOfTests, &testuInt);
	sinkSource.read (nrOfTests, &testInt64);
	sinkSource.read (nrOfTests, &testuInt64);
	sinkSource.read (nrOfTests, &testFloat);
	sinkSource.read (nrOfTests, &testDouble);
	sinkSource.read (nrOfTests, &testComplex);
	sinkSource.read (nrOfTests, &testDComplex);
	sinkSource.read (nrOfTests, &testChar);
	sinkSource.read (nrOfTests, &testuChar);
	sinkSource.read (nrOfTests, &testString);
	
	cout << testBool      << endl;
	cout << testShort     << endl;
	cout << testuShort    << endl;
	cout << testInt       << endl;
	cout << testuInt      << endl;
	cout << testInt64     << endl;
	cout << testuInt64    << endl;
	cout << testFloat     << endl;
	cout << testDouble    << endl;
	cout << testComplex   << endl;
	cout << testDComplex  << endl;
	cout << testChar      << endl;
	cout << testuChar     << endl;
	cout << testString    << endl;
    }
    {
	RegularFileIO regularFileIO (Path("tByteSinkSource_tmp.dat1"),
				     ByteIO::Update);
	CanonicalIO canonicalIO(&regularFileIO, 5);
	ByteSinkSource sinkSource(&canonicalIO);

	String   testString;
	sinkSource << "This is a teststring";
	sinkSource.seek (0);
	sinkSource >> testString;
	cout << testString    << endl;
    }
    {
	try {
	    RegularFileIO regularFileIO (Path("tByteSinkSource_tmp.dat"),
					 ByteIO::NewNoReplace);
	} 
	catch (AipsError x)  {
	    cout << x.getMesg () << endl;
	} 
    }
    return 0;
}
