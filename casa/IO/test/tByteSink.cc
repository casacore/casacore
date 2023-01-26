//# tByteSink.cc: Test program for classes ByteSink and ByteSource
//# Copyright (C) 1996,2001
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

#include <casacore/casa/aips.h>
#include <casacore/casa/iostream.h>
#include <casacore/casa/IO/ByteSink.h>
#include <casacore/casa/IO/ByteSource.h>
#include <casacore/casa/IO/RegularFileIO.h>
#include <casacore/casa/IO/CanonicalIO.h>
#include <casacore/casa/OS/RegularFile.h>
#include <casacore/casa/OS/Path.h>

#include <casacore/casa/namespace.h>
const int32_t nrOfTests = 1;

int main ()
{
    { 
	bool     testBool = true;
	int16_t    testShort = -30;
	uint16_t   testuShort = 10;
	int32_t      testInt = -20;
	uint32_t     testuInt = 80;
	int64_t    testInt64 = -100000;
	uint64_t   testuInt64 = 100000;
	float    testFloat = 18.45;
	double   testDouble = 23.987; 
	Complex  testComplex(2,3);
	DComplex testDComplex(2.5,3.8);
	char     testChar = 'A';
	unsigned char    testuChar = 'B';
	String   testString("This is a teststring");

	RegularFileIO regularFileIO (Path("tByteSink_tmp.dat"),
				     ByteIO::New);
	CanonicalIO canonicalIO(&regularFileIO);
	ByteSink  sink(&canonicalIO);
	cout << sink.isReadable() << endl;
	cout << sink.isWritable() << endl;
	cout << sink.isSeekable() << endl;

	sink << testBool;
	sink << testShort;
	sink << testuShort;
	sink << testInt;
	sink << testuInt;
	sink << testInt64;
	sink << testuInt64;
	sink << testFloat;
	sink << testDouble;
	sink << testComplex;
	sink << testDComplex;
	sink << testChar;
	sink << testuChar;
	sink << testString;

	sink.write (nrOfTests, &testBool);
	sink.write (nrOfTests, &testShort);
	sink.write (nrOfTests, &testuShort);
	sink.write (nrOfTests, &testInt);
	sink.write (nrOfTests, &testuInt);
	sink.write (nrOfTests, &testInt64);
	sink.write (nrOfTests, &testuInt64);
	sink.write (nrOfTests, &testFloat);
	sink.write (nrOfTests, &testDouble);
	sink.write (nrOfTests, &testComplex);
	sink.write (nrOfTests, &testDComplex);
	sink.write (nrOfTests, &testChar);
	sink.write (nrOfTests, &testuChar);
	sink.write (nrOfTests, &testString);
    }
    {
	bool     testBool;
	int16_t    testShort;
	uint16_t   testuShort;
	int32_t      testInt;
	uint32_t     testuInt;
	int64_t    testInt64;
	uint64_t   testuInt64;
	float    testFloat;
	double   testDouble; 
	Complex  testComplex;
	DComplex testDComplex;
	char     testChar;
	unsigned char    testuChar;
	String   testString;

	RegularFileIO regularFileIO (Path("tByteSink_tmp.dat"));
	CanonicalIO canonicalIO(&regularFileIO);
	ByteSource  source(&canonicalIO);

	source.seek(0);
	cout << source.isReadable() << endl;
	cout << source.isWritable() << endl;
	cout << source.isSeekable() << endl;

	source >> testBool;
	source >> testShort;
	source >> testuShort;
	source >> testInt;
	source >> testuInt;
	source >> testInt64;
	source >> testuInt64;
	source >> testFloat;
	source >> testDouble;
	source >> testComplex;
	source >> testDComplex;
	source >> testChar;
	source >> testuChar;
	source >> testString;

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

	source.read (nrOfTests, &testBool);
	source.read (nrOfTests, &testShort);
	source.read (nrOfTests, &testuShort);
	source.read (nrOfTests, &testInt);
	source.read (nrOfTests, &testuInt);
	source.read (nrOfTests, &testInt64);
	source.read (nrOfTests, &testuInt64);
	source.read (nrOfTests, &testFloat);
	source.read (nrOfTests, &testDouble);
	source.read (nrOfTests, &testComplex);
	source.read (nrOfTests, &testDComplex);
	source.read (nrOfTests, &testChar);
	source.read (nrOfTests, &testuChar);
	source.read (nrOfTests, &testString);
	
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
    return 0;
}
