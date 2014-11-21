//# tTypeIO.cc: Test program for class TypeIO and derived classes
//# Copyright (C) 1996,2001,2002
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
#include <casacore/casa/IO/CanonicalIO.h>
#include <casacore/casa/IO/LECanonicalIO.h>
#include <casacore/casa/IO/RawIO.h>
#include <casacore/casa/IO/ConversionIO.h>
#include <casacore/casa/OS/CanonicalDataConversion.h>
#include <casacore/casa/OS/LECanonicalDataConversion.h>
#include <casacore/casa/OS/IBMDataConversion.h>
#include <casacore/casa/OS/VAXDataConversion.h>
#include <casacore/casa/OS/RawDataConversion.h>
#include <casacore/casa/IO/RegularFileIO.h>
#include <casacore/casa/OS/RegularFile.h>
#include <casacore/casa/OS/Path.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/iostream.h>


#include <casacore/casa/namespace.h>
void doIt (TypeIO* io)
{
    // Save current file position.
    Int64 position = io->seek (0, ByteIO::Current);

    AlwaysAssertExit (io->isReadable());
    AlwaysAssertExit (io->isWritable());
    AlwaysAssertExit (io->isSeekable());

    Bool     testBool = True;
    Short    testShort = -30;
    uShort   testuShort = 10;
    Int      testInt = -20;
    uInt     testuInt = 80;
    Int64    testInt64 = -100000;
    uInt64   testuInt64 = 100000;
    Float    testFloat = 18.5;
    Double   testDouble = 23.5; 
    Complex  testComplex(2,3);
    DComplex testDComplex(2.5,3.8);
    Char     testChar = 'A';
    uChar    testuChar = 'B';
    String   testString("This is a teststring");

    io->write (1, &testBool);
    io->write (1, &testShort);
    io->write (1, &testuShort);
    io->write (1, &testInt);
    io->write (1, &testuInt);
    io->write (1, &testInt64);
    io->write (1, &testuInt64);
    io->write (1, &testFloat);
    io->write (1, &testDouble);
    io->write (1, &testComplex);
    io->write (1, &testDComplex);
    io->write (1, &testChar);
    io->write (1, &testuChar);
    io->write (1, &testString);

    io->seek (position);

    Bool     tBool;
    Short    tShort;
    uShort   tuShort;
    Int      tInt;
    uInt     tuInt;
    Int64    tInt64;
    uInt64   tuInt64;
    Float    tFloat;
    Double   tDouble; 
    Complex  tComplex;
    DComplex tDComplex;
    Char     tChar;
    uChar    tuChar;
    String   tString;
    
    io->read (1, &tBool);
    io->read (1, &tShort);
    io->read (1, &tuShort);
    io->read (1, &tInt);
    io->read (1, &tuInt);
    io->read (1, &tInt64);
    io->read (1, &tuInt64);
    io->read (1, &tFloat);
    io->read (1, &tDouble);
    io->read (1, &tComplex);
    io->read (1, &tDComplex);
    io->read (1, &tChar);
    io->read (1, &tuChar);
    io->read (1, &tString);

    AlwaysAssertExit (tBool == testBool);
    AlwaysAssertExit (tChar == testChar);
    AlwaysAssertExit (tuChar == testuChar);
    AlwaysAssertExit (tShort == testShort);
    AlwaysAssertExit (tuShort == testuShort);
    AlwaysAssertExit (tInt == testInt);
    AlwaysAssertExit (tuInt == testuInt);
    AlwaysAssertExit (tInt64 == testInt64);
    AlwaysAssertExit (tuInt64 == testuInt64);
    AlwaysAssertExit (tFloat == testFloat);
    AlwaysAssertExit (tDouble == testDouble);
    AlwaysAssertExit (tComplex == testComplex);
    AlwaysAssertExit (tDComplex == testDComplex);
    AlwaysAssertExit (tString == testString);
}


int main()
{
    RegularFileIO regularFileIO (Path("tTypeIO_tmp.dat"), ByteIO::New);
    CanonicalIO canonicalIO (&regularFileIO);
    doIt (&canonicalIO);

    LECanonicalIO lecanonicalIO (&regularFileIO);
    doIt (&lecanonicalIO);

    RawIO rawIO (&regularFileIO);
    doIt (&rawIO);

    CanonicalDataConversion canConv;
    ConversionIO canConvIO (&canConv, &regularFileIO);
    doIt (&canConvIO);
    
    LECanonicalDataConversion lecanConv;
    ConversionIO lecanConvIO (&lecanConv, &regularFileIO);
    doIt (&lecanConvIO);
    
    IBMDataConversion ibmConv;
    ConversionIO ibmConvIO (&ibmConv, &regularFileIO);
    doIt (&ibmConvIO);
    
    VAXDataConversion vaxConv;
    ConversionIO vaxConvIO (&vaxConv, &regularFileIO);
    doIt (&vaxConvIO);
    
    RawDataConversion rawConv;
    ConversionIO rawConvIO (&rawConv, &regularFileIO);
    doIt (&rawConvIO);
    
    cout << "OK" << endl;
    return 0;
}
