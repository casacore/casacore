//# tTiledFileAccess.h: Test program for class TiledFileAccess
//# Copyright (C) 2001,2002
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


#include <casacore/tables/DataMan/TiledFileAccess.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/casa/Arrays/IPosition.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/Arrays/Array.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/Arrays/ArrayLogical.h>
#include <casacore/casa/Arrays/ArrayIO.h>
#include <casacore/casa/Arrays/Slicer.h>
#include <casacore/casa/IO/CanonicalIO.h>
#include <casacore/casa/IO/RawIO.h>
#include <casacore/casa/IO/RegularFileIO.h>
#include <casacore/casa/OS/RegularFile.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/iostream.h>


#include <casacore/casa/namespace.h>
int main()
{
  // Test for a Float array written in big endian canonical format starting
  // at offset 0.
  {
    IPosition shape(2,16,32);
    Array<Float> arr(shape);
    indgen(arr);
    {
      Bool deleteIt;
      const Float* dataPtr = arr.getStorage (deleteIt);
      RegularFileIO fios(RegularFile("tTiledFileAccess_tmp.dat"), ByteIO::New);
      CanonicalIO ios (&fios);
      ios.write (shape.product(), dataPtr);
      arr.freeStorage (dataPtr, deleteIt);
    }
    try {
      TiledFileAccess tfa ("tTiledFileAccess_tmp.dat", 0, shape,
			   IPosition(2,16,1), TpFloat, TSMOption::Cache,
                           False, True);
      AlwaysAssertExit (tfa.shape() == shape);
      AlwaysAssertExit (tfa.tileShape() == IPosition(2,16,1));
      AlwaysAssertExit (! tfa.isWritable());
      AlwaysAssertExit (tfa.maximumCacheSize() == 0);
      cout << tfa.cacheSize() << endl;
      tfa.setMaximumCacheSize (100000);
      AlwaysAssertExit (tfa.maximumCacheSize() == 100000);
      
      AlwaysAssertExit (allEQ (arr, tfa.getFloat (Slicer(IPosition(2,0,0),
							 shape))));
      tfa.showCacheStatistics (cout);
      cout << tfa.cacheSize() << endl;
    } catch (AipsError x) {
      cout << "Exception: " << x.getMesg() << endl;
      return 1;
    }
  }

  // Test for a Float array written in local format starting
  // at offset 1. The tile size is half the length of the first axis.
  // This assumes local format is the same as local canonical format
  // which is true for all data types except long.
  {
    IPosition shape(2,32,16);
    Array<Float> arr(shape);
    indgen(arr);
    uInt off2;
    {
      Bool deleteIt;
      const Float* dataPtr = arr.getStorage (deleteIt);
      RegularFileIO fios(RegularFile("tTiledFileAccess_tmp.dat"), ByteIO::New);
      RawIO ios (&fios);
      uChar nr  = 0;
      off2 = ios.write (1, &nr);
      ios.write (shape.product(), dataPtr);
      arr.freeStorage (dataPtr, deleteIt);
    }
    try {
      // The array starts at offset off2.
      TiledFileAccess tfa ("tTiledFileAccess_tmp.dat", off2, shape,
			   IPosition(2,16,1), TpFloat,
                           TSMOption(TSMOption::MMap, 0, 1));
      AlwaysAssertExit (tfa.shape() == shape);
      AlwaysAssertExit (tfa.tileShape() == IPosition(2,16,1));
      AlwaysAssertExit (! tfa.isWritable());
      AlwaysAssertExit (tfa.maximumCacheSize() == 1024*1024);
      cout << tfa.cacheSize() << endl;
      tfa.setMaximumCacheSize (100000);
      AlwaysAssertExit (tfa.maximumCacheSize() == 100000);
      AlwaysAssertExit (allEQ (arr, tfa.getFloat (Slicer(IPosition(2,0,0),
							 shape))));
      tfa.showCacheStatistics (cout);
      cout << tfa.cacheSize() << endl;
    } catch (AipsError x) {
      cout << "Exception: " << x.getMesg() << endl;
      return 1;
    }
  }

  // Test for a DComplex array written in canonical and in local format.
  // Open it writable and update the values.
  // Check it after writing by iterating through the data.
  {
    IPosition shape(2,17,40);
    Array<DComplex> arr(shape);
    indgen(arr);
    uInt off2;
    {
      Bool deleteIt;
      const DComplex* dataPtr = arr.getStorage (deleteIt);
      RegularFileIO fios(RegularFile("tTiledFileAccess_tmp.dat"), ByteIO::New);
      CanonicalIO ios (&fios);
      off2 = ios.write (shape.product(), dataPtr);
      RawIO cios (&fios);
      cios.write (shape.product(), dataPtr);
      arr.freeStorage (dataPtr, deleteIt);
    }
    try {
      Slicer slicer (IPosition(2,0,0), shape);
      TiledFileAccess tfac ("tTiledFileAccess_tmp.dat", 0, shape,
			    IPosition(2,17,1), TpDComplex,
                            TSMOption::Cache, True, True);
      AlwaysAssertExit (tfac.isWritable());
      AlwaysAssertExit (allEQ (arr, tfac.getDComplex (slicer)));
      AlwaysAssertExit (tfac.shape() == shape);
      AlwaysAssertExit (tfac.tileShape() == IPosition(2,17,1));
      TiledFileAccess tfal ("tTiledFileAccess_tmp.dat", off2, shape,
			    IPosition(2,17,1), TpDComplex,
                            TSMOption::Cache, True);
      AlwaysAssertExit (allEQ (arr, tfal.getDComplex (slicer)));
      tfac.put (tfac.getDComplex(slicer) + DComplex(1,2), slicer);
      tfal.put (tfal.getDComplex(slicer) + DComplex(3,5), slicer);
    } catch (AipsError x) {
      cout << "Exception: " << x.getMesg() << endl;
      return 1;
    }
    try {
      TiledFileAccess tfac ("tTiledFileAccess_tmp.dat", 0, shape,
			    IPosition(2,17,1), TpDComplex,
                            TSMOption::Buffer, True, True);
      AlwaysAssertExit (tfac.shape() == shape);
      AlwaysAssertExit (tfac.tileShape() == IPosition(2,17,1));
      TiledFileAccess tfal ("tTiledFileAccess_tmp.dat", off2, shape,
			    IPosition(2,17,1), TpDComplex,
                            TSMOption::Buffer, True);
      IPosition st(2,0,0);
      IPosition end(2,15,0);
      IPosition leng(2,16,1);
      for (Int i=0; i<shape(0); i++) {
	st(1) = i;
	end(1) = i;
	AlwaysAssertExit (allEQ (arr(st,end) + DComplex(1,2),
				 tfac.getDComplex (Slicer(st,leng))));
	AlwaysAssertExit (allEQ (arr(st,end) + DComplex(3,5),
				 tfal.getDComplex (Slicer(st,leng))));
      }
      cout << end << endl;
    } catch (AipsError x) {
      cout << "Exception: " << x.getMesg() << endl;
      return 1;
    }
  }

  // Test for a uChar array written in canonical format.
  // Read it also back as Float with a scale and offset.
  {
    IPosition shape(2,10,10);
    Array<uChar> arrs(shape);
    Array<Float> arrf(shape);
    Float scale = 2;
    Float offset = 2;
    indgen(arrs);
    indgen(arrf, float(2), float(2));
    {
      Bool deleteIt;
      const uChar* dataPtr = arrs.getStorage (deleteIt);
      RegularFileIO fios(RegularFile("tTiledFileAccess_tmp.dat"), ByteIO::New);
      CanonicalIO ios (&fios);
      ios.write (shape.product(), dataPtr);
      arrs.freeStorage (dataPtr, deleteIt);
    }
    try {
      Slicer slicer (IPosition(2,0,0), shape);
      TiledFileAccess tfac ("tTiledFileAccess_tmp.dat", 0, shape,
			    IPosition(2,10,5), TpUChar,
                            TSMOption::Cache, True, True);
      AlwaysAssertExit (allEQ (arrs, tfac.getUChar (slicer)));
      AlwaysAssertExit (allEQ (arrf, tfac.getFloat (slicer, scale, offset,
						    uChar(255))));
      AlwaysAssertExit (tfac.shape() == shape);
      AlwaysAssertExit (tfac.tileShape() == IPosition(2,10,5));
    } catch (AipsError x) {
      cout << "Exception: " << x.getMesg() << endl;
      return 1;
    }
  }

  // Test for a Short array written in canonical format.
  // Read it also back as Float with a scale and offset.
  {
    IPosition shape(2,17,40);
    Array<Short> arrs(shape);
    Array<Float> arrf(shape);
    Float scale = 2;
    Float offset = -10;
    indgen(arrs);
    indgen(arrf, float(-10), float(2));
    {
      Bool deleteIt;
      const Short* dataPtr = arrs.getStorage (deleteIt);
      RegularFileIO fios(RegularFile("tTiledFileAccess_tmp.dat"), ByteIO::New);
      CanonicalIO ios (&fios);
      ios.write (shape.product(), dataPtr);
      arrs.freeStorage (dataPtr, deleteIt);
    }
    try {
      Slicer slicer (IPosition(2,0,0), shape);
      TiledFileAccess tfac ("tTiledFileAccess_tmp.dat", 0, shape,
			    IPosition(2,17,4), TpShort,
                            TSMOption::Cache, True, True);
      AlwaysAssertExit (allEQ (arrs, tfac.getShort (slicer)));
      AlwaysAssertExit (allEQ (arrf, tfac.getFloat (slicer, scale, offset,
						    short(-32768))));
      AlwaysAssertExit (tfac.shape() == shape);
      AlwaysAssertExit (tfac.tileShape() == IPosition(2,17,4));
    } catch (AipsError x) {
      cout << "Exception: " << x.getMesg() << endl;
      return 1;
    }
  }

  // Test the tileShape function in various ways.
  {
    try {
      cout << TiledFileAccess::makeTileShape (IPosition(2,17,40)) << endl;
      cout << TiledFileAccess::makeTileShape (IPosition(2,17,40), 17) << endl;
      cout << TiledFileAccess::makeTileShape (IPosition(2,17,40), 34) << endl;
      cout << TiledFileAccess::makeTileShape (IPosition(2,17,40), 33) << endl;
      cout << TiledFileAccess::makeTileShape (IPosition(2,17,40), 15) << endl;
      cout << TiledFileAccess::makeTileShape (IPosition(2,17,40), 3) << endl;
    } catch (AipsError x) {
      cout << "Exception: " << x.getMesg() << endl;
      return 1;
    }
  }
  return 0;
}
