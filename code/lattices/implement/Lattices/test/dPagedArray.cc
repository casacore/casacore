//# dPagedArray.cc:  this contains the examples from the PagedArray.h file
//# Copyright (C) 1997,1998
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

#include <aips/aips.h>
#include <trial/Lattices/PagedArray.h>
#include <trial/Lattices/LatticeIterator.h>
#include <trial/Lattices/LatticeStepper.h>
#include <trial/Lattices/TiledLineStepper.h>
#include <aips/Arrays/Array.h>
#include <aips/Arrays/ArrayMath.h>
#include <aips/Exceptions/Error.h>
#include <aips/Functionals/Gaussian1D.h>
#include <aips/Lattices/IPosition.h>
#include <aips/OS/Timer.h>
#include <aips/Tables/SetupNewTab.h>
#include <aips/Tables/Table.h>
#include <aips/Tables/TableDesc.h>
#include <aips/Utilities/COWPtr.h>
#include <aips/Utilities/String.h>

int main(){
  try {
    // Create a PagedArray of Floats of shape [512,512,4,32] in a file
    // and initialise it to zero. This will create a directory on disk
    // called "dPagedArray_tmp.data" that contains files that
    // exceed 512*512*4*32*4 (=128MBytes) in size.
    const String filename("dPagedArray_tmp.data");
    {
      const IPosition arrayShape(4,512,512,4,32);
      PagedArray<Float> diskArray(arrayShape, filename);
      cout << "Created a PagedArray of shape " << diskArray.shape() 
	   << " (" << diskArray.shape().product()/1024/1024*sizeof(Float) 
	   << " MBytes)" << endl
	   << "in the table called " << diskArray.tableName() << endl;
      Timer timer;
      diskArray.set (0.0f);
      timer.show ("set          ");
      diskArray.showCacheStatistics (cout);
      // Using the set function is an efficient way to initialise the PagedArray
      // as it uses a PagedArrIter internally. Note that the set function is
      // defined in the Lattice class that PagedArray is derived from. 
    }
    // Read the PagedArray produced in Example 1 and put a Gaussian profile into
    // each spectral channel.
    {
      PagedArray<Float> diskArray(filename);
      IPosition shape = diskArray.shape();
      // Time how long it takes to iterate without doing IO.
      {
        RO_LatticeIterator<Float> iter(diskArray,
				       TiledLineStepper(shape,
							diskArray.tileShape(),
							3));
        Timer timer;
        for (iter.reset(); !iter.atEnd(); iter++) {
	}
        timer.show ("iterate, no IO");
	diskArray.showCacheStatistics (cout);
      }
      // Time how long it takes to iterate witt doing input only.
      {
        RO_LatticeIterator<Float> iter(diskArray,
				       TiledLineStepper(shape,
							diskArray.tileShape(),
							3));
        Timer timer;
        for (iter.reset(); !iter.atEnd(); iter++) {
	  iter.cursor();
	}
        timer.show ("iterate, input");
	diskArray.showCacheStatistics (cout);
      }
      // Construct a Gaussian Profile to be 10 channels wide and centred on
      // channel 16. Its height is 1.0.
      Gaussian1D<Float> g(1.0f, 16.0f, 10.0f);
      // Create a vector to cache a sampled version of this profile.
      Array<Float> profile(IPosition(4,1,1,1,shape(3)));
      indgen(profile);
      profile.apply(g);
      // Now put this profile into every spectral channel in the paged array.
      // This is best done using an iterator.
      LatticeIterator<Float> iter(diskArray,
				  TiledLineStepper(shape,
						   diskArray.tileShape(),
						   3));
      Timer timer;
      for (iter.reset(); !iter.atEnd(); iter++) {
	iter.woCursor() = profile;
      }
      timer.show ("set vectors   ");
      diskArray.showCacheStatistics (cout);
    }
    // Now multiply the I-polarization data by 10.0 in this PagedArray. The
    // I-polarization data occupies 32MBytes of RAM which is too big to read
    // into the memory of most computers. So an iterator is used to get suitable
    // sized chunks.
    {
      Table t(filename, Table::Update);
      PagedArray<Float> da(t);
      const IPosition latticeShape = da.shape();
      const nx = latticeShape(0);
      const ny = latticeShape(1);
///      const npol = latticeShape(2);
      const nchan = latticeShape(3);
      IPosition cursorShape = da.niceCursorShape (da.maxPixels());
      cursorShape(2) = 1;
      LatticeStepper step(latticeShape, cursorShape);
      step.subSection (IPosition(4,0), IPosition(4,nx-1,ny-1,0,nchan-1));
      LatticeIterator<Float> iter(da, step);
      Timer timer;
      for (iter.reset(); !iter.atEnd(); iter++) {
	iter.rwCursor() *= 10.0f;
      }
      timer.show ("set I-pol     ");
      da.showCacheStatistics (cout);
    }
    // Use a direct call to getSlice to access a small central region of the
    // V-polarization in spectral channel 0 only. The region is small enough
    // to not warrent constructing iterators and setting up
    // LatticeNavigators. In this example the call to the getSlice function
    // is unnecessary but is done for illustration purposes anyway.
    {
      SetupNewTable maskSetup(filename, TableDesc(), Table::New);
      Table maskTable(maskSetup);
      PagedArray<Bool> maskArray(IPosition(4, 512, 512, 4, 32), maskTable);
      Timer timer;
      maskArray.set(False);
      timer.show ("setmask");
      COWPtr<Array<Bool> > maskPtr;
      timer.mark();
      maskArray.getSlice (maskPtr, IPosition(4,240,240,3,0),
			  IPosition(4,32,32,1,1), IPosition(4,1));
      timer.show ("getmask      ");
      maskPtr.rwRef() = True;
      timer.mark();
      maskArray.putSlice (*maskPtr, IPosition(4,240,240,3,1));
      timer.show ("putmask");
      maskArray.showCacheStatistics (cout);
    }
    // In this example the data in the PagedArray will be accessed a row at
    // a time while setting the cache size to different values. The comments
    // illustrate the results when running on an Ultra 1/140 with 64MBytes
    // of memory.
    {
      PagedArray<Float> pa(IPosition(4,128,128,4,32), filename);
      const IPosition latticeShape = pa.shape();
      cout << "The tile shape is:" << pa.tileShape() << endl;
      // Setup to access the PagedArray a row at a time
      const IPosition sliceShape(4,latticeShape(0), 1, 1, 1);
      const IPosition stride(4,1);
      Array<Float> row(sliceShape);
      IPosition start(4, 0);
      
      // Set the cache size to enough pixels for one tile only. This uses
      // 128kBytes of cache memory and takes 125 secs
      pa.setCacheSizeInTiles (1);
      Timer timer;
      for (start(3) = 0; start(3) < latticeShape(3); start(3)++) {
	for (start(2) = 0; start(2) < latticeShape(2); start(2)++) {
	  for (start(1) = 0; start(1) < latticeShape(1); start(1)++){
	    pa.getSlice (row,  start, sliceShape, stride);
	  }
	}
      }
      timer.show();
      pa.showCacheStatistics (cout);
      pa.clearCache();
      
      // Set the cache size to enough pixels for one row of tiles (ie. 4)
      // This uses 512 kBytes of cache memory and takes 10 secs
      pa.setCacheSizeInTiles (4);
      timer.mark();
      for (start(3) = 0; start(3) < latticeShape(3); start(3)++) {
	for (start(2) = 0; start(2) < latticeShape(2); start(2)++) {
          for (start(1) = 0; start(1) < latticeShape(1); start(1)++) {
	    pa.getSlice (row,  start, sliceShape, stride);
	  }
	}
      }
      timer.show();
      pa.showCacheStatistics (cout);
      pa.clearCache();
      
      // Set the cache size to enough pixels for one plane of tiles
      // (ie. 4*8) This uses 4MBytes of cache memory and takes 2 secs
      pa.setCacheSizeInTiles (4*8);
      timer.mark();
      for (start(3) = 0; start(3) < latticeShape(3); start(3)++) {
	for (start(2) = 0; start(2) < latticeShape(2); start(2)++) {
	  for (start(1) = 0; start(1) < latticeShape(1); start(1)++) {
	    pa.getSlice (row,  start, sliceShape, stride);
	  }
	}
      }
      timer.show();
      pa.showCacheStatistics (cout);
      pa.clearCache();
    }
  } catch (AipsError x) {
    cerr << x.getMesg() << endl;
    cout << "FAIL" << endl;
    return 1;
  } end_try;
  cout << "OK" << endl;
  return 0;
}
