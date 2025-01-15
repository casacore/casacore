//# tMultiFileLarge.cc: Test program for class MultiFile getting aborted
//# Copyright (C) 2014
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
//#        Internet email: casa-feedback@nrao.edu.
//#        Postal address: AIPS++ Project Office
//#                        National Radio Astronomy Observatory
//#                        520 Edgemont Road
//#                        Charlottesville, VA 22903-2475 USA

//# Includes
#include <casacore/casa/IO/MultiFile.h>
#include <casacore/casa/IO/MFFileIO.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/Arrays/ArrayLogical.h>
#include <casacore/casa/IO/ArrayIO.h>
#include <casacore/casa/BasicSL/STLIO.h>
#include <casacore/casa/OS/Timer.h>
#include <iostream>
#include <stdexcept>

using namespace casacore;

namespace casacore {
  
  // Derive from MultiFile to make a special test mode possible
  // for extremely large files. Writing all data takes too long, but
  // it makes it possible to test such an index.
  class MultiFileLarge : public MultiFile
  {
  public:
    MultiFileLarge (const String& name, ByteIO::OpenOption option,
                    Int blockSize, Bool useODirect, Bool useCRC,
                    Int testMode)
      : MultiFile (name, option, blockSize, useODirect, useCRC),
        itsTestMode (testMode)
    {}
    ~MultiFileLarge() override
      { flush(); }
    void readBlock (MultiFileInfo&, Int64, void*) override;
    void writeBlock (MultiFileInfo&, Int64, const void*) override;
    void extendVF (MultiFileInfo& info, Int64 lastblk, Bool useFreeBlocks) override;
    void writeHeaderShow (Int64 ncont, Int64 todo) const override;
    void writeHeaderTest() override;
  private:
    Int itsTestMode;
  };

  void MultiFileLarge::readBlock (MultiFileInfo& info, Int64 blknr,
                                 void* buffer)
  {
    if (itsTestMode != 0) {
      MultiFile::readBlock (info, blknr, buffer);
    }
  }

  void MultiFileLarge::writeBlock (MultiFileInfo& info, Int64 blknr,
                              const void* buffer)
  {
    if (itsTestMode != 0) {
      MultiFile::writeBlock (info, blknr, buffer);
    } else if (itsUseCRC) {
      storeCRC (buffer, info.blockNrs[blknr]);
    }
  }

  void MultiFileLarge::extendVF (MultiFileInfo& info, Int64 lastblk, Bool useFreeBlocks)
  {
    Int64 nrb1 = 0;
    Int64 nrb2 = 0;
    if (!useFreeBlocks  &&  itsTestMode != 0) {
      nrb1 = itsNrBlock;
      itsNrBlock = 1 + itsHdrCont[0].blockNrs.size() + itsHdrCont[1].blockNrs.size();
      nrb2 = itsNrBlock;
    }
    MultiFile::extendVF (info, lastblk, useFreeBlocks);
    if (!useFreeBlocks  &&  itsTestMode != 0) {
      // Reset the real size. Blocks might have been added for the header.
      itsNrBlock = nrb1 + itsNrBlock - nrb2;
      cout << "test mode: " << itsNrBlock << ' '<<nrb1<<' '<<nrb2<<' '<<endl;
    }
  }

  void MultiFileLarge::writeHeaderShow (Int64 ncont, Int64 todo) const
  {
    if (itsTestMode != 0) {
      int newContInx = 1 - itsHdrContInx;
      const std::vector<Int64>& hdrContNrs = itsHdrCont[newContInx].blockNrs;
      cout << ' ' << newContInx << itsHdrCont[0].blockNrs << itsHdrCont[1].blockNrs
           << hdrContNrs[0] << ' ' << ncont << ' ' << todo << endl;
    }
  }

  void MultiFileLarge::writeHeaderTest()
  {
    if (itsTestMode > 0  &&  itsNrBlock > itsTestMode) {
      // Mimic an abort by not writing the first header block.
      cout << "MultiFileLarge::writeHeader ended prematurely for nrBlock="
           << itsNrBlock << endl;
      exit (0);            // abort
    }
  }
}



void readFile (const String& name)
{
  MultiFile mfile(name, ByteIO::Old);
  AlwaysAssertExit (! mfile.isWritable());
  mfile.show (cout);
  for (uInt i=0; i<mfile.info().size(); ++i) {
    String nm = "file" + String::toString(i);
    cout << nm << ' ' << mfile.fileId(nm, False) << endl;
  }
}

void testLarge (Bool oDirect, Bool useCRC, int testMode)
{
  cout << "Test with ODirect=" << oDirect << ", useCRC=" << useCRC << endl;
  {
    // Use 4 KB blocks, but optionally use test mode (do not write actual data).
    MultiFileLarge mfile("tMultiFileLarge_tmp.dat", ByteIO::New, 4096,
                         oDirect, useCRC, testMode);
    Int fid0 = mfile.createFile ("file0");
    Int fid1 = mfile.createFile ("file1");
    Vector<Int64> buf(4096/sizeof(Int64));
    indgen(buf);
    Int64 offs0 = 0;
    Int64 offs1 = 0;
    for (size_t i=0; i<1024; ++i) {
      for (size_t j=0; j<128; ++j) {
        mfile.write (fid0, buf.data(), 4096, offs0);
        offs0 += 4096;
      }
      for (size_t j=0; j<32; ++j) {
        mfile.write (fid1, buf.data(), 4096, offs1);
        offs1 += 4096;
      }
      // Do an occasional flush.
      if (i%200 == 5) {
        mfile.flush();
      }
    }
    AlwaysAssertExit (mfile.info()[fid0].buffer.get());
    mfile.closeFile (fid0);
    AlwaysAssertExit (! mfile.info()[fid0].buffer.get());
    AlwaysAssertExit (mfile.info()[fid1].buffer.get());
    mfile.closeFile (fid1);
    AlwaysAssertExit (! mfile.info()[fid1].buffer.get());
  }
  cout << "read the large file" << endl;
  readFile ("tMultiFileLarge_tmp.dat");
  cout << endl;
}


int main (int argc, char* argv[])
{
  // Run as:   tMultiFileLarge testMode
  //    testMode = 0:  do several tests for large files without writing data
  //    testMode > 0:  create 1000000 blocks without writing data and exit prematurely
  //    testMode < 0:  read the file produced by testMode>0
  try {
    Int testMode = 0;      // normal 
    if (argc > 1) {
      testMode = atoi(argv[1]);
    }
    if (testMode == 0) {
      // Test a large MultiFile without actually writing data.
      testLarge (False, False, -1);
      testLarge (False, True, -1);
      testLarge (True, False, -1);
      testLarge (True, True, -1);
    } else if (testMode < 0) {
      readFile ("tMultiFileLarge_tmp.dat");
    } else {
      // Test a large MultiFile forcing an exception when itsNrBlock > 100000
      testLarge (False, False, 100000);
    }
  } catch (std::exception& x) {
    cout << "Unexpected exception: " << x.what() << endl;
    return 1;
  }
  cout << "tMultiFileLarge ended OK" << endl;
  return 0;
}
