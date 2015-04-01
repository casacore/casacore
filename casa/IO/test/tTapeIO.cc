//# tTapeIO.cc: Test program for TapeIO class
//# Copyright (C) 1995,1996,1999,2000,2001
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
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/casa/OS/Path.h>
#include <casacore/casa/iostream.h>
#include <casacore/casa/iomanip.h>
#include <casacore/casa/OS/Timer.h>
#include <casacore/casa/BasicMath/Random.h>
#include <casacore/casa/Inputs.h>
#include <casacore/casa/IO/TapeIO.h>

#include <casacore/casa/namespace.h>
int main(int argc, const char* argv[])
{
  try {
    Input inputs(1);
    inputs.create ("device", "", "Tape device name");
    inputs.create ("bytes", "32768000", "Number of bytes to transfer", 
		   "Int", "1-10000000000");
    inputs.create ("record", "32768", "Number of bytes in each record", 
		   "Int", "1-262144");
    inputs.create ("check", "False", "Verify if the data read is correct", 
		   "Bool");
    inputs.create ("interactive", "True", "Don't run this test automatically", 
		   "Bool");
    inputs.readArguments (argc, argv);

    if (inputs.getBool("interactive") == False) {
      cout << "UNTESTED" << endl;
      return 3;
    }
    const Path device(inputs.getString("device"));
    const uInt recordSize = inputs.getInt("record");
    uInt totalBytes = inputs.getInt("bytes");
    const Bool checkContents = inputs.getBool("check");;
    const uInt nRecords = totalBytes/recordSize;
    totalBytes = nRecords * recordSize; // To account for roundoff.
    uChar* writeBuffer = new uChar[recordSize];
    { // Do the write test
      TapeIO tape(device, True);
      AlwaysAssert(tape.isWritable(), AipsError);
      tape.rewind();
      ACG g;
      DiscreteUniform rand(&g, 0, 255);
      for (uInt i = 0; i < recordSize; i++) {
	writeBuffer[i] = static_cast<uChar>(rand.asInt());
      }
      Timer clock;
      for (uInt i = 0; i < nRecords; i++) {
	tape.write(recordSize, writeBuffer);
      }
      Double elapsedTime = clock.real();
      cout << "It took " << elapsedTime << " seconds to write " 
	   << totalBytes/1024 << " kbytes using a record size of " 
	   << recordSize << " bytes" << endl;
      cout << "Transfer rate is: " <<  totalBytes/elapsedTime/1024 
	   << " kbytes/sec" << endl;
    }
    cout << "PASSED the write test" << endl;
    { // Do the skip test
      TapeIO tape(device);
      tape.rewind();
      AlwaysAssert(tape.isReadable(), AipsError);
      uChar* readBuffer = new uChar[recordSize];
      Int bytesRead = tape.read(recordSize, readBuffer, False);
      AlwaysAssert(bytesRead == Int(recordSize), AipsError);
      tape.skip(1);
      bytesRead = tape.read(recordSize, readBuffer, False);
      AlwaysAssert(bytesRead == 0, AipsError);
      delete [] readBuffer;
    }
    cout << "PASSED the skip test" << endl;
    { // Do the read test
      const int fd = TapeIO::open(device);
      TapeIO tape(fd);
      AlwaysAssert(tape.isReadable(), AipsError);
      tape.rewind();
      uChar* readBuffer = new uChar[recordSize];
      for (uInt i = 0; i < recordSize; i++) {
	*(readBuffer + i) = uChar(0xa5);
      }
      Timer clock;
      for (uInt l = 0; l < nRecords; l++) {
	tape.read(recordSize, readBuffer);
	if (checkContents) {
	  for (uInt i = 0; i < recordSize; i++) {
	    AlwaysAssert(readBuffer[i] == writeBuffer[i], AipsError);
	  }
 	}
      }
      Double elapsedTime = clock.real();
      cout << "It took " << elapsedTime << " seconds to read " 
	   << totalBytes/1024 << " kbytes using a record size of " 
	   << recordSize << " bytes" << endl;
      cout << "Transfer rate is: " <<  totalBytes/elapsedTime/1024 
	   << " kbytes/sec" << endl;
      tape.rewind();
      TapeIO::close(fd);
      delete [] readBuffer;
      cout << "PASSED the read test" << endl;
    }
    delete [] writeBuffer;
  }
  catch (AipsError x) {
    cerr << x.getMesg() << endl;
    cout << "FAIL" << endl;
    return 1;
  } 
  cout << "OK" << endl;
}

// Local Variables: 
// compile-command: "gmake OPTLIB=1 tTapeIO"
// End: 
