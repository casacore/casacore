#include <aips/aips.h>
#include <aips/Exceptions/Error.h>
#include <aips/Exceptions/Excp.h>
#include <aips/Utilities/Assert.h>
#include <aips/Utilities/String.h>
#include <aips/OS/Path.h>
#include <iostream.h>
#include <iomanip.h>
#include <aips/OS/Timer.h>
#include <aips/Mathematics/Random.h>
#include <aips/Inputs.h>
#include <trial/IO/TapeIO.h>

int main(int argc, char* argv[]) {
  try {
    Input inputs(1);
    inputs.Create ("device", "", "Tape device name");
    inputs.Create ("bytes", "32768000", "Number of bytes to transfer", 
		   "Int", "1-10000000000");
    inputs.Create ("record", "32768", "Number of bytes in each record", 
		   "Int", "1-262144");
    inputs.Create ("check", "False", "Verify if the data read is correct", 
		   "Bool");
    inputs.ReadArguments (argc, argv);

    const Path device(inputs.GetString("device"));
    const uInt recordSize = inputs.GetInt("record");
    uInt totalBytes = inputs.GetInt("bytes");
    const Bool checkContents = inputs.GetBool("check");;
    const uInt nRecords = totalBytes/recordSize;
    totalBytes = nRecords * recordSize; // To account for roundoff.
    uChar* writeBuffer = new uChar[recordSize];
    { // Do the write test
      TapeIO tape(device, True);
      AlwaysAssert(tape.isWritable(), AipsError);
      tape.rewind();
      RandomInteger rand(0L, 255L, new ACG());
      for (uInt i = 0; i < recordSize; i++) {
	writeBuffer[i] = (uChar) rand.asInt();
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
  } end_try;
  cout << "OK" << endl;
}

// Local Variables: 
// compile-command: "gmake OPTLIB=1 tTapeIO"
// End: 
