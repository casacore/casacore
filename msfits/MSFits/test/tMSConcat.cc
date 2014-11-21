#include <casacore/casa/aips.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/casa/iostream.h>

#include <casacore/ms/MeasurementSets/MSConcat.h>
#include <casacore/ms/MeasurementSets/MeasurementSet.h>
#include <casacore/msfits/MSFits/MSFitsInput.h>
#include <casacore/tables/Tables/Table.h>
#include <casacore/casa/Inputs.h>

#include <casacore/casa/namespace.h>
int main(int argc, const char* argv[])
{
  try {
    Input inputs(1);
    inputs.create("ms", "", "Initial measurement set");
    inputs.create("append", "", "Measurement set to append");
    inputs.create("fits", "", "Initial fits file");
    inputs.create("fitsappend", "", "Fits file to append");
    inputs.readArguments (argc, argv);
    
    const String fitsName = inputs.getString("fits");
    const String fitsAppendName = inputs.getString("fitsappend");
    const String msName = inputs.getString("ms");
    const String appendName = inputs.getString("append");
    if (!Table::isReadable(msName)) {
      if (fitsName.length() == 0) {
	String errorMsg = "Input ms called " + msName + " does not exist\n" +
	  " and no FITS file is specified";
	throw(AipsError(errorMsg));
      }
      cout << "Converting FITS file called " << fitsName 
	   << " to and MS called " << msName << endl;
      MSFitsInput msfitsin(msName, fitsName);
      msfitsin.readFitsFile();
    }
    if (!Table::isReadable(appendName)) {
      if (fitsAppendName.length() == 0) {
	String errorMsg = "Input ms called " + msName + " does not exist\n" +
	  " and no FITS file is specified";
	throw(AipsError(errorMsg));
      }
      cout << "Converting FITS file called " << fitsAppendName 
	   << " to and MS called " << appendName << endl;
      MSFitsInput msfitsin(appendName, fitsAppendName);
      msfitsin.readFitsFile();
    }
    if (!Table::isWritable(msName)) {
      throw(AipsError("MS to append to is not writable"));
    }
    if (!Table::isReadable(appendName)) {
      throw(AipsError("MS to append is not readable"));
    }
    MeasurementSet ms(msName, Table::Update);
    MeasurementSet appendedMS(appendName, Table::Old);
    MSConcat mscat(ms);
    mscat.concatenate(appendedMS);
  }
  catch (AipsError x) {
    cerr << x.getMesg() << endl;
    cout << "FAIL" << endl;
    return 1;
  }
  catch (...) {
    cerr << "Exception not derived from AipsError" << endl;
    cout << "FAIL" << endl;
    return 2;
  }
  cout << "OK" << endl;
  return 0;
}
// Local Variables: 
// compile-command: "gmake OPTLIB=1 tMSConcat"
// End: 
