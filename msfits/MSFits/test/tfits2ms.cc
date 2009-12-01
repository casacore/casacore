#include <casa/aips.h>
#include <casa/Exceptions/Error.h>
#include <casa/BasicSL/String.h>
#include <casa/iostream.h>

#include <ms/MeasurementSets/MSConcat.h>
#include <ms/MeasurementSets/MeasurementSet.h>
#include <msfits/MSFits/MSFitsInput.h>
#include <tables/Tables/Table.h>
#include <casa/Inputs.h>

#include <casa/namespace.h>

int main(int argc, const char* argv[])
{
  try {
    Input inputs(1);
    inputs.create("ms", "", "Initial measurement set");
    inputs.create("fits", "", "Initial fits file");
    inputs.readArguments (argc, argv);
    
    const String fitsName = inputs.getString("fits");
    const String msName = inputs.getString("ms");
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
  }
  catch (AipsError x) {
    cerr << x.getMesg() << endl;
    cout << "FAIL!!!" << endl;
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
// compile-command: "gmake OPTLIB=1 tfits2ms"
// End: 
