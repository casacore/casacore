#include <aips/aips.h>
#include <aips/Exceptions/Error.h>
#include <aips/Exceptions/Excp.h>
#include <aips/Utilities/String.h>
#include <iostream.h>

#include <trial/MeasurementSets/MSConcat.h>
#include <aips/MeasurementSets/MeasurementSet.h>
#include <aips/Tables/Table.h>
#include <aips/Inputs.h>

int main(int argc, char** argv) {
  try {
    Input inputs(1);
    inputs.create("ms", "", "Initial measurement set");
    inputs.create("append", "", "Measurement set to append");
    inputs.readArguments (argc, argv);
    
    MeasurementSet ms(inputs.getString("ms"), Table::Update);
    MeasurementSet appendedMS(inputs.getString("append"), Table::Old);
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
