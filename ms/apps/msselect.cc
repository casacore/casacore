#include <ms/MeasurementSets/MSSelection.h>
#include <casa/Inputs/Input.h>
#include <iostream>

using namespace casa;
using namespace std;

int main (int argc, char* argv[])
{
  try {
    // enable input in no-prompt mode
    Input inputs(1);
    // define the input structure
    inputs.version("20100520GvD");
    inputs.create ("in", "",
		   "Name of input MeasurementSet",
		   "string");
    inputs.create ("out", "",
		   "Name of output table",
		   "string");
    inputs.create ("deep", "0",
		   "Is the output a deep copy of the MeasurementSet selection?" 
		   "int");
    inputs.create ("baseline", "",
                   "selection string for antennae and baselines",
                   "string");
    // Fill the input structure from the command line.
    inputs.readArguments (argc, argv);

    // Get and check the input specification.
    String msin (inputs.getString("in"));
    if (msin.empty()) {
      throw AipsError(" an input MeasurementSet must be given");
    }
    // Get the output name.
    String out(inputs.getString("out"));
    if (out.empty()) {
      throw AipsError(" an output table name must be given");
    }
    // Get the deep option.
    int deep = inputs.getInt("deep");
    // Get the baseline selection string.
    string baseline(inputs.getString("baseline"));

    MeasurementSet ms(msin);
    MSSelection select;
    // Set given selection strings.
    if (!baseline.empty()) {
      select.setAntennaExpr (baseline);
    }
    // Create a table expression over a MS representing the selection
    TableExprNode node = select.toTableExprNode (&ms);
    // Make the selection and write the resulting RefTable.
    Table mssel = ms(node);
    if (mssel.nrow() == ms.nrow()) {
      throw AipsError(" no selection has been made");
    }
    if (deep == 0) {
      mssel.rename (out, Table::New);
      cout << "Created RefTable " << out;
    } else {
      mssel.deepCopy (out, Table::New);
      cout << "Created MeasurementSet " << out;
    }
    cout << " containing " << mssel.nrow() << " rows (out of "
         << ms.nrow() << ')' << endl;
  } catch (std::exception& x) {
    cerr << "Error: " << x.what() << endl;
      return 1;
  }
  return 0;
}
