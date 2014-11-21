
#include <casacore/casa/aips.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/casa/iostream.h>


#include <casacore/tables/TaQL/ExprNode.h>
#include <casacore/tables/Tables/RefRows.h>
#include <casacore/ms/MeasurementSets/MSColumns.h>
#include <casacore/casa/Arrays/Matrix.h>
#include <casacore/casa/Arrays/Cube.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/Arrays/ArrayUtil.h>
#include <casacore/casa/Logging/LogIO.h>
#include <casacore/casa/OS/File.h>
#include <casacore/casa/Containers/Record.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/IO/AipsIO.h>
#include <casacore/casa/IO/ByteIO.h>


#include <casacore/ms/MeasurementSets/MSScanGram.h>
#include <casacore/ms/MeasurementSets/MeasurementSet.h>
#include <casacore/tables/Tables/Table.h>
#include <casacore/tables/Tables/TableDesc.h>
#include <casacore/tables/Tables/TableRecord.h>

#include <casacore/casa/iostream.h>

#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Inputs.h>

#include <casacore/casa/namespace.h>


int main(int argc, const char* argv[])
{
  if (argc != 2) {
    cout << "Usage: "<< argv[0] << " MS_filename" << endl;
    return 0;
  }
  try {
    cout << "before ms constructor called " << endl;
    const String msName = argv[1];
    MeasurementSet ms(msName);
    MeasurementSet * mssel;
    cout << "Original table has rows " << ms.nrow() << endl;
    Vector<Int> selectedIds;
    const TableExprNode node = msScanGramParseCommand(&ms, "1", selectedIds);
    if (!node.isNull()) {
      cout << "TableExprNode has rows = " << node.nrow() << endl;
      Table tablesel(ms.tableName(), Table::Update);
      mssel = new MeasurementSet(tablesel(node, node.nrow() ));
      cout << "After mssel constructor called " << endl;
      mssel->rename(ms.tableName()+"/SELECTED_TABLE", Table::Scratch);
      mssel->flush();
      if(mssel->nrow()==0) {
	cout << "Check your input, No data selected" << endl;
      }
      else {
	cout << "selected table has rows " << mssel->nrow() << endl;
      }
      delete mssel;
    }
    else {
      cout << "ERROR: failed to parse expression " << endl;
    }
  } catch (AipsError x) {
    cout << "ERROR: " << x.getMesg() << endl;
    return 1;
  } 
  return 0;
}
