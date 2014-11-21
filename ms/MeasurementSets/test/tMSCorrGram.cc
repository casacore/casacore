
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


#include <casacore/ms/MeasurementSets/MSCorrGram.h>
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
  try {
    if(argc<3) {
      cout << " please input ms file and selection string on command line " << endl;
      return 0;
    }
    const String msName = argv[1];
    MeasurementSet ms(msName);
    MeasurementSet * mssel;
    cout << "Original table has rows " << ms.nrow() << endl;
    if(msCorrGramParseCommand(&ms, argv[2])==0) {
      const TableExprNode *node = msCorrGramParseNode();
      if(node->isNull()) {
	cout << "NULL node " << endl;
	return 0;
      }
      cout << "TableExprNode has rows = " << node->nrow() << endl;
      Table tablesel(ms.tableName(), Table::Update);
      mssel = new MeasurementSet(tablesel(*node, node->nrow() ));

      mssel->rename(ms.tableName()+"/SELECTED_TABLE", Table::Scratch);
      if(mssel->isColumnWritable("SELECTED_DATA"))
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
      cout << "failed to parse expression" << endl;
    }
  } catch (AipsError x) {
    cout << "ERROR: " << x.getMesg() << endl;
    return 1;
  } 
  return 0;
}
