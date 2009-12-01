
#include <casa/aips.h>
#include <casa/Exceptions/Error.h>
#include <casa/BasicSL/String.h>
#include <casa/iostream.h>


#include <tables/Tables/ExprNode.h>
#include <tables/Tables/RefRows.h>
#include <ms/MeasurementSets/MSColumns.h>
#include <casa/Arrays/Matrix.h>
#include <casa/Arrays/Cube.h>
#include <casa/Arrays/ArrayMath.h>
#include <casa/Arrays/ArrayUtil.h>
#include <casa/Logging/LogIO.h>
#include <casa/OS/File.h>
#include <casa/Containers/Record.h>
#include <casa/Utilities/Assert.h>
#include <casa/IO/AipsIO.h>
#include <casa/IO/ByteIO.h>


#include <ms/MeasurementSets/MSScanGram.h>
#include <ms/MeasurementSets/MeasurementSet.h>
#include <tables/Tables/Table.h>
#include <tables/Tables/TableDesc.h>
#include <tables/Tables/TableRecord.h>

#include <casa/iostream.h>

#include <casa/Utilities/Assert.h>
#include <casa/Inputs.h>

#include <casa/namespace.h>


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
    if(msScanGramParseCommand(&ms, "1", selectedIds)==0) {
      const TableExprNode *node = msScanGramParseNode();
      cout << "TableExprNode has rows = " << node->nrow() << endl;
      Table tablesel(ms.tableName(), Table::Update);
      mssel = new MeasurementSet(tablesel(*node, node->nrow() ));
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
      cout << "failed to parse expression" << endl;
    }
  } catch (AipsError x) {
    cout << "ERROR: " << x.getMesg() << endl;
    return 1;
  } 
  return 0;
}
