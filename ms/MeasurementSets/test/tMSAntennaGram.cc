
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


#include <casacore/ms/MeasurementSets/MSAntennaGram.h>
#include <casacore/ms/MeasurementSets/MeasurementSet.h>
#include <casacore/ms/MeasurementSets/MSSelection.h>
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
    if(argc < 3) {
      cout << "Please input ms file and selection string on command line " << endl;
      return 3;
    } 
    const String msName = argv[1];
    cout << "ms file is  " << msName << endl;
    MeasurementSet ms(msName);
    MSSelection mss;
    mss.setAntennaExpr(String(argv[2]));
    TableExprNode node = mss.toTableExprNode(&ms);

    MeasurementSet* mssel = 0;
    cout << "Original table has rows " << ms.nrow() << endl;
    Vector<Int> selectedAnt1, selectedAnt2;
    Matrix<Int> selectedBaselines;
    node = msAntennaGramParseCommand(&ms, argv[2],
                                     selectedAnt1, selectedAnt2,
                                     selectedBaselines);
    if(node.isNull()) {
      cout << "NULL node " << endl;
      return 0;
    }
    cout << "TableExprNode has rows = " << node.nrow() << endl;
    Table tablesel(ms.tableName(), Table::Update);
    mssel = new MeasurementSet(tablesel(node, node.nrow() ));
    mssel->rename(ms.tableName()+"/SELECTED_TABLE", Table::New);
    mssel->flush();
    if(mssel->nrow()==0) {
      cout << "Check your input, No data selected" << endl;
    }
    else {
      cout << "selected table has rows " << mssel->nrow() << endl;
      cout << "selected ant1 = " << selectedAnt1 << endl
           << "selected ant2 = " << selectedAnt2 << endl;
    }
    delete mssel;
  } catch (AipsError& x) {
    cout << "ERROR: " << x.getMesg() << endl;
    return 1;
  } 
  return 0;
}
