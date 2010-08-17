
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
#include <ms/MeasurementSets/MSSelection.h>


#include <ms/MeasurementSets/MSAntennaGram.h>
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
  try 
    {
      if(argc < 3) 
	cout << "Please input ms file and selection string on command line " << endl;
      
      const String msName = argv[1];
      cout << "ms file is  " << msName << endl;
      MeasurementSet ms(msName);
      MSSelection mss;
      for(Int i=2;i<argc;i++)
	{
	  cout << "Parsing expression: " << argv[i] << endl;
	  mss.setAntennaExpr(String(argv[i]));
	}
      TableExprNode node = mss.toTableExprNode(&ms);
      
      MeasurementSet* mssel = 0;
      cout << "Original table has rows " << ms.nrow() << endl;
      if(node.isNull()) 
	{
	  cout << "NULL node " << endl;
	  return 0;
	}
      cout << "TableExprNode has rows = " << node.nrow() << endl;
      Table tablesel(ms.tableName(), Table::Update);
      mssel = new MeasurementSet(tablesel(node, node.nrow() ));
      mssel->rename(ms.tableName()+"/SELECTED_TABLE", Table::Scratch);
      mssel->flush();
      if(mssel->nrow()==0) 
        cout << "Check your input, No data selected" << endl;
      else 
	{
	  cout << "selected table has rows " << mssel->nrow() << endl;
	  cout << "selected ant1 = " << mss.getAntenna1List() << endl
	       << "selected ant2 = " << mss.getAntenna2List() << endl;
	}
      delete mssel;
    } 
  catch (AipsError x) 
    {
      cout << "ERROR: " << x.getMesg() << endl;
      return 1;
    } 
  return 0;
}
