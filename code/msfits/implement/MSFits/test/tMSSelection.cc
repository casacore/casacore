//#include <tasking/Glish.h>
#include <casa/Arrays/Vector.h>
#include <casa/Arrays/VectorIter.h>
#include <casa/Arrays/VectorSTLIterator.h>
#include <casa/BasicSL/String.h>

#include <casa/Exceptions/Error.h>
#include <ms/MeasurementSets/MeasurementSet.h>

#include <ms/MeasurementSets/MSSelection.h>
/*
#include <ms/MeasurementSets/MSExpr.h>
#include <ms/MeasurementSets/MSAntennaExpr.h>
#include <ms/MeasurementSets/MSCorrExpr.h>
#include <ms/MeasurementSets/MSFieldExpr.h>
#include <ms/MeasurementSets/MSSPWExpr.h>
#include <ms/MeasurementSets/MSTimeExpr.h>
#include <ms/MeasurementSets/MSUVDistExpr.h>
*/
#include <casa/iostream.h>


#include <casa/Utilities/Assert.h>
#include <casa/Arrays/ArrayLogical.h>
#include <casa/Arrays/Cube.h>
#include <casa/Arrays/IPosition.h>
#include <casa/Containers/Record.h>
#include <casa/Containers/RecordDesc.h>
#include <casa/Containers/RecordField.h>
#include <tables/Tables/TableRecord.h>


// This is a very simple test, not in the repository
int main(int argc, char **argv)
{
  /*
    try {

      MeasurementSet ms("3C273XC1.ms");

      MSSelection mysel;
      //      mysel.setFieldExpr("'field_id = 1'");
      mysel.setCorrExpr("'RL'");
      mysel.toTableExprNode(ms);

    } catch (AipsError x) {
	cout << "ERROR: " << x.getMesg() << endl;
	return 1;
    } 
  */
    return 0;
}

      /*
      GlishRecord gr;
      gr.add("FIELD_ID", 1);           
      cout << "glish record is created" << endl;
      MSSelection mysel(gr);
      cout << "expression is available" << endl;
      */
