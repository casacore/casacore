#include <aips/Glish.h>
#include <aips/Arrays/Vector.h>
#include <aips/Arrays/VectorIter.h>
#include <aips/Arrays/VectorSTLIterator.h>
#include <aips/Utilities/String.h>

#include <aips/Exceptions/Error.h>
#include <aips/MeasurementSets/MeasurementSet.h>

#include <trial/MeasurementSets/MSSelection.h>
#include <trial/MeasurementSets/MSExpr.h>
#include <trial/MeasurementSets/MSAntennaExpr.h>
#include <trial/MeasurementSets/MSCorrExpr.h>
#include <trial/MeasurementSets/MSFieldExpr.h>
#include <trial/MeasurementSets/MSSPWExpr.h>
#include <trial/MeasurementSets/MSTimeExpr.h>
#include <trial/MeasurementSets/MSUVDistExpr.h>

#include <aips/iostream.h>


#include <aips/Utilities/Assert.h>
#include <aips/Arrays/ArrayLogical.h>
#include <aips/Arrays/Cube.h>
#include <aips/Arrays/IPosition.h>
#include <aips/Containers/Record.h>
#include <aips/Containers/RecordDesc.h>
#include <aips/Containers/RecordField.h>
#include <aips/Tables/TableRecord.h>


int main(int argc, char **argv)
{
    try {

      MeasurementSet ms("5921.ms");
      /*
      GlishRecord gr;
      gr.add("FIELD_ID", 1);           
      cout << "glish record is created" << endl;
      MSSelection mysel(gr);
      cout << "expression is available" << endl;
      */
      MSSelection mysel;
      mysel.setFieldExpr("'field_id = 1'");
      mysel.toTableExprNode(ms);

    } catch (AipsError x) {
	cout << "ERROR: " << x.getMesg() << endl;
	return 1;
    } 
    return 0;
}
