//#include <tasking/Glish.h>
#include <casa/Arrays/Vector.h>
#include <casa/Arrays/VectorIter.h>
#include <casa/Arrays/VectorSTLIterator.h>
#include <casa/BasicSL/String.h>

#include <casa/Exceptions/Error.h>
#include <ms/MeasurementSets/MeasurementSet.h>

#include <ms/MeasurementSets/MSSelection.h>
#include <casa/iostream.h>


#include <casa/Utilities/Assert.h>
#include <casa/Arrays/ArrayLogical.h>
#include <casa/Arrays/Cube.h>
#include <casa/Arrays/IPosition.h>
#include <casa/Containers/Record.h>
#include <casa/Containers/RecordDesc.h>
#include <casa/Containers/RecordField.h>
#include <tables/Tables/TableRecord.h>

#include <casa/namespace.h>

// This is a very simple test, not in the repository
int main(int argc, char **argv)
{
    try {
        const String name = "5921.ms.raw";
        MeasurementSet ms(name);

        MSSelection select;
        select.setUVDistExpr("uvdist='3727km:5%'");
        select.setFieldExpr("field='>0'");

        cout << "Original table has rows " << ms.nrow() << endl;

        select.toTableExprNode(ms);

        cout << "TableExprNode has rows = " << MSSelection::msTableExprNode->nrow() << endl;

        Table tablesel(ms.tableName(), Table::Update);
        MeasurementSet mssel(tablesel(*MSSelection::msTableExprNode, MSSelection::msTableExprNode->nrow() ));

        cout << "After mssel constructor called " << endl;
        mssel.rename(ms.tableName()+"/SELECTED_TABLE", Table::Scratch);
        mssel.flush();
        if(mssel.nrow()==0){
          cout << "Check your input, No data selected" << endl;
        }
        else {
          cout << "selected table has rows " << mssel.nrow() << endl;
        }
        delete MSSelection::msTableExprNode;
    } catch (AipsError x) {
        cout << "ERROR: " << x.getMesg() << endl;
    }

    return 1;
}
