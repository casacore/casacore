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

#include <ms/MeasurementSets/MSFitsInput.h>
#include <tables/Tables/Table.h>
#include <casa/Inputs.h>

#include <casa/namespace.h>

void convert(String fitsName, String msName)
{
    if (!Table::isReadable(msName)) {
        cout << "Converting FITS file called " << fitsName
             << " to and MS called " << msName << endl;
        MSFitsInput msfitsin(msName, fitsName);
        msfitsin.readFitsFile();
    }
}

// This is a very simple test, not in the repository
int main(int argc, char **argv)
{
    Input inputs(1);
    inputs.create("ms", "", "Initial measurement set");
    inputs.create("fits", "", "Initial fits file");
    inputs.readArguments (argc, argv);

    const String fitsName = inputs.getString("fits");
    String msName = inputs.getString("ms");

    if(msName.length() == 0) msName = "3C273XC1_tmp.ms";
    if(fitsName.length() != 0) convert(fitsName, msName);

    // Do selection over newly created ms
    try {
        MeasurementSet ms(msName);

        MSSelection select;
        select.setUVDistExpr("uvdist='3727km:5%'");
//        select.setFieldExpr("field='>0'");

        cout << "Original table has rows " << ms.nrow() << endl;

        TableExprNode node = select.toTableExprNode(ms);

        cout << "TableExprNode has rows = " << node.nrow() << endl;

        Table tablesel(ms.tableName(), Table::Update);
        MeasurementSet mssel(tablesel(&node, node.nrow() ));

        cout << "After mssel constructor called " << endl;
        mssel.rename(ms.tableName()+"/SELECTED_TABLE", Table::Scratch);
        mssel.flush();
        if(mssel.nrow()==0){
          cout << "Check your input, No data selected" << endl;
        }
        else {
          cout << "selected table has rows " << mssel.nrow() << endl;
        }
    } catch (AipsError x) {
        cout << "ERROR: " << x.getMesg() << endl;
    }

    return 1;
}
