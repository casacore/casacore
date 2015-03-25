//# Copyright (C) 1995,1996,1997,1999,2001,2002,2005
//# Associated Universities, Inc. Washington DC, USA.
//#
//# This program is free software; you can redistribute it and/or modify it
//# under the terms of the GNU General Public License as published by the Free
//# Software Foundation; either version 2 of the License, or (at your option)
//# any later version.
//#
//# This program is distributed in the hope that it will be useful, but WITHOUT
//# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
//# FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
//# more details.
//#
//# You should have received a copy of the GNU General Public License along
//# with this program; if not, write to the Free Software Foundation, Inc.,
//# 675 Massachusetts Ave, Cambridge, MA 02139, USA.
//#
//# Correspondence concerning AIPS++ should be addressed as follows:
//#        Internet email: aips2-request@nrao.edu.
//#        Postal address: AIPS++ Project Office
//#                        National Radio Astronomy Observatory
//#                        520 Edgemont Road
//#                        Charlottesville, VA 22903-2475 USA
//#
//# $Id$

#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Arrays/VectorIter.h>
#include <casacore/casa/Arrays/VectorSTLIterator.h>
#include <casacore/casa/BasicSL/String.h>

#include <casacore/casa/Exceptions/Error.h>
#include <casacore/ms/MeasurementSets/MeasurementSet.h>

#include <casacore/ms/MSSel/MSSelection.h>
#include <casacore/casa/iostream.h>


#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Arrays/ArrayLogical.h>
#include <casacore/casa/Arrays/Cube.h>
#include <casacore/casa/Arrays/IPosition.h>
#include <casacore/casa/Containers/Record.h>
#include <casacore/casa/Containers/RecordDesc.h>
#include <casacore/casa/Containers/RecordField.h>
#include <casacore/tables/Tables/TableRecord.h>

#include <casacore/msfits/MSFits/MSFitsInput.h>
#include <casacore/tables/Tables/Table.h>
#include <casacore/casa/Inputs.h>

#include <casacore/casa/namespace.h>

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
int main(int argc, const char* argv[])
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
    for(int i=0; i<4; i++) {
        MeasurementSet ms(msName);

        MSSelection select;
        switch(i) {
            case 0:
                select.setFieldExpr("0");
                select.setSpwExpr("0");
                break;
            case 1:
                select.setFieldExpr("1");
                select.setSpwExpr("1");
                break;
            case 2:
                select.setFieldExpr("0,1");
                select.setSpwExpr("0,1");
                break;
            case 3:
                select.setFieldExpr(">0");
                select.setSpwExpr("0");
                break;
            default:
                break;
        }

        cout << "Original table has rows " << ms.nrow() << endl;

        TableExprNode node = select.toTableExprNode(&ms);

        MS *mssel_ = new MS((ms)(node));
        delete mssel_;

        cout << "TableExprNode has rows = " << node.nrow() << endl;

        Table tablesel(ms.tableName(), Table::Update);
        MeasurementSet mssel(tablesel(node, node.nrow() ));

        cout << "After mssel constructor called " << endl;
        mssel.rename(ms.tableName()+"/SELECTED_TABLE", Table::Scratch);
        mssel.flush();
        if(mssel.nrow()==0){
          cout << "Check your input, No data selected" << endl;
        }
        else {
          cout << "selected table has rows " << mssel.nrow() << endl;
        }
    }
    } catch (AipsError x) {
        cout << "ERROR: " << x.getMesg() << endl;
    }

    return 1;
}
