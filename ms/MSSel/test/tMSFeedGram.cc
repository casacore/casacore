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


#include <casacore/ms/MSSel/MSFeedGram.h>
#include <casacore/ms/MeasurementSets/MeasurementSet.h>
#include <casacore/ms/MSSel/MSSelection.h>
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
    mss.setFeedExpr(String(argv[2]));
    cout << "feed selection is " << String(argv[2]) << endl;
    TableExprNode node = mss.toTableExprNode(&ms);

    cout << "Original table has rows " << ms.nrow() << endl;
    Vector<Int> selectedFeed1, selectedFeed2;
    Matrix<Int> selectedFeedPairs;
    node = msFeedGramParseCommand(&ms, argv[2],
                                     selectedFeed1, selectedFeed2,
                                     selectedFeedPairs);
    if(node.isNull()) {
      cout << "NULL node " << endl;
      return 0;
    }
    Table tablesel(ms.tableName(), Table::Update);
    MeasurementSet* mssel = new MeasurementSet(tablesel(node, node.nrow() ));
    mssel->rename(ms.tableName()+"/SELECTED_TABLE", Table::New);
    mssel->flush();
    if(mssel->nrow()==0) {
      cout << "Check your input, No data selected" << endl;
    }
    else {
      cout << "Selected table has nrows " << mssel->nrow() << endl;
      cout << "Selected feed1 = " << selectedFeed1 << endl
           << "Selected feed2 = " << selectedFeed2 << endl;
    }
    delete mssel;
  } catch (std::exception& x) {
    cout << "ERROR: " << x.what() << endl;
    return 1;
  } 
  return 0;
}
