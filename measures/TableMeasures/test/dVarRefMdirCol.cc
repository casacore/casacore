//# tTableMeasures.cc: test program for the TableMeasures class.
//# Copyright (C) 1997,1998,1999,2000,2001
//# Associated Universities, Inc. Washington DC, USA.
//#
//# This library is free software; you can redistribute it and/or modify it
//# under the terms of the GNU Library General Public License as published by
//# the Free Software Foundation; either version 2 of the License, or (at your
//# option) any later version.
//#
//# This library is distributed in the hope that it will be useful, but WITHOUT
//# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
//# FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Library General Public
//# License for more details.
//#
//# You should have received a copy of the GNU Library General Public License
//# along with this library; if not, write to the Free Software Foundation,
//# Inc., 675 Massachusetts Ave, Cambridge, MA 02139, USA.
//#
//# Correspondence concerning AIPS++ should be addressed as follows:
//#        Internet email: aips2-request@nrao.edu.
//#        Postal address: AIPS++ Project Office
//#                        National Radio Astronomy Observatory
//#                        520 Edgemont Road
//#                        Charlottesville, VA 22903-2475 USA
//#
//# $Id$

#include <casacore/measures/TableMeasures/ScalarMeasColumn.h>
#include <casacore/measures/TableMeasures/ArrayMeasColumn.h>
#include <casacore/measures/TableMeasures/TableMeasValueDesc.h>
#include <casacore/measures/TableMeasures/TableMeasOffsetDesc.h>
#include <casacore/measures/TableMeasures/TableMeasRefDesc.h>
#include <casacore/measures/TableMeasures/TableMeasDesc.h>
#include <casacore/measures/TableMeasures/ArrayQuantColumn.h>
#include <casacore/measures/Measures/MEpoch.h>
#include <casacore/measures/Measures/MDirection.h>
#include <casacore/measures/Measures/MPosition.h>
#include <casacore/measures/Measures/MCEpoch.h>
#include <casacore/measures/Measures/MCDirection.h>
#include <casacore/measures/Measures/MCPosition.h>
#include <casacore/measures/Measures/MeasTable.h>
#include <casacore/measures/Measures/MeasData.h>
#include <casacore/measures/Measures/MeasRef.h>
#include <casacore/measures/Measures/MeasFrame.h>
#include <casacore/measures/Measures/MeasConvert.h>
#include <casacore/casa/Quanta/MVEpoch.h>
#include <casacore/casa/Quanta/MVTime.h>
#include <casacore/casa/Quanta/MVAngle.h>
#include <casacore/casa/Quanta/QLogical.h>
#include <casacore/casa/Quanta/Quantum.h>
#include <casacore/tables/Tables/Table.h>
#include <casacore/tables/Tables/TableDesc.h>
#include <casacore/tables/Tables/ArrayColumn.h>
#include <casacore/tables/Tables/ColumnDesc.h>
#include <casacore/tables/Tables/ArrColDesc.h>
#include <casacore/tables/Tables/SetupNewTab.h>
#include <casacore/tables/Tables/ScaColDesc.h>
#include <casacore/tables/Tables/ScalarColumn.h>
#include <casacore/tables/Tables/TableRecord.h>
#include <casacore/casa/Arrays/Array.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Arrays/ArrayLogical.h>
#include <casacore/casa/Arrays/ArrayIO.h>
#include <casacore/casa/Utilities/DataType.h>
#include <casacore/casa/Utilities/ValType.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/OS/Timer.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/iostream.h>

#include <casacore/casa/namespace.h>

// A demo of how to write variable ref MDirection column (i.e each row has a different
// ref)
// Filler writers should make use of it if FIELD and POINTING subtable etc if necessary 

int main()
{
  try {
    {
      TableDesc td;
      ArrayColumnDesc<Double> cdMDir("PHASE_CENTER", "Variable Ref MDIR test",
				     IPosition(1,2), ColumnDesc::Direct);
      ScalarColumnDesc<Int> cdMDirRef("PhaseCenter_Ref");
      td.addColumn(cdMDir);
      td.addColumn(cdMDirRef);
      TableMeasRefDesc tmrd(td, "PhaseCenter_Ref");
      TableMeasValueDesc tmvd(td, "PHASE_CENTER");
      TableMeasDesc<MDirection> tmdMDirection(tmvd, tmrd);
      tmdMDirection.write(td);
      SetupNewTable newtab("dVarRefMdirCol_tmp.tab", td, Table::New);
      Table tab(newtab, 4);
      MDirection::ScalarColumn tmpCol(tab, "PHASE_CENTER");
      tmpCol.put (0, MDirection(Quantity(0.0,"deg"), Quantity(0.0, "deg"),
                                MDirection::URANUS));
      tmpCol.put (1, MDirection(Quantity(150.0,"deg"), Quantity(15.0, "deg"),
                                MDirection::J2000));
      tmpCol.put (2, MDirection(Quantity(170.0,"deg"), Quantity(12.0, "deg"),
                                MDirection::B1950));
      tmpCol.put (3, MDirection(Quantity(80.0,"deg"), Quantity(10.0, "deg"),
                                MDirection::GALACTIC));
    }
    {
      //get the values now
      Table tab("dVarRefMdirCol_tmp.tab");
      MDirection::ScalarColumn tmpCol(tab, "PHASE_CENTER");
      for(uInt k =0 ; k < 4; ++k){
	String Ref = tmpCol(k).getRefString();
	MVAngle mvRA=tmpCol(k).getAngle().getValue()(0);
	MVAngle mvDEC=tmpCol(k).getAngle().getValue()(1);
	cout << "Row "<< k << " Ref " << Ref << ": "
             << mvRA(0.0).string(MVAngle::TIME,8) << ", "
             << mvDEC(0.0).string(MVAngle::ANGLE_CLEAN,8)  << endl;
      }
    }

  } catch (AipsError& x) {
    cout << "An error occurred.  The test ended early with the following";
    cout << " message:\n";
    cout << x.getMesg() << endl;
    return 1;
  }
  return 0;
}
