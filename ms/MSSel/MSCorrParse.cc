//# MSCorrParse.cc: Classes to hold results from corr grammar parser
//# Copyright (C) 1994,1995,1997,1998,1999,2000,2001,2003
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

#include <casacore/ms/MSSel/MSCorrParse.h>
#include <casacore/ms/MSSel/MSDataDescIndex.h>
#include <casacore/ms/MSSel/MSPolIndex.h>
#include <casacore/ms/MeasurementSets/MSMainColumns.h>
#include <casacore/ms/MeasurementSets/MSColumns.h>
#include <casacore/tables/TaQL/RecordGram.h>
#include <casacore/casa/Arrays/Slicer.h>
#include <casacore/casa/Arrays/Slice.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/Arrays/IPosition.h>
#include <casacore/casa/Arrays/ArrayUtil.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/tables/Tables/ArrColDesc.h>
#include <casacore/tables/Tables/ArrayColumn.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

TableExprNode* MSCorrParse::node_p = 0x0;

//# Constructor
MSCorrParse::MSCorrParse ()
: MSParse()
{
}

//# Constructor with given ms name.
MSCorrParse::MSCorrParse (const MeasurementSet* ms)
: MSParse(ms, "Corr")
{
    if(node_p) delete node_p;
    node_p = new TableExprNode();
}

// MS selection
const TableExprNode *MSCorrParse::selectCorrType(const String& corrType)
{
  
  MeasurementSet selms= Table(ms()->tableName(), Table::Update);
  if(!selms.isWritable()) {
    cout << "Table is not writable " << endl;
    return NULL;
  } 

  IPosition rowShape;
  Slicer slicer;
  Bool corrTypeExist = False;

  ROArrayColumn<Complex> data(selms, MS::columnName(MS::DATA));
  TableDesc tdSel;
  String colSel = "SELECTED_DATA";

  if(selms.tableDesc().isColumn("SELECTED_DATA")) {
    selms.removeColumn("SELECTED_DATA");
  }

  ColumnDesc & cdSel = tdSel.addColumn(ArrayColumnDesc<Complex>(colSel," selected data", 2));					
  selms.addColumn(cdSel);
  
  ArrayColumn<Complex> selData(selms, "SELECTED_DATA");
  
  ROMSPolarizationColumns polc(selms.polarization());
  Array<Int> corrtypeArray = polc.corrType().getColumn().nonDegenerate();
  IPosition ip = corrtypeArray.shape();

  Vector<Int> nCorr(corrtypeArray);

  for (uInt row=0; row < selms.nrow(); row++) {
    rowShape=data.shape(row);
    selData.setShape(row,IPosition(2, 1, rowShape(1)) );
  }

  Vector<Int> corrtype(nCorr);
  if(nCorr.nelements() != 0) {
    for (uInt i = 0; i < nCorr.nelements(); i ++) {
      if(nCorr(i) == Stokes::type(corrType)){
	slicer = Slicer(IPosition(2, i, 0), IPosition(2, i, rowShape(1)-1 ), IPosition(2, 1, 1), Slicer::endIsLast);   
	corrTypeExist = True;
      }
    }
  }
  if(!corrTypeExist) {
    cout << " corrtype " << corrType << " does not exist" << endl;
    return NULL;
  }

  Array<Complex> datacol = data.getColumn(slicer);

  selData.putColumn( Slicer(IPosition(2, 0, 0), IPosition(2, 0, rowShape(1)-1 ), IPosition(2, 1, 1), Slicer::endIsLast), datacol);
  
  // To tableExprNode
  MSDataDescIndex msDDI(selms.dataDescription());
  String colName = MS::columnName(MS::DATA_DESC_ID);
  MSPolarizationIndex msPI(selms.polarization());

  TableExprNode condition = 
    selms.col(colName).in(msDDI.matchPolId(msPI.matchCorrType(corrtype)));
    
  if(node_p->isNull()){
    *node_p = condition;
  }
  else
    *node_p = *node_p || condition;
  
  return node_p;
}

const TableExprNode* MSCorrParse::node()
{
    return node_p;
}

} //# NAMESPACE CASACORE - END
