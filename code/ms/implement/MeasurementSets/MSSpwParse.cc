//# MSSpwParse.cc: Classes to hold results from spw grammar parser
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

#include <ms/MeasurementSets/MSSpwParse.h>
#include <ms/MeasurementSets/MSDataDescIndex.h>
#include <ms/MeasurementSets/MSSpWindowIndex.h>
#include <ms/MeasurementSets/MSPolIndex.h>
#include <ms/MeasurementSets/MSSourceColumns.h>
#include <casa/Logging/LogIO.h>
#include <casa/Arrays/Slicer.h>
#include <casa/Arrays/IPosition.h>
#include <tables/Tables/ArrColDesc.h>
#include <tables/Tables/ArrayColumn.h>


namespace casa { //# NAMESPACE CASA - BEGIN

TableExprNode* MSSpwParse::node_p = 0x0;

//# Constructor
MSSpwParse::MSSpwParse ()
: MSParse()
{
}

//# Constructor with given ms name.
MSSpwParse::MSSpwParse (const MeasurementSet* ms)
: MSParse(ms, "SPW")
{
    if(node_p) delete node_p;
    node_p = new TableExprNode();
}

const TableExprNode *MSSpwParse::selectSpwIds(const Vector<Int>& spwIds)
{
  LogIO os(LogOrigin("MSSpwParse", "selectSpwIds()", WHERE));
    // Look-up in DATA_DESC sub-table
    MSDataDescIndex msDDI(ms()->dataDescription());
    String colName = MS::columnName(MS::DATA_DESC_ID);

   TableExprNode condition =
       (ms()->col(colName).in(msDDI.matchSpwId(spwIds)));

    if(node_p->isNull())
        *node_p = condition;
    else
        *node_p = *node_p || condition;

    return node_p;
}

const TableExprNode *MSSpwParse::selectChaninASpw(const Int spw, const Int channel) 
{

  LogIO os(LogOrigin("MSSpwParse", "selectChaninASpw()", WHERE)); 
  /////////     work space        //////////////////
  MeasurementSet selms= Table(ms()->tableName(), Table::Update);
  if(!selms.isWritable()) {
    os << "Table is not writable " << LogIO::POST;
    //    exit(0);
  } 

  IPosition rowShape;
  Slicer slicer;

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
    selData.setShape(row,IPosition(2, rowShape(0), 1) );
  }

  //  Vector<Int> corrtype(nCorr);
  slicer = Slicer(IPosition(2, 0, channel-1), IPosition(2, nCorr.nelements()-1, channel-1 ), IPosition(2, 1, 1), Slicer::endIsLast);   

  Array<Complex> datacol = data.getColumn(slicer);

  selData.putColumn( Slicer(IPosition(2, 0, 0), IPosition(2, rowShape(0)-1, 0 ), IPosition(2, 1, 1), Slicer::endIsLast), datacol);
  
  // To tableExprNode
  MSDataDescIndex msDDI(selms.dataDescription());
  String colName = MS::columnName(MS::DATA_DESC_ID);

  TableExprNode condition =
    (ms()->col(colName).in(msDDI.matchSpwId(spw)));
  ///////////////////////////////////////////////////////////////
  if(node_p->isNull())
    *node_p = condition;
  else
    *node_p = *node_p || condition;
  
  return node_p;
}

const TableExprNode *MSSpwParse::selectChanRangeinASpw(const Int spw, const Int startChan, const Int endChan) 
{
  ////////////////// work space /////////////////////////////
  LogIO os(LogOrigin("MSSpwParse", "selectChanRangeinASpw()", WHERE)); 
  MeasurementSet selms= Table(ms()->tableName(), Table::Update);
  if(!selms.isWritable()) {
    os << "Table is not writable " << endl;
    //    exit(0);
  } 

  IPosition rowShape;
  Slicer slicer;

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
    selData.setShape(row,IPosition(2, rowShape(0), endChan-startChan+1) );
  }

  //  Vector<Int> corrtype(nCorr);
  slicer = Slicer(IPosition(2, 0, startChan-1), IPosition(2, nCorr.nelements()-1, endChan-1 ), IPosition(2, 1, 1), Slicer::endIsLast);   

  Array<Complex> datacol = data.getColumn(slicer);

  selData.putColumn( Slicer(IPosition(2, 0, 0), IPosition(2, rowShape(0)-1, endChan-startChan ), IPosition(2, 1, 1), Slicer::endIsLast), datacol);
  
  // To tableExprNode
  MSDataDescIndex msDDI(selms.dataDescription());
  String colName = MS::columnName(MS::DATA_DESC_ID);

  TableExprNode condition =
    (ms()->col(colName).in(msDDI.matchSpwId(spw)));

  if(node_p->isNull())
    *node_p = condition;
  else
    *node_p = *node_p || condition;
  
  return node_p;
}

const TableExprNode *MSSpwParse::selectVelRangeinASpw(const Int spw, const Double startVel, const Double endVel) 
{
  LogIO os(LogOrigin("MSSpwParse", "selectVelRangeinASpw()", WHERE)); 
  Double factor = 1000000;
  //---------------------------------------------------------------
  if(!ms()->source().isReadable(ms()->source().tableName())){
    os <<" Source table does not exist, No rest Frequency! " << LogIO::POST;
    exit(0);
  }
  ROMSSourceColumns msSrcCol( ms()->source());
  ROArrayColumn<Double> restFreqCol = msSrcCol.restFrequency();
  if(restFreqCol.nrow()==0){
    os <<" Source table is empty, No rest Frequency! " << LogIO::POST;
    exit(0);
  }
  Array<Double> restFreqArray = restFreqCol.getColumn();
  IPosition ip = restFreqArray.shape();
  Vector<Double> restFreqVec(restFreqArray.nonDegenerate());
  Double restFreq = restFreqVec(0);
  // Note: Large velocity means small frequency
  Double startFreq = restFreq / (1000 * endVel/C::c + 1) / factor;
  Double endFreq = restFreq / (1000 * startVel/C::c + 1) / factor;
  return selectFreRangeinASpw(spw, startFreq, endFreq);

}

const TableExprNode *MSSpwParse::selectFreRangeinASpw(const Int spw, const Double startFreq, const Double endFreq) 
{
  LogIO os(LogOrigin("MSSpwParse", "selectFreRangeinASpw()", WHERE)); 
  //Convert between MHz and Hz
  Double factor = 1000000;
  long double adjStartFreq = 0;
  long double adjEndFreq = 0;
  Int startChan = 0;
  Int endChan = 0;
  ROMSSpWindowColumns msSpwCol( ms()->spectralWindow());
  Array<Double> freqArray = msSpwCol.chanFreq().getColumn();
  //ROArrayColumn<Double> freqArray = msSpwCol.chanFreq();
  IPosition ips = freqArray.shape();

  Array<Double> freq;
  if(spw < ips(1)) {
    freq = freqArray(Slicer(IPosition(2, 0, spw), IPosition(2, ips(0)-1, spw), IPosition(2, 1, 1), Slicer::endIsLast));
    IPosition fps = freq.shape();
  } else {
    os <<" spw id is not in range " << LogIO::POST;
  }
  
  Vector<Double> freqVec(freq.nonDegenerate());
  Int numChan = freqVec.nelements();

  if(startFreq <= freqVec(0)/factor){
    adjStartFreq = freqVec(0)/factor;
  } else {
    adjStartFreq = startFreq;
  }
  if(endFreq >= freqVec(numChan - 1)/factor) {
    adjEndFreq = freqVec(numChan - 1)/factor;
  } else {
    adjEndFreq = endFreq;
  }

  if(startFreq >= freqVec(numChan-1)/factor || endFreq <= freqVec(0)/factor){
    os <<" Selection is not in the range! " <<LogIO::POST;
    exit(0);
  }
  //  cout << " adj startFreq " << adjStartFreq << " adj endFreq " << adjEndFreq << endl;
  //Channel starts from 1
  for (Int i = 0; i < numChan - 1 ; i++) {
    if ( adjStartFreq == freqVec(i)/factor ) {
      startChan= i + 1;
      break;
    } else if( freqVec(i)/factor < adjStartFreq && freqVec(i+1)/factor > adjStartFreq) {
      startChan = i + 2;
      break;
    }
  }
  // Find end channel
  for (Int i = numChan - 1; i > 1; i--) {
    if ( freqVec(i)/factor == adjEndFreq) {
      endChan= i+1;
      break;
    } else if ( freqVec(i)/factor > adjEndFreq && freqVec(i-1)/factor < adjEndFreq){
      endChan = i;
      break;
    } 
  }
  
  //  cout << " startChan  " << startChan << " endChan " << endChan << endl;
  if(startChan > endChan ) {
    os <<" Start is greater than End ! " <<LogIO::POST;
    exit(0);
  }
  return selectChanRangeinASpw(spw, startChan, endChan);
  //////////////////////////////////////////////////////////////////
}

const TableExprNode *MSSpwParse::selectSpwName(const String& name)
{
    const String colName = MS::columnName(MS::DATA_DESC_ID);
    bool selectName;

    ROMSSpWindowColumns msSWC(ms()->spectralWindow());
    ROScalarColumn<String> names(msSWC.name());

    for (uInt i = 0; i < names.getColumn().nelements(); i++)
    {
        if(strcmp(names(i).chars(), name.chars())==0)
            selectName = True;
    }

    TableExprNode condition;
    if(selectName)
    {
        MSSpWindowIndex msSWI(ms()->spectralWindow());
        condition = 0;
    }
    else
        condition = 0;


    if(node_p->isNull())
        *node_p = condition;
    else
        *node_p = *node_p || condition;

    return node_p;
}

const TableExprNode* MSSpwParse::node()
{
    return node_p;
}

} //# NAMESPACE CASA - END
