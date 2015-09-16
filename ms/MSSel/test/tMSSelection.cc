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

#include <casa/aips.h>
#include <ms/MSSel/MSSelection.h>
#include <ms/MSSel/MSSelectionError.h>
#include <ms/MSSel/MSSelectionTools.h>
#include <ms/MSSel/MSSelectableTable.h>
#include <tables/Tables/Table.h>
#include <tables/Tables/TableCache.h>
#include <tables/Tables/PlainTable.h>
#include <casacore/casa/Inputs.h>

using namespace std;
using namespace casa;

//
//-------------------------------------------------------------------------
//
void UI(int argc, char **argv, string& MSNBuf, string& OutMSBuf, bool& deepCopy,
	string& fieldStr, string& timeStr, string& spwStr, string& baselineStr,
	string& scanStr, string& arrayStr, string& uvdistStr,string& taqlStr, string& polnStr,
	string& stateObsModeStr, string& observationStr)
{
      Input inputs(1);

      inputs.create("ms",MSNBuf,"Input MS Name");
      inputs.create("outms",OutMSBuf,"Output MS Name");
      inputs.create("deepcopy","0","Make a deepcopy in the output?");
      inputs.create("field",fieldStr,"FIELD selection expr.");
      inputs.create("time",timeStr,"TIME selection expr.");  
      inputs.create("spw",spwStr,"SPW selection expr.");  
      inputs.create("poln",polnStr,"POLN selection expr.");  
      inputs.create("baseline",baselineStr,"BASELINE selection expr.");  
      inputs.create("scan",scanStr,"SCAN selection expr.");  
      inputs.create("array",arrayStr,"ARRAY selection expr.");  
      inputs.create("uvdist",uvdistStr,"UVDIST selection expr.");  
      inputs.create("stateobsmode",stateObsModeStr,"STATE selection expr.");  
      inputs.create("observation",observationStr,"OBS selection expr.");  
      inputs.create("taql",taqlStr,"TaQL selection expr.");  
      inputs.readArguments(argc, argv);

      MSNBuf=inputs.getString("ms");
      OutMSBuf=inputs.getString("outms");
      deepCopy=inputs.getBool("deepcopy");
      fieldStr=inputs.getString("field");
      timeStr=inputs.getString("time");
      spwStr=inputs.getString("spw");
      polnStr=inputs.getString("poln");
      baselineStr=inputs.getString("baseline");
      scanStr=inputs.getString("scan");
      arrayStr=inputs.getString("array");
      uvdistStr=inputs.getString("uvdist");
      stateObsModeStr=inputs.getString("stateobsmode");
      observationStr=inputs.getString("observation");
      taqlStr=inputs.getString("taql");
}
//
//-------------------------------------------------------------------------
//
void showTableCache()
{
  const TableCache& cache = PlainTable::tableCache();
  Vector<String> lockedTables = cache.getTableNames();

  Int n=lockedTables.nelements();
  if(n > 0)
    cout << endl << "####WARNING!!!!: The Table Cache has the following " << n << " entries:"  << endl;
  
  for (Int i=0; i<n; ++i) 
    cout << "    " << i << ": \"" <<  lockedTables(i) << "\"" << endl;
}
//
//-------------------------------------------------------------------------
//
void printBaselineList(Matrix<Int> list,ostream& os)
{
  os << "\tBaselines = ";
  IPosition shp=list.shape();
  for(Int j=0;j<shp(1);j++)
    {
      for(Int i=0;i<shp(0);i++)
	os << list(i,j) << " ";
      os << endl << "\t            " ;
    }
  os << endl;
}
//
//-------------------------------------------------------------------------
//
void printInfo(MSSelection& msSelection, Int& nRows)
{
  cout << "BE: Baseline Expr=" << msSelection.getExpr(MSSelection::ANTENNA_EXPR) << endl;
  cout << "\tBE: Ant1         = " << msSelection.getAntenna1List() << endl;
  cout << "\tBE: Ant2         = " << msSelection.getAntenna2List() << endl;
  printBaselineList(msSelection.getBaselineList(),cout);
  //  cout << "Baselines    = " << msSelection.getBaselineList() << endl;

  cout << "FE: Field Expr=" << msSelection.getExpr(MSSelection::FIELD_EXPR) << endl;
  cout << "\tFE: Field        = " << msSelection.getFieldList()    << endl;

  cout << "SE: SPW Expr=" << msSelection.getExpr(MSSelection::SPW_EXPR) << endl;
  cout << "\tSE: SPW          = " << msSelection.getSpwList()      << endl;
  cout << "\tSE: Chan         = " << msSelection.getChanList(NULL,1,True)     << endl;
  //cout << "\tSE: Freq         = " << msSelection.getChanFreqList(NULL,True)     << endl;

  cout << "ScE: Scan Expr=" << msSelection.getExpr(MSSelection::SCAN_EXPR) << endl;
  cout << "\tScE: tScan         = " << msSelection.getScanList()     << endl;
  
  cout << "StE: STATE Expr=" << msSelection.getExpr(MSSelection::STATE_EXPR) << endl;
  cout << "\tStE: StateObsMode = " << msSelection.getStateObsModeList()     << endl;

  cout << "AE: Array Expr=" << msSelection.getExpr(MSSelection::ARRAY_EXPR) << endl;
  cout << "\tAE: Array        = " << msSelection.getSubArrayList() << endl;

  cout << "TE: Time Expr=" << msSelection.getExpr(MSSelection::TIME_EXPR) << endl;
  cout << "\tTE: Time         = " << msSelection.getTimeList()     << endl;

  cout << "UVE: UVRange Expr=" << msSelection.getExpr(MSSelection::UVDIST_EXPR) << endl;
  cout << "\tUVE: UVRange      = " << msSelection.getUVList()       << endl;
  cout << "\tUVE: UV in meters = " << msSelection.getUVUnitsList()  << endl;

  cout << "OE: Observation Expr=" << msSelection.getExpr(MSSelection::OBSERVATION_EXPR) << endl;
  cout << "\tOE: ObservationIDList    = " << msSelection.getObservationList() << endl;

  cout << "PE: Poln Expr=" << msSelection.getExpr(MSSelection::POLN_EXPR) << endl;
  cout << "\tPE: PolMap       = " << msSelection.getPolMap()       << endl;
  cout << "\tPE: CorrMap      = " << msSelection.getCorrMap( )     << endl;

  cout << endl << "===========================================================" << endl;
  cout << "DDIDs(Poln)  = " << msSelection.getDDIDList()     << endl;
  cout << "DDIDs(SPW)   = " << msSelection.getSPWDDIDList()     << endl;
  cout << "StateList    = " << msSelection.getStateObsModeList() << endl;

  cout << "Number of selected rows: " << nRows << endl;

  cout << "+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++" << endl;
}
//
//-------------------------------------------------------------------------
//
int main(int argc, char **argv)
{
  //
  //---------------------------------------------------
  //
  //  MSSelection msSelection;
  string MSNBuf,OutMSBuf,fieldStr,timeStr,spwStr,baselineStr,
    uvdistStr,taqlStr,scanStr,arrayStr, polnStr,stateObsModeStr,
    observationStr;
  Bool deepCopy=0;

  MSNBuf=OutMSBuf=fieldStr=timeStr=spwStr=baselineStr=
    uvdistStr=taqlStr=scanStr=arrayStr=polnStr=stateObsModeStr=observationStr="";
  deepCopy=0;
  fieldStr=spwStr="*";
  fieldStr=spwStr="";
  UI(argc, argv, MSNBuf,OutMSBuf, deepCopy,
     fieldStr,timeStr,spwStr,baselineStr,scanStr,arrayStr,
     uvdistStr,taqlStr,polnStr,stateObsModeStr,observationStr);
  //
  //---------------------------------------------------
  //
  //      MS ms(MSNBuf,Table::Update),selectedMS(ms);
  
  //
  // Make a new scope, outside of which there should be no tables left open.
  //
  {
    try
      {
	MS ms(MSNBuf,TableLock(TableLock::AutoNoReadLocking)),selectedMS(ms);
	//
	// Setup the MSSelection thingi
	//
    
	MSInterface msInterface(ms);
	MSSelection msSelection;
	MSSelectionLogError mssLEA,mssLES;
	msSelection.setErrorHandler(MSSelection::ANTENNA_EXPR, &mssLEA);
	msSelection.setErrorHandler(MSSelection::STATE_EXPR, &mssLES);

    	// msSelection.reset(ms,MSSelection::PARSE_NOW,
    	// 			timeStr,baselineStr,fieldStr,spwStr,
    	// 			uvdistStr,taqlStr,polnStr,scanStr,arrayStr,
    	// 			stateObsModeStr,observationStr);
    	msSelection.reset(msInterface,MSSelection::PARSE_NOW,
    			  timeStr,baselineStr,fieldStr,spwStr,
    			  uvdistStr,taqlStr,polnStr,scanStr,arrayStr,
    			  stateObsModeStr,observationStr);
    	// TableExprNode ten=msSelection.toTableExprNode(&msInterface);
    	// cerr << "TEN rows = " << ten.nrow() << endl;
	msSelection.getSelectedMS(selectedMS);
	Int nRows =  selectedMS.nrow();

    	printInfo(msSelection,nRows);

    	if (nRows==0)
    	  {
    	    cout << "###Informational:  Nothing selected.  ";
    	    if (OutMSBuf != "")
    	      cout << "New MS not written." << endl;
    	    else
    	      cout << endl;
    	  }
    	else
	  {
	    if (OutMSBuf != "")
	      {
	      if (deepCopy) selectedMS.deepCopy(OutMSBuf,Table::New);
	      else          selectedMS.rename(OutMSBuf,Table::New);
	      }
	  }
      }
    catch (MSSelectionError& x)
      {
    	cout << "###MSSelectionError: " << x.getMesg() << endl;
      }
    //
    // Catch any exception thrown by AIPS++ libs.  Do your cleanup here
    // before returning to the UI (if you choose to).  Without this, all
    // exceptions (AIPS++ or otherwise) are caught in the default
    // exception handler (which is installed by the CLLIB as the
    // clDefaultErrorHandler).
    //
    catch (AipsError& x)
      {
    	cout << "###AipsError: " << x.getMesg() << endl;
      }
  }

  showTableCache();
}
