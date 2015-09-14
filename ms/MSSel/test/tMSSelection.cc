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
#define RestartUI(Label)  {if(clIsInteractive()) {goto Label;}}
//#define RestartUI(Label)  {if(clIsInteractive()) {clRetry();goto Label;}}
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
    cerr << endl << "####WARNING!!!!: The Table Cache has the following " << n << " entries:"  << endl;
  
  for (Int i=0; i<n; ++i) 
    cerr << "    " << i << ": \"" <<  lockedTables(i) << "\"" << endl;
}
//
//-------------------------------------------------------------------------
//
void printBaselineList(Matrix<Int> list,ostream& os)
{
  os << "Baselines = ";
  IPosition shp=list.shape();
  for(Int j=0;j<shp(1);j++)
    {
      for(Int i=0;i<shp(0);i++)
	os << list(i,j) << " ";
      os << endl << "            " ;
    }
  os << endl;
}
//
//-------------------------------------------------------------------------
//
void printInfo(MSSelection& msSelection)
{
  cout << "Ant1         = " << msSelection.getAntenna1List() << endl;
  cout << "Ant2         = " << msSelection.getAntenna2List() << endl;
  //  cout << "Baselines    = " << msSelection.getBaselineList() << endl;
  cout << "Field        = " << msSelection.getFieldList()    << endl;
  cout << "SPW          = " << msSelection.getSpwList()      << endl;
  cout << "Chan         = " << msSelection.getChanList(NULL,1,True)     << endl;
  cout << "Freq         = " << msSelection.getChanFreqList(NULL,True)     << endl;
  cout << "Scan         = " << msSelection.getScanList()     << endl;
  cout << "StateObsMode = " << msSelection.getStateObsModeList()     << endl;
  cout << "Array        = " << msSelection.getSubArrayList() << endl;
  cout << "Time         = " << msSelection.getTimeList()     << endl;
  cout << "UVRange      = " << msSelection.getUVList()       << endl;
  cout << "UV in meters = " << msSelection.getUVUnitsList()  << endl;
  cout << "DDIDs(Poln)  = " << msSelection.getDDIDList()     << endl;
  cout << "DDIDs(SPW)   = " << msSelection.getSPWDDIDList()     << endl;
  cout << "PolMap       = " << msSelection.getPolMap()       << endl;
  cout << "CorrMap      = " << msSelection.getCorrMap( )     << endl;
  cout << "StateList    = " << msSelection.getStateObsModeList() << endl;
  cout << "ObservationIDList    = " << msSelection.getObservationList() << endl;
  printBaselineList(msSelection.getBaselineList(),cout);
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
    	printInfo(msSelection);
	msSelection.getSelectedMS(selectedMS);
    	if (selectedMS.nrow()==0)
    	  {
    	    cerr << "###Informational:  Nothing selected.  ";
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
    	cerr << "Number of selected rows: " << selectedMS.nrow() << endl;
      }
    catch (MSSelectionError& x)
      {
    	cerr << "###MSSelectionError: " << x.getMesg() << endl;
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
    	cerr << "###AipsError: " << x.getMesg() << endl;
      }
  }

  showTableCache();
}
