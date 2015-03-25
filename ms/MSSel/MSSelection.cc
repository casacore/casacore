//# MSSelection.cc: Implementation of MSSelection.h
//# Copyright (C) 1996,1997,1998,1999,2000,2001,2002,2003
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
//----------------------------------------------------------------------------

#include <casacore/ms/MSSel/MSSelection.h>
#include <casacore/ms/MSSel/MSAntennaGram.h>
#include <casacore/ms/MSSel/MSCorrGram.h>
#include <casacore/ms/MSSel/MSFieldGram.h>
#include <casacore/ms/MSSel/MSSpwGram.h>
#include <casacore/ms/MSSel/MSScanGram.h>
#include <casacore/ms/MSSel/MSArrayGram.h>
#include <casacore/ms/MSSel/MSTimeGram.h>
#include <casacore/ms/MSSel/MSUvDistGram.h>
#include <casacore/ms/MSSel/MSPolnGram.h>
#include <casacore/ms/MSSel/MSStateGram.h>
#include <casacore/ms/MSSel/MSObservationGram.h>
#include <casacore/ms/MeasurementSets/MSRange.h>
#include <casacore/tables/TaQL/TableParse.h>
#include <casacore/tables/TaQL/RecordGram.h>

#include <casacore/ms/MeasurementSets/MSMainColumns.h>
#include <casacore/measures/Measures/MeasureHolder.h>
#include <casacore/measures/Measures/MEpoch.h>
#include <casacore/casa/Quanta/MVTime.h>
#include <casacore/casa/Quanta/QuantumHolder.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/Containers/Record.h>
#include <casacore/casa/Utilities/DataType.h>
#include <casacore/casa/iostream.h>
#include <casacore/ms/MSSel/MSSelectionError.h>
#include <casacore/ms/MSSel/MSSelectionTools.h>
#include <casacore/ms/MSSel/MSSelectableTable.h>
#include <casacore/ms/MSSel/MSSelectableMainColumn.h>
#include <casacore/ms/MSSel/MSAntennaParse.h>
#include <casacore/ms/MSSel/MSStateParse.h>
#include <casacore/casa/Logging/LogIO.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/Utilities/GenSort.h>
#include <casacore/ms/MeasurementSets/MSColumns.h>
namespace casacore { //# NAMESPACE CASACORE - BEGIN
  
  //----------------------------------------------------------------------------
  
  MSSelection::MSSelection() : 
    fullTEN_p(),ms_p(NULL),  antennaExpr_p(""), fieldExpr_p(""),
    spwExpr_p(""), scanExpr_p(""), arrayExpr_p(""), timeExpr_p(""), uvDistExpr_p(""),
    polnExpr_p(""), taqlExpr_p(""), stateExpr_p(""), observationExpr_p(""),
    exprOrder_p(MAX_EXPR, NO_EXPR), antenna1IDs_p(), antenna2IDs_p(), fieldIDs_p(), 
    spwIDs_p(), scanIDs_p(), arrayIDs_p(), ddIDs_p(), observationIDs_p(), baselineIDs_p(),
    selectedTimesList_p(), selectedUVRange_p(),selectedUVUnits_p(),
    selectedPolMap_p(Vector<Int>(0)), selectedSetupMap_p(Vector<Vector<Int> >(0)),
    maxScans_p(1000), maxObs_p(1000), maxArray_p(1000), mssErrHandler_p(NULL), 
    isMS_p(True), toTENCalled_p(False)
  {
    clear();
  }
  
  //----------------------------------------------------------------------------
  
  MSSelection::MSSelection(const MeasurementSet& ms,
			   const MSSMode& mode,
			   const String& timeExpr,
			   const String& antennaExpr,
			   const String& fieldExpr,
			   const String& spwExpr,
			   const String& uvDistExpr,
			   const String& taqlExpr,
			   const String& polnExpr,
			   const String& scanExpr,
			   const String& arrayExpr,
			   const String& stateExpr,
			   const String& observationExpr):
    fullTEN_p(), ms_p(&ms), antennaExpr_p(""), fieldExpr_p(""),
    spwExpr_p(""), scanExpr_p(""), arrayExpr_p(""), timeExpr_p(""), uvDistExpr_p(""),
    polnExpr_p(""),taqlExpr_p(""), stateExpr_p(""), observationExpr_p(""),
    exprOrder_p(MAX_EXPR, NO_EXPR), antenna1IDs_p(), antenna2IDs_p(), fieldIDs_p(), 
    spwIDs_p(), scanIDs_p(),ddIDs_p(),baselineIDs_p(), selectedTimesList_p(),
    selectedUVRange_p(),selectedUVUnits_p(),selectedPolMap_p(Vector<Int>(0)),
    selectedSetupMap_p(Vector<Vector<Int> >(0)),
    maxScans_p(1000), maxObs_p(1000), maxArray_p(1000), mssErrHandler_p(NULL), 
    isMS_p(True), toTENCalled_p(False)
  {
    //
    // Do not initialize the private string variables directly. Instead
    // using the setExpr* methods to do that keeps that state of the
    // object consistent.
    //
    reset(ms,mode,
	  timeExpr,
	  antennaExpr,
	  fieldExpr,
	  spwExpr,
	  uvDistExpr,
	  taqlExpr,
	  polnExpr,
	  scanExpr,
	  arrayExpr,
	  stateExpr,
	  observationExpr);
  }
  
  
  //----------------------------------------------------------------------------
  
  void MSSelection::reset(MSSelectableTable& msLike,
			  const MSSMode& mode,
			  const String& timeExpr,
			  const String& antennaExpr,
			  const String& fieldExpr,
			  const String& spwExpr,
			  const String& uvDistExpr,
			  const String& taqlExpr,
			  const String& polnExpr,
			  const String& scanExpr,
			  const String& arrayExpr,
			  const String& stateExpr,
			  const String& observationExpr)
  {
    ms_p=msLike.asMS();
    isMS_p=msLike.isMS();
    toTENCalled_p=False;
    //
    // Do not initialize the private string variables
    // directly. Instead using the setExpr* methods to do that so that
    // it keeps that state of the object consistent.
    //
    clear(); // Clear everything
    setAntennaExpr(antennaExpr);
    setFieldExpr(fieldExpr);
    setSpwExpr(spwExpr);
    setScanExpr(scanExpr);
    setArrayExpr(arrayExpr);
    setTimeExpr(timeExpr);
    setUvDistExpr(uvDistExpr);
    setPolnExpr(polnExpr);
    setTaQLExpr(taqlExpr);
    setStateExpr(stateExpr);
    setObservationExpr(observationExpr);

    if (mode==PARSE_NOW)
      fullTEN_p = toTableExprNode(&msLike);
  }
  void MSSelection::reset(const MeasurementSet& ms,
			  const MSSMode& mode,
			  const String& timeExpr,
			  const String& antennaExpr,
			  const String& fieldExpr,
			  const String& spwExpr,
			  const String& uvDistExpr,
			  const String& taqlExpr,
			  const String& polnExpr,
			  const String& scanExpr,
			  const String& arrayExpr,
			  const String& stateExpr,
			  const String& observationExpr)
  {
    //
    // Do not initialize the private string variables directly. Instead
    // using the setExpr* methods to do that keeps that state of the
    // object consistent.
    //
    //    msFace_p.setTable(ms);
    //    ms_p = msFace_p.asMS();
    ms_p = &ms;
    
    clear(); // Clear everything
    setAntennaExpr(antennaExpr);
    setFieldExpr(fieldExpr);
    setSpwExpr(spwExpr);
    setScanExpr(scanExpr);
    setArrayExpr(arrayExpr);
    setTimeExpr(timeExpr);
    setUvDistExpr(uvDistExpr);
    setPolnExpr(polnExpr);
    setTaQLExpr(taqlExpr);
    setStateExpr(stateExpr);
    setObservationExpr(observationExpr);

    if (mode==PARSE_NOW)
      fullTEN_p = toTableExprNode(ms_p);
  }
  
  //----------------------------------------------------------------------------
  
  MSSelection::~MSSelection()
  {
    // If we created the error handler, we also take it out
    deleteErrorHandlers();
    deleteNodes();
  }
  
  //----------------------------------------------------------------------------
  
  MSSelection::MSSelection(const Record& selectionItem) : 
    antennaExpr_p(""), fieldExpr_p(""), spwExpr_p(""), scanExpr_p(""), arrayExpr_p(""), 
    timeExpr_p(""), uvDistExpr_p(""), polnExpr_p(""),taqlExpr_p(""),stateExpr_p(""), 
    observationExpr_p(""), antenna1IDs_p(), antenna2IDs_p(), fieldIDs_p(), spwIDs_p(),
    ddIDs_p(),baselineIDs_p(), selectedPolMap_p(Vector<Int>(0)), 
    selectedSetupMap_p(Vector<Vector<Int> >(0))
    
  {
    // Construct from a record representing a selection item
    // Output to private data:
    //    antennaExpr_p             String   Antenna STaQL expression
    //    polnExpr_p                String   Polarization STaQL expression
    //    fieldExpr_p               String   Field STaQL expression
    //    spwExpr_p                 String   SPW STaQL expression
    //    scanExpr_p                String   Scan STaQL expression
    //    arrayExpr_p               String   Array_ID STaQL expression
    //    timeExpr_p                String   Time STaQL expression
    //    uvDistExpr_p              String   UV Distribution STaQL expression
    //    observation_p             String   Observation ID STaQL expression
    //    taqlExpr_p                String   TaQL expression
    //
    // Extract fields from the selection item record
    fromSelectionItem(selectionItem);
  }
  
  //----------------------------------------------------------------------------
  
  MSSelection::MSSelection (const MSSelection& other):
    selectedPolMap_p(Vector<Int>(0)),
    selectedSetupMap_p(Vector<Vector<Int> >(0))
    
  {
    // Copy constructor
    // Input:
    //    other            const MSSelection&    Existing MSSelection object
    // Output to private data:
    //
    if(this != &other) {
      this->antennaExpr_p = other.antennaExpr_p;
      this->fieldExpr_p   = other.fieldExpr_p;
      this->spwExpr_p     = other.spwExpr_p;
      this->scanExpr_p    = other.scanExpr_p;
      this->observationExpr_p    = other.observationExpr_p;
      this->arrayExpr_p   = other.arrayExpr_p;
      this->timeExpr_p    = other.timeExpr_p;
      this->uvDistExpr_p  = other.uvDistExpr_p;
      this->taqlExpr_p    = other.taqlExpr_p;
      this->polnExpr_p    = other.polnExpr_p;
      this->stateExpr_p   = other.stateExpr_p;
      this->exprOrder_p   = other.exprOrder_p;
    }
  }
  
  //----------------------------------------------------------------------------
  
  MSSelection& MSSelection::operator= (const MSSelection& other)
  {
    // Assignment operator
    // Input:
    //    other            const MSSelection&    RHS MSSelection object
    // Output to private data:
    //
    if(this != &other) {
      this->antennaExpr_p = other.antennaExpr_p;
      this->fieldExpr_p   = other.fieldExpr_p;
      this->spwExpr_p     = other.spwExpr_p;
      this->scanExpr_p    = other.scanExpr_p;
      this->observationExpr_p    = other.observationExpr_p;
      this->arrayExpr_p   = other.arrayExpr_p;
      this->timeExpr_p    = other.timeExpr_p;
      this->uvDistExpr_p  = other.uvDistExpr_p;
      this->taqlExpr_p    = other.taqlExpr_p;
      this->polnExpr_p    = other.polnExpr_p;
      this->stateExpr_p   = other.stateExpr_p;
      this->exprOrder_p   = other.exprOrder_p;
      this->isMS_p        = other.isMS_p;
    }
    
    return *this;
  }
  
  //----------------------------------------------------------------------------
  
  TableExprNode MSSelection::getTEN(const MeasurementSet*ms)
  {

    // if (ms!=NULL) {resetTEN();toTableExprNode(ms);}
    // else if (ms_p==NULL) throw(MSSelectionError("MSSelection::getTEN() called without setting the MS"));
    // else toTableExprNode(ms_p); 

    if (isMS_p==False)
      {
	if (toTENCalled_p==True) return fullTEN_p;
	else 
	  throw(MSSelectionError("MSSelection::getTEN() called before calling MSSelection::toTableExprNode()"));
      }

    if (ms==NULL) 
      if (ms_p==NULL)
	throw(MSSelectionError("MSSelection::getTEN() called without setting the MS"));
      else toTableExprNode(ms_p); 
    else {resetTEN();toTableExprNode(ms);}


    return fullTEN_p;
  }
  
  //----------------------------------------------------------------------------
  
  String MSSelection::indexExprStr(Vector<Int> index)
  {
    String expression;
    
    for(uInt i=0; i<index.nelements(); i++)
      {
	if(i==0)
	  expression = String::toString(index[i]);
	else
	  expression = expression + "," + String::toString(index[i]);
      }
    
    return expression;
  }
  
  //----------------------------------------------------------------------------
  
  String MSSelection::nameExprStr(Vector<String> name)
  {
    String expression;
    
    // SDJ Removed the placement of quotes around field names.  This seems
    // to be invalid now (Nov. 2006).
    //expression = "'";
    
    for(uInt i=0; i<name.nelements(); i++)
      {
	if(i==0)
	  expression = expression + name[i];
	else
	  expression = expression + ", " + name[i];
      }
    
    //expression = expression + "'";
    
    return expression;
  }
  
  //----------------------------------------------------------------------------
  
  void MSSelection::deleteErrorHandlers()
  {
    if (mssErrHandler_p != NULL) 
      {
	delete mssErrHandler_p;
	mssErrHandler_p=MSAntennaParse::thisMSAErrorHandler=NULL;
	mssErrHandler_p=MSStateParse::thisMSSErrorHandler=NULL;
      }
    mssErrHandler_p=MSAntennaParse::thisMSAErrorHandler=NULL;
    mssErrHandler_p=MSStateParse::thisMSSErrorHandler=NULL;
    //    mssErrHandler_p=NULL;
  }
  
  void MSSelection::deleteNodes()
  {
    //    msArrayGramParseDeleteNode();
    msCorrGramParseDeleteNode();
    msFieldGramParseDeleteNode();
    //    msScanGramParseDeleteNode();
    msSpwGramParseDeleteNode();
    msTimeGramParseDeleteNode();
    msUvDistGramParseDeleteNode();
    msStateGramParseDeleteNode();
  }
  
  //----------------------------------------------------------------------------
  const MeasurementSet* MSSelection::getMS(MSSelectableTable* msLike)
  {
    const MeasurementSet *ms=msLike->asMS();
    isMS_p=msLike->isMS();
    
    //
    // When msLike is CTInterface, msLike->asMS() will return NULL.
    // In this case, *ms should also not be used anyway.
    //
    // When msLike is MSInterface, msLike->asMS() must return a usable
    // *ms.
    //
    if (msLike->isMS() && (ms==NULL))
      throw(MSSelectionError(String("MSSelection::toTableExprNode(MSSelectableTable*): "
				    "MS pointer from MS-Like object is null")));

    if (!msLike->isMS()         &&
	(
	 // (fieldExpr_p != "")     ||     
	 // (antennaExpr_p != "")     || 
	 // (timeExpr_p != "") ||         // Will be opened-up for CalTables in the future
	 // (spwExpr_p != "")  ||         // Will be opened-up for CalTables in the future
	 //(scanExpr_p != "")  ||
	 //(observationExpr_p != "") || 
	 (arrayExpr_p != "") || (uvDistExpr_p != "")      ||
	 //(taqlExpr_p != "")  || 
	 (polnExpr_p != "")        || 
	 (stateExpr_p != "")
	 ))
      throw(MSSelectionError(String("MSSelection::toTableExprNode(MSSelectableTable*): "
				    "Only field-, spw-, scan-, time- and antenna-selection is supported for CalTables")));
    return ms;
  }
  //----------------------------------------------------------------------------
  void MSSelection::initErrorHandler(const MSExprType type)
  {
    switch (type)
      {
      case ANTENNA_EXPR:
	{
	  if (MSAntennaParse::thisMSAErrorHandler == NULL)
	    {
	      if (mssErrHandler_p == NULL) mssErrHandler_p = new MSSelectionErrorHandler();
	      setErrorHandler(ANTENNA_EXPR, mssErrHandler_p);
	    }
	  else
	    {
	      //mssErrHandler_p = MSAntennaParse::thisMSAErrorHandler;
	      MSAntennaParse::thisMSAErrorHandler->reset();
	      //	mssErrHandler_p->reset();
	    }
	  break;
	}
      case STATE_EXPR:
	{
	  if (MSStateParse::thisMSSErrorHandler == NULL)
	    {
	      if (mssErrHandler_p == NULL) mssErrHandler_p = new MSSelectionErrorHandler();
	      setErrorHandler(STATE_EXPR, mssErrHandler_p);
	    }
	  else
	    {
	      //mssErrHandler_p = MSAntennaParse::thisMSAErrorHandler;
	      MSStateParse::thisMSSErrorHandler->reset();
	      //	mssErrHandler_p->reset();
	    }
	  break;
	}
      default:  
	throw(MSSelectionError(String("Wrong MSExprType in MSSelection::initErrorHandler()")));
	break;
      };
  }
  //----------------------------------------------------------------------------
  
  TableExprNode MSSelection::toTableExprNode(MSSelectableTable* msLike)
  {
    // Convert the MS selection to a TableExprNode object, 
    // representing a TaQL selection in C++.
    // Input:
    //    msLike           const MSSelectableTable&  MeasurementSet or CalTable 
    //                                               to bind TaQL
    // Output:
    //    toTableExprNode  TableExprNode             Table expression node
    //
    // Interpret all expressions and produce a consolidated TEN.  
    //
    if (fullTEN_p.isNull()==False) return fullTEN_p;

    const MeasurementSet *ms=getMS(msLike);
    resetMS(*ms);
    toTENCalled_p=True;
    //    ms_p = msLike->asMS();


    TableExprNode condition;
    
    initErrorHandler(ANTENNA_EXPR);
    initErrorHandler(STATE_EXPR);

    try
      {
	for(uInt i=0; i<exprOrder_p.nelements(); i++)
	  {
	    TableExprNode node;
	    switch(exprOrder_p[i])
	      {
	      case ANTENNA_EXPR:
		{
		  if(antennaExpr_p != "")
		    {
		      antenna1IDs_p.resize(0);
		      antenna2IDs_p.resize(0);
		      baselineIDs_p.resize(0,2);
		      node = msAntennaGramParseCommand(*msLike, antennaExpr_p, 
						       antenna1IDs_p, antenna2IDs_p, 
						       baselineIDs_p);
		    }
		  // if(antennaExpr_p != "")
		  //   {
		  //     TableExprNode col1AsTEN = msLike->col(msLike->columnName(MS::ANTENNA1)),
		  // 	col2AsTEN = msLike->col(msLike->columnName(MS::ANTENNA2));

		  //     antenna1IDs_p.resize(0);
		  //     antenna2IDs_p.resize(0);
		  //     baselineIDs_p.resize(0,2);
		  //     node = msAntennaGramParseCommand(msLike->antenna(), 
		  // 				       col1AsTEN, col2AsTEN, antennaExpr_p, 
		  // 				       antenna1IDs_p, antenna2IDs_p, baselineIDs_p);
		  //   }
		  break;
		}
	      case FIELD_EXPR:
		{
		  if(fieldExpr_p != "")
		    {
		      fieldIDs_p.resize(0);

		      TableExprNode colAsTEN = msLike->col(msLike->columnName(MS::FIELD_ID));
		      node = msFieldGramParseCommand(msLike->field(), colAsTEN, fieldExpr_p,fieldIDs_p);
		    }
		  break;
		}
	      case SPW_EXPR:
		{
		  if (spwExpr_p != "")
		    {
		      TableExprNode colAsTEN = msLike->col(msLike->columnName(MS::DATA_DESC_ID));
		      spwIDs_p.resize(0);
		      if (msSpwGramParseCommand(msLike->spectralWindow(), 
						msLike->dataDescription(),
						colAsTEN, spwExpr_p,
						spwIDs_p, chanIDs_p,spwDDIDs_p) == 0)
			node = *(msSpwGramParseNode());
		    }
		  break;
		}
	      case SCAN_EXPR:
		{
		  TableExprNode colAsTEN = msLike->col(msLike->columnName(MS::SCAN_NUMBER));
		  scanIDs_p.resize(0);
		  if(scanExpr_p != "")
		    node = msScanGramParseCommand(ms, colAsTEN, scanExpr_p, scanIDs_p, maxScans_p);
		  break;
		}
	      case OBSERVATION_EXPR:
		{
		  TableExprNode colAsTEN = msLike->col(msLike->columnName(MS::OBSERVATION_ID));
		  observationIDs_p.resize(0);
		  if(observationExpr_p != "")
		    node = msObservationGramParseCommand(ms, msLike->observation(),
							 colAsTEN,
							 observationExpr_p, 
							 observationIDs_p);
		  break;
		}
	      case ARRAY_EXPR:
		{
		  arrayIDs_p.resize(0);
		  if(arrayExpr_p != "")
		    node = msArrayGramParseCommand(ms, arrayExpr_p, arrayIDs_p, maxArray_p);
		  break;
		}
	      case UVDIST_EXPR:
		{
		  selectedUVRange_p.resize(2,0);
		  selectedUVUnits_p.resize(0);
		  if(uvDistExpr_p != "" &&
		     msUvDistGramParseCommand(ms, uvDistExpr_p, 
					      selectedUVRange_p, 
					      selectedUVUnits_p) == 0)
		    node = *(msUvDistGramParseNode());
		  break;
		}
	      case TAQL_EXPR:
		{
		  if(taqlExpr_p != "")
		    {
		      node = RecordGram::parse(*msLike->table(),taqlExpr_p);
		    }
		  break;
		}
	      case POLN_EXPR:
		{
		  // This expression is a pure in-row selection.  No
		  // need to add to the tree of TENs (the condition
		  // variable).
		  if (polnExpr_p != "")
		    {
		      msPolnGramParseCommand(ms, 
					     polnExpr_p,
					     node,
					     ddIDs_p,
					     selectedPolMap_p,
					     selectedSetupMap_p);
		    }
		  break;
		}
	      case STATE_EXPR:
		{
		  stateObsModeIDs_p.resize(0);
		  if(stateExpr_p != "" &&
		     msStateGramParseCommand(ms, stateExpr_p,stateObsModeIDs_p) == 0)
		    {
		      node = *(msStateGramParseNode());
		      if (stateObsModeIDs_p.nelements()==0)
			throw(MSSelectionStateError(String("No match found for state expression: ")+stateExpr_p));
		    }
		  break;
		}
	      case NO_EXPR:break;
	      default:  break;
	      } // Switch
	    
	    condition = condition && node;
	  }//For
	//
	// Now parse the time expression.  Internally use the condition
	// generated so far to find the first logical row to use to get
	// value of the wild-card fields in the time expression. 
	//
	selectedTimesList_p.resize(2,0);

	const TableExprNode *timeNode = 0x0;
	TableExprNode colAsTEN = msLike->col(msLike->columnName(MS::TIME));
	MSSelectableMainColumn *mainColInterface=msLike->mainColumns();
	// MSMainColInterface msMainColInterface;
	// msMainColInterface.init(*(msLike->table()));

	if(timeExpr_p != "" &&
	   //msTimeGramParseCommand(ms, timeExpr_p, condition, selectedTimesList_p) == 0)
	   msTimeGramParseCommand(ms, timeExpr_p, colAsTEN, *mainColInterface, condition, selectedTimesList_p) == 0)
	  timeNode = msTimeGramParseNode();

	//
	// Add the time-expression TEN to the condition
	//
	if(timeNode && !timeNode->isNull()) 
	  {
	    if(condition.isNull()) 
	      condition = *timeNode;
	    else 
	      condition = condition && *timeNode;
	  }
	
	fullTEN_p = condition;
      }
    catch(AipsError& x)
      {
	runErrorHandler();
	deleteNodes();
	deleteErrorHandlers();
	throw(x);
      }	

    runErrorHandler();
    deleteErrorHandlers();
    deleteNodes();
    return condition;
  }

  TableExprNode MSSelection::toTableExprNode(const MeasurementSet* ms)
  {
    // Convert the MS selection to a TableExprNode object, 
    // representing a TaQL selection in C++.
    // Input:
    //    ms               const MeasurementSet&     MeasurementSet to bind TaQL
    // Output:
    //    toTableExprNode  TableExprNode             Table expression node
    //
    // Interpret all expressions in the MS selection
    //
    // This method now is purely for backwards compatibility reasons.
    // Its usage is discouraged.
    //
    // The original code using old-styled interface to the various
    // parsers is available as comments in r19937 in the SVN repos.
    //
    if (fullTEN_p.isNull()==False) return fullTEN_p;
    
    MSInterface msLike(*ms);
    return toTableExprNode(&msLike);

  }
  
  //----------------------------------------------------------------------------
  void MSSelection::setErrorHandler(const MSExprType type,  MSSelectionErrorHandler* mssEH,
				    const Bool overRide)
  {
    switch (type)
      {
      case ANTENNA_EXPR:
	{
	  if (overRide)
	    MSAntennaParse::thisMSAErrorHandler = mssEH;
	  else if (MSAntennaParse::thisMSAErrorHandler == NULL)
	    MSAntennaParse::thisMSAErrorHandler = mssEH;
	  break;
	}
      case STATE_EXPR:
	{
	  if (overRide)
	    MSStateParse::thisMSSErrorHandler = mssEH;
	  else if (MSStateParse::thisMSSErrorHandler == NULL)
	    MSStateParse::thisMSSErrorHandler = mssEH;
	  break;
	}
      default: throw(MSSelectionError(String("Wrong MSExprType in MSSelection::setErrorHandler()")));
      };
  }

  //----------------------------------------------------------------------------
  void MSSelection::runErrorHandler()
  {
    if (MSAntennaParse::thisMSAErrorHandler->nMessages() > 0)
      {
	MSSelectionAntennaParseError msAntException(String(""));
	MSAntennaParse::thisMSAErrorHandler->handleError(msAntException);
      }
 
    if (MSStateParse::thisMSSErrorHandler->nMessages() > 0)
      {
	MSSelectionStateParseError msStateException(String(""));
	MSStateParse::thisMSSErrorHandler->handleError(msStateException);
      }
  }

  //----------------------------------------------------------------------------
  Bool MSSelection::getSelectedMS(MeasurementSet& selectedMS, 
				  const String& outMSName)
  {
    if (fullTEN_p.isNull()) fullTEN_p=toTableExprNode(ms_p);
    if ((ms_p == NULL) || ms_p->isNull())
      throw(MSSelectionError("MSSelection::getSelectedMS() called without setting the parent MS.\n"
  			     "Hint: Need to use MSSelection::resetMS() perhaps?"));
    //    return baseGetSelectedMS_p(selectedMS, *ms_p, fullTEN_p, outMSName);
    return getSelectedTable(selectedMS, *ms_p, fullTEN_p, outMSName);
  }
  
  //----------------------------------------------------------------------------
  
  Bool MSSelection::exprIsNull(const MSExprType type)
  {
    Bool exprIsNull=False;
    if (type == NO_EXPR)
      for(uInt i=0; i<exprOrder_p.nelements();  i++)
	{
	  exprIsNull = 
	    (antennaExpr_p     == "") & 
	    (fieldExpr_p       == "") & 
	    (spwExpr_p         == "") &
	    (scanExpr_p        == "") & 
	    (arrayExpr_p       == "") &
	    (timeExpr_p        == "") & 
	    (uvDistExpr_p      == "") & 
	    (taqlExpr_p        == "") & 
	    (polnExpr_p        == "") & 
	    (stateExpr_p       == "") & 
	    (observationExpr_p == "");
	}
    else
      {
	switch (type)
	  {
	  case ANTENNA_EXPR:     exprIsNull = (antennaExpr_p == "");break;
	  case FIELD_EXPR:       exprIsNull = (fieldExpr_p   == "");break;
	  case SPW_EXPR:         exprIsNull = (spwExpr_p     == "");break;
	  case SCAN_EXPR:        exprIsNull = (scanExpr_p    == "");break;
	  case ARRAY_EXPR:       exprIsNull = (arrayExpr_p   == "");break;
	  case TIME_EXPR:        exprIsNull = (timeExpr_p    == "");break;
	  case UVDIST_EXPR:      exprIsNull = (uvDistExpr_p  == "");break;
	  case TAQL_EXPR:        exprIsNull = (taqlExpr_p    == "");break;
	  case POLN_EXPR:        exprIsNull = (polnExpr_p    == "");break;
	  case STATE_EXPR:       exprIsNull = (stateExpr_p   == "");break;
	  case OBSERVATION_EXPR: exprIsNull = (observationExpr_p    == "");break;
	  default:;
	  };
      }
    return exprIsNull;
  }
  
  //----------------------------------------------------------------------------
  
  void MSSelection::clear(const MSExprType type)
  {
    if (type==NO_EXPR)
      {
	antennaExpr_p = "";
	fieldExpr_p   = "";
	spwExpr_p     = "";
	scanExpr_p    = "";
	arrayExpr_p   = "";
	timeExpr_p    = "";
	uvDistExpr_p  = "";
	taqlExpr_p    = "";
	polnExpr_p    = "";
	stateExpr_p    = "";
	observationExpr_p    = "";
	exprOrder_p = Vector<Int>(MAX_EXPR, NO_EXPR);
      }
    else
      {
	for(uInt i=0; i<exprOrder_p.nelements();  i++)
	  if (exprOrder_p[i] == type)
	    {
	      exprOrder_p[i] = NO_EXPR;
	      switch (type)
		{
		case ANTENNA_EXPR: antennaExpr_p = "";break;
		case FIELD_EXPR:   fieldExpr_p   = "";break;
		case SPW_EXPR:     spwExpr_p     = "";break;
		case SCAN_EXPR:    scanExpr_p    = "";break;
		case ARRAY_EXPR:   arrayExpr_p   = "";break;
		case TIME_EXPR:    timeExpr_p    = "";break;
		case UVDIST_EXPR:  uvDistExpr_p  = "";break;
		case TAQL_EXPR:    taqlExpr_p    = "";break;
		case POLN_EXPR:    polnExpr_p    = "";break;
		case STATE_EXPR:   stateExpr_p   = "";break;
		case OBSERVATION_EXPR:    observationExpr_p    = "";break;
		default:;
		};
	    }
	if (!fullTEN_p.isNull()) resetTEN();
      }
  }
  
  //----------------------------------------------------------------------------
  
  Bool MSSelection::setOrder(MSSelection::MSExprType type)
  {
    Bool ret=False;
    for(uInt i=0; i<exprOrder_p.nelements(); i++)
      {
	if(exprOrder_p[i] == NO_EXPR)
	  {
	    exprOrder_p[i] = type;
	    ret=True;
	    break;
	  }
      }
    return ret;
  }
  
  //----------------------------------------------------------------------------
  
  Bool MSSelection::setAntennaExpr(const String& antennaExpr)
  {
    // Set the antenna
    // Input:
    //    antennaExpr       const String&  Supplementary antenna expression
    // Output to private data:
    //    antennaExpr_p     String         Supplementary antenna expression
    //
    if(setOrder(ANTENNA_EXPR))
      {
	antennaExpr_p = antennaExpr;
	resetTEN();
	return True;
      }
    
    return False;
  }
  
  //----------------------------------------------------------------------------
  
  Bool MSSelection::setFieldExpr(const String& fieldExpr)
  {
    // Set the field
    // Input:
    //    fieldExpr         const String&  Supplementary field expression
    // Output to private data:
    //    fieldExpr_p       String         Supplementary field expression
    //
    if(setOrder(FIELD_EXPR))
      {
	fieldExpr_p = fieldExpr;
	resetTEN();
	return True;
      }
    
    return False;
  }
  
  //----------------------------------------------------------------------------
  
  Bool MSSelection::setSpwExpr(const String& spwExpr)
  {
    // Set the SPW
    // Input:
    //    spwExpr           const String&  Supplementary SPW expression
    // Output to private data:
    //    spwExpr_p         String         Supplementary SPW expression
    //
    if(setOrder(SPW_EXPR))
      {
	spwExpr_p = spwExpr;
	resetTEN();
	return True;
      }
    
    return False;
  }
  
  //----------------------------------------------------------------------------
  
  Bool MSSelection::setArrayExpr(const String& arrayExpr)
  {
    // Set the array
    // Input:
    //    arrayExpr          const String&  Supplementary array expression
    // Output to private data:
    //    arrayExpr_p        String         Supplementary array expression
    //
    if(setOrder(ARRAY_EXPR))
      {
	arrayExpr_p = arrayExpr;
	resetTEN();
	return True;
      }
    
    return False;
  }
  
  //----------------------------------------------------------------------------
  
  Bool MSSelection::setScanExpr(const String& scanExpr)
  {
    // Set the scan
    // Input:
    //    scanExpr          const String&  Supplementary scan expression
    // Output to private data:
    //    scanExpr_p        String         Supplementary scan expression
    //
    if(setOrder(SCAN_EXPR))
      {
	scanExpr_p = scanExpr;
	resetTEN();
	return True;
      }
    
    return False;
  }
  
  //----------------------------------------------------------------------------
  
  Bool MSSelection::setObservationExpr(const String& observationExpr)
  {
    // Set the scan
    // Input:
    //    scanExpr          const String&  Supplementary scan expression
    // Output to private data:
    //    scanExpr_p        String         Supplementary scan expression
    //
    if(setOrder(OBSERVATION_EXPR))
      {
	observationExpr_p = observationExpr;
	resetTEN();
	return True;
      }
    
    return False;
  }
  
  //----------------------------------------------------------------------------
  
  Bool MSSelection::setTimeExpr(const String& timeExpr)
  {
    // Set the time
    // Input:
    //    timeExpr          const String&  Supplementary time expression
    // Output to private data:
    //    timeExpr_p        String         Supplementary time expression
    //
    if(setOrder(TIME_EXPR))
      {
	timeExpr_p = timeExpr;
	resetTEN();
	return True;
      }
    
    return False;
  }
  
  //----------------------------------------------------------------------------
  
  Bool MSSelection::setUvDistExpr(const String& uvDistExpr)
  {
    // Set the UV distribution
    // Input:
    //    uvdistExpr        const String&  Supplementary uvdist expression
    // Output to private data:
    //    uvdistExpr_p      String         Supplementary uvdist expression
    //
    if(setOrder(UVDIST_EXPR))
      {
	uvDistExpr_p = uvDistExpr;
	resetTEN();
	return True;
      }
    
    return False;
  }
  
  //----------------------------------------------------------------------------
  
  Bool MSSelection::setStateExpr(const String& stateExpr)
  {
    // Set the state table Obs_mode selection expression
    // Input:
    //    stateExpr        const String&  Supplementary state obs_mode expression
    // Output to private data:
    //    stateExpr_p      String         Supplementary state obs_mode expression
    //
    if(setOrder(STATE_EXPR))
      {
	stateExpr_p = stateExpr;
	resetTEN();
	return True;
      }
    
    return False;
  }
  
  //----------------------------------------------------------------------------
  
  Bool MSSelection::setTaQLExpr(const String& taqlExpr)
  {
    // Set the TaQL expression
    // Input:
    //    taqlExpr        const String&  Supplementary taql expression
    // Output to private data:
    //    taqlExpr_p      String         Supplementary taql expression
    //
    if(setOrder(TAQL_EXPR))
      {
	taqlExpr_p = taqlExpr;
	resetTEN();
	return True;
      }
    
    return False;
  }
  //----------------------------------------------------------------------------
  
  Bool MSSelection::setPolnExpr(const String& polnExpr)
  {
    // Set the Poln expression
    // Input:
    //    polnExpr        const String&  Supplementary poln expression
    // Output to private data:
    //    polnExpr_p      String         Supplementary poln expression
    //
    if(setOrder(POLN_EXPR))
      {
	polnExpr_p = polnExpr;
	resetTEN();
	return True;
      }
    
    return False;
  }
  //----------------------------------------------------------------------------
  // This function also optionally sorts the matrix of SPWIDs and
  // associated channel selection indices in ascending order of
  // SPWIDs.
  //
  Matrix<Int> MSSelection::getChanList(const MeasurementSet* ms, const Int defaultStep,
				       const Bool sorted) 
  {
    if (chanIDs_p.nelements() <= 0) getTEN(ms); 
    uInt nrows=chanIDs_p.nrow(), ncols=chanIDs_p.ncolumn();
    Matrix<Int> chanIDList;
    if (nrows > 0)
      {
	if (sorted)
	  {
	    Vector<Int> spwIDList(chanIDs_p.column(0));//Extract the SPW IDs
	    Vector<uInt> sortedNdx;
	    //
	    // Make a list of indices which will sort the chanID_p Matrix on
	    // SPW ID (the first column of each row).
	    //
	    Bool deleteit;
	    Sort sort(spwIDList.getStorage(deleteit), sizeof(Int));
	    sort.sortKey((uInt)0, TpInt);
	    sort.sort(sortedNdx, nrows);
	    //
	    // Using the sorted indices, copy from the unsorted private
	    // ChaIDs_p to the output (sorted) Matrix chandIDList.
	    //
	    chanIDList.resize(chanIDs_p.shape());
	    for(uInt targetRow=0; targetRow<nrows; targetRow++)
	      for (uInt j=0; j<ncols; j++)
		chanIDList(targetRow,j)=chanIDs_p(sortedNdx(targetRow),j);
	  }
	else
	  chanIDList = chanIDs_p;
	
	for(uInt targetRow=0; targetRow<nrows; targetRow++)
	  {
	    if (chanIDList(targetRow,ncols-1) == 0)
	      chanIDList(targetRow,ncols-1)=defaultStep;
	    // if (chanIDList(targetRow,ncols-2) == chanIDList(targetRow,ncols-3)) // Stop == Step
	    //   chanIDList(targetRow,ncols-1)=0;
	  }
      }
    
    
    return chanIDList;
  }
  //----------------------------------------------------------------------------
  // This function also optionally sorts the matrix of SPWIDs and
  // associated channel selection indices in ascending order of
  // SPWIDs.
  //
  Matrix<Double> MSSelection::getChanFreqList(const MeasurementSet* ms, 
					      const Bool sorted) 
  {
    LogIO log_l(LogOrigin("MSSelection", "getChanFreqList"));

    if (chanIDs_p.nelements() == 0) getTEN(ms); 
    Matrix<Int> chanList_l = getChanList(ms, 1, sorted);
    Matrix<Double> freqList_l;
    freqList_l.resize(chanList_l.shape());
    
    if (chanList_l.shape()(0) == 0) return freqList_l;

    const ROMSSpWindowColumns msSpwSubTable(ms_p->spectralWindow());
    if (msSpwSubTable.nrow() <= (uInt)max(chanList_l.column(0)))
	throw(MSSelectionError(String("MSS::getChanFreqList:: Internal error:  Selected list of SPW IDs > "
				      "no. of rows in the SPECTRAL_WINDOW sub-table.")));
    Int spwID;
    for (uInt i=0; i < chanList_l.shape()(0); i++)
      {
	spwID = chanList_l(i,0); // First column has the SPW ID
	Array<Double> chanFreq(msSpwSubTable.chanFreq()(spwID));
	Double avgChanWidth = chanList_l(i,3)*sum(msSpwSubTable.chanWidth()(spwID))
	  /msSpwSubTable.chanWidth()(spwID).nelements();
	
	freqList_l(i,0) = (Double)chanList_l(i,0);
	freqList_l(i,1) = chanFreq(IPosition(1,chanList_l(i,1)));
	freqList_l(i,2) = chanFreq(IPosition(1,chanList_l(i,2)));
	freqList_l(i,3) = avgChanWidth;
      }
    
    return freqList_l;
  }
  //----------------------------------------------------------------------------
  
  void MSSelection::getChanSlices(Vector<Vector<Slice> >& chanslices,
				  const MeasurementSet* ms,const Int defaultChanStep) {
    
    // The total number of spws
    Int nspw=ms->spectralWindow().nrow();
    
    // Nominally empty selection for all spws
    chanslices.resize(nspw);
    chanslices.set(Vector<Slice>());
    
    // Get the chan selection matrix
    Matrix<Int> chanmat=this->getChanList(ms,defaultChanStep);
    
    for (uInt i=0;i<chanmat.nrow();++i) {
      // Reference to the current spw's slice list
      Vector<Slice>& currspwsl(chanslices(chanmat(i,0)));
      
      // Add a slice element and fill it
      Int islice=currspwsl.nelements();
      currspwsl.resize(islice+1,True);
      currspwsl(islice)=Slice(chanmat(i,1),
			      (chanmat(i,2)-chanmat(i,1)+chanmat(i,3))/chanmat(i,3),
			      // chanmat(i,2)-chanmat(i,1)+1,
			      chanmat(i,3));
    }
    
  }
  
  
  //----------------------------------------------------------------------------
  void MSSelection::getCorrSlices(Vector<Vector<Slice> >& corrslices,
				  const MeasurementSet* ms) {
    
    // The total number of polids
    Int npol=ms->polarization().nrow();
    
    // Nominally empty selection for all polids
    corrslices.resize(npol);
    corrslices.set(Vector<Slice>());
    
    // Get the corr indices as an ordered map
    OrderedMap<Int, Vector<Vector<Int> > > corrmap(this->getCorrMap(ms));
    
    // Iterate over the ordered map to fill the slices
    ConstMapIter<Int, Vector<Vector<Int> > > mi(corrmap);
    for (mi.toStart(); !mi.atEnd(); mi++) {
      Int pol=mi.getKey();
      Vector<Int> corridx=mi.getVal()[0];
      
      Int ncorr=corridx.nelements();
      corrslices(pol).resize(ncorr);
      for (Int i=0;i<ncorr;++i)
	corrslices(pol)(i)=Slice(corridx(i),1,1);
    }
    
  }
  
  //----------------------------------------------------------------------------
  
  void MSSelection::fromSelectionItem(const Record& selectionItem)
  {
    // Convert from an input selection data item
    // Input:
    //    selectionItem     const Record&     Selection item
    // Output to private data:
    // 
    // Debug print statements
    
    //   cout << "MSSel::fromSI, at start, selectionItem.nfields=" << selectionItem.nfields()
    //        << endl;
    //   for (uInt fld=0; fld<selectionItem.nfields(); fld++) {
    //     cout << "MSSel::fromGR, fld= " << fld << ", name= "
    // 	 << selectionItem.name(fld) << ", type= "
    // 	 << selectionItem.type(fld) << endl;
    //   }
    //   cout << "------------------------------------------------------" << endl;
    
    exprOrder_p = Vector<Int>(MAX_EXPR, NO_EXPR);
    
    // Extract and set all expressions
    //
    // Antenna expression
    if (definedAndSet(selectionItem,"antenna")) {
      setAntennaExpr(selectionItem.asString("antenna"));
      //    cout << antennaExpr_p << ", antenna" << endl;
    }
    // Field expression
    if (definedAndSet(selectionItem,"field")) {
      setFieldExpr(selectionItem.asString("field"));
      //    cout << fieldExpr_p << ", field" << endl;
    }
    
    // SPW expression
    if (definedAndSet(selectionItem,"spw")) {
      setSpwExpr(selectionItem.asString("spw"));
      //    cout << spwExpr_p << ", spw" << endl;
    }
    
    // Scan expression
    if (definedAndSet(selectionItem,"scan")) {
      setScanExpr(selectionItem.asString("scan"));
      //    cout << scanExpr_p << ", scan" << endl;
    }
    
    // Observation expression
    if (definedAndSet(selectionItem,"obsrevation")) {
      setObservationExpr(selectionItem.asString("observation"));
    }
    
    // Array expression
    if (definedAndSet(selectionItem,"array")) {
      setArrayExpr(selectionItem.asString("array"));
    }
    
    // Time expression
    if (definedAndSet(selectionItem,"time")) {
      setTimeExpr(selectionItem.asString("time"));
      //    cout << timeExpr_p << ", time" << endl;
    }
    
    // UV Distribution expression
    if (definedAndSet(selectionItem,"uvdist")) {
      setUvDistExpr(selectionItem.asString("uvdist"));
      //    cout << uvDistExpr_p << ", uvdist" << endl;
    }
    // Poln expression
    if (definedAndSet(selectionItem,"poln")) {
      setUvDistExpr(selectionItem.asString("poln"));
      //    cout << polnExpr_p << ", poln" << endl;
    }
  }
  
  Bool MSSelection::definedAndSet(const Record& inpRec, const String& fieldName)
  {
    // Check if a record field is defined and not unset
    // Input:
    //    inpRec          const Record&     Input Record
    //    fieldName       const String&     Field name
    // Ouput:
    //    definedAndSet   Bool              True if field defined and
    //                                      not unset
    //
    Bool retval = False;
    // Check if record field is defined
    if (inpRec.isDefined(fieldName)) {
      retval = True;
      // Now check if unset
      //    if (inpRec.dataType(fieldName) == TpRecord) {
      //      retval = !Unset::isUnset(inpRec.subRecord(fieldName));
      //    };
    };
    return retval;
  }
  
  //----------------------------------------------------------------------------
  
  
} //# NAMESPACE CASACORE - END

