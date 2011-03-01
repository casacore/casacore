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

#include <ms/MeasurementSets/MSSelection.h>
#include <ms/MeasurementSets/MSAntennaGram.h>
#include <ms/MeasurementSets/MSCorrGram.h>
#include <ms/MeasurementSets/MSFieldGram.h>
#include <ms/MeasurementSets/MSSpwGram.h>
#include <ms/MeasurementSets/MSScanGram.h>
#include <ms/MeasurementSets/MSArrayGram.h>
#include <ms/MeasurementSets/MSTimeGram.h>
#include <ms/MeasurementSets/MSUvDistGram.h>
#include <ms/MeasurementSets/MSPolnGram.h>
#include <ms/MeasurementSets/MSRange.h>
#include <tables/Tables/TableParse.h>
#include <tables/Tables/RecordGram.h>

#include <ms/MeasurementSets/MSMainColumns.h>
#include <measures/Measures/MeasureHolder.h>
#include <measures/Measures/MEpoch.h>
#include <casa/Quanta/MVTime.h>
#include <casa/Quanta/QuantumHolder.h>
#include <casa/Arrays/ArrayMath.h>
#include <casa/Containers/Record.h>
#include <casa/Utilities/DataType.h>
#include <casa/iostream.h>
#include <ms/MeasurementSets/MSSelectionError.h>
#include <casa/Logging/LogIO.h>
#include <casa/Exceptions/Error.h>

namespace casa { //# NAMESPACE CASA - BEGIN
  
  //----------------------------------------------------------------------------
  
  MSSelection::MSSelection() : 
    fullTEN_p(),ms_p(NULL), antennaExpr_p(""), fieldExpr_p(""),
    spwExpr_p(""), scanExpr_p(""), arrayExpr_p(""), timeExpr_p(""), uvDistExpr_p(""),
    polnExpr_p(""), taqlExpr_p(""), exprOrder_p(MAX_EXPR, NO_EXPR),
    antenna1IDs_p(), antenna2IDs_p(), fieldIDs_p(), spwIDs_p(), 
    scanIDs_p(),arrayIDs_p(), ddIDs_p(), baselineIDs_p(),selectedTimesList_p(),
    selectedUVRange_p(),selectedUVUnits_p(),selectedPolMap_p(Vector<Int>(0)),
    selectedSetupMap_p(Vector<Vector<Int> >(0)),
    maxScans_p(1000), maxArray_p(1000)
  {
    clear();
    // Default null constructor 
    // Output to private data:
    //    antennaExpr_p             String   Antenna STaQL expression
    //    polnExpr_p                String   Polarization STaQL expression
    //    fieldExpr_p               String   Field STaQL expression
    //    spwExpr_p                 String   SPW STaQL expression
    //    scanExpr_p                String   Scan STaQL expression
    //    arrayExpr_p               String   Array_ID STaQL expression
    //    timeExpr_p                String   Time STaQL expression
    //    uvDistExpr_p              String   UV Distribution STaQL expression
    //    taqlExpr_p                String   TaQL expression
    //
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
			   const String& arrayExpr):
    fullTEN_p(), ms_p(&ms), antennaExpr_p(""), fieldExpr_p(""),
    spwExpr_p(""), scanExpr_p(""), arrayExpr_p(""), timeExpr_p(""), uvDistExpr_p(""),
    polnExpr_p(""),taqlExpr_p(""), exprOrder_p(MAX_EXPR, NO_EXPR),
    antenna1IDs_p(), antenna2IDs_p(), fieldIDs_p(), spwIDs_p(), 
    scanIDs_p(),ddIDs_p(),baselineIDs_p(), selectedTimesList_p(),
    selectedUVRange_p(),selectedUVUnits_p(),selectedPolMap_p(Vector<Int>(0)),
    selectedSetupMap_p(Vector<Vector<Int> >(0)),
    maxScans_p(1000), maxArray_p(1000)
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
	  arrayExpr);
  }
  
  
  //----------------------------------------------------------------------------
  
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
			  const String& arrayExpr)
  {
    //
    // Do not initialize the private string variables directly. Instead
    // using the setExpr* methods to do that keeps that state of the
    // object consistent.
    //
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
    
    if (mode==PARSE_NOW)
      fullTEN_p = toTableExprNode(ms_p);
  }
  
  //----------------------------------------------------------------------------
  
  MSSelection::~MSSelection()
  {
    // Default desctructor
    //
  }
  
  //----------------------------------------------------------------------------
  
  MSSelection::MSSelection(const Record& selectionItem) : 
    antennaExpr_p(""), fieldExpr_p(""),
    spwExpr_p(""), scanExpr_p(""), arrayExpr_p(""), timeExpr_p(""), uvDistExpr_p(""),
    polnExpr_p(""),taqlExpr_p(""),antenna1IDs_p(), antenna2IDs_p(), fieldIDs_p(), 
    spwIDs_p(),ddIDs_p(),baselineIDs_p(), selectedPolMap_p(Vector<Int>(0)),
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
      this->arrayExpr_p   = other.arrayExpr_p;
      this->timeExpr_p    = other.timeExpr_p;
      this->uvDistExpr_p  = other.uvDistExpr_p;
      this->taqlExpr_p    = other.taqlExpr_p;
      this->polnExpr_p    = other.polnExpr_p;
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
      this->arrayExpr_p   = other.arrayExpr_p;
      this->timeExpr_p    = other.timeExpr_p;
      this->uvDistExpr_p  = other.uvDistExpr_p;
      this->taqlExpr_p    = other.taqlExpr_p;
      this->polnExpr_p    = other.polnExpr_p;
      this->exprOrder_p   = other.exprOrder_p;
    }
    
    return *this;
  }
  
  //----------------------------------------------------------------------------
  
  TableExprNode MSSelection::getTEN(const MeasurementSet*ms)
  {
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
    if (fullTEN_p.isNull()==False) return fullTEN_p;
    
    resetMS(*ms);
    TableExprNode condition;
    
    for(uInt i=0; i<exprOrder_p.nelements(); i++)
      {
	TableExprNode node;
	switch(exprOrder_p[i])
	  {
	  case ANTENNA_EXPR:
	    {
	      antenna1IDs_p.resize(0);
	      antenna2IDs_p.resize(0);
	      baselineIDs_p.resize(0,2);
	      if(antennaExpr_p != "") {
                node = msAntennaGramParseCommand(ms, antennaExpr_p, 
                                                 antenna1IDs_p, 
                                                 antenna2IDs_p,
                                                 baselineIDs_p);
              }
	      break;
	    }
	  case FIELD_EXPR:
	    {
	      fieldIDs_p.resize(0);
	      if(fieldExpr_p != "" &&
		 msFieldGramParseCommand(ms, fieldExpr_p,fieldIDs_p) == 0)
		node = *(msFieldGramParseNode());
	      break;
	    }
	  case SPW_EXPR:
	    {
	      spwIDs_p.resize(0);
	      if (spwExpr_p != "" &&
		  msSpwGramParseCommand(ms, spwExpr_p,spwIDs_p, chanIDs_p) == 0)
		node = *(msSpwGramParseNode());
	      break;
	    }
	  case SCAN_EXPR:
	    {
	      scanIDs_p.resize(0);
	      if(scanExpr_p != "" && 
		 msScanGramParseCommand(ms, scanExpr_p, scanIDs_p, maxScans_p) == 0)
		node = *(msScanGramParseNode());
	      break;
	    }
	  case ARRAY_EXPR:
	    {
	      arrayIDs_p.resize(0);
	      if(arrayExpr_p != "" &&
		 msArrayGramParseCommand(ms, arrayExpr_p, arrayIDs_p, maxArray_p) == 0)
		node = *(msArrayGramParseNode());
	      break;
	    }
	    /*
	      case TIME_EXPR:
	      if(timeExpr_p != "" &&
	      msTimeGramParseCommand(ms, timeExpr_p) == 0)
	      node = *(msTimeGramParseNode());
	      break;
	    */
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
		  //	      taql = tableCommand(taqlExpr_p).node();
		  node = RecordGram::parse(*ms,taqlExpr_p);
		}
	      break;
	    }
	  case POLN_EXPR:
	    {
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
    const TableExprNode *timeNode = 0x0;
    selectedTimesList_p.resize(2,0);
    if(timeExpr_p != "" &&
       msTimeGramParseCommand(ms, timeExpr_p, condition, selectedTimesList_p) == 0)
      timeNode = msTimeGramParseNode();
    //
    // Add the time-expression TEN to the condition
    //
    if(timeNode && !timeNode->isNull()) {
      if(condition.isNull()) {
	condition = *timeNode;
      } else {
	condition = condition && *timeNode;
      }
    }
    
    fullTEN_p = condition;
    msArrayGramParseDeleteNode();
    msCorrGramParseDeleteNode();
    msFieldGramParseDeleteNode();
    msScanGramParseDeleteNode();
    msSpwGramParseDeleteNode();
    msTimeGramParseDeleteNode();
    msUvDistGramParseDeleteNode();
    
    return condition;
  }
  
  //----------------------------------------------------------------------------
  Bool MSSelection::getSelectedMS(MeasurementSet& selectedMS, 
				  const String& outMSName)
  {
    Bool newRefMS=False;
    if (fullTEN_p.isNull()) fullTEN_p=toTableExprNode(ms_p);
    if ((!fullTEN_p.isNull()) && (fullTEN_p.nrow() > 0))
      {
	selectedMS = MS((*ms_p)(fullTEN_p));
	if (outMSName!="") selectedMS.rename(outMSName,Table::New);
	selectedMS.flush();
	newRefMS=True;
      }
    //     MSRange msr(selectedMS);
    //     Vector<String> items(1); items(0)="SCAN_NUMBER";
    //     Record scanRange=msr.range(items);
    //     scanRange.get(scanRange.fieldNumber("scan_number"),scanIDs_p);
    return newRefMS;
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
		default:;
		}
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
			      chanmat(i,2)-chanmat(i,1)+1,
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
    // Check if a record field is defined and not AIPS++ unset
    // Input:
    //    inpRec          const Record&     Input Record
    //    fieldName       const String&     Field name
    // Ouput:
    //    definedAndSet   Bool              True if field defined and
    //                                      not AIPS++ unset
    //
    Bool retval = False;
    // Check if record field is defined
    if (inpRec.isDefined(fieldName)) {
      retval = True;
      // Now check if AIPS++ unset
      //    if (inpRec.dataType(fieldName) == TpRecord) {
      //      retval = !Unset::isUnset(inpRec.subRecord(fieldName));
      //    }
    }
    return retval;
  }
  
  //----------------------------------------------------------------------------
  
  
} //# NAMESPACE CASA - END

