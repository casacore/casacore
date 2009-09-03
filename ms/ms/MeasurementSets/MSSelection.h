//# MSSelection.h: Class to represent a selection on an MS
//# Copyright (C) 1996,1997,1998,1999,2001
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
//# Correspondence concerning AIPS++ should be adressed as follows:
//#        Internet email: aips2-request@nrao.edu.
//#        Postal address: AIPS++ Project Office
//#                        National Radio Astronomy Observatory
//#                        520 Edgemont Road
//#                        Charlottesville, VA 22903-2475 USA
//#
//#
//# $Id$

#ifndef MS_MSSELECTION_H
#define MS_MSSELECTION_H

#include <casa/aips.h>
#include <casa/BasicSL/String.h>
#include <casa/Arrays/Vector.h>
#include <measures/Measures/MEpoch.h>
#include <measures/Measures/MRadialVelocity.h>
#include <tables/Tables/ExprNode.h>
#include <ms/MeasurementSets/MeasurementSet.h>
#include <casa/Arrays/Matrix.h>
#include <casa/Arrays/Cube.h>
#include <ms/MeasurementSets/MSSelectionError.h>
#include <casa/Containers/OrderedMap.h>
#include <casa/Containers/MapIO.h>
namespace casa { //# NAMESPACE CASA - BEGIN

// <summary> 
// MSSelection: Class to represent a selection on an MS
// </summary>

// <use visibility=export>

// <reviewed reviewer="" date="" tests="" demos="">

// <prerequisite>
//   <li> <linkto class="MeasurementSet">MeasurementSet</linkto> module
// </prerequisite>
//
// <etymology>
// From "MeasurementSet" and "selection".
// </etymology>
//
// <synopsis>
// The MSSelection class represents a selection on a MeasurementSet (MS).
// This class is used in translating MS selections represented as
// selection items in the user interface, and for converting between
// MS selection and pure TaQL selection.
//
// The purpose of this class is to provides a simple expression based
// selection mechanism to both the end-user and developer wishing to
// perform query operations over a measurement set.  This is
// accomplished by abstracting the TaQL interface through an
// adapter/translation interface which converts STaQL (Simple Table
// Query Language) expressions into the equivalent table expression
// form, reducing the knowledge necessary to perform powerful query
// operations directly in TaQL.  It is also possible to supply pure
// TaQL expression(s) as sub-expressions if required. For a complete
// list of the STaQL interface refer to: <a
// href="http://casa.nrao.edu/Memos/msselection/index.html">Data
// Selection</a>
//
// The subexpressions are interpreted in the order which they were
// set.  The order however in not important - any dependency on the
// order in which the expressions are evaluated is handled internally.
// The result of parsing the expressions is TableExprNode (TEN).  All
// TENs from sub-expressions are finally ANDed and the resultant TEN
// is used to select the rows of the MS table.
//
// </synopsis>
//
// <example>
// <srcblock>
// // Create a MS and a MS selection
// MeasurementSet ms(msName);
// MSSelection select;
// // Setup any subexpressions of interest directly
// // (or optionally send this information through a Record)
// select.setFieldExpr("0,1");
// select.setSpwExpr(">0");
// // Create a table expression over a MS representing the selection
// TableExprNode node = select.toTableExprNode(&ms);
// // Optionally create a table and new MS based on this node
// Table tablesel(ms.tableName(), Table::Update);
// MeasurementSet mssel(tablesel(node, node.nrow()));
// </srcblock>
// </example>
//
// <motivation>
// This class is used by the MS access classes.
// </motivation>
//
// <todo asof="Aug/14/2009">
// Generalize SpwExpressions and PolnExpressions to optionally include
// DataDescription ID specifications.
// </todo>

  class MSSelection
  {
  public:
    enum MSExprType {NO_EXPR = 0,
		     ANTENNA_EXPR,
		     CORR_EXPR,
		     FIELD_EXPR,
		     SPW_EXPR,
		     SCAN_EXPR,
		     ARRAY_EXPR,
		     TIME_EXPR,
		     UVDIST_EXPR,
		     POLN_EXPR,
		     TAQL_EXPR,
		     MAX_EXPR = TAQL_EXPR};
    enum MSSMode {PARSE_NOW=0, PARSE_LATE};
    // Default null constructor, and destructor
    MSSelection();
    virtual ~MSSelection();
    
    // Construct from a record representing a selection item
    // at the CLI or user interface level.
    MSSelection(const MeasurementSet& ms,
		const MSSMode& mode=PARSE_NOW,
		const String& timeExpr="",
		const String& antennaExpr="",
		const String& fieldExpr="",
		const String& spwExpr="",
		const String& uvDistExpr="",
		const String& taqlExpr="",
		const String& polnExpr="",
		const String& scanExpr="",
		const String& arrayExpr="");
    
    MSSelection(const Record& selectionItem);
    
    // Copy constructor
    MSSelection(const MSSelection& other);
    
    // Assignment operator
    MSSelection& operator=(const MSSelection& other);
    
    // Helper method for converting index vectors to expression strings
    static String indexExprStr(Vector<Int> index);
    
    // Helper method for converting name vectors to expression strings
    static String nameExprStr(Vector<String> name);
    
    // Expression accessors
    Bool setAntennaExpr(const String& antennaExpr);
    Bool setFieldExpr(const String& fieldExpr);
    Bool setSpwExpr(const String& spwExpr);
    Bool setScanExpr(const String& scanExpr);
    Bool setArrayExpr(const String& ArrayExpr);
    Bool setTimeExpr(const String& timeExpr);
    Bool setUvDistExpr(const String& uvDistExpr);
    Bool setTaQLExpr(const String& taqlExpr);
    Bool setPolnExpr(const String& polnExpr);
    
    //   inline virtual Vector<Int> getAntenna1List() {return antenna1IDs_p;}
    //   inline virtual Vector<Int> getAntenna2List() {return antenna2IDs_p;}
    //   inline virtual Vector<Int> getFieldList() {return fieldIDs_p;}
    //   inline virtual Vector<Int> getSpwList() {return spwIDs_p;}
    //   inline virtual Matrix<Int> getBaselineList() {return baselineIDs_p;}
    
    TableExprNode getTEN(const MeasurementSet*ms = NULL);
    
    inline Vector<Int> getAntenna1List(const MeasurementSet* ms=NULL) 
    {getTEN(ms); return antenna1IDs_p.copy();}
    
    inline Vector<Int> getAntenna2List(const MeasurementSet* ms=NULL) 
    {getTEN(ms); return antenna2IDs_p.copy();}
    
    inline Matrix<Int> getBaselineList(const MeasurementSet* ms=NULL) 
    {getTEN(ms); return baselineIDs_p.copy();}
    
    inline Vector<Int> getFieldList(const MeasurementSet* ms=NULL) 
    {
      if (fieldIDs_p.nelements() <= 0)
	getTEN(ms); 
      return fieldIDs_p.copy();
    }
    
    inline Vector<Int> getSpwList(const MeasurementSet* ms=NULL) 
    {
      if (spwIDs_p.nelements() <= 0)
	getTEN(ms); 
      return spwIDs_p.copy();
    }
    
    inline Matrix<Int> getChanList(const MeasurementSet* ms=NULL) 
    {if (chanIDs_p.nelements() <= 0) getTEN(ms); return chanIDs_p.copy();}
    
    inline Vector<Int> getDDIDList(const MeasurementSet* ms=NULL) 
    {if (ddIDs_p.nelements() <= 0) getTEN(ms); return ddIDs_p.copy();}
    
    inline OrderedMap<Int, Vector<Int> > getPolMap(const MeasurementSet* ms=NULL) 
    {getTEN(ms); return selectedPolMap_p;};
    
    inline OrderedMap<Int, Vector<Vector<Int> > > getCorrMap(const MeasurementSet* ms=NULL) 
    {getTEN(ms); return selectedSetupMap_p;};
    
    inline Vector<Int> getScanList(const MeasurementSet* ms=NULL) 
    {getTEN(ms); return scanIDs_p.copy();}
    
    inline Vector<Int> getSubArrayList(const MeasurementSet* ms=NULL) 
    {getTEN(ms); return arrayIDs_p.copy();}
    
    inline Matrix<Double> getTimeList(const MeasurementSet* ms=NULL)
    {getTEN(ms); return selectedTimesList_p.copy();}
    
    inline Matrix<Double> getUVList(const MeasurementSet* ms=NULL) 
    {getTEN(ms); return selectedUVRange_p.copy();}
    
    inline Vector<Bool> getUVUnitsList(const MeasurementSet* ms=NULL) 
    {getTEN(ms); return selectedUVUnits_p.copy();}
    
    // Clear subexpression and reset priority.  Default behaviour is to
    // reset all sub-expressions.
    void clear(const MSExprType type=NO_EXPR);
    
    // Convert to TableExprNode format (C++ interface to TaQL)
    TableExprNode toTableExprNode(const MeasurementSet* ms);
    
    Bool getSelectedMS(MeasurementSet& selectedMS,
		       const String& outMSName="");
    
    void resetMS(const MeasurementSet& ms) {resetTEN(); ms_p=&ms;};
    void resetTEN() {fullTEN_p=TableExprNode();};
    
    void reset(const MeasurementSet& ms,
	       const MSSMode& mode=PARSE_NOW,
	       const String& timeExpr="",
	       const String& antennaExpr="",
	       const String& fieldExpr="",
	       const String& spwExpr="",
	       const String& uvDistExpr="",
	       const String& taqlExpr="",
	       const String& polnExpr="",
	       const String& scanExpr="",
	       const String& arrayExpr="");
    
    void setMaxScan(const Int& n) {maxScans_p=n;};
    
  private:
    // Set into the order of the selection expression
    Bool setOrder(MSSelection::MSExprType type);
    
    // Initialize from a Record representing a selection
    // item from the user interface or CLI
    void fromSelectionItem(const Record& selectionItem);
    
    // Check if record field exists and is not unset
    Bool definedAndSet(const Record& inpRec, const String& fieldName);
    
    // Convert an MS select string to TaQL
    //   const String msToTaQL(const String& msSelect) {};
    
    TableExprNode fullTEN_p;
    const MeasurementSet *ms_p;
    // Selection expressions
    String antennaExpr_p;
    String fieldExpr_p;
    String spwExpr_p;
    String scanExpr_p;
    String arrayExpr_p;
    String timeExpr_p;
    String uvDistExpr_p;
    String polnExpr_p;
    String taqlExpr_p;
    // Priority
    Vector<Int> exprOrder_p;
    Vector<Int> antenna1IDs_p,antenna2IDs_p,fieldIDs_p, spwIDs_p, scanIDs_p, arrayIDs_p,
      ddIDs_p;
    Matrix<Int> chanIDs_p;
    Matrix<Int> baselineIDs_p;
    Matrix<Double> selectedTimesList_p;
    Matrix<Double> selectedUVRange_p;
    Vector<Bool> selectedUVUnits_p;
    OrderedMap<Int, Vector<Int> > selectedPolMap_p;
    OrderedMap<Int, Vector<Vector<Int> > > selectedSetupMap_p;
    Int maxScans_p, maxArray_p;
  };
  
} //# NAMESPACE CASA - END

#endif
