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
//# Correspondence concerning AIPS++ should be addressed as follows:
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

#include <casacore/casa/aips.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/measures/Measures/MEpoch.h>
#include <casacore/measures/Measures/MRadialVelocity.h>
#include <casacore/tables/TaQL/ExprNode.h>
#include <casacore/ms/MeasurementSets/MeasurementSet.h>
#include <casacore/casa/Arrays/Matrix.h>
#include <casacore/casa/Arrays/Cube.h>
#include <casacore/ms/MSSel/MSSelectionError.h>
#include <casacore/ms/MSSel/MSSelectionErrorHandler.h>
#include <casacore/ms/MSSel/MSSelectableTable.h>
#include <casacore/casa/Containers/OrderedMap.h>
#include <casacore/casa/Containers/MapIO.h>
namespace casacore { //# NAMESPACE CASACORE - BEGIN

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
// list of the STaQL interface refer to the MeasurementSet Selection Syntax document at: <a
// href="http://casa.nrao.edu/other_doc.shtml">Data
// Selection</a>
//
// The sub-expressions are interpreted in the order which they were
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
// // Setup any sub-expressions of interest directly
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
		     STATE_EXPR,
		     OBSERVATION_EXPR,
		     TAQL_EXPR,
		     MAX_EXPR = TAQL_EXPR};
    enum MSSMode {PARSE_NOW=0, PARSE_LATE};

    // Default null constructor, and destructor
    MSSelection();
    virtual ~MSSelection();
    
    // Construct using an MS and the various selection expressions to
    // be applied to the given MS.  By default, the expressions will
    // be parsed immediately.  With mode=PARSE_LATE, the parsing will
    // be done with a call to toTableExprNode().
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
		const String& arrayExpr="",
		const String& stateExpr="",
		const String& observationExpr="");
    
    // Construct from a record representing a selection item at the
    // CLI or user interface level.  This is functionally same as the
    // constructor above with mode=PARSE_LATE.
    MSSelection(const Record& selectionItem);
    
    // Copy constructor
    MSSelection(const MSSelection& other);
    
    // Assignment operator
    MSSelection& operator=(const MSSelection& other);
    
    // Helper method for converting index vectors to expression strings
    static String indexExprStr(Vector<Int> index);
    
    // Helper method for converting name vectors to expression strings
    static String nameExprStr(Vector<String> name);
    
    // Expression setters.  The following set*Expr() methods only set
    // the expressions.  Parsing is done with a call to
    // toTableExprNode().
    Bool setAntennaExpr(const String& antennaExpr);
    Bool setFieldExpr(const String& fieldExpr);
    Bool setSpwExpr(const String& spwExpr);
    Bool setScanExpr(const String& scanExpr);
    Bool setArrayExpr(const String& ArrayExpr);
    Bool setTimeExpr(const String& timeExpr);
    Bool setUvDistExpr(const String& uvDistExpr);
    Bool setTaQLExpr(const String& taqlExpr);
    Bool setPolnExpr(const String& polnExpr);
    Bool setStateExpr(const String& stateExpr);
    Bool setObservationExpr(const String& obervationExpr);
    
    // Accessor for result of parsing all of the selection
    // expressions.  The final TableExprNode (TEN) is the result of
    // ANDing the TENs for the individual expressions.
    TableExprNode getTEN(const MeasurementSet*ms = NULL);

    // Accessor for the list of the selected scan IDs.
    inline Vector<Int> getScanList(const MeasurementSet* ms=NULL) 
    {getTEN(ms); return scanIDs_p;}

    // Accessor for the list of the selected observation IDs.
    inline Vector<Int> getObservationList(const MeasurementSet* ms=NULL) 
    {getTEN(ms); return observationIDs_p;}

    // Accessor for the list of selected sub-array IDs.
    inline Vector<Int> getSubArrayList(const MeasurementSet* ms=NULL) 
    {getTEN(ms); return arrayIDs_p;}
    
    // Accessor for the list of antenna-1 of the selected baselines.
    // Antennas affected by the baseline negation operator have the
    // antenna IDs multiplied by -1.
    inline Vector<Int> getAntenna1List(const MeasurementSet* ms=NULL) 
    {// if (antenna1IDs_p.nelements() <= 0) 
	getTEN(ms); return antenna1IDs_p;}
    
    // Accessor for the list of antenna-2 of the selected baselines.
    // Antennas affected by the baseline negation operator have the
    // antenna IDs multiplied by -1.
    inline Vector<Int> getAntenna2List(const MeasurementSet* ms=NULL) 
    {// if (antenna2IDs_p.nelements() <= 0) 
	getTEN(ms); return antenna2IDs_p;}
    
    // Accessor for the list of selected baselines.  The list is a Nx2
    // Matrix with one row per baseline containing the antenna IDs of
    // the two antenna associated with the baseline.
    //
    // Baselines affected by the negation operator in the baseline
    // selection expression are reported with one or both the antenna
    // IDs multiplied by -1.  E.g. a baseline selection expression
    // "!1" will result in a baseline list
    //
    // [-1, 2],
    // [-1, 3],
    // [-1, 4],
    // ....
    //
    // The expression "!1&10" will result in a baseline list [-1,
    // -10].  Etc...
    //
    inline Matrix<Int> getBaselineList(const MeasurementSet* ms=NULL) 
    {getTEN(ms); return baselineIDs_p;}
    
    // Accessor for the list of selected field IDs.
    inline Vector<Int> getFieldList(const MeasurementSet* ms=NULL) 
    {// if (fieldIDs_p.nelements() <= 0) 
	getTEN(ms); return fieldIDs_p;}

    // Accessor for the list of selected state Obs_Modes.
    inline Vector<Int> getStateObsModeList(const MeasurementSet* ms=NULL) 
    {if (stateObsModeIDs_p.nelements() <= 0) getTEN(ms); return stateObsModeIDs_p;}
    
    // Accessor for the list of the specified time range(s) as the
    // start and end MJD values.  The time ranges are stored as columns,
    // i.e. the output Matrix is 2 x n_ranges.
    inline Matrix<Double> getTimeList(const MeasurementSet* ms=NULL)
    {getTEN(ms); return selectedTimesList_p;}
    
    // Accessor for the list of the specified uv-range(s) as the start
    // and end values in units used in the MS.
    inline Matrix<Double> getUVList(const MeasurementSet* ms=NULL) 
    {getTEN(ms); return selectedUVRange_p;}
    
    // Accessor for the list of user defined units for the
    // uv-range(s).  The uv-range(s) return by getUVList is always in
    // the units used in the MS.
    inline Vector<Bool> getUVUnitsList(const MeasurementSet* ms=NULL) 
    {getTEN(ms); return selectedUVUnits_p;}

    // Accessor for the list of the selected Spectral Window IDs.
    inline Vector<Int> getSpwList(const MeasurementSet* ms=NULL) 
    {// if (spwIDs_p.nelements() <= 0) 
	getTEN(ms); return spwIDs_p;}
    
    // Accessor for the table (as a nx4 Matrix) of the selected
    // Spectral Windows and associated ranges of selected channels.
    // Each row of the Matrix has the following elements:
    //
    //    SpwID StartCh StopCh Step
    //
    // where StartCh, StopCh and Step are the first and the last
    // selected channels and step is the step size.  If no step size
    // was supplied as part of the expression, the value of Step is
    // replaced with the value of the defaultStep parameter. Multiple
    // channel specifications for the same Spectral Window selection,
    // results in multiple rows in the Matrix. If sorted is True, the
    // rows of the output Matrix will be sorted by the SPW IDs (the
    // entries in the first column).
    Matrix<Int> getChanList(const MeasurementSet* ms=NULL, 
			    const Int defaultStep=1,
			    const Bool sorted=False);

    //
    // Same as getChanList, except that the channels and steps are in Hz.
    //    
    Matrix<Double> getChanFreqList(const MeasurementSet* ms=NULL, 
				   const Bool sorted=False);

    // Accessor for the list of the selected Data Description IDs
    // (DDID) from the polarization expression parsing.  The actual
    // selected DDIDs would be an intersection of the DDIDs selected
    // from polarization and SPW expressions parsing (see
    // getSPWDDIDList() below).
    inline Vector<Int> getDDIDList(const MeasurementSet* ms=NULL) 
    {if (ddIDs_p.nelements() <= 0) getTEN(ms); return ddIDs_p;}

    // Accessor for the list of the selected Data Description IDs from
    // the SPW expression parsing.  The actual
    // selected DDIDs would be an intersection of the DDIDs selected
    // from polarization and SPW expressions parsing (see
    // getDDIDList() above).
    //
    // The actual DDIDs selected will be an intersection of the lists
    // from getDDIDList() and getSPWDDIDList() (which can be generated
    // using the set_intersection(Vector<Int>&, Vector<Int>&) global
    // method in MSSelectionTool.{cc,h}).
    inline Vector<Int> getSPWDDIDList(const MeasurementSet* ms=NULL) 
    {if (spwDDIDs_p.nelements() <= 0) getTEN(ms); return spwDDIDs_p;}

    //
    // The key in the ordered map returned by getPolMap() is the Data
    // Description ID (DDID). The value is a vector containing the
    // list of in-row indices to pick out the selected polarizations
    // (or equivalently, the list of indices for the vector in the
    // corrType column of the POLARIZATION sub-table). These are also
    // what the user intended (i.e., e.g. not all DD IDs due to user
    // POL expression might be selected due to SPW expressions).
    //
    inline OrderedMap<Int, Vector<Int> > getPolMap(const MeasurementSet* ms=NULL) 
    {getTEN(ms); return selectedPolMap_p;};

    //
    // The key in the ordered map returned by getCorrMap() is the
    // pol. in the Data Description ID (DDID) sub-table.  The value is
    // a Vector of two Vectors.
    //
    // The returned Map<T> has a key that maps to two vectors:
    // Key  ---->    Vector1               Vector2
    //
    // Key : Row index in the POLARIZATION sub-table
    //
    // Vector1 : List of poln. indices selected from the row pointed
    //           by Key.  These are the in-row indices to pick-out the
    //           desired (selected) polarization products from the
    //           selected rows of the MS (or equivalently, the list of
    //           indices for the vector in the corrType column of the
    //           POLARIZATION sub-table).
    //
    // Vector2 : List of selected rows from the DATA_DESCRIPTION sub-table 
    //
    // An example: following are the sub-tables used for the example
    // explaination below:
    //
    // POLARIZATION Sub-table
    // ======================
    // Row    Poln
    // ------------
    // 0         RR, LL
    // 1         RR, LR, RL, LL
    //
    // DATA_DESCRIPTION Sub-table
    // ==========================
    // Row      PolnID         SpwID
    // ------------------------------
    // 0          0              0
    // 1          1              1
    // 2          1              2
    // 3          1              3
    // 4          1              4
    // 5          1              5
    // 6          1              6
    // 7          1              7
    // 8          1              8
    //
    //
    // E.g., the expression poln='LL'
    //
    // returns the Map:
    //
    // corrmap = (0, [[1], [0]]) (1, [[3], [0,1,2,3,4,5,6,7,8]] )
    //
    // The rows from the POLARIZATION table selected are 0 and 1,  These are
    // two keys for the two entries in the map.
    //
    //  1. The two vectors in map 1 are:  [1] and [0].  The this reads as:
    //      From the 0th. row of the POLARIZATION table, use the indices [1].  The
    //      relevant list of associated DD rows are [0]
    //
    //  2. The two vectors in map 2 are:  [3] and  [0,1,2,3,4,5,6,7,8].  This reads as:
    //       From the 1st. row of the POLARIZATION table, use the indices [3].  The
    //       relevant list of associated DD rows are [0,1,2,3,4,5,6,7,8].
    //
    // For a client code:
    //
    // o To get a list of the DDIDs selected, iterate over all entries of the
    //   map and collate the second vector from each entry.
    //
    //        Or, use getDDIDList().
    //
    // o To get the list of the selected poln. *in-row indices*, collate the
    //   first vector from each entry.
    //
    // o To get a list of POLARIZATION IDs selected (rows of the POLARIZATION
    //   table), make a list of all the keys of this map.
    inline OrderedMap<Int, Vector<Vector<Int> > > getCorrMap(const MeasurementSet* ms=NULL) 
    {getTEN(ms); return selectedSetupMap_p;};

    // Methods to convert the maps return by getChanList and
    // getCorrMap to a list of Slice which can be directly used by
    // Table system for in-row selection of frequency channels and
    // polarizations.
    void getChanSlices(Vector<Vector<Slice> >& chanslices, 
		       const MeasurementSet* ms=NULL, 
		       const Int defaultChanStep=1);

    void getCorrSlices(Vector<Vector<Slice> >& corrslices,
		       const MeasurementSet* ms=NULL);
    
    
    // Clear sub-expression and reset priority.  Default behavior is to
    // reset all sub-expressions.
    void clear(const MSExprType type=NO_EXPR);
    Bool exprIsNull(const MSExprType type=NO_EXPR);
    
    // Convey to the various parsers to delete the TENs they hold
    void deleteNodes();

    // Delete error handlers (mostly the internally allocated ones).
    void deleteErrorHandlers();

    // Convert to TableExprNode format (C++ interface to TaQL).  This
    // is now for purely backwards compatibility and ease of use.  It
    // internally constructs the MSSelectableTable from the supplied
    // MS and calls the generic version of toTableExprNode below
    // (which works with MSSelectableTable object).
    TableExprNode toTableExprNode(const MeasurementSet* ms);

    // Convert to TableExprNode format (C++ interface to TaQL).  The
    // MSSelectableTable is a pure-virtual base class which provides a
    // generic interface both to MeasurementSet and CalTable (in the
    // synthesis module) services used in MSSelection.  The actual
    // objects used for supplying MeasurementSet or CalTable to
    // MSSelection are MSInterface and CTInterface classes
    // respectively.  With this, MSSelection module can be used for
    // selection on MeasurementSet or CalTable.
    TableExprNode toTableExprNode(MSSelectableTable* msLike);
    
    // Return the selected MS.  The selected MS reflects only row
    // selections (as against in-row selections).  If outMSName != "",
    // the selected MS is also written to the disk (a shallow copy).
    //
    // For in-row selection, use the appropriate global function
    // mssSetData() MSSelectionTools.h which also returns the in-row
    // (corr/chan) slices that can be supplied to the VisIter object
    // for on-the-fly in-row selection.
    Bool getSelectedMS(MeasurementSet& selectedMS,
		       const String& outMSName="");
    
    void resetMS(const MeasurementSet& ms) {resetTEN(); ms_p=&ms;};
    void resetTEN() {fullTEN_p=TableExprNode();};
    
    
    // The MSSelection object is designed to be re-usable object.  The
    // following reset() methods set the internal state of the object
    // to same state as with the equivalent constructor.
    //
    // mode can be one of the MSSModes.  MSSMode::PARSE_NOW will parse
    // the given expressions and internally hold the final TEN
    // (i.e. will also internally call toTableExprNode()).  The
    // internal TEN can be accessed via the getTEN() method.
    // MSSMode::PARSE_LATER will only set the expression strings.
    // Parsing will be done later with a call to toTableExprNode().
    //
    // This version, here for backward compatibility reasons,
    // internally constructs a
    // <linkto class="MSSelectableTable">MSSelectableTable</linkto>
    // object and calls the reset() method below that works with
    // MSSelectableTable.
    void reset(const MeasurementSet& ms,
	       const MSSMode& mode           = PARSE_NOW,
	       const String& timeExpr        = "",
	       const String& antennaExpr     = "",
	       const String& fieldExpr       = "",
	       const String& spwExpr         = "",
	       const String& uvDistExpr      = "",
	       const String& taqlExpr        = "",
	       const String& polnExpr        = "",
	       const String& scanExpr        = "",
	       const String& arrayExpr       = "",
	       const String& stateExpr       = "",
	       const String& observationExpr = "");

    // This version of reset() works with generic MSSeletableTable
    // object.  Accessing the services of the MSSelection module via
    // this interface is recommended over the version of reset() that
    // uses MeasurementSet.
    void reset(MSSelectableTable& msLike,
	       const MSSMode& mode           = PARSE_NOW,
	       const String& timeExpr        = "",
	       const String& antennaExpr     = "",
	       const String& fieldExpr       = "",
	       const String& spwExpr         = "",
	       const String& uvDistExpr      = "",
	       const String& taqlExpr        = "",
	       const String& polnExpr        = "",
	       const String& scanExpr        = "",
	       const String& arrayExpr       = "",
	       const String& stateExpr       = "",
	       const String& observationExpr = "");

    // Set the maximum value acceptable for SCAN, OBSERVATION or
    // SUB-ARRAY IDs. The main-table columns for these do not refere
    // to rows of sub-tables and therefore there is no cheap way to
    // find a valid range for these which can be used in the parsers
    // to generate error or warning messages if a value outside the
    // range is used in the expressions.  The default maximum value
    // for scan, observation and sub-array IDs is 1000.
    inline void setMaxScans(const Int& n=1000) {maxScans_p=n;};
    inline void setMaxObs(const Int& n=1000)   {maxObs_p=n;};
    inline void setMaxArray(const Int& n=1000) {maxArray_p=n;};
    
    // Set the error handler to be used for reporting errors while
    // parsing the type of expression give by the first argument.
    void setErrorHandler(const MSExprType type, MSSelectionErrorHandler* mssEH,
			 const Bool overRide=False);
    
    // Initialize the error handler.  This is set the error-handler to
    // the user supplied error handler via setErrorHandler() or to the
    // default built-in error handler.
    void initErrorHandler(const MSExprType tye=NO_EXPR);

    // Execute the handleError() method of the error-handlers.  This
    // is called in the catch code for any exceptions emitted from any
    // of the parsers. It is also called at the end of the
    // parsing cycle.
    void runErrorHandler();

    // Return the pointer to the MS used internally.
    const MeasurementSet* getMS(MSSelectableTable* msLike);

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
    String stateExpr_p;
    String observationExpr_p;
    // Priority
    Vector<Int> exprOrder_p;
    Vector<Int> antenna1IDs_p,antenna2IDs_p,fieldIDs_p, spwIDs_p, scanIDs_p, arrayIDs_p,
      ddIDs_p,stateObsModeIDs_p, observationIDs_p, spwDDIDs_p;
    Matrix<Int> chanIDs_p;
    Matrix<Int> baselineIDs_p;
    Matrix<Double> selectedTimesList_p;
    Matrix<Double> selectedUVRange_p;
    Vector<Bool> selectedUVUnits_p;
    OrderedMap<Int, Vector<Int> > selectedPolMap_p;
    OrderedMap<Int, Vector<Vector<Int> > > selectedSetupMap_p;
    Int maxScans_p, maxObs_p, maxArray_p;
    MSSelectionErrorHandler* mssErrHandler_p;
    Bool isMS_p,toTENCalled_p;
  };
  
} //# NAMESPACE CASACORE - END

#endif


