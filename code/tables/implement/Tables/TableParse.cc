//# TableParse.cc: Classes to hold results from table grammar parser
//# Copyright (C) 1994,1995,1997
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

#include <aips/Tables/TableParse.h>
#include <aips/Tables/TableGram.h>
#include <aips/Tables/ExprDerNode.h>
#include <aips/Tables/ExprNode.h>
#include <aips/Tables/ExprRange.h>
#include <aips/Tables/TableColumn.h>
#include <aips/Tables/TableDesc.h>
#include <aips/Tables/ColumnDesc.h>
#include <aips/Tables/TableError.h>
#include <aips/Measures/MUString.h>
#include <aips/Measures/MVTime.h>
#include <aips/Measures/MVAngle.h>
#include <aips/Arrays/Vector.h>
#include <aips/Arrays/ArrayIO.h>
#include <aips/Utilities/Sort.h>
#include <aips/Utilities/Assert.h>
#include <aips/IO/AipsIO.h>
#include <iostream.h>

typedef Quantum<double> gpp_bug1;



//# Make a new TableParseVal object.
TableParseVal* TableParseVal::makeValue()
{
    TableParseVal* tp = new TableParseVal;
    if (tp == 0) {
	throw (AllocError ("TableParseVal", 1));
    }
    return tp;
}



TableParseSort::TableParseSort (const TableExprNode& node, Sort::Order order)
: node_p  (node),
  order_p (order)
{}
TableParseSort::~TableParseSort()
{}



//# Default constructor.
TableParse::TableParse ()
{}

//# Constructor with given table name and possible shorthand.
TableParse::TableParse (const Table& table, const String& shorthand)
: shorthand_p (shorthand),
  table_p     (table)
{
    table_p.makePermanent();
}

TableParse::TableParse (const TableParse& that)
: shorthand_p (that.shorthand_p),
  table_p     (that.table_p)
{
    table_p.makePermanent();
}

TableParse& TableParse::operator= (const TableParse& that)
{
    shorthand_p = that.shorthand_p;
    table_p     = that.table_p;
    table_p.makePermanent();
    return *this;
}


//# The AipsIO functions are needed for the list of TableParse, but
//# we do not support it actually.
AipsIO& operator<< (AipsIO& ios, const TableParse&)
{
    throw (AipsError ("AipsIO << TableParse& not possible"));
    return ios;
}
AipsIO& operator>> (AipsIO& ios, TableParse&)
{
    throw (AipsError ("AipsIO >> TableParse& not possible"));
    return ios;
}



//# Initialize the static members.
PtrBlock<TableParseSelect*> TableParseSelect::blockSelect_p;
TableParseSelect* TableParseSelect::currentSelect_p = 0;


TableParseSelect::TableParseSelect()
: nothingDone_p (True)
{
    parseList_p = new List<TableParse>;
    parseIter_p = new ListIter<TableParse> (parseList_p);
    parseList_p->makePermanent();
    parseIter_p->makePermanent();
}

TableParseSelect::~TableParseSelect()
{
    delete parseIter_p;
    delete parseList_p;
}

const Table& TableParseSelect::getTable() const
{
    parseIter_p->toStart();
    return parseIter_p->getRight().table();
}
 
void TableParseSelect::newSelect()
{
    // Create a new one and put in the Block.
    // currentSelect_p always points to the first one
    // Check if it's the first object.
    uInt n = 0;
    if (currentSelect_p != 0) {
	n = blockSelect_p.nelements();
    }
    blockSelect_p.resize (n+1);
    currentSelect_p = new TableParseSelect;
    blockSelect_p[n] = currentSelect_p;
}

void TableParseSelect::deleteSelect()
{
    // Don't delete current if it's the only one.
    uInt n = blockSelect_p.nelements() - 1;
    if (n > 0) {
	// Delete current and let currentSelect_p point to previous.
	delete currentSelect_p;
	blockSelect_p.resize (n, True, True);
	currentSelect_p = blockSelect_p[n-1];
    }
}


//# Construct a TableParse object and add it to the linked list.
void TableParseSelect::addTable (const String& name, const String& shorthand)
{
    parseIter_p->addRight (TableParse(Table(name), shorthand));
    (*parseIter_p)++;
}


//# Lookup a field name in the table for which the shorthand is given.
//# If no shorthand is given, use the first table.
//# The shorthand and name are separated by a period.
TableExprNode TableParseSelect::handleName (String& str, Bool isArray)
{
    parseIter_p->toStart();
    int i = str.index('.');
    //# Use first table if there is no shorthand given.
    //# Construct a TableExprNode object for this column or keyword.
    if (i < 0) {
	return parseIter_p->getRight().table().keyCol(str, isArray);
    }
    //# Split string in shorthand table name and the field name.
    //# Try to find the table with that shorthand name.
    //# Exception if unknown.
    String shand = str.before(i);
    String name  = str.after(i);
    while (!parseIter_p->atEnd()) {
	if (parseIter_p->getRight().test (shand)) {
	    return parseIter_p->getRight().table().keyCol(name, isArray);
	}
	(*parseIter_p)++;
    }
    throw (TableInvExpr ("Shorthand " + shand + " has not been defined"));
    return 0;
}

TableExprNode TableParseSelect::handleArray (String& str,
					     Block<TableExprNode>& indices)
{
    TableExprNode anode = handleName (str, True);
    return TableExprNode::newArrayElementNode (anode, indices);
}
 
//# Parse the name of a function.
TableExprNode TableParseSelect::handleFunc (const String& name,
					    Block<TableExprNode>& nodes)
{
    TableExprFuncNode::FunctionType ftype;
    String funcName (name);
    funcName.downcase();
    if (funcName == "pi") {
	ftype = TableExprFuncNode::piFUNC;
    } else if (funcName == "e") {
	ftype = TableExprFuncNode::eFUNC;
    } else if (funcName == "cos") {
	ftype = TableExprFuncNode::cosFUNC;
    } else if (funcName == "cosh") {
	ftype = TableExprFuncNode::coshFUNC;
    } else if (funcName == "exp") {
	ftype = TableExprFuncNode::expFUNC;
    } else if (funcName == "log") {
	ftype = TableExprFuncNode::logFUNC;
    } else if (funcName == "log10") {
	ftype = TableExprFuncNode::log10FUNC;
    } else if (funcName == "sin") {
	ftype = TableExprFuncNode::sinFUNC;
    } else if (funcName == "sinh") {
	ftype = TableExprFuncNode::sinhFUNC;
    } else if (funcName == "square") {
	ftype = TableExprFuncNode::squareFUNC;
    } else if (funcName == "sqrt") {
	ftype = TableExprFuncNode::sqrtFUNC;
    } else if (funcName == "norm") {
	ftype = TableExprFuncNode::normFUNC;
    } else if (funcName == "acos") {
	ftype = TableExprFuncNode::acosFUNC;
    } else if (funcName == "asin") {
	ftype = TableExprFuncNode::asinFUNC;
    } else if (funcName == "atan") {
	ftype = TableExprFuncNode::atanFUNC;
    } else if (funcName == "sign") {
	ftype = TableExprFuncNode::signFUNC;
    } else if (funcName == "round") {
	ftype = TableExprFuncNode::roundFUNC;
    } else if (funcName == "ceil") {
	ftype = TableExprFuncNode::ceilFUNC;
    } else if (funcName == "floor") {
	ftype = TableExprFuncNode::floorFUNC;
    } else if (funcName == "tan") {
	ftype = TableExprFuncNode::tanFUNC;
    } else if (funcName == "tanh") {
	ftype = TableExprFuncNode::tanhFUNC;
    } else if (funcName == "pow") {
	ftype = TableExprFuncNode::powFUNC;
    } else if (funcName == "atan2") {
	ftype = TableExprFuncNode::atan2FUNC;
    } else if (funcName == "fmod") {
	ftype = TableExprFuncNode::fmodFUNC;
    } else if (funcName == "min") {
	ftype = TableExprFuncNode::minFUNC;
    } else if (funcName == "max") {
	ftype = TableExprFuncNode::maxFUNC;
    } else if (funcName == "complex") {
	ftype = TableExprFuncNode::complexFUNC;
    } else if (funcName == "abs"  ||  funcName == "amplitude") {
	ftype = TableExprFuncNode::absFUNC;
    } else if (funcName == "arg"  ||  funcName == "phase") {
	ftype = TableExprFuncNode::argFUNC;
    } else if (funcName == "conj") {
	ftype = TableExprFuncNode::conjFUNC;
    } else if (funcName == "real") {
	ftype = TableExprFuncNode::realFUNC;
    } else if (funcName == "imag") {
	ftype = TableExprFuncNode::imagFUNC;
    } else if (funcName == "datetime"  ||  funcName == "ctod") {
	ftype = TableExprFuncNode::datetimeFUNC;
    } else if (funcName == "mjdtodate") {
	ftype = TableExprFuncNode::mjdtodateFUNC;
    } else if (funcName == "mjd") {
	ftype = TableExprFuncNode::mjdFUNC;
    } else if (funcName == "date") {
	ftype = TableExprFuncNode::dateFUNC;
    } else if (funcName == "weekday"   ||  funcName == "dow") {
	ftype = TableExprFuncNode::weekdayFUNC;
    } else if (funcName == "year") {
	ftype = TableExprFuncNode::yearFUNC;
    } else if (funcName == "month") {
	ftype = TableExprFuncNode::monthFUNC;
    } else if (funcName == "day") {
	ftype = TableExprFuncNode::dayFUNC;
    } else if (funcName == "cmonth") {
	ftype = TableExprFuncNode::cmonthFUNC;
    } else if (funcName == "cweekday"   ||  funcName == "cdow") {
	ftype = TableExprFuncNode::cdowFUNC;
    } else if (funcName == "week") {
	ftype = TableExprFuncNode::weekFUNC;
    } else if (funcName == "time") {
	ftype = TableExprFuncNode::timeFUNC;
    } else if (funcName == "strlength" ||  funcName == "len") {
	ftype = TableExprFuncNode::strlengthFUNC;
    } else if (funcName == "upcase"    ||  funcName == "upper") {
	ftype = TableExprFuncNode::upcaseFUNC;
    } else if (funcName == "downcase"  ||  funcName == "lower") {
	ftype = TableExprFuncNode::downcaseFUNC;
    } else if (funcName == "trim") {
	ftype = TableExprFuncNode::trimFUNC;
    } else if (funcName == "regex") {
	ftype = TableExprFuncNode::regexFUNC;
    } else if (funcName == "pattern") {
	ftype = TableExprFuncNode::patternFUNC;
    } else if (funcName == "rownumber") {
	ftype = TableExprFuncNode::rownrFUNC;
    } else if (funcName == "rand") {
	ftype = TableExprFuncNode::randFUNC;
    } else {
	throw (TableInvExpr ("Function " + funcName + " is unknown"));
    }
    parseIter_p->toStart();
    return TableExprNode::newFunctionNode (ftype, nodes,
					   parseIter_p->getRight().table());
}


//# Convert a constant to a TableExprNode object.
//# The leading and trailing " is removed from a string.
TableExprNode TableParseSelect::handleLiteral (TableParseVal* val)
{
    TableExprNodeRep* tsnp = 0;
    String s;
    int pos = 0;
    int inx;
    switch (val->type) {
    case 'i':
	tsnp = new TableExprNodeConstDouble (double (val->ival));
	break;
    case 'f':
	tsnp = new TableExprNodeConstDouble (val->dval[0]);
	break;
    case 'c':
	tsnp = new TableExprNodeConstDComplex
	                   (DComplex (val->dval[0], val->dval[1]));
	break;
    case 's':
	//# A string is formed as "..."'...''...' etc.
	//# All ... parts will be extracted and concatenated into string s.
	while (pos < val->str.length()) {
	    //# Find next occurrence of leading ' or ""
	    inx = val->str.index (val->str[pos], pos+1);
	    if (inx < 0) {
		throw (TableInvExpr ("Ill-formed string constant: " +
				     val->str));
		break;
	    }else{
		s += val->str.at (pos+1, inx-pos-1);     // add substring
		pos = inx+1;
	    }
	}
	tsnp = new TableExprNodeConstString (s);
	break;
    case 'd':
      {
	MUString str (val->str);
	Quantity res;
	if (! MVTime::read (res, str)) {
	    throw (TableInvExpr ("invalid date string " + val->str));
	}
	tsnp = new TableExprNodeConstDate (MVTime(res));
      }
	break;
    case 't':
      {
	Quantity res;
	//# Skip a possible leading / which acts as an escape character.
	if (val->str.length() > 0  &&  val->str[0] == '/') {
	    val->str = val->str.after(0);
	}
	if (! MVAngle::read (res, val->str)) {
	    throw (TableInvExpr ("invalid time/pos string " + val->str));
	}
	tsnp = new TableExprNodeConstDouble (MVAngle(res).radian());
      }
	break;
    default:
	throw (TableInvExpr ("TableParseSelect: unhandled literal type"));
    }
    if (tsnp == 0) {
	throw (AllocError ("TableParse-node", 1));
    }
    return tsnp;
}


//# Add a column name to the block of column names.
//# Only take the part beyond the period.
//# Extend the block each time. Since there are only a few column names,
//# this will not be too expensive.
void TableParseSelect::handleColumn (String& name)
{
    Int i = name.index('.');
    Int j = columnNames_p.nelements();
    columnNames_p.resize (j+1);                  // extend block
    if (i < 0) {
	(columnNames_p)[j] = name;
    }else{
	(columnNames_p)[j] = name.after(i);
    }
}


//# Execute the sort (if possible).
//# The result will be the table now.
void TableParseSelect::doSort (PtrBlock<TableParseSort*>* sortList)
{
    uInt i;
    const Table& table = getTable();
    uInt nrkey = sortList->nelements();
    //# First check if the sort keys are correct.
    for (i=0; i<nrkey; i++) {
	const TableParseSort& key = *(*sortList)[i];
	//# Check if the correct table is used in the sort key expression.
	if (! key.node().checkTable (table)) {
	    throw (TableInvExpr ("Incorrect table used in a sort key"));
	}
	//# This throws an exception for unknown data types (datetime, regex).
	key.node().getColumnDataType();
    }
    Block<void*> arrays(nrkey);
    Sort sort;
    Bool deleteIt;
    for (i=0; i<nrkey; i++) {
	const TableParseSort& key = *(*sortList)[i];
	switch (key.node().getColumnDataType()) {
	case TpBool:
	    {
		Array<Bool>* array = new Array<Bool>
                                            (key.node().getColumnBool());
		arrays[i] = array;
		const Bool* data = array->getStorage (deleteIt);
		sort.sortKey (data, TpBool, 0, key.order());
		array->freeStorage (data, deleteIt);
	    }
	    break;
	case TpUChar:
	    {
		Array<uChar>* array = new Array<uChar>
                                            (key.node().getColumnuChar());
		arrays[i] = array;
		const uChar* data = array->getStorage (deleteIt);
		sort.sortKey (data, TpUChar, 0, key.order());
		array->freeStorage (data, deleteIt);
	    }
	    break;
	case TpShort:
	    {
		Array<Short>* array = new Array<Short>
                                            (key.node().getColumnShort());
		arrays[i] = array;
		const Short* data = array->getStorage (deleteIt);
		sort.sortKey (data, TpShort, 0, key.order());
		array->freeStorage (data, deleteIt);
	    }
	    break;
	case TpUShort:
	    {
		Array<uShort>* array = new Array<uShort>
                                            (key.node().getColumnuShort());
		arrays[i] = array;
		const uShort* data = array->getStorage (deleteIt);
		sort.sortKey (data, TpUShort, 0, key.order());
		array->freeStorage (data, deleteIt);
	    }
	    break;
	case TpInt:
	    {
		Array<Int>* array = new Array<Int>
                                            (key.node().getColumnInt());
		arrays[i] = array;
		const Int* data = array->getStorage (deleteIt);
		sort.sortKey (data, TpInt, 0, key.order());
		array->freeStorage (data, deleteIt);
	    }
	    break;
	case TpUInt:
	    {
		Array<uInt>* array = new Array<uInt>
                                            (key.node().getColumnuInt());
		arrays[i] = array;
		const uInt* data = array->getStorage (deleteIt);
		sort.sortKey (data, TpUInt, 0, key.order());
		array->freeStorage (data, deleteIt);
	    }
	    break;
	case TpFloat:
	    {
		Array<Float>* array = new Array<Float>
                                            (key.node().getColumnFloat());
		arrays[i] = array;
		const Float* data = array->getStorage (deleteIt);
		sort.sortKey (data, TpFloat, 0, key.order());
		array->freeStorage (data, deleteIt);
	    }
	    break;
	case TpDouble:
	    {
		Array<Double>* array = new Array<Double>
                                            (key.node().getColumnDouble());
		arrays[i] = array;
		const Double* data = array->getStorage (deleteIt);
		sort.sortKey (data, TpDouble, 0, key.order());
		array->freeStorage (data, deleteIt);
	    }
	    break;
	case TpComplex:
	    {
		Array<Complex>* array = new Array<Complex>
                                            (key.node().getColumnComplex());
		arrays[i] = array;
		const Complex* data = array->getStorage (deleteIt);
		sort.sortKey (data, TpComplex, 0, key.order());
		array->freeStorage (data, deleteIt);
	    }
	    break;
	case TpDComplex:
	    {
		Array<DComplex>* array = new Array<DComplex>
                                            (key.node().getColumnDComplex());
		arrays[i] = array;
		const DComplex* data = array->getStorage (deleteIt);
		sort.sortKey (data, TpDComplex, 0, key.order());
		array->freeStorage (data, deleteIt);
	    }
	    break;
	case TpString:
	    {
		Array<String>* array = new Array<String>
                                            (key.node().getColumnString());
		arrays[i] = array;
		const String* data = array->getStorage (deleteIt);
		sort.sortKey (data, TpString, 0, key.order());
		array->freeStorage (data, deleteIt);
	    }
	    break;
	default:
	    AlwaysAssert (False, AipsError);
	}
    }
    uInt nrrow = table.nrow();
    Vector<uInt> rownrs (nrrow);
    uInt* rows = rownrs.getStorage (deleteIt);
    sort.sort (nrrow, rows);
    rownrs.putStorage (rows, deleteIt);
    for (i=0; i<nrkey; i++) {
	const TableParseSort& key = *(*sortList)[i];
	switch (key.node().getColumnDataType()) {
	case TpBool:
	    delete (Array<Bool>*)arrays[i];
	    break;
	case TpUChar:
	    delete (Array<uChar>*)arrays[i];
	    break;
	case TpShort:
	    delete (Array<Short>*)arrays[i];
	    break;
	case TpUShort:
	    delete (Array<uShort>*)arrays[i];
	    break;
	case TpInt:
	    delete (Array<Int>*)arrays[i];
	    break;
	case TpUInt:
	    delete (Array<uInt>*)arrays[i];
	    break;
	case TpFloat:
	    delete (Array<Float>*)arrays[i];
	    break;
	case TpDouble:
	    delete (Array<Double>*)arrays[i];
	    break;
	case TpComplex:
	    delete (Array<Complex>*)arrays[i];
	    break;
	case TpDComplex:
	    delete (Array<DComplex>*)arrays[i];
	    break;
	case TpString:
	    delete (Array<String>*)arrays[i];
	    break;
	default:
	    AlwaysAssert (False, AipsError);
	}
	delete (*sortList)[i];
    }
    delete sortList;
    Table resultTable (table(rownrs));
    nothingDone_p = False;
    //# Store the resulting table in the list, so it will be used
    //# by the possible sorting step.
    TableParse* primary = &(parseIter_p->getRight());
    *primary = TableParse (resultTable, primary->shorthand());
}


//# Write the table.
//# This should only be done if selection and/or sorting has been done,
//# otherwise the original table may get renamed.
void TableParseSelect::handleGiving (const String& name)
{
    resultName_p = name;
}


//# Execute the selection on the 1st table and save the resulting table.
//# This will also delete all TableExprNode objects.
void TableParseSelect::doSelect (const TableExprNode* tsnp)
{
    //# Use the first table mentioned as the source table.
    parseIter_p->toStart();
    //# Return entire table if no selection given, otherwise select.
    Table resultTable;
    if (tsnp == 0) {
	resultTable = parseIter_p->getRight().table();
    }else{
//#//	cout << "Showing TableExprRange values ..." << endl;
//#//	Block<TableExprRange> rang;
//#//	tsnp->ranges(rang);
//#//	for (Int i=0; i<rang.nelements(); i++) {
//#//	    cout << rang[i].getColumn().columnDesc().name() << rang[i].start()
//#//		 << rang[i].end() << endl;
//#//	}
	resultTable = parseIter_p->getRight().table() (*tsnp);
	nothingDone_p = False;
    }
    //# Store the resulting table in the list, so it will be used
    //# by the possible sorting step.
    TableParse* primary = &(parseIter_p->getRight());
    *primary = TableParse (resultTable, primary->shorthand());
}



//# Simplified form of general tableCommand function.
Table tableCommand (const String& str)
{
    Vector<String> cols;
    return tableCommand (str, cols);
}

//# Do the actual parsing of a command and execute it.
Table tableCommand (const String& str, Vector<String>& cols)
{
    TableParseSelect::currentSelect() = 0;
    String message;
    Bool error = False;
    try {
	if (tableGramParseCommand(str) != 0) { // parse and execute the command
	    throw (TableParseError(str));      // throw exception if error
	}
    }catch (AipsError x) {
	message = x.getMesg();
	error = True;
    } end_try;

    //# If an exception was thrown; throw it again with the message.
    //# Delete the table object if so.
    if (error) {
	delete TableParseSelect::currentSelect();
	throw (AipsError(message));
    }

    //# Copy the possibly selected column names.
    cols.resize (TableParseSelect::currentSelect()->
		                            getColumnNames().nelements());
    cols = TableParseSelect::currentSelect()->getColumnNames();
    //# Do projection if columns were selected.
    //# Throw exception if no operations are done on the table.
    Table tab;
    if (cols.nelements() == 0) {
	if (TableParseSelect::currentSelect()->getNothingDone()) {
	    delete TableParseSelect::currentSelect();
	    throw (TableError
	    ("tableCommand error: no projection, selection or sorting done"));
	}
	tab = TableParseSelect::currentSelect()->getTable();     // copy table
    }else{
	Block<String> block(cols.nelements());
	uInt nr = 0;
	for (uInt i=0; i<block.nelements(); i++) {
	    if (TableParseSelect::currentSelect()->
		               getTable().tableDesc().isColumn (cols(i))) {
		block[nr++] = cols(i);
	    }
	}
	block.resize (nr, True);
	tab = TableParseSelect::currentSelect()->getTable().project (block); 
    }
    //# Name the output table when needed.
    if (TableParseSelect::currentSelect()->getTableName() != "") {
	tab.rename (TableParseSelect::currentSelect()->getTableName(),
		    Table::New);
    }
    delete TableParseSelect::currentSelect();
    return tab;
}
