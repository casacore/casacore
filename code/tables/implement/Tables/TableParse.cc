//# TableParse.cc: Classes to hold results from table grammar parser
//# Copyright (C) 1994,1995,1997,1998
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
#include <aips/Tables/ExprNode.h>
#include <aips/Tables/ExprDerNode.h>
#include <aips/Tables/ExprDerNodeArray.h>
#include <aips/Tables/ExprNodeSet.h>
#include <aips/Tables/ExprRange.h>
#include <aips/Tables/TableColumn.h>
#include <aips/Tables/ScalarColumn.h>
#include <aips/Tables/TableRecord.h>
#include <aips/Tables/TableDesc.h>
#include <aips/Tables/ColumnDesc.h>
#include <aips/Tables/TableError.h>
#include <aips/Quanta/MUString.h>
#include <aips/Quanta/MVTime.h>
#include <aips/Quanta/MVAngle.h>
#include <aips/Arrays/Vector.h>
#include <aips/Arrays/ArrayIO.h>
#include <aips/Utilities/Sort.h>
#include <aips/Utilities/Assert.h>
#include <aips/IO/AipsIO.h>
#include <aips/OS/Timer.h>
#include <iostream.h>

typedef Quantum<Double> TableParse_gpp_bug1;

static const PtrBlock<const Table*>* theTempTables;


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
    if (this != &that) {
	shorthand_p = that.shorthand_p;
	table_p     = that.table_p;
	table_p.makePermanent();
    }
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



//# Initialize static members.
PtrBlock<TableParseSelect*> TableParseSelect::blockSelect_p;
uInt TableParseSelect::currentSelect_p = 0;


TableParseSelect::TableParseSelect()
: resultSet_p (0),
  node_p      (0),
  sort_p      (0),
  noDupl_p    (False)
{
    parseList_p = new List<TableParse>;
    parseIter_p = new ListIter<TableParse> (parseList_p);
    parseList_p->makePermanent();
    parseIter_p->makePermanent();
    table_p.makePermanent();
}

TableParseSelect::~TableParseSelect()
{
    delete parseIter_p;
    delete parseList_p;
    delete node_p;
    if (sort_p != 0) {
	uInt nrkey = sort_p->nelements();
	for (uInt i=0; i<nrkey; i++) {
	    delete (*sort_p)[i];
	}
	delete sort_p;
    }
    delete resultSet_p;
}

void TableParseSelect::newSelect()
{
    // Create a new one and push it on the "stack".
    uInt n = currentSelect_p;
    currentSelect_p++;
    blockSelect_p.resize (currentSelect_p);
    blockSelect_p[n] = new TableParseSelect;
}

TableParseSelect* TableParseSelect::popSelect()
{
    if (currentSelect_p == 0) {
	throw (TableError ("TableParse::popSelect: internal error"));
    }
    currentSelect_p--;
    TableParseSelect* p = blockSelect_p[currentSelect_p];
    blockSelect_p[currentSelect_p] = 0;
    return p;
}

void TableParseSelect::clearSelect()
{
    while (currentSelect_p > 0) {
	currentSelect_p--;
	delete blockSelect_p[currentSelect_p];
	blockSelect_p[currentSelect_p] = 0;
    }
}


//# Construct a TableParse object and add it to the linked list.
void TableParseSelect::addTable (const TableParseVal* name,
				 const String& shorthand)
{
    Table table;
    //# When the table name is numeric, we have a temporary table number.
    //# Find it in the block of temporary tables.
    if (name->type == 'i') {
	Int tabnr = name->ival - 1;
	if (tabnr < 0  ||  tabnr >= Int(theTempTables->nelements())
	||  (*theTempTables)[tabnr] == 0) {
	    throw (TableInvExpr ("Invalid temporary table number given"));
	}
	table = *((*theTempTables)[tabnr]);
    }else{
	//# The table name is a string.
	//# When the name contains ::, it is a keyword in a table at an outer
	//# SELECT statement.
	String shand, columnName, keyName;
	if (splitName (shand, columnName, keyName, name->str)) { 
	    table = tableKey (shand, columnName, keyName);
	}else{
	    table = Table(name->str);
	}
    }
    parseIter_p->toEnd();
    parseIter_p->addRight (TableParse(table, shorthand));
}

Table TableParseSelect::tableKey (const String& shorthand,
				  const String& columnName,
				  const String& keyName)
{
    //# Find the given shorthand on all levels.
    for (Int i=currentSelect_p - 1; i>=0; i--) {
	Table tab = blockSelect_p[i]->findTable (shorthand);
	if (! tab.isNull()) {
	    Table result = findTableKey (tab, columnName, keyName);
	    if (! result.isNull()) {
		return result;
	    }
	}
    }
    throw (TableInvExpr ("Keyword " + columnName + "::" + keyName +
			 " not found in tables in outer SELECT's"));
    return Table();
}

Table TableParseSelect::findTableKey (const Table& table,
				      const String& columnName,
				      const String& keyName)
{
    //# Pick the table or column keyword set.
    if (columnName.empty()  ||  table.tableDesc().isColumn (columnName)) {
	const TableRecord& keyset = 
		           columnName.empty()  ?
		                 table.keywordSet() :
		                 ROTableColumn (table, columnName).keywordSet();
	//# If the keyword exists and is a table, take it.
	Int fieldnr = keyset.fieldNumber (keyName);
	if (fieldnr >= 0  &&  keyset.dataType(fieldnr) == TpTable) {
	    return keyset.asTable (fieldnr);
	}
    }
    //# Not found.
    return Table();
}

Bool TableParseSelect::splitName (String& shorthand, String& columnName,
				  String& keyName, const String& name) const
{
    //# Make a copy, because some String functions are non-const.
    //# Usually the name consists of a columnName only, so use that.
    //# Find the . which is the separator between shorthand and field name.
    //# Find the :: which indicates a keyword.
    columnName = name;
    int i = columnName.index('.');
    int j = columnName.index("::");
    if (j >= 0  &&  j < i) {
	throw (TableInvExpr (name + " is invalid name: . not before ::"));
    }
    if (i > 0) {
	shorthand = columnName.before(i);
	j -= i+1;
	columnName = columnName.after(i);
    }else{
	shorthand = "";
    }
    if (j < 0) {
	keyName = "";
	return False;
    }
    keyName = columnName.after(j+1);
    columnName = columnName.before(j);
    if (keyName.empty()) {
	throw (TableInvExpr ("No keyword given in name " + name));
    }
    return True;
}

Table TableParseSelect::findTable (const String& shorthand) const
{
    //# If no shorthand given, take first table (if there).
    parseIter_p->toStart();
    if (shorthand == "") {
	if (parseIter_p->len() > 0) {
	    return parseIter_p->getRight().table();
	}
    }else{
	while (! parseIter_p->atEnd()) {
	    if (parseIter_p->getRight().test (shorthand)) {
		return parseIter_p->getRight().table();
	    }
	    (*parseIter_p)++;
	}
    }
    return Table();
}

//# Lookup a field name in the table for which the shorthand is given.
//# If no shorthand is given, use the first table.
//# The shorthand and name are separated by a period.
TableExprNode TableParseSelect::handleKeyCol (const String& name)
{
    //# Split the name into shorthand, column and keyword.
    String shand, columnName, keyName;
    Bool hasKey = splitName (shand, columnName, keyName, name);
    //# Use first table if there is no shorthand given.
    //# Otherwise find the table.
    Table tab = findTable (shand);
    if (tab.isNull()) {
	throw (TableInvExpr
	       ("Shorthand " + shand + " has not been defined"));
	return 0;
    }
    //# If :: is not given, we have a column or keyword.
    if (!hasKey) {
	return tab.keyCol (columnName);
    }
    //# If no column name, we have a table keyword.
    //# Otherwise we have a column keyword.
    if (columnName.empty()) {
	return tab.key (keyName);
    }
    ROTableColumn col (tab, columnName);
    return TableExprNode::newKeyConst (col.keywordSet(), keyName);
}

TableExprNode TableParseSelect::handleSlice (const TableExprNode& array,
					     const TableExprNodeSet& indices)
{
    // TaQL indexing is 1-based.
    return TableExprNode::newArrayPartNode (array, indices, 1);
}
 
//# Parse the name of a function.
TableExprNode TableParseSelect::handleFunc (const String& name,
					    const TableExprNodeSet& arguments)
{
    //# Determine if there is a single argument
    //# (needed for overloading functions min and max).
    Bool isOneArg = ToBool (arguments.nelements() == 1);
    //# Determine the function type.
    TableExprFuncNode::FunctionType ftype = TableExprFuncNode::piFUNC;
    String funcName (name);
    funcName.downcase();
    if (funcName == "pi") {
	ftype = TableExprFuncNode::piFUNC;
    } else if (funcName == "e") {
	ftype = TableExprFuncNode::eFUNC;
    } else if (funcName == "near") {
	ftype = TableExprFuncNode::near2FUNC;
	if (arguments.nelements() == 3) {
	    ftype = TableExprFuncNode::near3FUNC;
	}
    } else if (funcName == "nearabs") {
	ftype = TableExprFuncNode::nearabs2FUNC;
	if (arguments.nelements() == 3) {
	    ftype = TableExprFuncNode::nearabs3FUNC;
	}
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
	if (isOneArg) {
	    ftype = TableExprFuncNode::arrminFUNC;
	}
    } else if (funcName == "max") {
	ftype = TableExprFuncNode::maxFUNC;
	if (isOneArg) {
	    ftype = TableExprFuncNode::arrmaxFUNC;
	}
    } else if (funcName == "sum") {
	ftype = TableExprFuncNode::arrsumFUNC;
    } else if (funcName == "product") {
	ftype = TableExprFuncNode::arrproductFUNC;
    } else if (funcName == "mean") {
	ftype = TableExprFuncNode::arrmeanFUNC;
    } else if (funcName == "variance") {
	ftype = TableExprFuncNode::arrvarianceFUNC;
    } else if (funcName == "stddev") {
	ftype = TableExprFuncNode::arrstddevFUNC;
    } else if (funcName == "avdev") {
	ftype = TableExprFuncNode::arravdevFUNC;
    } else if (funcName == "median") {
	ftype = TableExprFuncNode::arrmedianFUNC;
    } else if (funcName == "any") {
	ftype = TableExprFuncNode::anyFUNC;
    } else if (funcName == "all") {
	ftype = TableExprFuncNode::allFUNC;
    } else if (funcName == "ntrue") {
	ftype = TableExprFuncNode::ntrueFUNC;
    } else if (funcName == "nfalse") {
	ftype = TableExprFuncNode::nfalseFUNC;
    } else if (funcName == "isdefined") {
	ftype = TableExprFuncNode::isdefFUNC;
    } else if (funcName == "nelements"  ||  funcName == "count") {
	ftype = TableExprFuncNode::nelemFUNC;
    } else if (funcName == "ndim") {
	ftype = TableExprFuncNode::ndimFUNC;
    } else if (funcName == "shape") {
	ftype = TableExprFuncNode::shapeFUNC;
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
    } else if (funcName == "upcase"    ||  funcName == "upper"  ||
	       funcName == "toupper"   ||  funcName == "to_upper") {
	ftype = TableExprFuncNode::upcaseFUNC;
    } else if (funcName == "downcase"  ||  funcName == "lower"  ||
	       funcName == "tolower"   ||  funcName == "to_lower") {
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
    return TableExprNode::newFunctionNode (ftype, arguments,
					   parseIter_p->getRight().table());
}


//# Convert a constant to a TableExprNode object.
//# The leading and trailing " is removed from a string.
TableExprNode TableParseSelect::handleLiteral (TableParseVal* val)
{
    TableExprNodeRep* tsnp = 0;
    switch (val->type) {
    case 'i':
	tsnp = new TableExprNodeConstDouble (Double (val->ival));
	break;
    case 'f':
	tsnp = new TableExprNodeConstDouble (val->dval[0]);
	break;
    case 'c':
	tsnp = new TableExprNodeConstDComplex
	                   (DComplex (val->dval[0], val->dval[1]));
	break;
    case 's':
    {
	tsnp = new TableExprNodeConstString (val->str);
	break;
    }
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
void TableParseSelect::handleSelectColumn (const String& name)
{
    String str = name;
    Int i = str.index('.');
    Int j = columnNames_p.nelements();
    columnNames_p.resize (j+1);                  // extend block
    if (i < 0) {
	(columnNames_p)[j] = str;
    }else{
	(columnNames_p)[j] = str.after(i);
    }
}

//# Eexecute a subquery and create the correct node object for it.
TableExprNode TableParseSelect::doSubQuery()
{
#if defined(AIPS_TRACE)
    Timer timer;
#endif
    // Execute the nested command.
    execute();
#if defined(AIPS_TRACE)
    timer.show ("Subquery");
#endif
    // Handle a set when that is given.
    if (resultSet_p != 0) {
	return makeSubSet();
    }
    // Otherwise a column should be given.
    // Check if there is only one column which has to contain a scalar.
    const TableDesc& tableDesc = table_p.tableDesc();
    if (tableDesc.ncolumn() != 1) {
	throw (TableInvExpr ("Nested query should select 1 column"));
    }
    const ColumnDesc& colDesc = tableDesc.columnDesc(0);
    if (! colDesc.isScalar()) {
	throw (TableInvExpr ("Nested query should select a scalar column"));
    }
    const String& name = colDesc.name();
    switch (colDesc.dataType()) {
    case TpBool:
	return new TableExprNodeArrayConstBool
                        (ROScalarColumn<Bool> (table_p, name).getColumn());
    case TpUChar:
	return new TableExprNodeArrayConstDouble
                        (ROScalarColumn<uChar> (table_p, name).getColumn());
    case TpShort:
	return new TableExprNodeArrayConstDouble
                        (ROScalarColumn<Short> (table_p, name).getColumn());
    case TpUShort:
	return new TableExprNodeArrayConstDouble
                        (ROScalarColumn<uShort> (table_p, name).getColumn());
    case TpInt:
	return new TableExprNodeArrayConstDouble
                        (ROScalarColumn<Int> (table_p, name).getColumn());
    case TpUInt:
	return new TableExprNodeArrayConstDouble
                        (ROScalarColumn<uInt> (table_p, name).getColumn());
    case TpFloat:
	return new TableExprNodeArrayConstDouble
                        (ROScalarColumn<Float> (table_p, name).getColumn());
    case TpDouble:
	return new TableExprNodeArrayConstDouble
                        (ROScalarColumn<Double> (table_p, name).getColumn());
    case TpComplex:
	return new TableExprNodeArrayConstDComplex
                        (ROScalarColumn<Complex> (table_p, name).getColumn());
    case TpDComplex:
	return new TableExprNodeArrayConstDComplex
                        (ROScalarColumn<DComplex> (table_p, name).getColumn());
    case TpString:
	return new TableExprNodeArrayConstString
                        (ROScalarColumn<String> (table_p, name).getColumn());
    default:
	throw (TableInvExpr ("Nested query column has unknown data type"));
    }
    return 0;
}


TableExprNode TableParseSelect::makeSubSet() const
{
    // Perform some checks on the given set.
    if (resultSet_p->nelements() != 1) {
	throw (TableInvExpr ("Set in GIVING clause can have 1 element only"));
    }
    if (resultSet_p->hasArrays()) {
	throw (TableInvExpr ("Set in GIVING clause should contain scalar"
			     " elements"));
    }
    // Link to set to make sure that TableExprNode hereafter does not delete
    // the object.
    resultSet_p->link();
    if (! TableExprNode(resultSet_p).checkReplaceTable (table_p)) {
	throw (TableInvExpr ("Incorrect table used in GIVING set expression"));
    }
    uInt nrow = table_p.nrow();
    TableExprNodeSet set(nrow, (*resultSet_p)[0]);
    return set.setOrArray();
}

//# Execute the sort.
Table TableParseSelect::doSort (const Table& table)
{
    uInt i;
    uInt nrkey = sort_p->nelements();
    //# First check if the sort keys are correct.
    for (i=0; i<nrkey; i++) {
	const TableParseSort& key = *(*sort_p)[i];
	//# Check if the correct table is used in the sort key expression.
	if (! key.node().checkReplaceTable (table)) {
	    throw (TableInvExpr ("Incorrect table used in a sort key"));
	}
	//# This throws an exception for unknown data types (datetime, regex).
	key.node().getColumnDataType();
    }
    Block<void*> arrays(nrkey);
    Sort sort;
    Bool deleteIt;
    for (i=0; i<nrkey; i++) {
	const TableParseSort& key = *(*sort_p)[i];
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
    int sortOpt = Sort::HeapSort;                  
    if (noDupl_p) {
	sortOpt += Sort::NoDuplicates;
    }
    sort.sort (rownrs, nrrow, sortOpt);
    for (i=0; i<nrkey; i++) {
	const TableParseSort& key = *(*sort_p)[i];
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
    }
    return table(rownrs);
}


//# Keep the name of the resulting table.
void TableParseSelect::handleGiving (const String& name)
{
    resultName_p = name;
}
//# Keep the resulting set expression.
void TableParseSelect::handleGiving (const TableExprNodeSet& set)
{
    resultSet_p = new TableExprNodeSet (set);
}


//# Execute all parts of the SELECT command.
void TableParseSelect::execute()
{
    //# Give an error if no command part has been given.
    if (node_p == 0  &&  sort_p == 0  &&  columnNames_p.nelements() == 0) {
	throw (TableError
	    ("TableParse error: no projection, selection or sorting given"));
    }
    //# The first table in the list is the source table.
    parseIter_p->toStart();
    Table table = parseIter_p->getRight().table();
    //# Check if all projected columns exist.
    for (uInt i=0; i<columnNames_p.nelements(); i++) {
	if (! table.tableDesc().isColumn (columnNames_p[i])) {
	    throw (TableError ("TableParse: projected column " +
			       columnNames_p[i] +
			       " does not exist in table " +
			       table.tableName()));
	}
    }
    //# First do the select.
    if (node_p != 0) {
//#//	cout << "Showing TableExprRange values ..." << endl;
//#//	Block<TableExprRange> rang;
//#//	node_p->ranges(rang);
//#//	for (Int i=0; i<rang.nelements(); i++) {
//#//	    cout << rang[i].getColumn().columnDesc().name() << rang[i].start()
//#//		 << rang[i].end() << endl;
//#//	}
	table = table(*node_p);
    }
    //# Then do the sort.
    if (sort_p != 0) {
	table = doSort (table);
    }
    //# Then do the projection.
    if (columnNames_p.nelements() > 0) {
	table = table.project (columnNames_p);
    }
    //# Finally give it the given name.
    if (! resultName_p.empty()) {
	table.rename (resultName_p, Table::New);
    }
    //# Keep the table for later.
    table_p = table;
    table_p.makePermanent();
}    

void TableParseSelect::show (ostream& os) const
{
    if (node_p != 0) {
	node_p->show (os);
    }
}


//# Simplified forms of general tableCommand function.
Table tableCommand (const String& str)
{
    Vector<String> cols;
    return tableCommand (str, cols);
}
Table tableCommand (const String& str, const Table& tempTable)
{
    PtrBlock<const Table*> tmp(1);
    tmp[0] = &tempTable;
    return tableCommand (str, tmp);
}
Table tableCommand (const String& str, const PtrBlock<const Table*>& tempTables)
{
    Vector<String> cols;
    return tableCommand (str, tempTables, cols);
}
Table tableCommand (const String& str, Vector<String>& cols)
{
    PtrBlock<const Table*> tmp;
    return tableCommand (str, tmp, cols);
}

//# Do the actual parsing of a command and execute it.
Table tableCommand (const String& str,
		    const PtrBlock<const Table*>& tempTables,
		    Vector<String>& cols)
{
#if defined(AIPS_TRACE)
    Timer timer;
#endif
    theTempTables = &tempTables;
    TableParseSelect::clearSelect();
    String message;
    String command = str + '\n';
    Bool error = False;
    try {
	// Parse and execute the command.
	if (tableGramParseCommand(command) != 0) {
	    throw (TableParseError(str));      // throw exception if error
	}
    }catch (AipsError x) {
	message = x.getMesg();
	error = True;
    } end_try;
#if defined(AIPS_TRACE)
    timer.show ("Parsing ");
#endif
    //# If an exception was thrown; throw it again with the message.
    //# Delete the table object if so.
    if (error) {
	TableParseSelect::clearSelect();
	throw (AipsError(message + '\n' + "Scanned so far: " +
	                 command.before(tableGramPosition())));
    }
    //# Execute the command and get the resulting table.
    TableParseSelect* p = TableParseSelect::popSelect();
#if defined(AIPS_TRACE)
    p->show (cout);
    timer.mark();
#endif
    p->execute();
#if defined(AIPS_TRACE)
    timer.show ("Execute ");
#endif
    Table table = p->getTable();
    //# Copy the possibly selected column names.
    cols.resize (p->getColumnNames().nelements());
    Vector<String> tmp(p->getColumnNames());
    cols = tmp;
    delete p;
    return table;
}
