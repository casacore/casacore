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

#include <tables/Tables/TaQLNode.h>
#include <tables/Tables/TaQLNodeHandler.h>
#include <tables/Tables/TaQLStyle.h>
#include <tables/Tables/TableParse.h>
#include <tables/Tables/TableGram.h>
#include <tables/Tables/TaQLResult.h>
#include <tables/Tables/ExprNode.h>
#include <tables/Tables/ExprDerNode.h>
#include <tables/Tables/ExprDerNodeArray.h>
#include <tables/Tables/ExprNodeSet.h>
#include <tables/Tables/ExprUnitNode.h>
#include <tables/Tables/ExprRange.h>
#include <tables/Tables/TableColumn.h>
#include <tables/Tables/ScalarColumn.h>
#include <tables/Tables/ArrayColumn.h>
#include <tables/Tables/TableCopy.h>
#include <tables/Tables/TableIter.h>
#include <tables/Tables/TableRow.h>
#include <tables/Tables/TableRecord.h>
#include <tables/Tables/TableDesc.h>
#include <tables/Tables/ColumnDesc.h>
#include <tables/Tables/ScaColDesc.h>
#include <tables/Tables/ArrColDesc.h>
#include <tables/Tables/SetupNewTab.h>
#include <tables/Tables/StandardStMan.h>
#include <tables/Tables/TableError.h>
#include <casa/Arrays/Vector.h>
#include <casa/Arrays/ArrayMath.h>
#include <casa/Arrays/ArrayUtil.h>
#include <casa/Arrays/ArrayIO.h>
#include <casa/Utilities/Sort.h>
#include <casa/Utilities/GenSort.h>
#include <casa/Utilities/LinearSearch.h>
#include <casa/Utilities/Assert.h>
#include <casa/IO/AipsIO.h>
#include <casa/OS/Timer.h>
#include <casa/ostream.h>


namespace casa { //# NAMESPACE CASA - BEGIN


//# Default constructor.
TableParse::TableParse()
{}

//# Constructor with given table name and possible shorthand.
TableParse::TableParse (const Table& table, const String& shorthand)
  : shorthand_p (shorthand),
    table_p     (table)
{}

TableParse::TableParse (const TableParse& that)
  : shorthand_p (that.shorthand_p),
    table_p     (that.table_p)
{}

TableParse& TableParse::operator= (const TableParse& that)
{
  if (this != &that) {
    shorthand_p = that.shorthand_p;
    table_p     = that.table_p;
  }
  return *this;
}



TableParseUpdate::TableParseUpdate (const String& columnName,
				    const TableExprNode& node)
  : columnName_p (columnName),
    indexPtr_p   (0),
    node_p       (node)
{}
TableParseUpdate::TableParseUpdate (const String& columnName,
				    const TableExprNodeSet& indices,
				    const TableExprNode& node,
				    const TaQLStyle& style)
  : columnName_p (columnName),
    indexPtr_p   (0),
    node_p       (node)
{
  indexPtr_p  = new TableExprNodeIndex (indices, style);
  indexNode_p = TableExprNode(indexPtr_p);
}
TableParseUpdate::~TableParseUpdate()
{
  // indexPtr_p does not need to be deleted because it is part of indexNode_p.
}



TableParseSort::TableParseSort()
  : order_p (Sort::Ascending),
    given_p (False)
{}
TableParseSort::TableParseSort (const TableExprNode& node)
  : node_p  (node),
    order_p (Sort::Ascending),
    given_p (False)
{}
TableParseSort::TableParseSort (const TableExprNode& node, Sort::Order order)
  : node_p  (node),
    order_p (order),
    given_p (True)
{}
TableParseSort::~TableParseSort()
{}



TableParseSelect::TableParseSelect (CommandType commandType)
  : commandType_p   (commandType),
    nrSelExprUsed_p (0),
    distinct_p      (False),
    resultType_p    (0),
    resultSet_p     (0),
    limit_p         (0),
    offset_p        (0),
    insSel_p        (0),
    noDupl_p        (False),
    order_p         (Sort::Ascending)
{}

TableParseSelect::~TableParseSelect()
{
  // Note that insSel_p is simply a pointer to another object,
  // so no delete should be done.
  delete resultSet_p;
  for (uInt i=0; i<update_p.size(); ++i) {
    delete update_p[i];
  }
}


//# Construct a TableParse object and add it to the container.
void TableParseSelect::addTable (Int tabnr, const String& name,
				 const Table& ftab,
				 const String& shorthand,
				 const vector<const Table*> tempTables,
				 const vector<TableParseSelect*>& stack)
{
  Table table;
  //# If the table name is numeric, we have a temporary table number
  //# which will be made 0-based.
  //# Find it in the block of temporary tables.
  if (tabnr >= 0) {
    tabnr -= 1;
    if (tabnr < 0  ||  tabnr >= Int(tempTables.size())
    ||  tempTables[tabnr] == 0) {
      throw (TableInvExpr ("Invalid temporary table number given"));
    }
    table = *(tempTables[tabnr]);
  } else if (! ftab.isNull()) {
    //# The table is a temporary table (from a select clause).
    table = ftab;
  } else {
    //# The table name is a string.
    //# When the name contains ::, it is a keyword in a table at an outer
    //# SELECT statement.
    String shand, columnName;
    Vector<String> fieldNames;
    if (splitName (shand, columnName, fieldNames, name, False)) { 
      table = tableKey (shand, columnName, fieldNames, stack);
    } else {
      // If no or equal shorthand is given, try to see if the
      // given name is already used as a shorthand.
      // If so, take the table of that shorthand.
      Bool foundSH = False;
      if (shorthand.empty()  ||  name == shorthand) {
	for (Int i=stack.size()-1; i>=0; i--) {
	  Table tab = stack[i]->findTable (name);
	  if (! tab.isNull()) {
	    table = tab;
	    foundSH = True;
	    break;
	  }
	}
      }
      if (!foundSH) {
	table = Table(name);
      }
    }
  }
  fromTables_p.push_back (TableParse(table, shorthand));
}

void TableParseSelect::replaceTable (const Table& table)
{
  AlwaysAssert (!fromTables_p.empty(), AipsError);
  // Replace table, but use same shorthand.
  fromTables_p[0] = TableParse(table, fromTables_p[0].shorthand());
}

Table TableParseSelect::tableKey (const String& shorthand,
				  const String& columnName,
				  const Vector<String>& fieldNames,
				  const vector<TableParseSelect*>& stack)
{
  //# Find the given shorthand on all levels.
  for (Int i=stack.size()-1; i>=0; i--) {
    Table tab = stack[i]->findTable (shorthand);
    if (! tab.isNull()) {
      Table result = findTableKey (tab, columnName, fieldNames);
      if (! result.isNull()) {
	return result;
      }
    }
  }
  throw (TableInvExpr ("Keyword " + columnName + "::" + fieldNames(0) +
		       " not found in tables in outer SELECTs"));
  return Table();
}

Table TableParseSelect::findTableKey (const Table& table,
				      const String& columnName,
				      const Vector<String>& fieldNames)
{
  //# Pick the table or column keyword set.
  if (columnName.empty()  ||  table.tableDesc().isColumn (columnName)) {
    const TableRecord* keyset =  columnName.empty()  ?
      &(table.keywordSet()) :
      &(ROTableColumn (table, columnName).keywordSet());
    // All fieldnames, except last one, should be records.
    uInt last = fieldNames.nelements() - 1;
    for (uInt i=0; i<last; i++) { 
      //# If the keyword does not exist or is not a record, return.
      Int fieldnr = keyset->fieldNumber (fieldNames(i));
      if (fieldnr < 0  ||  keyset->dataType(fieldnr) != TpRecord) {
	return Table();
      }
      keyset = &(keyset->subRecord(fieldnr));
    }
    //# If the keyword exists and is a table, take it.
    Int fieldnr = keyset->fieldNumber (fieldNames(last));
    if (fieldnr >= 0  &&  keyset->dataType(fieldnr) == TpTable) {
      return keyset->asTable (fieldnr);
    }
  }
  //# Not found.
  return Table();
}

// This function can split a name.
// The name can consist of an optional shorthand, a column or keyword name,
// followed by zero or more subfield names (separated by dots).
// In the future it should also be possible to have a subfield name
// followed by a keyword name, etc. to cater for something like:
//   shorthand::key.subtable::key.subsubtable::key.
// If that gets possible, TableGram.l should also be changed to accept
// such a string in the scanner.
// It is a question whether :: should be part of the scanner or grammar.
// For columns one can go a bit further by accepting something like:
//  col.subtable[select expression resulting in scalar]
// which is something for the far away future.
Bool TableParseSelect::splitName (String& shorthand, String& columnName,
				  Vector<String>& fieldNames,
				  const String& name,
				  Bool checkError) const
{
  //# Make a copy, because some String functions are non-const.
  //# Usually the name consists of a columnName only, so use that.
  //# A keyword is given if :: is part of the name.
  shorthand = "";
  columnName = name;
  String restName;
  Bool isKey = False;
  int j = columnName.index("::");
  Vector<String> fldNam;
  uInt stfld = 0;
  if (j >= 0) {
    // The name represents a keyword name.
    isKey = True;
    // There should be something after the ::
    // which can be multiple names separated by dots.
    // They represent the keyword name and possible subfields in case
    // the keyword is a record.
    restName = columnName.after(j+1);
    if (restName.empty()) {
      if (checkError) {
	throw (TableInvExpr ("No keyword given in name " + name));
      }
      return False;
    }
    fldNam = stringToVector (restName, '.');
    // The part before the :: can be empty, an optional shorthand,
    // and an optional column name (separated by a dot).
    if (j == 0) {
      columnName = "";
    } else {
      Vector<String> scNames = stringToVector(columnName.before(j), '.');
      switch (scNames.nelements()) {
      case 2:
	shorthand = scNames(0);
	columnName = scNames(1);
	break;
      case 1:
	columnName = scNames(0);
	break;
      default:
	if (checkError) {
	  throw TableInvExpr ("Name " + name + " is invalid: More"
			      " than 2 name parts given before ::");
	}
	return False;
      }
    }
  } else {
    // The name is a column name optionally preceeded by a shorthand
    // and optionally followed by subfields in case the column contains
    // records. The separator is a dot.
    // A name like a.b is ambiguous because:
    // - it can be shorthand.column
    // - it can be column.subfield
    // If a is a shorthand, that case it taken. Otherwise it's a column.
    // Users can make it unambiguous by preceeding it with a dot
    // (.a.b always means column.subfield).
    fldNam = stringToVector (columnName, '.');
    if (fldNam.nelements() == 1) {
      stfld = 0;                      // one part simply means column
    } else if (fldNam(0).empty()) {
      stfld = 1;                      // .column was used
    } else {
      Table tab = findTable(fldNam(0));
      if (! tab.isNull()) {
	shorthand = fldNam(0);      // a known shorthand is used
	stfld = 1;
      }
    }
    columnName = fldNam(stfld++);
    if (columnName.empty()) {
      if (checkError) {
	throw (TableInvExpr ("No column given in name " + name));
      }
      return False;
    }
  }
  fieldNames.resize (fldNam.nelements() - stfld);
  for (uInt i=stfld; i<fldNam.nelements(); i++) {
    if (fldNam(i).empty()) {
      if (checkError) {
	throw (TableInvExpr ("Name " + name +
			     " has empty field names"));
      }
      return False;
    }
    fieldNames(i-stfld) = fldNam(i);
  }
  return isKey;
}

Table TableParseSelect::findTable (const String& shorthand) const
{
  //# If no shorthand given, take first table (if there).
  if (shorthand.empty()) {
    if (fromTables_p.size() > 0) {
      return fromTables_p[0].table();
    }
  } else {
    for (uInt i=0; i<fromTables_p.size(); i++) {
      if (fromTables_p[i].test (shorthand)) {
	return fromTables_p[i].table();
      }
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
  String shand, columnName;
  Vector<String> fieldNames;
  Bool hasKey = splitName (shand, columnName, fieldNames, name, True);
  //# Use first table if there is no shorthand given.
  //# Otherwise find the table.
  Table tab = findTable (shand);
  if (tab.isNull()) {
    throw (TableInvExpr("Shorthand " + shand + " has not been defined"));
    return 0;
  }
  //# If :: is not given, we have a column or keyword.
  if (!hasKey) {
    return tab.keyCol (columnName, fieldNames);
  }
  //# If no column name, we have a table keyword.
  if (columnName.empty()) {
    return tab.key (fieldNames);
  }
  //# Otherwise we have a column keyword.
  ROTableColumn col (tab, columnName);
  return TableExprNode::newKeyConst (col.keywordSet(), fieldNames);
}

TableExprNode TableParseSelect::handleSlice (const TableExprNode& array,
					     const TableExprNodeSet& indices,
					     const TaQLStyle& style)
{
  return TableExprNode::newArrayPartNode (array, indices, style);
}
 
//# Parse the name of a function.
TableExprFuncNode::FunctionType TableParseSelect::findFunc
                               (const String& name,
				uInt narguments,
				const Vector<Int>& ignoreFuncs)
{
  //# Determine the function type.
  //# Use the function name in lower case.
  //# Error if name in ingoreNames.
  TableExprFuncNode::FunctionType ftype = TableExprFuncNode::piFUNC;
  String funcName (name);
  funcName.downcase();
  if (funcName == "pi") {
    ftype = TableExprFuncNode::piFUNC;
  } else if (funcName == "e") {
    ftype = TableExprFuncNode::eFUNC;
  } else if (funcName == "c") {
    ftype = TableExprFuncNode::cFUNC;
  } else if (funcName == "near") {
    ftype = TableExprFuncNode::near2FUNC;
    if (narguments == 3) {
      ftype = TableExprFuncNode::near3FUNC;
    }
  } else if (funcName == "nearabs") {
    ftype = TableExprFuncNode::nearabs2FUNC;
    if (narguments == 3) {
      ftype = TableExprFuncNode::nearabs3FUNC;
    }
  } else if (funcName == "cones") {
    ftype = TableExprConeNode::conesFUNC;
    if (narguments == 3) {
      ftype = TableExprConeNode::cones3FUNC;
    }
  } else if (funcName == "anycone") {
    ftype = TableExprConeNode::anyconeFUNC;
    if (narguments == 3) {
      ftype = TableExprConeNode::anycone3FUNC;
    }
  } else if (funcName == "findcone") {
    ftype = TableExprConeNode::findconeFUNC;
    if (narguments == 3) {
      ftype = TableExprConeNode::findcone3FUNC;
    }
  } else if (funcName == "cos") {
    ftype = TableExprFuncNode::cosFUNC;
  } else if (funcName == "cosh") {
    ftype = TableExprFuncNode::coshFUNC;
  } else if (funcName == "exp") {
    ftype = TableExprFuncNode::expFUNC;
  } else if (funcName == "log"  ||  funcName == "ln") {
    ftype = TableExprFuncNode::logFUNC;
  } else if (funcName == "log10") {
    ftype = TableExprFuncNode::log10FUNC;
  } else if (funcName == "sin") {
    ftype = TableExprFuncNode::sinFUNC;
  } else if (funcName == "sinh") {
    ftype = TableExprFuncNode::sinhFUNC;
  } else if (funcName == "square"  ||  funcName == "sqr") {
    ftype = TableExprFuncNode::squareFUNC;
  } else if (funcName == "cube") {
    ftype = TableExprFuncNode::cubeFUNC;
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
    if (narguments == 1) {
      ftype = TableExprFuncNode::arrminFUNC;
    }
  } else if (funcName == "mins") {
    ftype = TableExprFuncNode::arrminsFUNC;
  } else if (funcName == "runningmin") {
    ftype = TableExprFuncNode::runminFUNC;
  } else if (funcName == "boxedmin") {
    ftype = TableExprFuncNode::boxminFUNC;
  } else if (funcName == "max") {
    ftype = TableExprFuncNode::maxFUNC;
    if (narguments == 1) {
      ftype = TableExprFuncNode::arrmaxFUNC;
    }
  } else if (funcName == "maxs") {
    ftype = TableExprFuncNode::arrmaxsFUNC;
  } else if (funcName == "runningmax") {
    ftype = TableExprFuncNode::runmaxFUNC;
  } else if (funcName == "boxedmax") {
    ftype = TableExprFuncNode::boxmaxFUNC;
  } else if (funcName == "sum") {
    ftype = TableExprFuncNode::arrsumFUNC;
  } else if (funcName == "sums") {
    ftype = TableExprFuncNode::arrsumsFUNC;
  } else if (funcName == "product") {
    ftype = TableExprFuncNode::arrproductFUNC;
  } else if (funcName == "products") {
    ftype = TableExprFuncNode::arrproductsFUNC;
  } else if (funcName == "sumsqr"  ||  funcName == "sumsquare") {
    ftype = TableExprFuncNode::arrsumsqrFUNC;
  } else if (funcName == "sumsqrs"  ||  funcName == "sumsquares") {
    ftype = TableExprFuncNode::arrsumsqrsFUNC;
  } else if (funcName == "mean") {
    ftype = TableExprFuncNode::arrmeanFUNC;
  } else if (funcName == "means") {
    ftype = TableExprFuncNode::arrmeansFUNC;
  } else if (funcName == "runningmean") {
    ftype = TableExprFuncNode::runmeanFUNC;
  } else if (funcName == "boxedmean") {
    ftype = TableExprFuncNode::boxmeanFUNC;
  } else if (funcName == "variance") {
    ftype = TableExprFuncNode::arrvarianceFUNC;
  } else if (funcName == "variances") {
    ftype = TableExprFuncNode::arrvariancesFUNC;
  } else if (funcName == "boxedvariance") {
    ftype = TableExprFuncNode::boxvarianceFUNC;
  } else if (funcName == "runningvariance") {
    ftype = TableExprFuncNode::runvarianceFUNC;
  } else if (funcName == "stddev") {
    ftype = TableExprFuncNode::arrstddevFUNC;
  } else if (funcName == "stddevs") {
    ftype = TableExprFuncNode::arrstddevsFUNC;
  } else if (funcName == "runningstddev") {
    ftype = TableExprFuncNode::runstddevFUNC;
  } else if (funcName == "boxedstddev") {
    ftype = TableExprFuncNode::boxstddevFUNC;
  } else if (funcName == "avdev") {
    ftype = TableExprFuncNode::arravdevFUNC;
  } else if (funcName == "avdevs") {
    ftype = TableExprFuncNode::arravdevsFUNC;
  } else if (funcName == "runningavdev") {
    ftype = TableExprFuncNode::runavdevFUNC;
  } else if (funcName == "boxedavdev") {
    ftype = TableExprFuncNode::boxavdevFUNC;
  } else if (funcName == "rms") {
    ftype = TableExprFuncNode::arrrmsFUNC;
  } else if (funcName == "rmss") {
    ftype = TableExprFuncNode::arrrmssFUNC;
  } else if (funcName == "runningrms") {
    ftype = TableExprFuncNode::runrmsFUNC;
  } else if (funcName == "boxedrms") {
    ftype = TableExprFuncNode::boxrmsFUNC;
  } else if (funcName == "median") {
    ftype = TableExprFuncNode::arrmedianFUNC;
  } else if (funcName == "medians") {
    ftype = TableExprFuncNode::arrmediansFUNC;
  } else if (funcName == "runningmedian") {
    ftype = TableExprFuncNode::runmedianFUNC;
  } else if (funcName == "boxedmedian") {
    ftype = TableExprFuncNode::boxmedianFUNC;
  } else if (funcName == "fractile") {
    ftype = TableExprFuncNode::arrfractileFUNC;
  } else if (funcName == "fractiles") {
    ftype = TableExprFuncNode::arrfractilesFUNC;
  } else if (funcName == "any") {
    ftype = TableExprFuncNode::anyFUNC;
  } else if (funcName == "anys") {
    ftype = TableExprFuncNode::anysFUNC;
  } else if (funcName == "runningany") {
    ftype = TableExprFuncNode::runanyFUNC;
  } else if (funcName == "boxedany") {
    ftype = TableExprFuncNode::boxanyFUNC;
  } else if (funcName == "all") {
    ftype = TableExprFuncNode::allFUNC;
  } else if (funcName == "alls") {
    ftype = TableExprFuncNode::allsFUNC;
  } else if (funcName == "runningall") {
    ftype = TableExprFuncNode::runallFUNC;
  } else if (funcName == "boxedall") {
    ftype = TableExprFuncNode::boxallFUNC;
  } else if (funcName == "ntrue") {
    ftype = TableExprFuncNode::ntrueFUNC;
  } else if (funcName == "ntrues") {
    ftype = TableExprFuncNode::ntruesFUNC;
  } else if (funcName == "nfalse") {
    ftype = TableExprFuncNode::nfalseFUNC;
  } else if (funcName == "nfalses") {
    ftype = TableExprFuncNode::nfalsesFUNC;
  } else if (funcName == "array") {
    ftype = TableExprFuncNode::arrayFUNC;
  } else if (funcName == "isnan") {
    ftype = TableExprFuncNode::isnanFUNC;
  } else if (funcName == "isinf") {
    ftype = TableExprFuncNode::isinfFUNC;
  } else if (funcName == "isfinite") {
    ftype = TableExprFuncNode::isfiniteFUNC;
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
  } else if (funcName == "int"  ||  funcName == "integer") {
    ftype = TableExprFuncNode::intFUNC;
  } else if (funcName == "datetime") {
    ftype = TableExprFuncNode::datetimeFUNC;
  } else if (funcName == "mjdtodate") {
    ftype = TableExprFuncNode::mjdtodateFUNC;
  } else if (funcName == "mjd") {
    ftype = TableExprFuncNode::mjdFUNC;
  } else if (funcName == "date") {
    ftype = TableExprFuncNode::dateFUNC;
  } else if (funcName == "time") {
    ftype = TableExprFuncNode::timeFUNC;
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
  } else if (funcName == "cdatetime"  ||  funcName == "ctod") {
    ftype = TableExprFuncNode::ctodFUNC;
  } else if (funcName == "cdate") {
    ftype = TableExprFuncNode::cdateFUNC;
  } else if (funcName == "ctime") {
    ftype = TableExprFuncNode::ctimeFUNC;
  } else if (funcName == "hms") {
    ftype = TableExprFuncNode::hmsFUNC;
  } else if (funcName == "dms") {
    ftype = TableExprFuncNode::dmsFUNC;
  } else if (funcName == "hdms") {
    ftype = TableExprFuncNode::hdmsFUNC;
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
  } else if (funcName == "ltrim") {
    ftype = TableExprFuncNode::ltrimFUNC;
  } else if (funcName == "rtrim") {
    ftype = TableExprFuncNode::rtrimFUNC;
  } else if (funcName == "substr"  ||  funcName == "substring") {
    ftype = TableExprFuncNode::substrFUNC;
  } else if (funcName == "replace") {
    ftype = TableExprFuncNode::replaceFUNC;
  } else if (funcName == "regex") {
    ftype = TableExprFuncNode::regexFUNC;
  } else if (funcName == "pattern") {
    ftype = TableExprFuncNode::patternFUNC;
  } else if (funcName == "sqlpattern") {
    ftype = TableExprFuncNode::sqlpatternFUNC;
  } else if (funcName == "rownumber") {
    ftype = TableExprFuncNode::rownrFUNC;
  } else if (funcName == "rowid") {
    ftype = TableExprFuncNode::rowidFUNC;
  } else if (funcName == "rand") {
    ftype = TableExprFuncNode::randFUNC;
  } else if (funcName == "iif") {
    ftype = TableExprFuncNode::iifFUNC;
  } else if (funcName == "angdist"  ||  funcName == "angulardistance") {
    ftype = TableExprFuncNode::angdistFUNC;
  } else {
    // unknown name can be a user-defined function.
    ftype = TableExprFuncNode::NRFUNC;
  }
  // Functions to be ignored are incorrect.
  Bool found;
  linearSearch (found, ignoreFuncs, Int(ftype), ignoreFuncs.nelements());
  if (found) {
    throw (TableInvExpr ("Function '" + funcName +
                         "' can only be used in TaQL"));
  }
  return ftype;
}

//# Parse the name of a function.
TableExprNode TableParseSelect::handleFunc (const String& name,
					    const TableExprNodeSet& arguments,
					    const TaQLStyle& style)
{
  //# No functions have to be ignored.
  Vector<Int> ignoreFuncs;
  // Use a default table if no one available.
  // This can only happen in the PCALC case.
  if (fromTables_p.size() == 0) {
    if (commandType_p != PCALC) {
      throw TableInvExpr("No table given");
    }
    return makeFuncNode (name, arguments, ignoreFuncs, Table(), style);
  }
  return makeFuncNode (name, arguments, ignoreFuncs,
		       fromTables_p[0].table(), style);
}

//# Parse the name of a function.
TableExprNode TableParseSelect::makeFuncNode
                                         (const String& name,
					  const TableExprNodeSet& arguments,
					  const Vector<int>& ignoreFuncs,
					  const Table& table,
					  const TaQLStyle& style)
{
  //# Determine the function type.
  TableExprFuncNode::FunctionType ftype = findFunc (name,
						    arguments.nelements(),
						    ignoreFuncs);
  if (ftype == TableExprFuncNode::NRFUNC) {
    // The function can be a user defined one (or unknown).
    return TableExprNode::newUDFNode (name, arguments, table, style);
  }
  // The axes of functions like SUMS can be given as a set or as
  // individual values. Turn it into an Array object.
  uInt axarg = 1;
  switch (ftype) {
  case TableExprFuncNode::arrfractilesFUNC:
    axarg = 2;    // fall through!!
  case TableExprFuncNode::arrsumsFUNC:
  case TableExprFuncNode::arrproductsFUNC:
  case TableExprFuncNode::arrsumsqrsFUNC:
  case TableExprFuncNode::arrminsFUNC:
  case TableExprFuncNode::arrmaxsFUNC:
  case TableExprFuncNode::arrmeansFUNC:
  case TableExprFuncNode::arrvariancesFUNC:
  case TableExprFuncNode::arrstddevsFUNC:
  case TableExprFuncNode::arravdevsFUNC:
  case TableExprFuncNode::arrrmssFUNC:
  case TableExprFuncNode::arrmediansFUNC:
  case TableExprFuncNode::anysFUNC:
  case TableExprFuncNode::allsFUNC:
  case TableExprFuncNode::ntruesFUNC:
  case TableExprFuncNode::nfalsesFUNC:
  case TableExprFuncNode::runminFUNC:
  case TableExprFuncNode::runmaxFUNC:
  case TableExprFuncNode::runmeanFUNC:
  case TableExprFuncNode::runvarianceFUNC:
  case TableExprFuncNode::runstddevFUNC:
  case TableExprFuncNode::runavdevFUNC:
  case TableExprFuncNode::runrmsFUNC:
  case TableExprFuncNode::runmedianFUNC:
  case TableExprFuncNode::runanyFUNC:
  case TableExprFuncNode::runallFUNC:
  case TableExprFuncNode::boxminFUNC:
  case TableExprFuncNode::boxmaxFUNC:
  case TableExprFuncNode::boxmeanFUNC:
  case TableExprFuncNode::boxvarianceFUNC:
  case TableExprFuncNode::boxstddevFUNC:
  case TableExprFuncNode::boxavdevFUNC:
  case TableExprFuncNode::boxrmsFUNC:
  case TableExprFuncNode::boxmedianFUNC:
  case TableExprFuncNode::boxanyFUNC:
  case TableExprFuncNode::boxallFUNC:
  case TableExprFuncNode::arrayFUNC:
    if (arguments.nelements() > axarg) {
      TableExprNodeSet parms;
      // Add normal arguments to the parms.
      for (uInt i=0; i<axarg; i++) {
	parms.add (arguments[i]);
      }
      // Now add the axes arguments.
      // They can be given as one single array or as individual scalars.
      Bool axesIsArray = False;
      if (arguments.nelements() == axarg+1
      &&  arguments[axarg].isSingle()) {
	const TableExprNodeSetElem& arg = arguments[axarg];
	if (arg.start()->valueType() == TableExprNodeRep::VTArray) {
	  parms.add (arg);
	  axesIsArray = True;
	}
      }
      if (!axesIsArray) {
	// Combine all axes in a single set and add to parms.
	TableExprNodeSet axes;
	for (uInt i=axarg; i<arguments.nelements(); i++) {
	  const TableExprNodeSetElem& arg = arguments[i];
	  const TableExprNodeRep* rep = arg.start();
	  if (rep == 0  ||  !arg.isSingle()
              ||  rep->valueType() != TableExprNodeRep::VTScalar
              ||  (rep->dataType() != TableExprNodeRep::NTInt
                   &&  rep->dataType() != TableExprNodeRep::NTDouble)) {
	    throw TableInvExpr ("Axes/shape arguments " +
				String::toString(i+1) +
				" are not one or more scalars"
				" or a single bounded range");
	  }
	  axes.add (arg);
	}
	parms.add (TableExprNodeSetElem(axes.setOrArray()));
      }
      return TableExprNode::newFunctionNode (ftype, parms, table, style);
    }
    break;
  case TableExprFuncNode::conesFUNC:
  case TableExprFuncNode::anyconeFUNC:
  case TableExprFuncNode::findconeFUNC:
  case TableExprFuncNode::cones3FUNC:
  case TableExprFuncNode::anycone3FUNC:
  case TableExprFuncNode::findcone3FUNC:
    return TableExprNode::newConeNode (ftype, arguments, style.origin());
  default:
    break;
  }
  return TableExprNode::newFunctionNode (ftype, arguments, table, style);
}


//# Add a column name to the block of column names.
//# Only take the part beyond the period.
//# Extend the block each time. Since there are only a few column names,
//# this will not be too expensive.
void TableParseSelect::handleColumn (Int stringType,
				     const String& name,
				     const TableExprNode& expr,
				     const String& newName,
				     const String& newDtype)
{
  if (expr.isNull()  &&  stringType >= 0) {
    // A wildcarded column name is given.
    handleWildColumn (stringType, name);
  } else {
    // A single column is given.
    Int nrcol = columnNames_p.nelements();
    columnNames_p.resize (nrcol+1);
    columnExpr_p.resize (nrcol+1);
    columnOldNames_p.resize (nrcol+1);
    columnDtypes_p.resize (nrcol+1);
    if (expr.isNull()) {
      // A true column name is given.
      String oldName;
      String str = name;
      Int inx = str.index('.');
      if (inx < 0) {
	oldName = str;
      } else {
	oldName = str.after(inx);
      }
      // Make an expression of the column or keyword name.
      columnExpr_p[nrcol] = handleKeyCol (str);
      if (columnExpr_p[nrcol].table().isNull()) {
        // A keyword was given which is returned as a constant.
        nrSelExprUsed_p++;
      } else {
        // If a data type or shorthand is given, the column must be handled
        // as an expression.
        // The same is true if the same column is already used. In such a case
        // the user likely wants to duplicate the column with a different name.
        columnOldNames_p[nrcol] = oldName;
        if (!newDtype.empty()  ||  inx >= 0) {
          nrSelExprUsed_p++;
        } else {
          for (Int i=0; i<nrcol-1; ++i) {
            if (str == columnOldNames_p[i]) {
              nrSelExprUsed_p++;
              break;
            }
          }
        }
      }
    } else {
      // An expression is given.
      columnExpr_p[nrcol] = expr;
      nrSelExprUsed_p++;
    }
    columnDtypes_p[nrcol] = newDtype;
    columnNames_p[nrcol]  = newName;
    if (newName.empty()) {
      columnNames_p[nrcol] = columnOldNames_p[nrcol];
    }
  }
}

//# Handle a wildcarded a column name.
//# Add or remove to/from the block of column names as needed.
void TableParseSelect::handleWildColumn (Int stringType, const String& name)
{
  Int nrcol = columnNames_p.nelements();
  Bool caseInsensitive = ((stringType & 1) != 0);
  Bool negate          = ((stringType & 2) != 0);
  String str = name.substr(2, name.size()-3);
  Regex regex;
  // See if the wildcarded name has a table shorthand in it.
  // That is not really handled yet (neither in handleColumn).
  // It should be done in a future TaQL version (supporting joins).
  String shorthand;
  if (name[0] == 'p') {
    if (!negate) {
      int j = str.index('.');
      if (j >= 0) {
	shorthand = str.before(j);
	str       = str.after(j);
      }
    }
    regex = Regex::fromPattern (str);
  } else {
    if (!negate) {
      int j = str.index("\\.");
      if (j >= 0) {
	shorthand = str.before(j);
	str       = str.after(j+1);
      }
    }
    if (name[0] == 'f') {
      regex = Regex(str);
    } else {
      regex = Regex(".*(" + str + ").*");
    }
  }
  if (!negate) {
    // Add back the delimiting . if a shorthand is given.
    if (! shorthand.empty()) {
      shorthand += '.';
    }
    // Find all matching columns.
    Table tab = findTable(String());
    Vector<String> columns = tab.tableDesc().columnNames();
    Int nr = 0;
    for (uInt i=0; i<columns.size(); ++i) {
      String col = columns[i];
      if (caseInsensitive) {
	col.downcase();
      }
      if (col.matches(regex)) {
	++nr;
      } else {
	columns[i] = String();
      }
    }
    // Add them to the list of column names.
    columnNames_p.resize    (nrcol+nr);
    columnExpr_p.resize     (nrcol+nr);
    columnOldNames_p.resize (nrcol+nr);
    columnDtypes_p.resize   (nrcol+nr);
    for (uInt i=0; i<columns.size(); ++i) {
      if (! columns[i].empty()) {
	// Add the shorthand to the name, so negation takes that into account.
	columnNames_p[nrcol++] = shorthand + columns[i];
      }
    }
  } else {
    // Negation of wildcard, thus remove columns if matching.
    // This is done until the last non-wildcarded column name.
    while (nrcol > 0) {
      --nrcol;
      if (! columnExpr_p[nrcol].isNull()) {
	break;
      }
      String col = columnNames_p[nrcol];
      if (!col.empty()) {
	if (caseInsensitive) {
	  col.downcase();
	}
	if (col.matches(regex)) {
	  columnNames_p[nrcol] = String();
	}
      }
    }
  }
}

//# Finish the additions to the block of column names
//# by removing the deleted empty names and creating Expr objects as needed.
void TableParseSelect::handleColumnFinish (Bool distinct)
{
  distinct_p = distinct;
  // Remove the deleted column names.
  // Create Expr objects for the wildcarded names.
  Int nrcol = columnNames_p.size();
  if (nrcol > 0) {
    Block<String> names(nrcol);
    Block<String> oldNames(nrcol);
    Block<TableExprNode> exprs(nrcol);
    Block<String> dtypes(nrcol);
    Int nr = 0;
    for (Int i=0; i<nrcol; ++i) {
      if (! (columnExpr_p[i].isNull()  &&  columnNames_p[i].empty())) {
	names[nr]    = columnNames_p[i];
	oldNames[nr] = columnOldNames_p[i];
	exprs[nr]    = columnExpr_p[i];
	dtypes[nr]   = columnDtypes_p[i];
	// Create a Expr object if needed.
	if (exprs[nr].isNull()) {
	  // That can only be the case if no old name is filled in.
	  AlwaysAssert (oldNames[nr].empty(), AipsError);
	  String name = names[nr];
	  Int j = name.index('.');
	  if (j >= 0) {
	    name = name.after(j);
	  }
	  // Make an expression of the column name.
	  exprs[nr]    = handleKeyCol (name);
	  names[nr]    = name;
	  oldNames[nr] = name;
	}
	++nr;
      }
    }
    names.resize   (nr, True);
    oldNames.resize(nr, True);
    exprs.resize   (nr, True);
    dtypes.resize  (nr, True);
    columnNames_p    = names;
    columnOldNames_p = oldNames;
    columnExpr_p     = exprs;
    columnDtypes_p   = dtypes;
  }
}

//# Add a column specification.
void TableParseSelect::handleColSpec (const String& colName,
				      const String& dtstr,
				      const Record& spec,
				      Bool isCOrder)
{
  // Check if specific column info is given.
  Int options = 0;
  Int ndim = -1;
  IPosition shape;
  String dmType;
  String dmGroup;
  String comment;
  String unit;
  for (uInt i=0; i<spec.nfields(); i++) {
    String name = spec.name(i);
    name.upcase();
    if (name == "NDIM") {
      ndim = spec.asInt(i);
    } else if (name == "SHAPE") {
      Vector<Int> ivec(spec.asArrayInt(i));
      if (isCOrder) {
	Int nd = ivec.nelements();
	shape.resize (nd);
	for (Int i=0; i<nd; ++i) {
	  shape[i] = ivec[nd-i-1];
	}
      } else {
	shape = IPosition(ivec);
      }
      if (ndim < 0) {
	ndim = 0;
      }
    } else if (name == "DMTYPE") {
      dmType = spec.asString(i);
    } else if (name == "DMGROUP") {
      dmGroup = spec.asString(i);
    } else if (name == "COMMENT") {
      comment = spec.asString(i);
    } else if (name == "UNIT") {
      unit = spec.asString(i);
    } else {
      throw TableError ("TableParseSelect::handleColSpec - "
			"column specification field name " + name +
			" is unknown");
    }
  }
  // Now add the scalar or array column description.
  DataType dtype = makeDataType (TpOther, dtstr, colName);
  addColumnDesc (tableDesc_p, dtype, colName, options, ndim, shape,
		 dmType, dmGroup, comment, unit);
  Int nrcol = columnNames_p.nelements();
  columnNames_p.resize (nrcol+1);
  columnNames_p[nrcol] = colName;
}

void TableParseSelect::handleCreTab (const String& tableName,
				     const Record& dmInfo)
{
  SetupNewTable newtab(tableName, tableDesc_p, Table::New);
  newtab.bindCreate (dmInfo);
  table_p = Table(newtab);
}

void TableParseSelect::handleSelect (const TableExprNode& node)
{
  node_p = node;
  if (distinct_p  &&  columnNames_p.nelements() == 0) {
    throw TableInvExpr ("SELECT DISTINCT can only be given with at least "
			"one column name");
  }
}

void TableParseSelect::handleSort (const std::vector<TableParseSort>& sort,
				   Bool noDuplicates,
				   Sort::Order order)
{
  noDupl_p = noDuplicates;
  order_p  = order;
  sort_p   = sort;
}

void TableParseSelect::handleCalcComm (const TableExprNode& node)
{
  node_p = node;
}

Block<String> TableParseSelect::getStoredColumns (const Table& tab) const
{
  Block<String> names;
  const TableDesc& tdesc = tab.tableDesc();
  for (uInt i=0; i<tdesc.ncolumn(); i++) {
    const String& colnm = tdesc[i].name();
    if (tab.isColumnStored(colnm)) {
      uInt inx = names.nelements();
      names.resize (inx + 1);
      names[inx] = colnm;
    }
  }
  return names;
}

//# Execute a query in the FROM clause and return the resulting table.
Table TableParseSelect::doFromQuery (Bool showTimings)
{
  Timer timer;
  // Execute the nested command.
  execute (False, False, True, 0);
  if (showTimings) {
    timer.show ("  From query  ");
  }
  return table_p;
}

//# Execute a subquery for an EXISTS operator.
TableExprNode TableParseSelect::doExists (Bool notexists, Bool showTimings)
{
  Timer timer;
  // Execute the nested command.
  // Default limit_p is 1.
  execute (False, True, True, 1);
  if (showTimings) {
    timer.show ("  Exists query");
  }
  // Flag notexists tells if NOT EXISTS or EXISTS was given.
  return TableExprNode (notexists == (table_p.nrow() < limit_p));
}

//# Execute a subquery and create the correct node object for it.
TableExprNode TableParseSelect::doSubQuery (Bool showTimings)
{
  Timer timer;
  // Execute the nested command.
  execute (False, True, True, 0);
  TableExprNode result;
  if (resultSet_p != 0) {
    // A set specification was given, so make the set.
    result = makeSubSet (fromTables_p[0].table());
  } else {
    // A single column was given, so get its data.
    result = getColSet();
  }
  if (showTimings) {
    timer.show ("  Subquery    ");
  }
  return result;
}

TableExprNode TableParseSelect::getColSet()
{
  // Check if there is only one column which has to contain a scalar.
  const TableDesc& tableDesc = table_p.tableDesc();
  if (tableDesc.ncolumn() != 1) {
    throw (TableInvExpr ("Nested query should select 1 column"));
  }
  const ColumnDesc& colDesc = tableDesc.columnDesc(0);
  if (! colDesc.isScalar()) {
    throw (TableInvExpr ("Nested query should select a scalar column"));
  }
  ROTableColumn tabcol (table_p, colDesc.name());
  TableExprNodeRep* tsnptr;
  switch (colDesc.dataType()) {
  case TpBool:
    tsnptr = new TableExprNodeArrayConstBool
                        (ROScalarColumn<Bool>(tabcol).getColumn());
    break;
  case TpUChar:
    tsnptr = new TableExprNodeArrayConstInt
                        (ROScalarColumn<uChar>(tabcol).getColumn());
    break;
  case TpShort:
    tsnptr = new TableExprNodeArrayConstInt
                        (ROScalarColumn<Short>(tabcol).getColumn());
    break;
  case TpUShort:
    tsnptr = new TableExprNodeArrayConstInt
                        (ROScalarColumn<uShort>(tabcol).getColumn());
    break;
  case TpInt:
    tsnptr = new TableExprNodeArrayConstInt
                        (ROScalarColumn<Int>(tabcol).getColumn());
    break;
  case TpUInt:
    tsnptr = new TableExprNodeArrayConstInt
                        (ROScalarColumn<uInt>(tabcol).getColumn());
    break;
  case TpFloat:
    tsnptr = new TableExprNodeArrayConstDouble
                        (ROScalarColumn<Float>(tabcol).getColumn());
    break;
  case TpDouble:
    tsnptr = new TableExprNodeArrayConstDouble
                        (ROScalarColumn<Double>(tabcol).getColumn());
    break;
  case TpComplex:
    tsnptr = new TableExprNodeArrayConstDComplex
                        (ROScalarColumn<Complex>(tabcol).getColumn());
    break;
  case TpDComplex:
    tsnptr = new TableExprNodeArrayConstDComplex
                        (ROScalarColumn<DComplex>(tabcol).getColumn());
    break;
  case TpString:
    tsnptr = new TableExprNodeArrayConstString
                        (ROScalarColumn<String>(tabcol).getColumn());
    break;
  default:
    throw (TableInvExpr ("Nested query column " + colDesc.name() +
			 " has unknown data type"));
  }
  //# Fill in the column unit (if defined).
  tsnptr->setUnit (TableExprNodeColumn::getColumnUnit (tabcol));
  return tsnptr;
}


TableExprNode TableParseSelect::makeSubSet (const Table& origTable) const
{
  // Perform some checks on the given set.
  if (resultSet_p->hasArrays()) {
    throw (TableInvExpr ("Set in GIVING clause should contain scalar"
			 " elements"));
  }
  resultSet_p->checkEqualDataTypes();
  // Link to set to make sure that TableExprNode hereafter does not delete
  // the object.
  resultSet_p->link();
  if (! TableExprNode(resultSet_p).checkTableSize (origTable, False)) {
    throw (TableInvExpr ("Tables with different sizes used in "
                         "GIVING set expression"));
  }
  TableExprNodeSet set(rownrs_p, *resultSet_p);
  return set.setOrArray();
}

void TableParseSelect::handleLimit (const TableExprNode& expr)
{
  Int64 val = evalIntScaExpr (expr);
  if (val < 1) {
      throw TableInvExpr ("LIMIT must have a value >= 1");
  }
  limit_p = val;
}

void TableParseSelect::handleOffset (const TableExprNode& expr)
{
  Int64 val = evalIntScaExpr (expr);
  offset_p = (val<0  ?  0 : val);
}

Int64 TableParseSelect::evalIntScaExpr (const TableExprNode& expr) const
{
  if (!expr.table().isNull()) {
    throw TableInvExpr ("LIMIT or OFFSET expression cannot contain columns");
  }
  TableExprId rowid(0);
  Int64 val;
  expr.get (rowid, val);
  return val;
}

void TableParseSelect::handleUpdate()
{
  columnNames_p.resize (update_p.size());
  for (uInt i=0; i<update_p.size(); i++) {
    columnNames_p[i] = update_p[i]->columnName();
  }
}

void TableParseSelect::handleInsert()
{
  // If no columns were given, all stored columns in the first table
  // are the target columns.
  if (columnNames_p.nelements() == 0) {
    columnNames_p = getStoredColumns (fromTables_p[0].table());
  }
  // Check if #columns and values match.
  // Copy the names to the update objects.
  if (update_p.size() != columnNames_p.nelements()) {
    throw TableInvExpr ("Error in INSERT command; nr of columns (=" +
			String::toString(columnNames_p.nelements()) +
			") mismatches "
			"number of VALUES expressions (=" +
			String::toString(Int(update_p.size())) + ")");
  }
  for (uInt i=0; i<update_p.size(); i++) {
    update_p[i]->setColumnName (columnNames_p[i]);
  }
}

void TableParseSelect::handleInsert (TableParseSelect* sel)
{
  insSel_p = sel;
}

void TableParseSelect::handleCount()
{
  if (columnExpr_p.size() == 0) {
    throw TableInvExpr ("No COUNT columns given");
  }
  for (uInt i=0; i<columnExpr_p.size(); i++) {
    if (!columnExpr_p[i].isScalar()) {
      throw TableInvExpr ("COUNT column " + columnNames_p[i] + " is not scalar");
    }
  }
}

//# Execute the updates.
void TableParseSelect::doUpdate (Bool showTimings, const Table& origTable,
                                 Table& updTable, const Vector<uInt>& rownrs)
{
  Timer timer;
  AlwaysAssert (updTable.nrow() == rownrs.size(), AipsError);
  //# If no rows to be updated, return immediately.
  //# (the code below will fail for empty tables)
  if (rownrs.empty()) {
    return;
  }
  // Reopen the table for write.
  updTable.reopenRW();
  if (! updTable.isWritable()) {
    throw TableInvExpr ("Table " + updTable.tableName() + " is not writable");
  }
  //# First check if the update columns and values are correct.
  const TableDesc& tabdesc = updTable.tableDesc();
  uInt nrkey = update_p.size();
  Block<TableColumn> cols(nrkey);
  Block<Int> dtypeCol(nrkey);
  Block<Bool> isScalarCol(nrkey);
  for (uInt i=0; i<nrkey; i++) {
    TableParseUpdate& key = *(update_p[i]);
    const String& colName = key.columnName();
    //# Check if the correct table is used in the update and index expression.
    //# A constant expression can be given.
    if (! key.node().checkTableSize (origTable, True)) {
      throw TableInvExpr ("Table(s) with incorrect size used in the "
                          "UPDATE expr of column " + colName);
    }
    if (key.indexPtr() != 0) {
      if (! key.indexNode().checkTableSize (updTable, True)) {
      	throw TableInvExpr ("Table(s) with incorrect size used in the "
                            "index expr in UPDATE of column " + colName);
      }
    }
    //# This throws an exception for unknown data types (datetime, regex).
    key.node().getColumnDataType();
    //# Check if the column exists and is writable.
    if (! tabdesc.isColumn (colName)) {
      throw TableInvExpr ("Update column " + colName +
			  " does not exist in table " +
			  updTable.tableName());
    }
    if (! updTable.isColumnWritable (colName)) {
      throw TableInvExpr ("Update column " + colName +
			  " is not writable in table " +
			  updTable.tableName());
    }
    //# An index expression can only be given for an array column.
    const ColumnDesc& coldesc = tabdesc[colName];
    Bool isScalar = coldesc.isScalar();
    isScalarCol[i] = isScalar;
    if (key.indexPtr() != 0) {
      if (isScalar) {
	throw TableInvExpr ("Index value cannot be given in UPDATE of "
			    " scalar column " + colName);
      }
      if (key.indexPtr()->isSingle()) {
	isScalar = True;
      }
    }
    //# Check if the value type matches.
    if (isScalar  &&  !key.node().isScalar()) {
      throw TableInvExpr ("An array value cannot be used in UPDATE of "
			  " scalar element of column " +
			  colName + " in table " +
			  updTable.tableName());
    }
    cols[i].attach (updTable, colName);
    dtypeCol[i] = coldesc.dataType();
    // If needed, make the expression's unit the same as the column unit.
    key.node().adaptUnit (TableExprNodeColumn::getColumnUnit
			          (ROTableColumn(updTable, colName)));
  }
  // IPosition objects in case slicer.inferShapeFromSource has to be used.
  IPosition trc,blc,inc;
  // Loop through all rows in the table and update each row.
  TableExprId rowid(0);
  for (uInt row=0; row<rownrs.size(); ++row) {
    rowid.setRownr (rownrs[row]);
    for (uInt i=0; i<nrkey; i++) {
      TableColumn& col = cols[i];
      const TableParseUpdate& key = *(update_p[i]);
      const TableExprNode& node = key.node();
      // Get possible subscripts.
      const Slicer* slicerPtr = 0;
      if (key.indexPtr() != 0) {
	slicerPtr = &(key.indexPtr()->getSlicer(rowid));
      }
      Bool isSca = isScalarCol[i];
      // The expression node type determines how to get the data.
      // The column data type determines how to put it.
      // The node data type should be convertible to the column data type.
      // The updateValue functions do the actual work.
      // We simply switch on the types.
      switch (node.dataType()) {
      case TpBool:
        switch (dtypeCol[i]) {
        case TpBool:
          updateValue1<Bool> (row, rowid, isSca, node, col, slicerPtr,
                              blc, trc, inc);
          break;
        default:
          throw TableInvExpr ("Column " + update_p[i]->columnName() +
                              " has an invalid data type for an"
                              " UPDATE with a bool value");
        }
        break;

      case TpString:
        switch (dtypeCol[i]) {
        case TpString:
          updateValue1<String> (row, rowid, isSca, node, col, slicerPtr,
                                blc, trc, inc);
          break;
        default:
          throw TableInvExpr ("Column " + update_p[i]->columnName() +
                              " has an invalid data type for an"
                              " UPDATE with a string value");
        }
        break;

      case TpInt:
        switch (dtypeCol[i]) {
        case TpUChar:
          updateValue2<uChar,Int64> (row, rowid, isSca, node, col, slicerPtr,
                                     blc, trc, inc);
          break;
        case TpShort:
          updateValue2<Short,Int64> (row, rowid, isSca, node, col, slicerPtr,
                                     blc, trc, inc);
	    break;
        case TpUShort:
          updateValue2<uShort,Int64> (row, rowid, isSca, node, col, slicerPtr,
                                      blc, trc, inc);
          break;
        case TpInt:
          updateValue2<Int,Int64> (row, rowid, isSca, node, col, slicerPtr,
                                   blc, trc, inc);
          break;
        case TpUInt:
          updateValue2<uInt,Int64> (row, rowid, isSca, node, col, slicerPtr,
                                    blc, trc, inc);
          break;
        case TpFloat:
          updateValue2<Float,Int64> (row, rowid, isSca, node, col, slicerPtr,
                                     blc, trc, inc);
          break;
        case TpDouble:
          updateValue2<Double,Int64> (row, rowid, isSca, node, col, slicerPtr,
                                      blc, trc, inc);
          break;
        case TpComplex:
          updateValue2<Complex,Int64> (row, rowid, isSca, node, col, slicerPtr,
                                       blc, trc, inc);
          break;
        case TpDComplex:
          updateValue2<DComplex,Int64> (row, rowid, isSca, node, col, slicerPtr,
                                        blc, trc, inc);
          break;
        default:
          throw TableInvExpr ("Column " + update_p[i]->columnName() +
                              " has an invalid data type for an"
                              " UPDATE with an integer value");
        }
	break;

      case TpDouble:
        switch (dtypeCol[i]) {
        case TpUChar:
          updateValue2<uChar,Double> (row, rowid, isSca, node, col, slicerPtr,
                                      blc, trc, inc);
          break;
        case TpShort:
          updateValue2<Short,Double> (row, rowid, isSca, node, col, slicerPtr,
                                      blc, trc, inc);
	    break;
        case TpUShort:
          updateValue2<uShort,Double> (row, rowid, isSca, node, col, slicerPtr,
                                       blc, trc, inc);
          break;
        case TpInt:
          updateValue2<Int,Double> (row, rowid, isSca, node, col, slicerPtr,
                                    blc, trc, inc);
          break;
        case TpUInt:
          updateValue2<uInt,Double> (row, rowid, isSca, node, col, slicerPtr,
                                     blc, trc, inc);
          break;
        case TpFloat:
          updateValue2<Float,Double> (row, rowid, isSca, node, col, slicerPtr,
                                      blc, trc, inc);
          break;
        case TpDouble:
          updateValue1<Double> (row, rowid, isSca, node, col, slicerPtr,
                                blc, trc, inc);
          break;
        case TpComplex:
          updateValue2<Complex,Double> (row, rowid, isSca, node, col, slicerPtr,
                                        blc, trc, inc);
          break;
        case TpDComplex:
          updateValue2<DComplex,Double> (row, rowid, isSca, node, col, slicerPtr,
                                         blc, trc, inc);
          break;
        default:
          throw TableInvExpr ("Column " + update_p[i]->columnName() +
                              " has an invalid data type for an"
                              " UPDATE with a double value");
        }
      break;

      case TpDComplex:
        switch (dtypeCol[i]) {
        case TpComplex:
          updateValue2<Complex,DComplex> (row, rowid, isSca, node, col, slicerPtr,
                                          blc, trc, inc);
          break;
        case TpDComplex:
          updateValue1<DComplex> (row, rowid, isSca, node, col, slicerPtr,
                                  blc, trc, inc);
          break;
        default:
          throw TableInvExpr ("Column " + update_p[i]->columnName() +
                              " has an invalid data type for an"
                              " UPDATE with a complex value");
        }
        break;
          
      default:
	throw TableInvExpr ("Unknown UPDATE expression data type");
      }
    }
  }
  if (showTimings) {
    timer.show ("  Update      ");
  }
}

template<typename T>
void TableParseSelect::updateValue1 (uInt row, const TableExprId& rowid,
                                     Bool isScalarCol,
                                     const TableExprNode& node,
                                     TableColumn& col,
                                     const Slicer* slicerPtr,
                                     IPosition& blc, IPosition& trc,
                                     IPosition& inc)
{
  // This is effectively the same function as below. Only the data type
  // of expression node and table column are the same, so no conversion
  // needs to be done.
  if (node.isScalar()) {
    T value;
    node.get (rowid, value);
    if (isScalarCol) {
      col.putScalar (row, value);
    } else {
      ArrayColumn<T> acol(col);
      Array<T> arr;
      if (slicerPtr == 0) {
        arr.resize (acol.shape(row));
        arr = value;
        acol.put (row, arr);
      } else {
        if (slicerPtr->isFixed()) {
          arr.resize (slicerPtr->length());
        } else {
          arr.resize (slicerPtr->inferShapeFromSource (acol.shape(row),
                                                       blc, trc, inc));
        }
        arr = value;
        acol.putSlice (row, *slicerPtr, arr);
      }
    }
  } else {
    // Only put an array if defined.
    if (node.isResultDefined (rowid)) {
      Array<T> value;
      node.get (rowid, value);
      ArrayColumn<T> acol(col);
      if (slicerPtr == 0) {
        acol.put (row, value);
      } else if (acol.isDefined(row)) {
        acol.putSlice (row, *slicerPtr, value);
      }
    }
  }
}

template<typename TCOL, typename TNODE>
void TableParseSelect::updateValue2 (uInt row, const TableExprId& rowid,
                                     Bool isScalarCol,
                                     const TableExprNode& node,
                                     TableColumn& col,
                                     const Slicer* slicerPtr,
                                     IPosition& blc, IPosition& trc,
                                     IPosition& inc)
{
  if (node.isScalar()) {
    // Expression node has a scalar value, so get it.
    TNODE val;
    node.get (rowid, val);
    TCOL value(static_cast<TCOL>(val));
    if (isScalarCol) {
      // The column is a scalar too, so put it.
      col.putScalar (row, value);
    } else {
      // The column contains an array which will be set to the scalar.
      // If a slice is given, only that slice of the array is set and put.
      // Only put if the row contains a data array.
      ArrayColumn<TCOL> acol(col);
      if (acol.isDefined(row)) {
        Array<TCOL> arr;
        if (slicerPtr == 0) {
          arr.resize (acol.shape(row));
          arr = value;
          acol.put (row, arr);
        } else {
          if (slicerPtr->isFixed()) {
            arr.resize (slicerPtr->length());
          } else {
            // Unbound slicer, so derive from array shape.
            arr.resize (slicerPtr->inferShapeFromSource (acol.shape(row),
                                                         blc, trc, inc));
          }
          arr = value;
          acol.putSlice (row, *slicerPtr, arr);
        }
      }
    }
  } else {
    // The expression node contains an array, so put it in the column array or
    // a slice of it. Note that putSlice takes care of possibly unbound slicers.
    // Only put if defined.
    if (node.isResultDefined(rowid)) {
      Array<TNODE> val;
      node.get (rowid, val);
      Array<TCOL> value(val.shape());
      convertArray (value, val);
      ArrayColumn<TCOL> acol(col);
      if (slicerPtr == 0) {
        acol.put (row, value);
      } else if (acol.isDefined(row)) {
        acol.putSlice (row, *slicerPtr, value);
      }
    }
  }
}


//# Execute the inserts.
Table TableParseSelect::doInsert (Bool showTimings, Table& table)
{
  Timer timer;
  // Reopen the table for write.
  table.reopenRW();
  if (! table.isWritable()) {
    throw TableInvExpr ("Table " + table.tableName() + " is not writable");
  }
  // Add a single row if the inserts are given as expressions.
  // Select the single row and use update to put the expressions into the row.
  if (update_p.size() > 0) {
    Vector<uInt> rownrs(1);
    rownrs[0] = table.nrow();
    table.addRow();
    Table sel = table(rownrs);
    doUpdate (False, Table(), sel, rownrs);
    return sel;
  }
  // Handle the inserts from another selection.
  // Do the selection.
  insSel_p->execute (False, False, False, 0);
  Table sel = insSel_p->getTable();
  if (sel.nrow() == 0) {
    return Table();
  }
  // Get the target columns if not given.
  if (columnNames_p.nelements() == 0) {
    columnNames_p = getStoredColumns (table);
  }
  // Get the source columns.
  Block<String> sourceNames;
  sourceNames = insSel_p->getColumnNames();
  if (sourceNames.nelements() == 0) {
    sourceNames = getStoredColumns (sel);
  }
  // Check if the number of columns match.
  if (sourceNames.nelements() != columnNames_p.nelements()) {
    throw TableInvExpr ("Error in INSERT command; nr of columns (=" +
			String::toString(columnNames_p.nelements()) +
			") mismatches "
			"number of columns in selection (=" +
			String::toString(sourceNames.nelements()) + ")");
  }
  // Check if the data types match.
  const TableDesc& tdesc1 = table.tableDesc();
  const TableDesc& tdesc2 = sel.tableDesc();
  for (uInt i=0; i<columnNames_p.nelements(); i++) {
    if (tdesc1[columnNames_p[i]].trueDataType() !=
	tdesc2[sourceNames[i]].trueDataType()) {
      throw TableInvExpr ("Error in INSERT command; data type of columns " +
			  columnNames_p[i] + " and " + sourceNames[i] +
			  " mismatch");
    }
  }
  // Add the required nr of rows to the table and make a selection of it.
  uInt rownr = table.nrow();
  table.addRow (sel.nrow());
  Vector<uInt> rownrs(sel.nrow());
  indgen (rownrs, rownr);     // fill with rownr, rownr+1, etc.
  Table tab = table(rownrs);
  TableRow rowto (tab, Vector<String>(columnNames_p));
  ROTableRow rowfrom (sel, Vector<String>(sourceNames));
  for (uInt i=0; i<sel.nrow(); i++) {
    rowto.put (i, rowfrom.get(i), False);
  }
  if (showTimings) {
    timer.show ("  Insert      ");
  }
  return tab;
}


//# Execute the deletes.
void TableParseSelect::doDelete (Bool showTimings, Table& table)
{
  //# If the selection is empty, return immediately.
  if (rownrs_p.empty()) {
    return;
  }
  Timer timer;
  // Reopen the table for write.
  table.reopenRW();
  if (! table.isWritable()) {
    throw TableInvExpr ("Table " + table.tableName() + " is not writable");
  }
  // Delete all rows.
  table.removeRow (rownrs_p);
  if (showTimings) {
    timer.show ("  Delete      ");
  }
}


//# Execute the counts.
Table TableParseSelect::doCount (Bool showTimings, const Table& table)
{
  Timer timer;
  // First do the column projection.
  Table intab = doProject (False, table);
  // Create an empty memory table with the same description as the input table.
  Table tab = TableCopy::makeEmptyMemoryTable ("", intab, True);
  // Add the uInt _COUNT_ column.
  ScalarColumnDesc<uInt> countDesc ("_COUNT_");
  tab.addColumn (countDesc);
  ScalarColumn<uInt> countCol(tab, "_COUNT_");
  // Iterate for all columns through the input table.
  Vector<String> colNames = intab.tableDesc().columnNames();
  Block<String> bcolNames(colNames.size());
  std::copy (colNames.begin(), colNames.end(), bcolNames.begin());
  TableIterator iter (intab, bcolNames);
  while (!iter.pastEnd()) {
    Table tabfrom = iter.table();
    // Add one row containing the column values.
    uInt rownr = tab.nrow();
    tab.addRow();
    Table tabto = tab.project (bcolNames);
    TableCopy::copyRows (tabto, tabfrom, rownr, 0, 1);
    // Put the count.
    countCol.put (rownr, tabfrom.nrow());
    iter++;
  }
  if (showTimings) {
    timer.show ("  Count       ");
  }
  return tab;
}


//# Execute the sort.
void TableParseSelect::doSort (Bool showTimings, const Table& origTable)
{
    //# If no rows, return immediately.
    //# (the code below will fail if empty)
    if (rownrs_p.empty()) {
	return;
    }
    Timer timer;
    uInt i;
    uInt nrkey = sort_p.size();
    //# First check if the sort keys are correct.
    for (i=0; i<nrkey; i++) {
	const TableParseSort& key = sort_p[i];
	//# Check if the correct table is used in the sort key expression.
        if (! key.node().checkTableSize (origTable, False)) {
            throw (TableInvExpr ("Table(s) with incorrect size used "
                                 "in sort key " + String::toString(i)));
        }
	//# This throws an exception for unknown data types (datetime, regex).
	key.node().getColumnDataType();
    }
    Block<void*> arrays(nrkey);
    Sort sort;
    Bool deleteIt;
    for (i=0; i<nrkey; i++) {
	const TableParseSort& key = sort_p[i];
	switch (key.node().getColumnDataType()) {
	case TpBool:
	    {
		Array<Bool>* array = new Array<Bool>
                  (key.node().getColumnBool(rownrs_p));
		arrays[i] = array;
		const Bool* data = array->getStorage (deleteIt);
		sort.sortKey (data, TpBool, 0, getOrder(key));
		array->freeStorage (data, deleteIt);
	    }
	    break;
	case TpUChar:
	    {
		Array<uChar>* array = new Array<uChar>
                  (key.node().getColumnuChar(rownrs_p));
		arrays[i] = array;
		const uChar* data = array->getStorage (deleteIt);
		sort.sortKey (data, TpUChar, 0, getOrder(key));
		array->freeStorage (data, deleteIt);
	    }
	    break;
	case TpShort:
	    {
		Array<Short>* array = new Array<Short>
                  (key.node().getColumnShort(rownrs_p));
		arrays[i] = array;
		const Short* data = array->getStorage (deleteIt);
		sort.sortKey (data, TpShort, 0, getOrder(key));
		array->freeStorage (data, deleteIt);
	    }
	    break;
	case TpUShort:
	    {
		Array<uShort>* array = new Array<uShort>
                  (key.node().getColumnuShort(rownrs_p));
		arrays[i] = array;
		const uShort* data = array->getStorage (deleteIt);
		sort.sortKey (data, TpUShort, 0, getOrder(key));
		array->freeStorage (data, deleteIt);
	    }
	    break;
	case TpInt:
	    {
                Array<Int>* array = new Array<Int>
                  (key.node().getColumnInt(rownrs_p));
		arrays[i] = array;
		const Int* data = array->getStorage (deleteIt);
		sort.sortKey (data, TpInt, 0, getOrder(key));
		array->freeStorage (data, deleteIt);
	    }
	    break;
	case TpUInt:
	    {
		Array<uInt>* array = new Array<uInt>
                  (key.node().getColumnuInt(rownrs_p));
		arrays[i] = array;
		const uInt* data = array->getStorage (deleteIt);
		sort.sortKey (data, TpUInt, 0, getOrder(key));
		array->freeStorage (data, deleteIt);
	    }
	    break;
	case TpFloat:
	    {
		Array<Float>* array = new Array<Float>
                  (key.node().getColumnFloat(rownrs_p));
		arrays[i] = array;
		const Float* data = array->getStorage (deleteIt);
		sort.sortKey (data, TpFloat, 0, getOrder(key));
		array->freeStorage (data, deleteIt);
	    }
	    break;
	case TpDouble:
	    {
		Array<Double>* array = new Array<Double>
                  (key.node().getColumnDouble(rownrs_p));
		arrays[i] = array;
		const Double* data = array->getStorage (deleteIt);
		sort.sortKey (data, TpDouble, 0, getOrder(key));
		array->freeStorage (data, deleteIt);
	    }
	    break;
	case TpComplex:
	    {
		Array<Complex>* array = new Array<Complex>
                  (key.node().getColumnComplex(rownrs_p));
		arrays[i] = array;
		const Complex* data = array->getStorage (deleteIt);
		sort.sortKey (data, TpComplex, 0, getOrder(key));
		array->freeStorage (data, deleteIt);
	    }
	    break;
	case TpDComplex:
	    {
		Array<DComplex>* array = new Array<DComplex>
                  (key.node().getColumnDComplex(rownrs_p));
		arrays[i] = array;
		const DComplex* data = array->getStorage (deleteIt);
		sort.sortKey (data, TpDComplex, 0, getOrder(key));
		array->freeStorage (data, deleteIt);
	    }
	    break;
	case TpString:
	    {
		Array<String>* array = new Array<String>
                  (key.node().getColumnString(rownrs_p));
		arrays[i] = array;
		const String* data = array->getStorage (deleteIt);
		sort.sortKey (data, TpString, 0, getOrder(key));
		array->freeStorage (data, deleteIt);
	    }
	    break;
	default:
	    AlwaysAssert (False, AipsError);
	}
    }
    uInt nrrow = rownrs_p.size();
    Vector<uInt> newRownrs (nrrow);
    int sortOpt = Sort::HeapSort;                  
    if (noDupl_p) {
	sortOpt += Sort::NoDuplicates;
    }
    sort.sort (newRownrs, nrrow, sortOpt);
    for (i=0; i<nrkey; i++) {
	const TableParseSort& key = sort_p[i];
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
    if (showTimings) {
      timer.show ("  Orderby     ");
    }
    // Convert index to rownr.
    for (uInt i=0; i<newRownrs.size(); ++i) {
      newRownrs[i] = rownrs_p[newRownrs[i]];
    }
    rownrs_p.reference (newRownrs);
}


void TableParseSelect::doLimOff (Bool showTimings)
{
  Timer timer;
  Vector<uInt> newRownrs;
  if (offset_p < rownrs_p.size()) {
    uInt nrleft = rownrs_p.size() - offset_p;
    if (limit_p > 0  &&  limit_p < nrleft) {
      nrleft = limit_p;
    }
    newRownrs.reference (rownrs_p(Slice(offset_p, nrleft)).copy());
  }
  rownrs_p.reference (newRownrs);
  if (showTimings) {
    timer.show ("  Limit/Offset");
  }
}


Table TableParseSelect::doProject (Bool showTimings, const Table& table)
{
  Timer timer;
  Table tabp;
  if (nrSelExprUsed_p > 0) {
    // Expressions used, so make a real table.
    tabp = doProjectExpr();
  } else {
    // Only column names used, so make a reference table.
    tabp = table(rownrs_p);
    tabp = tabp.project (columnOldNames_p);
    for (uInt i=0; i<columnNames_p.nelements(); i++) {
      // Rename column if new name is given to a column.
      if (columnNames_p[i] != columnOldNames_p[i]) {
	tabp.renameColumn (columnNames_p[i], columnOldNames_p[i]);
      }
    }
  }
  if (showTimings) {
    timer.show ("  Projection  ");
  }
  if (distinct_p) {
    tabp = doDistinct (showTimings, tabp);
  }
  return tabp;
}

Table TableParseSelect::doProjectExpr()
{
  // Make a column description for all expressions.
  TableDesc td;
  for (uInt i=0; i<columnExpr_p.nelements(); i++) {
    // If no new name is given, make one (unique).
    String newName = columnNames_p[i];
    if (newName.empty()) {
      String nm = "Col_" + String::toString(i+1);
      Int seqnr = 0;
      newName = nm;
      Bool unique = False;
      while (!unique) {
	unique = True;
	for (uInt i=0; i<columnNames_p.nelements(); i++) {
	  if (newName == columnNames_p[i]) {
	    unique = False;
	    seqnr++;
	    newName = nm + "_" + String::toString(seqnr);
	    break;
	  }
	}
      }
      columnNames_p[i] = newName;
    }
    DataType dtype = makeDataType (columnExpr_p[i].dataType(),
				   columnDtypes_p[i], columnNames_p[i]);
    addColumnDesc (td, dtype, newName, 0,
		   columnExpr_p[i].isScalar() ? -1:0,    //ndim
		   IPosition(), "", "", "",
		   columnExpr_p[i].unit().getName());
  }
  // Create the table.
  // The types are defined in class TaQLGivingNodeRep.
  Table::TableType    ttype = Table::Plain;
  Table::TableOption  topt  = Table::New;
  if (resultName_p.empty()) {
    topt = Table::Scratch;
  }
  Table::EndianFormat tendf = Table::AipsrcEndian;
  if (resultType_p == 1) {
    ttype = Table::Memory;
  } else if (resultType_p == 3) {
    tendf = Table::BigEndian;
  } else if (resultType_p == 4) {
    tendf = Table::LittleEndian;
  } else if (resultType_p == 5) {
    tendf = Table::LocalEndian;
  }
  SetupNewTable newtab(resultName_p, td, topt);
  Table tabp(newtab, ttype, rownrs_p.size(), False, tendf);
  // Turn the expressions into update objects.
  for (uInt i=0; i<columnExpr_p.nelements(); i++) {
    if (! columnExpr_p[i].isNull()) {
      addUpdate (new TableParseUpdate (columnNames_p[i], columnExpr_p[i]));
    }
  }
  // Fill the columns in the table.
  doUpdate (False, Table(), tabp, rownrs_p);
  tabp.flush();
  // Indicate that no table needs to be created anymore.
  resultName_p = "";
  return tabp;
}

Table TableParseSelect::doFinish (Bool showTimings, Table& table)
{
  Timer timer;
  Table result;
  if (resultType_p == 1) {
    result = table.copyToMemoryTable (resultName_p);
  } else if (resultType_p > 0){
    Table::EndianFormat tendf = Table::AipsrcEndian;
    if (resultType_p == 3) {
      tendf = Table::BigEndian;
    } else if (resultType_p == 4) {
      tendf = Table::LittleEndian;
    } else if (resultType_p == 5) {
      tendf = Table::LocalEndian;
    }
    table.deepCopy (resultName_p, Table::New, True, tendf);
    result = Table(resultName_p);
  } else {
    // Normal reference table.
    table.rename (resultName_p, Table::New);
    table.flush();
    result = table;
  }
  if (showTimings) {
    timer.show ("  Giving      ");
  }
  return result;
}

DataType TableParseSelect::makeDataType (DataType dtype, const String& dtstr,
					 const String& colName)
{
  if (! dtstr.empty()) {
    if (dtstr == "B") {
      if (dtype != TpOther  &&  dtype != TpBool) {
	throw TableInvExpr ("Expression of column " + colName +
			    " does not have data type Bool");
      }
      return TpBool;
    }
    if (dtstr == "S") {
      if (dtype != TpOther  &&  dtype != TpString) {
	throw TableInvExpr ("Expression of column " + colName +
			    " does not have data type String");
      }
      return TpString;
    }
    if (dtype == TpBool  ||  dtype == TpString) {
      throw TableInvExpr ("Expression of column " + colName +
			  " does not have a numeric data type");
    }
    // Any numeric data type can be converted to Complex.
    if (dtstr == "C4") {
      return TpComplex;
    } else if (dtstr == "C8") {
      return TpDComplex;
    }
    // Real numeric data types cannot have a complex value.
    if (dtype == TpComplex  ||  dtype == TpDComplex) {
      throw TableInvExpr ("Expression of column " + colName +
			  " does not have a real numeric data type");
    }
    if (dtstr == "U1") {
      return TpUChar;
    } else if (dtstr == "I2") {
      return TpShort;
    } else if (dtstr == "U2") {
      return TpUShort;
    } else if (dtstr == "I4") {
      return TpInt;
    } else if (dtstr == "U4") {
      return TpUInt;
    } else if (dtstr == "R4") {
      return TpFloat;
    } else if (dtstr == "R8") {
      return TpDouble;
    }
    throw TableInvExpr ("Datatype " + dtstr + " of column " + colName +
			" is invalid");
  }
  if (dtype == TpOther) {
    throw TableInvExpr ("Datatype " + dtstr + " of column " + colName +
			" is invalid");
  }
  return dtype;
}

void TableParseSelect::addColumnDesc (TableDesc& td,
				      DataType dtype,
				      const String& colName,
				      Int options,
				      Int ndim, const IPosition& shape,
				      const String& dmType,
				      const String& dmGroup,
				      const String& comment,
				      const String& unitName)
{
  if (ndim < 0) {
    switch (dtype) {
    case TpBool:
      td.addColumn (ScalarColumnDesc<Bool> (colName, comment,
					    dmType, dmGroup, options));
      break;
    case TpUChar:
      td.addColumn (ScalarColumnDesc<uChar> (colName, comment,
					     dmType, dmGroup, 0, options));
      break;
    case TpShort:
      td.addColumn (ScalarColumnDesc<Short> (colName, comment,
					     dmType, dmGroup, 0, options));
      break;
    case TpUShort:
      td.addColumn (ScalarColumnDesc<uShort> (colName, comment,
					      dmType, dmGroup, 0, options));
      break;
    case TpInt:
      td.addColumn (ScalarColumnDesc<Int> (colName, comment,
					   dmType, dmGroup, 0, options));
      break;
    case TpUInt:
      td.addColumn (ScalarColumnDesc<uInt> (colName, comment,
					    dmType, dmGroup, 0, options));
      break;
    case TpFloat:
      td.addColumn (ScalarColumnDesc<Float> (colName, comment,
					     dmType, dmGroup, options));
      break;
    case TpDouble:
      td.addColumn (ScalarColumnDesc<Double> (colName, comment,
					      dmType, dmGroup, options));
      break;
    case TpComplex:
      td.addColumn (ScalarColumnDesc<Complex> (colName, comment,
					       dmType, dmGroup, options));
      break;
    case TpDComplex:
      td.addColumn (ScalarColumnDesc<DComplex> (colName, comment,
						dmType, dmGroup, options));
      break;
    case TpString:
      td.addColumn (ScalarColumnDesc<String> (colName, comment,
					      dmType, dmGroup, options));
      break;
    default:
      AlwaysAssert (False, AipsError);
    }
  } else {
    // Giving a shape means fixed shape arrays.
    if (shape.nelements() > 0) {
      options |= ColumnDesc::FixedShape;
    }
    switch (dtype) {
    case TpBool:
      td.addColumn (ArrayColumnDesc<Bool> (colName, comment,
					   dmType, dmGroup,
					   shape, options, ndim));
      break;
    case TpUChar:
      td.addColumn (ArrayColumnDesc<uChar> (colName, comment,
					    dmType, dmGroup,
					    shape, options, ndim));
      break;
    case TpShort:
      td.addColumn (ArrayColumnDesc<Short> (colName, comment,
					    dmType, dmGroup,
					    shape, options, ndim));
      break;
    case TpUShort:
      td.addColumn (ArrayColumnDesc<uShort> (colName, comment,
					     dmType, dmGroup,
					     shape, options, ndim));
      break;
    case TpInt:
      td.addColumn (ArrayColumnDesc<Int> (colName, comment,
					  dmType, dmGroup,
					  shape, options, ndim));
      break;
    case TpUInt:
      td.addColumn (ArrayColumnDesc<uInt> (colName, comment,
					   dmType, dmGroup,
					   shape, options, ndim));
      break;
    case TpFloat:
      td.addColumn (ArrayColumnDesc<Float> (colName, comment,
					    dmType, dmGroup,
					    shape, options, ndim));
      break;
    case TpDouble:
      td.addColumn (ArrayColumnDesc<Double> (colName, comment,
					     dmType, dmGroup,
					     shape, options, ndim));
      break;
    case TpComplex:
      td.addColumn (ArrayColumnDesc<Complex> (colName, comment,
					      dmType, dmGroup,
					      shape, options, ndim));
      break;
    case TpDComplex:
      td.addColumn (ArrayColumnDesc<DComplex> (colName, comment,
					       dmType, dmGroup,
					       shape, options, ndim));
      break;
    case TpString:
      td.addColumn (ArrayColumnDesc<String> (colName, comment,
					     dmType, dmGroup,
					     shape, options, ndim));
      break;
    default:
      AlwaysAssert (False, AipsError);
    }
  }
  // Write unit in column keywords (in TableMeasures compatible way).
  // Check if it is valid by constructing the Unit object.
  if (! unitName.empty()) {
    Unit unit(unitName);
    ColumnDesc& cd = td.rwColumnDesc(colName);
    cd.rwKeywordSet().define ("QuantumUnits",
			      Vector<String>(1, unit.getName()));
  }
}

Table TableParseSelect::doDistinct (Bool showTimings, const Table& table)
{
  Timer timer;
  Table result;
  // Sort the table uniquely on all columns.
  Table tabs = table.sort (columnNames_p, Sort::Ascending,
                           Sort::QuickSort|Sort::NoDuplicates);
  if (tabs.nrow() == table.nrow()) {
    // Everything was already unique.
    result = table;
  } else {
    // Get the rownumbers.
    // Make sure it does not reference an internal array.
    Vector<uInt> rownrs(tabs.rowNumbers(table));
    rownrs.unique();
    // Put the rownumbers back in the original order.
    GenSort<uInt>::sort (rownrs);
    result = table(rownrs);
    rownrs_p.reference (rownrs);
  }
  if (showTimings) {
    timer.show ("  Distinct    ");
  }
  return result;
}


//# Keep the name of the resulting table.
void TableParseSelect::handleGiving (const String& name, Int type)
{
  resultName_p = name;
  resultType_p = type;
}
//# Keep the resulting set expression.
void TableParseSelect::handleGiving (const TableExprNodeSet& set)
{
  resultSet_p = new TableExprNodeSet (set);
}


//# Execute all parts of a TaQL command doing some selection.
void TableParseSelect::execute (Bool showTimings, Bool setInGiving,
				Bool mustSelect, uInt maxRow)
{
  //# A selection query consists of:
  //#  - SELECT to do projection
  //#     can only refer to columns in FROM
  //#     can contain aggregate functions
  //#  - FROM to tell the tables to use
  //#  - WHERE to select rows from tables
  //#     can only refer to columns in FROM
  //#  - GROUPBY to group result of WHERE
  //#     can refer to columns in SELECT, FROM or expressions of FROM
  //#     (in SQL92 only to columns in FROM), thus look in FROM first
  //#  - HAVING to select groups
  //#     aggregate functions of FROM
  //#     or aggregate column in SELECT
  //#     HAVING is possible without GROUPBY (-> one group only)
  //#  - apply combination (UNION, INTERSECTION, DIFFERENCE)
  //#     must have equal SELECT result (with equal column names)
  //#  - ORDERBY to sort result of HAVING
  //#     can refer to columns in SELECT, FROM, or expressions of FROM
  //#      not FROM if combination used
  //#     (in SQL92 only to columns in SELECT), thus look in SELECT first
  //#  - LIMIT to skip latest results of ORDERBY
  //#  - OFFSET to ignore first results of ORDERBY
  //# If GROUPBY is given, SELECT can only contain aggregate or GROUPBY columns
  //# Plan:
  //#  - make columnlist of SELECT columns to make them known for parser
  //#     thus there is a FROM and a SELECT columnlist (as TableDesc?)
  //#  - execute WHERE and make reftable
  //#  - if aggregates
  //#      make temptable if GROUPBY or HAVING has non-FROM columns
  //#      tableiter over GROUPBY columns (if any) and
  //#       make temptable of aggregates in SELECT and possibly HAVING
  //#        get expressions underneath the aggregate
  //#      apply HAVING if needed
  //#      apply SELECT
  //#    else
  //#      apply SELECT
  //#  - order result
  //#  - apply limit/offset
  //# Note:
  //# - if only COUNT(*) and no GROUPBY given, shortcut (result is nrows)
  //#     there can be a HAVING though
  //# - error if non-aggregate in SELECT if GROUPBY given

  //# Set limit if not given.
  if (limit_p == 0) {
    limit_p = maxRow;
  }
  //# Give an error if no command part has been given.
  if (mustSelect  &&  commandType_p == PSELECT
  &&  node_p.isNull()  &&  sort_p.size() == 0
  &&  columnNames_p.nelements() == 0  &&  resultSet_p == 0
  &&  limit_p <= 0  &&  offset_p <= 0) {
    throw (TableError
	   ("TableParse error: no projection, selection, sorting, "
	    "limit, offset, or giving-set given in SELECT command"));
  }
  // Test if a "giving set" is possible.
  if (resultSet_p != 0  &&  !setInGiving) {
    throw TableInvExpr ("A query in a FROM can only have "
			"'GIVING tablename'");
  }
  //# The first table in the list is the source table.
  Table table = fromTables_p[0].table();
  //# Check if all selected columns exist.
  for (uInt i=0; i<columnNames_p.nelements(); i++) {
    String nm = columnNames_p[i];        // name in update,insert
    if (columnOldNames_p.nelements() > 0) {
      nm = columnOldNames_p[i];          // possible name in select
    }
    if (! nm.empty()) {
      if (! table.tableDesc().isColumn (nm)) {
	throw (TableError ("TableParse: projected column " +
			   nm +
			   " does not exist in table " +
			   table.tableName()));
      }
    }
  }
  //# Determine if we can pre-empt the selection loop.
  //# That is possible if a limit is given without sorting.
  uInt nrmax=0;
  if (sort_p.size() == 0  &&  limit_p > 0) {
    nrmax = limit_p + offset_p;
  }
  //# First do the where selection.
  Table resultTable(table);
  if (! node_p.isNull()) {
//#//	cout << "Showing TableExprRange values ..." << endl;
//#//	Block<TableExprRange> rang;
//#//	node_p->ranges(rang);
//#//	for (Int i=0; i<rang.nelements(); i++) {
//#//	    cout << rang[i].getColumn().columnDesc().name() << rang[i].start()
//#//		 << rang[i].end() << endl;
//#//	}
    Timer timer;
    resultTable = table(node_p, nrmax);
    if (showTimings) {
      timer.show ("  Where       ");
    }
  }
  // Get the row numbers of the result of the possible first step.
  rownrs_p.reference (resultTable.rowNumbers(table));
  //# Then do the sort and the limit/offset.
  if (sort_p.size() > 0) {
    doSort (showTimings, table);
  }
  if (offset_p > 0  ||  limit_p > 0) {
    doLimOff (showTimings);
  }
  resultTable = table(rownrs_p);
  //# Then do the update, delete, insert, or projection and so.
  if (commandType_p == PUPDATE) {
    doUpdate (showTimings, table, resultTable, rownrs_p);
    table.flush();
  } else if (commandType_p == PINSERT) {
    Table tabNewRows = doInsert (showTimings, table);
    table.flush();
    resultTable = tabNewRows;
  } else if (commandType_p == PDELETE) {
    doDelete (showTimings, table);
    table.flush();
  } else if (commandType_p == PCOUNT) {
    resultTable = doCount (showTimings, table);
  } else {
    //# Then do the projection.
    if (columnNames_p.nelements() > 0) {
      resultTable = doProject (showTimings, table);
    }
    //# Finally rename or copy using the given name (and flush it).
    if (! resultName_p.empty()) {
      resultTable = doFinish (showTimings, resultTable);
    }
  }
  //# Keep the table for later.
  table_p = resultTable;
}    

void TableParseSelect::show (ostream& os) const
{
  if (! node_p.isNull()) {
    node_p.show (os);
  }
}


//# Simplified forms of general tableCommand function.
TaQLResult tableCommand (const String& str)
{
  Vector<String> cols;
  return tableCommand (str, cols);
}
TaQLResult tableCommand (const String& str, const Table& tempTable)
{
  std::vector<const Table*> tmp(1);
  tmp[0] = &tempTable;
  return tableCommand (str, tmp);
}
TaQLResult tableCommand (const String& str,
			 const std::vector<const Table*>& tempTables)
{
  Vector<String> cols;
  return tableCommand (str, tempTables, cols);
}
TaQLResult tableCommand (const String& str, Vector<String>& cols)
{
  std::vector<const Table*> tmp;
  return tableCommand (str, tmp, cols);
}

TaQLResult tableCommand (const String& str,
			 Vector<String>& cols,
			 String& commandType)
{
  std::vector<const Table*> tmp;
  return tableCommand (str, tmp, cols, commandType);
}

TaQLResult tableCommand (const String& str,
			 const std::vector<const Table*>& tempTables,
			 Vector<String>& cols)
{
  String commandType;
  return tableCommand (str, tempTables, cols, commandType);
}

//# Do the actual parsing of a command and execute it.
TaQLResult tableCommand (const String& str,
			 const std::vector<const Table*>& tempTables,
			 Vector<String>& cols,
			 String& commandType)
{
  commandType = "error";
  // Do the first parse step. It returns a raw parse tree
  // (or throws an exception).
  Timer timer;
  TaQLNode tree = TaQLNode::parse(str);
  // Now process the raw tree and get the final ParseSelect object.
  try {
    TaQLNodeHandler treeHandler;
    TaQLNodeResult res = treeHandler.handleTree (tree, tempTables);
    const TaQLNodeHRValue& hrval = TaQLNodeHandler::getHR(res);
    commandType = hrval.getString();
    TableExprNode expr = hrval.getExpr();
    if (tree.style().doTiming()) {
      timer.show (" Total time   ");
    }
    if (! expr.isNull()) {
      return TaQLResult(expr);                 // result of CALC command
    }
    //# Copy the possibly selected column names.
    if (hrval.getNames()) {
      Vector<String> tmp(*(hrval.getNames()));
      cols.reference (tmp);
    } else {
      cols.resize (0);
    }
    return hrval.getTable();
  } catch (std::exception& x) {
    throw TableParseError ("'" + str + "'\n  " + x.what());
  } 
}

} //# NAMESPACE CASA - END
