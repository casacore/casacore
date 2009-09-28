//# tTableGram.cc: This program tests table commands using TableGram/Parse
//# Copyright (C) 1994,1995,1996,1998,2000,2002,2003
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

#include <tables/Tables/TableParse.h>
#include <tables/Tables/Table.h>
#include <tables/Tables/TableDesc.h>
#include <tables/Tables/TableColumn.h>
#include <tables/Tables/ExprNodeArray.h>
#include <casa/Arrays/Matrix.h>
#include <casa/Arrays/Vector.h>
#include <casa/Arrays/ArrayIO.h>
#include <casa/BasicSL/Complex.h>
#include <casa/BasicMath/Math.h>
#include <casa/Utilities/Assert.h>
#include <casa/Exceptions/Error.h>
#include <casa/iostream.h>

#include <casa/namespace.h>
// <summary>
// Test program for table commands from user interface
// </summary>

// This program tests table commands with an SQL-like grammar.
// The grammar is scanned and parsed using the flex/bison file TableGram.l/y
// and with the help of the class TableParse.
// It ask for commands until a "q" is given.
// When columns are selected, it will show their contents.

void testUnit (const String& comm, double expResult, const String& expUnit)
{
  TaQLResult result = tableCommand (comm);
  AlwaysAssert (!result.isTable(), AipsError);
  TableExprNode node = result.node();
  if (!near (node.getDouble(0), expResult)
  ||  node.unit().getName() != expUnit) {
    cout << "Error in evaluating: " + comm << endl;
    cout << " expected " << expResult << ' ' << expUnit << endl;
    cout << " found    " << node.getDouble(0) << ' '
	 << node.unit().getName() << endl;
  }
}
void testUnit (const String& comm, Bool expResult)
{
  TaQLResult result = tableCommand (comm);
  AlwaysAssert (!result.isTable(), AipsError);
  TableExprNode node = result.node();
  if (node.getBool(0) != expResult  ||  node.unit().getName() != "") {
    cout << "Error in evaluating: " + comm << endl;
    cout << " expected " << expResult << endl;
    cout << " found    " << node.getBool(0) << ' '
	 << node.unit().getName() << endl;
  }
}

void checkUnits()
{
  testUnit ("calc 100mm", 100., "mm");
  testUnit ("calc 100mm cm", 10., "cm");
  testUnit ("calc (100mm cm)dm", 1., "dm");
  testUnit ("calc sin(90 deg)", 1., "");
  testUnit ("calc asin(1) deg", 90., "deg");
  testUnit ("calc min(1cm, 2mm)", 0.2, "cm");
  testUnit ("calc min(1cm, 2mm)mm", 2., "mm");
  testUnit ("calc min([2mm, 0.1cm])", 1., "mm");
  testUnit ("calc iif(T, 1mm, 2cm)", 1., "mm");
  testUnit ("calc iif(F, 1mm, 2cm)", 20., "mm");
  testUnit ("calc 20mm+3cm", 50., "mm");
  testUnit ("calc 2 cm + 30 'mm'", 5., "cm");
  testUnit ("calc 2km/20s", 0.1, "km/(s)");
  testUnit ("calc 2km/20", 0.1, "km");
  testUnit ("calc 2 'km/s' * 20s", 40., "km/s.s");
  testUnit ("calc 2 'km/h' + 1 'm/s'", 5.6, "km/h");
  testUnit ("calc 2m*3m", 6., "m.m");
  testUnit ("calc sumsqr([3.m,10dm])", 10., "(m)2");
  testUnit ("calc sqrt(9 'm2')", 3., "m");
  testUnit ("calc 20Aug06 - 13Aug06", 7., "d");
  testUnit ("calc 20Aug06 +86400s + 12*60min - 13Aug06", 8.5, "d");
  testUnit ("calc sum([2mm,0.1cm] + [3cm,2mm])", 35, "mm");
  testUnit ("calc 1mm in [2mm,0.1cm]", True);
  testUnit ("calc 0.02dm in [2mm,0.1cm]", True);
  testUnit ("calc 0.025dm in [2mm<:<0.3cm]", True);
  testUnit ("calc 0.02dm in [2mm<:<0.3cm]", False);
  testUnit ("calc !near(2cm,20mm)", False);
  testUnit ("calc 0.002km == 2m", True);
  testUnit ("calc [180deg/pi(),180deg/pi()] incone [2rad,2rad,1rad]", True);
  testUnit ("calc [90deg/pi(),90deg/pi()] incone [2rad,2rad,1rad]", False);
  testUnit ("calc [1rad,1.rad] incone [1rad,1rad,1arcsec]", True);
  testUnit ("calc [1rad,1.0001rad] incone [1rad,1rad,1arcsec]", False);
  testUnit ("calc [1h0m,15d0m] incone [15deg,15deg,1arcsec]", True);
  testUnit ("calc near(4.67312e+09s-3200, mjd('2006/12/18'))", True);
  testUnit ("calc 172800s / 86400", 2., "d");
  testUnit ("calc 172800m / 86400", 2., "m");
}

void seltab (const String&);
void docomm ();

int main (int argc, const char* argv[])
{
  try {
    if (argc > 1) {
      if (String(argv[1]) == "0") {
	// Check the unit handling in TaQL.
	checkUnits();
      } else {
	// Execute the given command.
	seltab(argv[1]);
      }
    } else {
    // Do some interactive tests.
      docomm();
    }
  } catch (AipsError x) {
    cout << "\nCaught an exception: " << x.getMesg() << endl;
    return 1;
  } 
  return 0;               // successfully executed
}


// Ask and execute command till empty string is given.
void docomm()
{
  char comm[1025];
  while (True) {
    cout << "Table command (q=quit): ";
    cin.getline (comm, 1024);
    String str(comm);
    if (str == "q") 
      break;
    try {
      seltab (str);
    } catch (AipsError x) {
      cout << x.getMesg() << endl;
    } 
  }
}


// Show the required columns.
// First test if they exist and contain scalars or arrays.
void showtab (const Table& tab, const Vector<String>& colnam)
{
  uInt nrcol = 0;
  PtrBlock<ROTableColumn*> tableColumns(colnam.nelements());
  uInt i;
  for (i=0; i<colnam.nelements(); i++) {
    if (! tab.tableDesc().isColumn (colnam(i))) {
      cout << "Column " << colnam(i) << " does not exist" << endl;
    }else{
      tableColumns[nrcol] = new ROTableColumn (tab, colnam(i));
      if (! tableColumns[nrcol]->columnDesc().isScalar()
      &&  ! tableColumns[nrcol]->columnDesc().isArray()) {
	cout << "Column " << colnam(i)
	     << " contains scalars nor arrays"
	     << endl;
	delete tableColumns[nrcol];
      }else{
	nrcol++;
      }
    }
  }
  if (nrcol == 0) {
    return;
  }
  
  for (i=0; i<tab.nrow(); i++) {
    for (uInt j=0; j<nrcol; j++) {
      if (tableColumns[j]->columnDesc().isArray()) {
	cout << " shape=" << tableColumns[j]->shape (i);
      }else{
	switch (tableColumns[j]->columnDesc().dataType()) {
	case TpBool:
	  cout << " " << tableColumns[j]->asBool (i);
	  break;
	case TpString:
	  cout << " " << tableColumns[j]->asString (i);
	  break;
	case TpComplex:
	case TpDComplex:
	  cout << " " << tableColumns[j]->asDComplex (i);
	  break;
	default:
	  cout << " " << tableColumns[j]->asdouble (i);
	}
      }
    }
    cout << endl;
  }
  
  for (i=0; i<nrcol; i++) {
    delete tableColumns[i];
  }
}


void showExpr(const TableExprNode& expr)
{
  // Print the index if possible.
  // Get internal node.
  const TableExprNodeArrayPart* nodePtr =
               dynamic_cast<const TableExprNodeArrayPart*>(expr.getNodeRep());
  if (nodePtr != 0) {
    // The node represents a part of an array; get its index node.
    const TableExprNodeIndex* inxNode = nodePtr->getIndexNode();
    // If a constant index accessing a single element,
    // get the Slicer defining the index.
    if (inxNode->isConstant()  &&  inxNode->isSingle()) {
      const Slicer& indices = inxNode->getConstantSlicer();
      // Extract the index from it.
      cout << "Index: " << indices.start() << endl;
    }
  }
  const Unit& unit = expr.unit();
  if (! unit.empty()) {
    cout << "Unit: " << unit.getName() << endl;
  }
  if (expr.isScalar()) {
    switch (expr.getColumnDataType()) {
    case TpBool:
      cout << expr.getColumnBool();
      break;
    case TpUChar:
      cout << expr.getColumnuChar();
      break;
    case TpShort:
      cout << expr.getColumnShort();
      break;
    case TpUShort:
      cout << expr.getColumnuShort();
      break;
    case TpInt:
      cout << expr.getColumnInt();
      break;
    case TpUInt:
      cout << expr.getColumnuInt();
      break;
    case TpFloat:
      cout << expr.getColumnFloat();
      break;
    case TpDouble:
      cout << expr.getColumnDouble();
      break;
    case TpComplex:
      cout << expr.getColumnComplex();
      break;
    case TpDComplex:
      cout << expr.getColumnDComplex();
      break;
    case TpString:
      cout << expr.getColumnString();
      break;
    default:
      cout << "Unknown expression scalar type " << expr.getColumnDataType();
    }
    cout << endl;
  } else {
    for (uInt i=0; i<expr.nrow(); i++) {
      cout << "  row " << i << ":  ";
      switch (expr.dataType()) {
      case TpBool:
	{
	  Array<Bool> arr;
	  expr.get (i, arr);
	  cout << arr;
	  break;
	}
      case TpDouble:
	{
	  Array<Double> arr;
	  expr.get (i, arr);
	  cout << arr;
	  break;
	}
      case TpDComplex:
	{
	  Array<DComplex> arr;
	  expr.get (i, arr);
	  cout << arr;
	  break;
	}
      case TpString:
	{
	  Array<String> arr;
	  expr.get (i, arr);
	  cout << arr;
	  break;
	}
      default:
	cout << "Unknown expression array type " << expr.dataType();
      }
      cout << endl;
    }
  }
}


// Sort and select data.
void seltab (const String& str)
{
  // If no command is given, assume it is CALC.
  String::size_type spos = str.find_first_not_of (' ');
  Bool addCalc = False;
  String s;
  if (spos != String::npos) {
    String::size_type epos = str.find (' ', spos);
    if (epos == String::npos) {
      addCalc = True;
    } else {
      s = str.substr(spos, epos-spos);
      s.downcase();
      addCalc = !(s=="select" || s=="update" || s=="insert" || s=="calc" ||
                  s=="delete" || s=="create" || s=="createtable" ||
                  s=="count"  || s=="using"  || s=="usingstyle");
    }
  } 
  String strc(str);
  if (addCalc) {
    strc = "CALC " + str;
  }
  cout << strc << endl;
  // Parse and execute the command.
  TaQLResult result;
  Table* tabp = 0;
  uInt i;
  Vector<String> vecstr;
  String cmd;
  // A semicolon can be used to specify a possible table after it (for $1).
  String::size_type semipos = strc.find(';');
  if (semipos == String::npos) {
    result = tableCommand (strc, vecstr, cmd);
  } else {
    Table tab(strc.after(semipos));
    std::vector<const Table*> tabblock(1, &tab);
    result = tableCommand (strc.before(semipos), tabblock, vecstr, cmd);
  }
  cout << "    has been executed" << endl;
  if (result.isTable()) {
    tabp = new Table(result.table());
    cout << "    " << cmd << " result of " << tabp->nrow()
	 << " rows" << endl;
    // Show the selected column names.
    // Add _COUNT_ column if counting is done.
    if (s == "count") {
      uInt nrcol = vecstr.size();
      vecstr.resize (nrcol+1, True);
      vecstr[nrcol] = "_COUNT_";
    }
    cout << vecstr.nelements() << " selected columns: ";
    for (i=0; i<vecstr.nelements(); i++) {
      cout << " " << vecstr(i);
    }
    cout << endl;

    // Show the contents of the columns.
    if (vecstr.nelements() > 0) {
      showtab (*tabp, vecstr);
    }
  } else {
    showExpr (result.node());
  }
  delete tabp;
}
