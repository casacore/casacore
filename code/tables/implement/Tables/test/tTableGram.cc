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
#include <casa/BasicSL/Complex.h>
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


void seltab (const String&);
void docomm ();

int main (int argc, char** argv)
{
  try {
    if (argc > 1) {
      seltab(argv[1]);
    }else{
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
	cout << " " << tableColumns[j]->shape (i);
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
  uInt i;
  Table tab;
  Vector<String> vecstr;
  String cmd;
  cout << str << endl;
  TaQLResult result;
  uInt semipos = str.find(';');
  if (semipos == string::npos) {
    result = tableCommand (str, vecstr, cmd);
  } else {
    String strc(str);
    Table tab(strc.after(semipos));
    std::vector<const Table*> tabblock(1, &tab);
    result = tableCommand (strc.before(semipos), tabblock, vecstr, cmd);
  }
  cout << "    has been executed" << endl;
  if (result.isTable()) {
    tab = result.table();
    cout << "    " << cmd << " of " << tab.nrow() << " rows" << endl;
    // Show the selected column names.
    cout << vecstr.nelements() << " selected columns: ";
    for (i=0; i<vecstr.nelements(); i++) {
      cout << " " << vecstr(i);
    }
    cout << endl;

    // Show the contents of the columns.
    if (vecstr.nelements() > 0) {
      showtab (tab, vecstr);
    }
  } else {
    showExpr (result.node());
  }
}
