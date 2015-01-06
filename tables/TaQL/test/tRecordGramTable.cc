//# tRecordGram.cc: Test program for the expression grammar on a table
//# Copyright (C) 2004
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

#include <casacore/tables/TaQL/RecordGram.h>
#include <casacore/tables/TaQL/ExprNode.h>
#include <casacore/tables/Tables/Table.h>
#include <casacore/casa/Arrays/Array.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/Arrays/ArrayIO.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/iostream.h>

#include <casacore/casa/namespace.h>

// <summary>
// Test program for the expression grammar on a table.
// </summary>

// This program tests the class RecordGram to do expressions on a table.


void doIt (const String& str)
{
  String name = "$testsrcdir/../../Tables/test/tTable_2.data_v0";
  Table tab(name);
  TableExprNode expr = RecordGram::parse (tab, str);
  cout << str << ": ";
  if (expr.isScalar()) {
    Vector<uInt> rownrs(expr.nrow());
    indgen (rownrs);
    switch (expr.getColumnDataType()) {
    case TpBool:
      cout << expr.getColumnBool (rownrs);
      break;
    case TpUChar:
      cout << expr.getColumnuChar (rownrs);
      break;
    case TpShort:
      cout << expr.getColumnShort (rownrs);
      break;
    case TpUShort:
      cout << expr.getColumnuShort (rownrs);
      break;
    case TpInt:
      cout << expr.getColumnInt (rownrs);
      break;
    case TpUInt:
      cout << expr.getColumnuInt (rownrs);
      break;
    case TpFloat:
      cout << expr.getColumnFloat (rownrs);
      break;
    case TpDouble:
      cout << expr.getColumnDouble (rownrs);
      break;
    case TpComplex:
      cout << expr.getColumnComplex (rownrs);
      break;
    case TpDComplex:
      cout << expr.getColumnDComplex (rownrs);
      break;
    case TpString:
      cout << expr.getColumnString (rownrs);
      break;
    default:
      cout << "Unknown expression scalar type " << expr.getColumnDataType();
    }
    cout << endl;
  } else {
    for (uInt i=0; i<tab.nrow(); i++) {
      cout << "  row " << i << ":" << endl;
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
    }
  }
}

// Ask and execute command till empty string is given.
void docomm()
{
  char comm[1025];
  while (True) {
    cout << "Table command (q=quit): ";
    cin.getline (comm, 1024);
    String str(comm);
    if (str.empty()  ||  str == "q") 
      break;
    try {
      doIt (str);
    } catch (AipsError& x) {
      cout << x.getMesg() << endl;
    } 
  }
}

int main (int argc, const char* argv[])
{
  if (argc < 2) {
    docomm();
    return 0;
  }
  try {
    doIt(argv[1]);
  } catch (AipsError x) {
    cout << "Unexpected exception: " << x.getMesg() << endl;
    return 1;
  } catch (...) {
    cout << "Unexpected unknown exception" << endl;
    return 1;
  }
  return 0;
}
