//# taql.cc: This program executes TaQL commands
//# Copyright (C) 2009
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
#include <map>
#include <vector>
#include <casa/iostream.h>
#include <casa/iomanip.h>

#ifdef HAVE_READLINE
# include <readline/readline.h>
# include <readline/history.h>
#endif

using namespace casa;
using namespace std;

// <summary>
// Execute table commands from user interface
// </summary>

// Define the type for the map of name to (resulttable,command).
typedef map<String, pair<Table,String> > TableMap;


#ifdef HAVE_READLINE
bool readLine (string& line, const string& prompt, bool addToHistory)
{
  char* str = readline(prompt.c_str());
  if (!str) return false;
  line = string(str);
  free(str);
  if (addToHistory) add_history (line.c_str());
  return true;
}
#else
bool readLine (String& line, const String& prompt, bool)
{
  if (!prompt.empty()) cerr << prompt;
  getline (cin, line);
  return cin;
}
#endif

bool readLineSkip (String& line, const String& prompt,
                   const String& commentChars)
{
  Regex lwhiteRE("^[ \t]*");
  Regex rwhiteRE("[ \t]*$");
  bool fnd = false;
  while (!fnd  &&  readLine (line, prompt, false)) {
    // Skip leading and trailing whitespace.
    line.del (lwhiteRE);
    line.del (rwhiteRE);
    if (! line.empty()) {                            
      // non-empty line.
      if (commentChars.empty()  ||  commentChars.size() > line.size()) {
        // Do not test for comment
        fnd = true;
      } else {
        for (uInt i=0; i<commentChars.size(); ++i) {
          if (commentChars[i] != line[i]) {
            // non-comment
            fnd = true;
            break;
          }
        }
      }
    }
  }
#ifdef HAVE_READLINE
  add_history (line.c_str());
#endif
  return fnd;
}

// Show a date/time. Do not show time part if 0.
void showTime (const MVTime& time)
{
  Double val = time.day();
  if (val == floor(val)) {
    time.print (cout, MVTime::Format
                (MVTime::formatTypes(MVTime::DMY | MVTime::NO_TIME)));
  } else {
    time.print (cout, MVTime::Format(MVTime::DMY, 9));
  }
}

// Show an array of values enclosed in square brackets.
// Omit square brackets if only one value.
template<typename T>
void showArray (const Array<T>& arr)
{
  if (arr.size() == 1) {
    cout << arr.data()[0];
  } else {
    cout << arr;
  }
}
template<> void showArray (const Array<MVTime>& arr)
{
  if (arr.size() == 1) {
    showTime (arr.data()[0]);
  } else {
    Bool firstTime = True;
    cout << '[';
    Array<MVTime>::const_iterator endIter = arr.end();
    for (Array<MVTime>::const_iterator iter= arr.begin();
         iter != endIter; ++iter) {
      if (!firstTime) {
        cout << ", ";
        firstTime = False;
      }
      showTime (*iter);
    }
    cout << ']';
  }
}

// Show the required columns.
// First test if they exist and contain scalars or arrays.
void showTable (const Table& tab, const Vector<String>& colnam)
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
  // Print the array index if possible.
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
      showArray (expr.getColumnBool());
      break;
    case TpUChar:
      showArray (expr.getColumnuChar());
      break;
    case TpShort:
      showArray (expr.getColumnShort());
      break;
    case TpUShort:
      showArray (expr.getColumnuShort());
      break;
    case TpInt:
      showArray (expr.getColumnInt());
      break;
    case TpUInt:
      showArray (expr.getColumnuInt());
      break;
    case TpFloat:
      showArray (expr.getColumnFloat());
      break;
    case TpDouble:
      showArray (expr.getColumnDouble());
      break;
    case TpComplex:
      showArray (expr.getColumnComplex());
      break;
    case TpDComplex:
      showArray (expr.getColumnDComplex());
      break;
    case TpString:
      showArray (expr.getColumnString());
      break;
    default:
      if (expr.getNodeRep()->dataType() == TableExprNodeRep::NTDate) {
        MVTime time;
        if (expr.nrow() != 1) cout << '[';
        for (uInt i=0; i<expr.nrow(); i++) {
          if (i > 0) cout << ", ";
          expr.get (i, time);
          showTime (time);
        }
        if (expr.nrow() != 1) cout << ']';
      } else {
        cout << "Unknown expression scalar type " << expr.getColumnDataType();
      }
    }
    cout << endl;
  } else {
    for (uInt i=0; i<expr.nrow(); i++) {
      if (expr.nrow() > 1) {
        cout << "  row " << i << ":  ";
      }
      switch (expr.dataType()) {
      case TpBool:
        showArray (expr.getArrayBool(i));
        break;
      case TpInt:
        showArray (expr.getArrayInt(i));
        break;
      case TpDouble:
        showArray (expr.getArrayDouble(i));
        break;
      case TpDComplex:
        showArray (expr.getArrayDComplex(i));
        break;
      case TpString:
        showArray (expr.getArrayString(i));
        break;
      default:
        if (expr.getNodeRep()->dataType() == TableExprNodeRep::NTDate) {
          Array<MVTime> arr;
          expr.get (i, arr);
          showArray (arr);
        } else {
          cout << "Unknown expression array type " << expr.dataType();
        }
      }
      cout << endl;
    }
  }
}


// Sort and select data.
Table doCommand (const String& str, const vector<const Table*>& tempTables)
{
  // If no command is given, assume it is CALC.
  // Only show results for SELECT and CALC.
  String::size_type spos = str.find_first_not_of (' ');
  Bool addCalc = False;
  Bool showResult = False;
  if (spos != String::npos) {
    String::size_type epos = str.find (' ', spos);
    if (epos == String::npos) {
      // single word (e.g. 1+2), so cannot contain a command name
      addCalc = True;
    } else {
      String s = str.substr(spos, epos-spos);
      s.downcase();
      addCalc = !(s=="select" || s=="update" || s=="insert" || s=="calc" ||
                  s=="delete" || s=="create" || s=="createtable" ||
                  s=="count"  || s=="using"  || s=="usingstyle");
      showResult = (s=="select" || s=="calc");
    }
    showResult = showResult || addCalc;
  } 
  String strc(str);
  if (addCalc) {
    strc = "CALC " + str;
  }
  Table tabp;
  uInt i;
  Vector<String> colNames;
  String cmd;
  TaQLResult result;
  result = tableCommand (strc, tempTables, colNames, cmd);
  cout << strc << endl;
  cout << "    has been executed" << endl;
  // Set default format for printing datetime.
  if (result.isTable()) {
    tabp = result.table();
    cout << "    " << cmd << " result of " << tabp.nrow()
	 << " rows" << endl;
    if (showResult  &&  colNames.nelements() > 0) {
      // Show the selected column names.
      cout << colNames.nelements() << " selected columns: ";
      for (i=0; i<colNames.nelements(); i++) {
        cout << " " << colNames(i);
      }
      cout << endl;
      // Show the contents of the columns.
      showTable (tabp, colNames);
    }
  } else {
    showExpr (result.node());
  }
  return tabp;
}


void showHelp()
{
  cerr << endl;
  cerr << "TaQL is the query language for casacore tables and is described at"
       << endl;
  cerr << "  http://www.astron.nl/casacore/trunk/casacore/doc/notes/199.html"
       << endl;
  cerr << "Any TaQL command can be given." << endl;
  cerr << "The result of a CALC command and selected columns will be printed."
       << endl;
  cerr << "If no command name is given, CALC is assumed" << endl;
  cerr << "For example:" << endl;
  cerr << "   mean([select EXPOSURE from my.ms])" << endl;
  cerr << "   mjdtodate([select distinct TIME from my.ms])    print as dates"
       << endl;
  cerr << "   date() + 60     which date is 60 days after today" << endl;
  cerr << "For other commands the number of selected rows is printed." << endl;
  cerr << endl;
  cerr << "It is possible to save the table resulting from a selection" << endl;
  cerr << "by assigning it like:" << endl;
  cerr << "   var = taqlcommand" << endl;
  cerr << "Thereafter var can be used as a table in another TaQL command like:"
       << endl;
  cerr << "   t1 = select from my.ms where ANTENNA1=1" << endl;
  cerr << "   t2 = select from $t1 where ANTENNA2=2" << endl;
  cerr << endl;
  cerr << "taql can be started with option -s or --style which defines the TaQL style." << endl;
  cerr << "The default style is python; if no value is given after -s it defaults to glish" << endl;
  cerr << endl;
}

void showTableInfo (const String& name, const Table& tab,
                    const String& command, Int level)
{
  TableDesc tdesc(tab.actualTableDesc());
  cout << "  " << name << " resulted from:" << endl
       << "    " << command << endl;
  cout << "  " << tab.nrow() << " rows, "
       << tdesc.ncolumn() << " columns" << endl;
  if (level > 0) {
    Vector<String> colNames = tdesc.columnNames();
    cout << "    " << colNames << endl;
    if (level > 1) {
      genSort (colNames);
      uInt maxLen = 0;
      for (uInt i=0; i<colNames.size(); ++i) {
        if (colNames[i].size() > maxLen) {
          maxLen = colNames[i].size();
        }
      }
      for (uInt i=0; i<colNames.size(); ++i) {
        const ColumnDesc& cdesc = tdesc[colNames[i]];
        cout << "    " << colNames[i];
        for (uInt j=colNames[i].size(); j<maxLen; ++j) {
          cout << ' ';
        }
        cout << ' ' << ValType::getTypeStr(cdesc.dataType());
        if (cdesc.isScalar()) {
          cout << " scalar";
        } else if (cdesc.isArray()) {
          cout << " array";
          if (cdesc.ndim() > 0) {
            cout << " ndim=" << cdesc.ndim();
          }
          if (! cdesc.shape().empty()) {
            cout << " shape=" << cdesc.shape();
          }
        }
        if (cdesc.comment().empty()) {
          cout << "  " << cdesc.comment();
        }
        cout << endl;
      }
    }
  }
}

String substituteName (const String& name, const TableMap& tables,
                       vector<const Table*>& tabs)
{
  TableMap::const_iterator fnd = tables.find(name);
  if (fnd == tables.end()) {
    return name;
  }
  tabs.push_back (&(fnd->second.first));
  return String::toString (tabs.size());
}

vector<const Table*> replaceVars (String& str, const TableMap& tables)
{
  vector<const Table*> tabs;
  // Initialize some variables.
  Bool backslash = False;
  Bool dollar = False;
  Bool squote = False;
  Bool dquote = False;
  String name;
  String out;
  out.reserve (str.size());
  // Loop through the entire string.
  for (uInt i=0; i<str.size(); ++i) {
    char tmp = str[i];
    // If a dollar was found, we might have a name.
    // Alphabetics and underscore are always part of name.
    if (dollar) {
      if (tmp=='_'  ||  (tmp>='a' && tmp<='z')  ||  (tmp>='A' && tmp<='Z')) {
        name += tmp;
        continue;
      } else if (tmp>='0' && tmp<='9' && !name.empty()) {
        // Numerics are only part if not first character.
        name += tmp;
        continue;
      } else {
        // End of name found. Try to substitute.
        dollar = False;
        out += substituteName(name, tables, tabs);
      }
    }
    // Handle possible single or double quotes.
    if (tmp == '"'  &&  !squote) {
      dquote = !dquote;
    } else if (tmp == '\''  &&  !dquote) {
      squote = !squote;
    } else if (!dquote && !squote) {
      // Set a switch if we have a dollar (outside quoted)
      // that is not preceeded by a backslash.
      if (tmp == '$'  &&  !backslash) {
        dollar = True;
        name = String();
      }
    }
    // Set a switch if we have a backslash (not preceeded by a backslash).
    backslash = (!backslash && tmp == '\\');
    // Add the character to output.
    out += tmp;
  }
  // The entire string has been handled.
  // Substitute a possible last name.
  // Insert a possible incomplete eval string as such.
  if (dollar) {
    out += substituteName(name, tables, tabs);
  }
  str = out;
  return tabs;
}

// Ask and execute commands till quit or ^D is given.
void askCommands (const String& prefix)
{
  Regex varassRE("^[a-zA-Z_][a-zA-Z0-9_]*[ \t]*=");
  Regex assRE("[ \t]*=");
  Regex whiteRE("^[ \t]*");
  TableMap tables;
  while (True) {
    String str;
    if (! readLineSkip (str, "TaQL> ", "#")) {
      cerr << endl;
      break;
    }
    // Remove leading whitespace.
    str.del (whiteRE);
    if (str.empty()) {
      cerr << "?, h, or help gives help info" << endl;
    } else {
      if (str == "?"  ||  str == "h"  ||  str == "help") {
        showHelp();
      } else if (str == "exit"  ||  str == "quit"  ||  str == "q") {
        break;
      } else {
        try {
          String varName;
          String::size_type assLen = varassRE.match (str.c_str(), str.size());
          if (assLen != String::npos) {
            // Assignment to variable; get its name and remove from command.
            varName = str.before(assLen);
            str = str.from(assLen);
            varName.del (assRE);
            if (varName.empty()) {
              throw AipsError ("Variable name before command is empty");
            }
          }
          str.del (whiteRE);
          if (str.empty()) {
            // No command means that the variable will be removed.
            tables.erase (varName);
          } else {
            // No assignment, so it is a name or a command.
            // First try it as a name.
            // A name can be followed by question marks giving the level of
            // info to be printed.
            Int sz = str.size();
            while (sz > 0  &&  str[sz-1] == '?') {
              --sz;
            }
            Int level = str.size() - sz;
            String name = str.substr(0, sz);
            TableMap::const_iterator it = tables.find (name);
            if (it != tables.end()) {
              // It exists, so it must be a name.
              showTableInfo (name, it->second.first, it->second.second, level);
            } else {
              // No name, so it must be a command.
              // Note that CALC commands can omit CALC.
              String command(str);
              vector<const Table*> tabs = replaceVars (str, tables);
              if (!varName.empty()) {
                cout << varName << " = ";
              }
              cout << command << endl;
              Table tab = doCommand (prefix + str, tabs);
              if (!varName.empty()  &&  !tab.isNull()) {
                // Keep the resulting table if a variable was given.
                tables[varName] = make_pair(tab, command);
              }
            }
          }
        } catch (AipsError& x) {
          cerr << x.getMesg() << endl;
        }
      }
    }
  }
}


int main (int argc, const char* argv[])
{
  try {
    string style = "python";
    int st = 1;
    if (argc > 1) {
      if (string(argv[1]) == "-s"  ||  string(argv[1]) == "--style") {
        style = string();
        ++st;
        if (argc > 2) {
          style = argv[2];
          ++st;
        }
      }
    }
    string prefix;
    if (style.empty()) {
      style = "glish";
    } else {
      prefix = "using style " + style + ' ';
    }
    if (st < argc) {
      // Execute the given command.
      doCommand (prefix + argv[st], vector<const Table*>());
    } else {
    // Ask the user for commands.
      cout << "Using default TaQL style " << style << endl;
      askCommands (prefix);
    }
  } catch (AipsError& x) {
    cerr << "\nCaught an exception: " << x.getMesg() << endl;
    return 1;
  } 
  return 0;               // successfully executed
}
