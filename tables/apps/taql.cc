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

#include <casacore/tables/TaQL/TableParse.h>
#include <casacore/tables/Tables/Table.h>
#include <casacore/tables/Tables/TableProxy.h>
#include <casacore/tables/Tables/TableRecord.h>
#include <casacore/tables/Tables/TableDesc.h>
#include <casacore/tables/Tables/TableColumn.h>
#include <casacore/tables/TaQL/ExprNodeArray.h>
#include <casacore/casa/Containers/ValueHolder.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Arrays/ArrayIO.h>
#include <casacore/casa/Quanta/MVPosition.h>
#include <casacore/casa/Quanta/MVAngle.h>
#include <casacore/casa/BasicSL/Complex.h>
#include <casacore/casa/BasicMath/Math.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/OS/EnvVar.h>
#include <casacore/casa/Exceptions/Error.h>
#include <map>
#include <vector>
#include <fstream>
#include <casacore/casa/iostream.h>
#include <casacore/casa/iomanip.h>

#ifdef HAVE_READLINE
# include <readline/readline.h>
# include <readline/history.h>
#endif

using namespace casacore;
using namespace std;

// <summary>
// Execute table commands from user interface
// </summary>

// Define the type for the map of name to (resulttable,command).
typedef map<String, pair<Table,String> > TableMap;

void removeCR (String& line)
{
  // Remove possible carriage-return (in DOS files).
  if (line.size() > 0  &&  line[line.size() - 1] == '\r') {
    line = line.substr(0, line.size()-1);
  }
}

uInt lskipws (const String& value, uInt st, uInt end)
{
  for (; st<end && isspace(value[st]); ++st)
    ;
  return st;
}
  
uInt rskipws (const String& value, uInt st, uInt end)
{
  for (; end>st && isspace(value[end-1]); --end)
    ;
  return end;
}

uInt skipQuoted (const String& str, uInt st, uInt end)
{
  // Skip until the matching end quote is found.
  char ch = str[st++];
  for (; st<end; ++st) {
    if (str[st] == ch) return st+1;
    if (str[st] == '\\') st++;   // skip escaped char
  }
  throw AipsError ("Unbalanced quoted string at position "
                   + String::toString(st) + " in " + str);
}

// Split a line using ; as separator.
// Skip comments indicated by #.
// Ignore those characters if in a quoted string.
// An empty string is added where a separator is used.
// In this way it is clear if the first part of the next line
// has to be added to the last part of this line.
vector<String> splitLine (const String& line)
{
  vector<String> parts;
  // Skip leading and trailing whitespace.
  uInt st = lskipws (line, 0, line.size());
  if (!line.empty()  &&  line[st] != '#') {         // skip if only comment
    uInt end = rskipws (line, st, line.size());
    uInt stcmd = st;                   // first non-blank character
    while (st<end) {
      if (line[st] == '"'  ||  line[st] == '\'') {
        st = skipQuoted (line, st, end);
      } else if (line[st] == '#') {
        end = rskipws(line, stcmd, st);     // A comment ends the line
      } else if (line[st] == ';') {
        // Save the command.
        uInt endcmd = rskipws(line, stcmd, st);
        if (stcmd < endcmd) {
          parts.push_back (line.substr(stcmd, endcmd-stcmd));
          parts.push_back (string());
        }
        st = lskipws (line, st+1, end);
        stcmd = st;
      } else {
        st++;
      }
    }
    // Handle possible last command.
    if (stcmd < end) {
      uInt endcmd = rskipws(line, stcmd, st);
      if (stcmd < endcmd) {
        parts.push_back (line.substr(stcmd, endcmd-stcmd));
      }
    }
  }
  return parts;
}


#ifdef HAVE_READLINE
bool readLine (String& line, const string& prompt)
{
  char* str = readline(prompt.c_str());
  if (!str) return false;
  line = string(str);
  removeCR (line);
  free(str);
  return true;
}
#else
bool readLine (String& line, const String& prompt)
{
  if (!prompt.empty()) cerr << prompt;
  getline (cin, line);
  removeCR (line);
  return cin.good();
}
#endif

bool readLineSkip (String& line, const String& prompt)
{
  bool fnd = false;
  while (!fnd  &&  readLine (line, prompt)) {
    vector<String> parts = splitLine(line);
    // Remove last part if empty.
    if (! parts.empty()  &&  parts[parts.size()-1].empty()) {
      parts.resize (parts.size() - 1);
    }
    if (parts.size() > 1) {
      cerr << "A single TaQL command must be given" << endl;
    } else if (! parts.empty()) {                            
      line = parts[0];
      fnd  = true;
    }
  }
#ifdef HAVE_READLINE
  if (fnd) add_history (line.c_str());
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

void showTime (double time, const String& unit)
{
  showTime (MVTime (Quantity(time, unit)));
}

void showTime (const Array<double>& times, const String& unit)
{
  Quantity q(0., unit);
  Bool firstTime = True;
  cout << '[';
  Array<double>::const_iterator endIter = times.end();
  for (Array<double>::const_iterator iter= times.begin();
       iter != endIter; ++iter) {
    if (!firstTime) {
      cout << ", ";
    } else {
      firstTime = False;
    }
    q.setValue (*iter);
    showTime (q);
  }
  cout << ']';
}

void showPos (const Array<double>& pos, const Vector<String>& units)
{
  AlwaysAssert (pos.size() % units.size() == 0, AipsError);
  Vector<Quantity> q(units.size());
  for (uInt i=0; i< units.size(); ++i) {
    q[i] = Quantity(0., units[i]);
  }
  Bool firstTime = True;
  if (pos.size() != units.size()) {
    cout << '[';
  }
  Array<double>::const_iterator endIter = pos.end();
  for (Array<double>::const_iterator iter= pos.begin(); iter != endIter;) {
    if (!firstTime) {
      cout << ", ";
    } else {
      firstTime = False;
    }
    for (uInt i=0; i<units.size(); ++i) {
      q[i].setValue (*iter);
      iter++;
    }
    MVPosition pos(q);
    cout << q;
  }
  if (pos.size() != units.size()) {
    cout << ']';
  }
}

void showDir (const Array<double>& dir, const Vector<String>& units)
{
  AlwaysAssert (dir.size() % units.size() == 0, AipsError);
  Vector<Quantity> q(units.size());
  for (uInt i=0; i< units.size(); ++i) {
    q[i] = Quantity(0., units[i]);
  }
  Bool firstTime = True;
  if (dir.size() != units.size()) {
    cout << '[';
  }
  Array<double>::const_iterator endIter = dir.end();
  for (Array<double>::const_iterator iter= dir.begin(); iter != endIter;) {
    if (!firstTime) {
      cout << ", ";
    } else {
      firstTime = False;
    }
    cout << '[';
    for (uInt i=0; i<units.size(); ++i) {
      q[i].setValue (*iter);
      MVAngle angle(q[i]);
      if (i == 0)  {
        ostringstream ostr;
        angle.print (ostr, MVAngle::Format(MVAngle::TIME, 9));
        string str(ostr.str());
        string::size_type pos = str.find(':');
        if (pos != string::npos) str[pos] = 'h';
        pos = str.find(':');
        if (pos != string::npos) str[pos] = 'm';
        cout << str;
      } else {
        cout << ", ";
        ostringstream ostr;
        angle.print (ostr, MVAngle::Format(MVAngle::ANGLE, 9));
        string str(ostr.str());
        string::size_type pos = str.find('.');
        if (pos != string::npos) str[pos] = 'd';
        pos = str.find('.');
        if (pos != string::npos) str[pos] = 'm';
        cout << str;
      }
      iter++;
    }
    cout << ']';
  }
  if (dir.size() != units.size()) {
    cout << ']';
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
      } else {
        firstTime = False;
      }
      showTime (*iter);
    }
    cout << ']';
  }
}

// Show the required columns.
// First test if they exist and contain scalars or arrays.
void showTable (const Table& tab, const Vector<String>& colnam,
                bool printMeas, const String& delim)
{
  uInt nrcol = 0;
  PtrBlock<TableColumn*> tableColumns(colnam.nelements());
  Block<Vector<String> > timeUnit(colnam.nelements());
  Block<Vector<String> > posUnit(colnam.nelements());
  Block<Vector<String> > dirUnit(colnam.nelements());
  uInt i;
  for (i=0; i<colnam.nelements(); i++) {
    if (! tab.tableDesc().isColumn (colnam(i))) {
      cout << "Column " << colnam(i) << " does not exist" << endl;
    }else{
      tableColumns[nrcol] = new TableColumn (tab, colnam(i));
      if (! tableColumns[nrcol]->columnDesc().isScalar()
      &&  ! tableColumns[nrcol]->columnDesc().isArray()) {
	cout << "Column " << colnam(i)
	     << " contains scalars nor arrays"
	     << endl;
	delete tableColumns[nrcol];
      }else{
        // If needed, see if it is a Measure type we know of.
        if (printMeas) {
          const TableRecord& keys = tableColumns[nrcol]->keywordSet();
          if (keys.isDefined ("MEASINFO")) {
            const TableRecord& meas = keys.subRecord("MEASINFO");
            if (keys.isDefined ("QuantumUnits")) {
              Vector<String> units (keys.asArrayString("QuantumUnits"));
              if (meas.isDefined ("type")) {
                String type = meas.asString("type");
                if (type == "epoch") {
                  timeUnit[nrcol] = units;
                } else if (type == "position") {
                  posUnit[nrcol] = units;
                } else if (type == "direction") {
                  dirUnit[nrcol] = units;
                }
              }
            }
          }
        }
	nrcol++;
      }
    }
  }
  if (nrcol == 0) {
    return;
  }
  // Use TableProxy, so we can be type-agnostic.
  TableProxy proxy(tab);
  for (i=0; i<tab.nrow(); i++) {
    for (uInt j=0; j<nrcol; j++) {
      if (j > 0) {
        cout << delim;
      }
      if (! tableColumns[j]->isDefined (i)) {
        cout << " no_array";
      } else {
        ValueHolder vh(proxy.getCell (tableColumns[j]->columnDesc().name(), i));
        if (! timeUnit[j].empty()) {
          if (tableColumns[j]->columnDesc().isScalar()) {
            showTime (vh.asDouble(), timeUnit[j][0]);
          } else {
            showTime (vh.asArrayDouble(), timeUnit[j][0]);
          }
        } else if (! posUnit[j].empty()) {
          showPos (vh.asArrayDouble(), posUnit[j]);
        } else if (! dirUnit[j].empty()) {
          showDir (vh.asArrayDouble(), dirUnit[j]);
        } else {
          cout << vh;
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
  Vector<uInt> rownrs (expr.nrow());
  indgen (rownrs);
  if (expr.isScalar()) {
    switch (expr.getColumnDataType()) {
    case TpBool:
      showArray (expr.getColumnBool (rownrs));
      break;
    case TpUChar:
      showArray (expr.getColumnuChar (rownrs));
      break;
    case TpShort:
      showArray (expr.getColumnShort (rownrs));
      break;
    case TpUShort:
      showArray (expr.getColumnuShort (rownrs));
      break;
    case TpInt:
      showArray (expr.getColumnInt (rownrs));
      break;
    case TpUInt:
      showArray (expr.getColumnuInt (rownrs));
      break;
    case TpFloat:
      showArray (expr.getColumnFloat (rownrs));
      break;
    case TpDouble:
      showArray (expr.getColumnDouble (rownrs));
      break;
    case TpComplex:
      showArray (expr.getColumnComplex (rownrs));
      break;
    case TpDComplex:
      showArray (expr.getColumnDComplex (rownrs));
      break;
    case TpString:
      showArray (expr.getColumnString (rownrs));
      break;
    case TpQuantity:
      {
        AlwaysAssert (expr.getNodeRep()->dataType() == TableExprNodeRep::NTDate,
                      AipsError);
        MVTime time;
        if (expr.nrow() != 1) cout << '[';
        for (uInt i=0; i<expr.nrow(); i++) {
          if (i > 0) cout << ", ";
          expr.get (i, time);
          showTime (time);
        }
        if (expr.nrow() != 1) cout << ']';
      }
      break;
    default:
      cout << "Unknown expression scalar type " << expr.getColumnDataType();
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
          cout << "Unknown expression array type " << expr.dataType();
      }
      cout << endl;
    }
  }
}

void showParseError (const TableParseError& x)
{
  // Try to highlight parse error on a tty. A color init
  //# string consists of one or more of the following numeric codes:
  //# Attribute codes:
  //# 00=none 01=bold 04=underscore 05=blink 07=reverse 08=concealed
  //# Text color codes:
  //# 30=black 31=red 32=green 33=yellow 34=blue 35=magenta 36=cyan 37=white
  //# Background color codes:
  //# 40=black 41=red 42=green 43=yellow 44=blue 45=magenta 46=cyan 47=white
  // cerr has fd 2 (per C++ standard)
  const String& msg(x.getMesg());
  if (isatty(2)  &&  x.pos() >= 0) {
    // Cater for leading part of the message.
    int errLen = x.token().size();
    int errPos = x.pos() - errLen + 23;
    if (msg[errPos + errLen -1] == '\n') {
      errLen--;
    }
    // For now use yellow background (43) to highlight the error.
    cerr << msg.substr(0, errPos) << "\033[1;43m"
         << msg.substr(errPos, errLen);
    cerr << "\033[0m" << msg.substr(errPos+errLen) << endl;
    if (errLen == 0) {
      cerr << "Probably a missing parenthesis or bracket" << endl;
    }
  } else {
    cerr << msg <<endl;
  }
}


// Sort and select data.
Table doCommand (bool printCommand, bool printSelect, bool printMeas,
                 bool printRows, bool printHeader, const String& delim,
                 const String& varName, const String& prefix, const String& str,
                 const vector<const Table*>& tempTables)
{
  // If no command is given, assume it is CALC.
  // Only show results for SELECT, COUNT and CALC.
  String::size_type spos = str.find_first_not_of (' ');
  Bool addCalc = False;
  Bool showHelp = False;
  Bool doCount = False;
  Bool showResult = False;
  if (spos != String::npos) {
    String::size_type epos = str.find (' ', spos);
    if (epos == String::npos) {
      epos = str.size();
    }
    String s = str.substr(spos, epos-spos);
    s.downcase();
    showHelp = (s=="show" || s=="help");
    addCalc = !(s=="select" || s=="update" || s=="insert" ||
                s=="calc" || s=="delete" || s=="count"  || 
                s=="create" || s=="createtable" ||
                s=="alter" || s=="altertable" ||
                s=="using"  || s=="usingstyle"  || s=="time" ||
                showHelp);
    showResult = (s=="select");
    if (s=="count") {
      doCount    = True;
      showResult = True;
    }
  }
  String strc(str);
  if (addCalc) {
    strc = "CALC " + str;
  }
  strc = prefix + strc;
  Table tabp;
  uInt i;
  Vector<String> colNames;
  String cmd;
  TaQLResult result;
  result = tableCommand (strc, tempTables, colNames, cmd);
  // Show result of COUNT as well.
  if (doCount) {
    colNames.resize (colNames.size() + 1, True);
    colNames[colNames.size() - 1] = "_COUNT_";
  }
  if (printCommand && !showHelp) {
    if (!varName.empty()) {
      cout << varName << " = ";
    }
    cout << strc << endl;
    cout << "    has been executed" << endl;
  }
  if (result.isTable()) {
    tabp = result.table();
    if (printRows) {
      cout << "    " << cmd << " result of " << tabp.nrow()
           << " rows" << endl;
    }
    if (printSelect && showResult && colNames.size() > 0) {
      if (printHeader) {
        // Show the selected column names.
        cout << colNames.nelements() << " selected columns: ";
        for (i=0; i<colNames.nelements(); i++) {
          cout << " " << colNames(i);
        }
        cout << endl;
      }
      // Show the contents of the columns.
      showTable (tabp, colNames, printMeas, delim);
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
  cerr << "  http://casacore.github.io/casacore-notes/199.html" << endl;
  cerr << "taql can be started with multiple arguments containing options and" << endl;
  cerr << "an optional TaQL command as the last argument(s)." << endl;
  cerr << "Using the -f option commands are taken from a file. The commands can be" << endl;
  cerr << "split over multiple lines. Therefore a ; has to be used to delimite a command" << endl;
  cerr << "After a # a line can contain comments." << endl;
  cerr << "It will run interactively if command nor file is given. If possible," << endl;
  cerr << "interactive commands are kept in $HOME/.taql_history for later reuse." << endl;
  cerr << "Use q, quit, exit, or ^D to exit." << endl;
  cerr << endl;
  cerr << "Any TaQL command can be used. If no command name is given, CALC is assumed." << endl;
  cerr << "For example:" << endl;
  cerr << "   date() + 107     #which date is 107 days after today" << endl;
  cerr << "   select from my.ms where ANTENNA1=1 giving sel.ms" << endl;
  cerr << "The result of a CALC command will be printed." << endl;
  cerr << "The number of resulting rows and the values of possible selected" << endl;
  cerr << "columns can be printed (see options below)." << endl;
  cerr << endl;
  cerr << "It is possible to save the table resulting from a selection" << endl;
  cerr << "by assigning it like:" << endl;
  cerr << "   var = taqlcommand" << endl;
  cerr << "Thereafter $var can be used as a table in another TaQL command like:"
       << endl;
  cerr << "   t1 = select from my.ms where ANTENNA1=1" << endl;
  cerr << "   t2 = select from $t1 where ANTENNA2=2" << endl;
  cerr << "A variable name followed by zero or more question marks gives info about" << endl;
  cerr << "the table. More info for more question marks (e.g. t1?)." << endl;
  cerr << "   var =" << endl;
  cerr << "clears 'var' (removes it from the saved selections)." << endl;
  cerr << "Use command ? to show all saved selections." << endl;
  cerr << endl;
  cerr << "The 'show' command shows some information." << endl;
  cerr << "   show units           show the possible units and prefixes" << endl;
  cerr << "   show meastypes       show the possible measure types" << endl;
  cerr << endl;
  cerr << "taql can be started with a few options:" << endl;
  cerr << " -s or --style defines the TaQL style." << endl;
  cerr << "  The default style is python; if no value is given after -s it defaults to glish" << endl;
  cerr << " -h  or --help          show this help and exits." << endl;
  cerr << " -f filename            name of file containing TaQL commands." << endl;
  cerr << " -d delim               delimiter used between column values." << endl;
  cerr << " -ps or --printselect   show the values of selected columns." << endl;
  cerr << " -pm or --printmeasure  if possible, show values as formatted measures" << endl;
  cerr << " -pc or --printcommand  show the (expanded) TaQL command." << endl;
  cerr << " -pr or --printrows     show the number of rows selected, updated, etc." << endl;
  cerr << " -ph or --printheader   show the header of names of the selected columns" << endl;
  cerr << "The default for -pc is on for interactive mode, otherwise off." << endl;
  cerr << "The default for -pr, -ph, -ps, and -pm is on." << endl;
  cerr << endl;
}

void showTableInfo (const String& name, const Table& tab,
                    const String& command, Int level)
{
  TableDesc tdesc(tab.actualTableDesc());
  cout << "  " << name << " resulted from:";
  if (level >= 0) {
    cout << endl;
    cout << "   ";
  }
  cout << ' ' << command << endl;
  if (level >= 0) {
    cout << "  " << tab.nrow() << " rows, "
         << tdesc.ncolumn() << " columns" << endl;
  }
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

// Show the variable names and tables associated to them.
void showTableMap (const TableMap& tables)
{
  if (tables.empty()) {
    cout << "  no saved selections;    note: use h to get help info" << endl;
      } else {
    for (TableMap::const_iterator iter = tables.begin();
         iter != tables.end(); ++iter) {
      showTableInfo (iter->first, iter->second.first, iter->second.second, -1);
    }
  }
}

// Substitute possible table variables given like $var.
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
void askCommands (bool printCommand, bool printSelect, bool printMeas,
                  bool printRows, bool printHeader, const String& delim,
                  const String& prefix, const vector<String>& commands)
{
#ifdef HAVE_READLINE
  string histFile;
  if (commands.empty()) {
    String homeDir = EnvironmentVariable::get("HOME");
    if (! homeDir.empty()) {
      histFile = homeDir + "/.taql_history";
      read_history(histFile.c_str());
    }
  }
#endif
  Regex varassRE("^[a-zA-Z_][a-zA-Z0-9_]*[ \t]*=");
  Regex assRE("[ \t]*=");
  Regex lwhiteRE("^[ \t]*");
  Regex rwhiteRE("[ \t]*$");
  TableMap tables;
  uInt inx=0;
  while (True) {
    try {
      String str;
      if (commands.empty()) {
        if (! readLineSkip (str, "TaQL> ")) {
          cerr << endl;
          break;
        }
      } else {
        if (inx >= commands.size()) {
          break;
        }
        str = commands[inx++];
      }
      if (str == "h"  ||  str == "-h"  ||  str == "--help") {
        showHelp();
      } else if (str == "?") {
        showTableMap (tables);
      } else if (str == "exit"  ||  str == "quit"  ||  str == "q") {
        break;
      } else {
        String varName;
        String::size_type assLen = varassRE.match (str.c_str(), str.size());
        if (assLen != String::npos) {
          // Assignment to variable; get its name and remove from command.
          varName = str.before(assLen);
          str = str.from(assLen);
          varName.del (assRE);
          if (varName.empty()) {
            throw AipsError ("Variable name before =command is empty");
          }
        }
        str.del (lwhiteRE);
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
          name.del (rwhiteRE);
          TableMap::const_iterator it = tables.find (name);
          if (it != tables.end()) {
            // It exists, so it must be a name.
            showTableInfo (name, it->second.first, it->second.second, level);
          } else {
            // No name, so it must be a command.
            // Note that CALC commands can omit CALC.
            String command(str);
            vector<const Table*> tabs = replaceVars (str, tables);
            Table tab = doCommand (printCommand, printSelect, printMeas,
                                   printRows, printHeader, delim,
                                   varName, prefix, str, tabs);
            if (!varName.empty()  &&  !tab.isNull()) {
              // Keep the resulting table if a variable was given.
              tables[varName] = make_pair(tab, command);
            }
          }
        }
      }
    } catch (const TableParseError& x) {
      showParseError (x);
    } catch (const AipsError& x) {
      cerr << x.getMesg() << endl;
    }
  }
#ifdef HAVE_READLINE
  if (! histFile.empty()) {
    write_history(histFile.c_str());
  }
#endif
}

vector<String> fileCommands (const string& fname)
{
  vector<String> commands;
  bool appendLast = false;
  std::ifstream ifs(fname.c_str());
  if (! ifs.good()) {
    throw AipsError("Cannot open file " + fname);
  }
  String line;
  getline (ifs, line);
  while (ifs.good()) {
    removeCR (line);
    vector<String> parts = splitLine(line);
    for (size_t i=0; i<parts.size(); ++i) {
      if (! parts[i].empty()) {
        if (appendLast) {
          commands[commands.size() - 1].append (' '+ parts[i]);
          appendLast = false;
        } else {
          commands.push_back (parts[i]);
        }
      }
    }
    appendLast = !parts.empty()  &&  !parts[parts.size()-1].empty();
    getline (ifs, line);
  }
  return commands;
}


int main (int argc, const char* argv[])
{
  try {
    string style = "python";
    string fname;
    int printCommand = -1;
    int printSelect  = 1;
    int printMeas    = 1;
    int printRows    = 1;
    int printHeader  = 1;
    String delim('\t');
    int st;
    for (st=1; st<argc; ++st) {
      string arg(argv[st]);
      if (arg == "-s"  ||  arg == "--style") {
        style = string();
        if (st+1 < argc) {
          style = argv[st+1];
          if (style.size() > 0  &&  style[0] == '-') {
            // no style value, thus ignore.
            style = string();
          } else {
            // use style value.
            st++;
          }
        }
      } else if (arg == "-d") {
        if (st+1 < argc) {
          delim = argv[st+1];
          st++;
        }
      } else if (arg == "-pc"  ||  arg == "--printcommand") {
        printCommand = 1;
      } else if (arg == "-ps"  ||  arg == "--printselect") {
        printSelect = 1;
      } else if (arg == "-pm"  ||  arg == "--printmeasure") {
        printMeas = 1;
      } else if (arg == "-pr"  ||  arg == "--printrows") {
        printRows = 1;
      } else if (arg == "-ph"  ||  arg == "--printheader") {
        printHeader = 1;
      } else if (arg == "-nopc"  ||  arg == "--noprintcommand") {
        printCommand = 0;
      } else if (arg == "-nops"  ||  arg == "--noprintselect") {
        printSelect = 0;
      } else if (arg == "-nopm"  ||  arg == "--noprintmeasure") {
        printMeas = 0;
      } else if (arg == "-nopr"  ||  arg == "--noprintrows") {
        printRows = 0;
      } else if (arg == "-noph"  ||  arg == "--noprintheader") {
        printHeader = 0;;
      } else if (arg == "-f") {
        if (st < argc-1) {
          st++;
          fname = argv[st];
        } else {
          throw AipsError("No file name given after -f");
        }
      } else if (arg == "-h"  ||  arg == "--help") {
        showHelp();
        return 0;
      } else if (arg[0] == '-') {
        cerr << arg << " is an invalid option" << endl;
        return 1;
      } else {
        break;
      }
    }
    string prefix;
    if (style.empty()) {
      style = "glish";
    }
    prefix = "using style " + style + ' ';
    if (! fname.empty()) {
      vector<String> commands = fileCommands (fname);
      if (! commands.empty()) {
        askCommands (printCommand!=0, printSelect!=0, printMeas!=0,
                     printRows!=0, printHeader!=0, delim, prefix, commands);
      }
    } else if (st < argc) {
      // A command can be given as multiple parameters to make tab-completion
      // easier. Thus combine it all.
      String command(argv[st]);
      while (++st < argc) {
        command += ' ' + String(argv[st]);
      }
      // Execute the given command.
      doCommand (printCommand==1, printSelect==1, printMeas==1,
                 printRows==1, printHeader==1,
                 delim, String(), prefix, command, vector<const Table*>());
    } else {
    // Ask the user for commands.
      cout << "Using default TaQL style " << style << endl;
      askCommands (printCommand!=0, printSelect!=0, printMeas!=0,
                   printRows!=0, printHeader!=0,
                   delim, prefix, vector<String>());
    }
  } catch (const TableParseError& x) {
    showParseError (x);
  } catch (const AipsError& x) {
    cerr << "\nCaught an exception: " << x.getMesg() << endl;
    return 1;
  } 
  return 0;               // successfully executed
}
