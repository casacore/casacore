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
#include <casacore/casa/IO/ArrayIO.h>
#include <casacore/casa/Quanta/MVPosition.h>
#include <casacore/casa/Quanta/MVAngle.h>
#include <casacore/casa/BasicSL/Complex.h>
#include <casacore/casa/BasicMath/Math.h>
#include <casacore/casa/OS/Path.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Utilities/GenSort.h>
#include <casacore/casa/OS/EnvVar.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/version.h>
#include <map>
#include <vector>
#include <fstream>
#include <casacore/casa/iostream.h>
#include <casacore/casa/iomanip.h>
#include <unistd.h>

#ifdef HAVE_READLINE
# include <readline/readline.h>
# include <readline/history.h>
#endif

using namespace casacore;
using namespace std;

// <summary>
// Execute any TaQL command from the shell.
// If a command is given at the taql command, only that command is executed.
// It is possible to execute command from a file using the -f option.
// If no command is given, an interactive session is started where options and/or
// commands can be given. In there the -f option can be used as well.
// -h or --help gives help info for the taql program.
// The command show or help gives help about the TaQL syntax and functionality.
// </summary>

//# The following functions are used (possibly recursively):
//# - main calls 'executeArgs'
//# - 'executeArgs' parses options and calls 'execCommand' if a command is given, calls
//#     'execFileCommands' if -f is given or calls 'askCommands' if no command given,
//#     but only if called from main
//# - 'execCommand' handles t=command and calls 'taqlComand' if a command is given
//# - 'execFileCommands' executes commands given in a file; splits and calls 'executeArgs'
//# - 'askCommands' ask commands, splits and calls 'executeArgs'
//# - 'taqlCommand' executes a TaQL command
//#
//# Options can be given at the start of any command line. If only options are given, they are
//# persistent. If a command is given as well, the options are only valid for that command.
//# The options set at a lower level, are not used at higher levels.
//#
//# The tableMap (mapping of name to Table object) is global, thus can be used at any level.


// Define the type for the map of name to (resulttable,command).
typedef map<String, pair<Table,String> > TableMap;

struct Options
{
  Bool printSelect;
  Bool printAuto;
  Bool printMeasure;
  Bool printHeader;
  Bool printCommand;
  Bool printNRows;
  uInt maxNRows;
  String separator;
  String fname;
  String style;
  String outName;
  CountedPtr<ostream> stream;

  Options()
    : printSelect  (False),   // print explicitly select result?
      printAuto    (True),    // print automatically if printSelect=False?
      printMeasure (True),    // print as measures when printing result?
      printHeader  (True),    // print column header when printing result?
      printCommand (False),   // print command?
      printNRows   (True),    // print nr of rows handled?
      maxNRows     (50),      // max #rows to print for auto print
      separator    ('\t'),    // default separator between printed columns
      style        ("python"),
      outName      ("stdout"),
      stream       (CountedPtr<ostream>(&cout, False))  // default stdout
  {}
};

//# Forward declare.
// Parse and execute the arguments given. <src>topLevel</src> tells if this is the top level.
// If so, an interactive session is started if no TaQL command is given. It also tells if
// possible quotes are removed from option values.
// The TableMap holds a map of name to temporary table.
// It returns False if exit (or quit) is given.
Bool executeArgs (const vector<String> args, Bool topLevel,
                  TableMap& tableMap, Options& options);



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

// Split a line using ; as delimiter.
// Skip comments indicated by #.
// Ignore those characters if in a quoted string.
// An empty string is added where a delimiter is used.
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
          parts.push_back (String());
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

// Split a line on whitespace (except in quoted parts).
vector<String> splitWS (const String& str)
{
  vector<String> parts;
  String part;
  int qpos = -1;
  for (uInt i=0; i<str.size(); ++i) {
    if (qpos < 0) {
      // Not in quoted string.
      if (str[i] == '"'  ||  str[i] == '\'') {
        // Start of quoted string.
        qpos = i;
      } else if (isspace(str[i])) {
        // Split at whitespace. Only store if not empty.
        if (part.size() > 0) {
          parts.push_back (part);
          part = String();
        }
        continue;
      }
    } else if (str[i] == str[qpos]) {
      // End of quoted string.
      qpos = -1;
    }
    part += str[i];
  }
  if (qpos >= 0) {
    throw AipsError ("Unbalanced quoted string at position " +
                     String::toString(qpos) + " in " + str);
  }
  if (part.size() > 0) {
    parts.push_back (part);    // last part
  }
  return parts;
}


// Read a line, if possible using the readline library to make command
// editing and history possible.
#ifdef HAVE_READLINE
Bool readLine (String& line, const String& prompt)
{
  char* str = readline(prompt.c_str());
  if (!str) return False;
  line = String(str);
  removeCR (line);
  free(str);
  return True;
}
#else
Bool readLine (String& line, const String& prompt)
{
  if (!prompt.empty()) cerr << prompt;
  getline (cin, line);
  removeCR (line);
  return cin.good();
}
#endif

// Read a line until a non-empty line is read. 
Bool readLineSkip (String& line, const String& prompt)
{
  Bool fnd = False;
  while (!fnd  &&  readLine (line, prompt)) {
    fnd = !line.empty();
  }
#ifdef HAVE_READLINE
  if (fnd) add_history (line.c_str());
#endif
  return fnd;
}

// Show a date/time. Do not show time part if 0.
void showTime (const MVTime& time, ostream& os)
{
  Double val = time.day();
  if (val == floor(val)) {
    time.print (os, MVTime::Format
                (MVTime::formatTypes(MVTime::DMY | MVTime::NO_TIME)));
  } else {
    time.print (os, MVTime::Format(MVTime::DMY, 9));
  }
}

void showTime (double time, const String& unit, ostream& os)
{
  showTime (MVTime (Quantity(time, unit)), os);
}

void showTime (const Array<double>& times, const String& unit, ostream& os)
{
  Quantity q(0., unit);
  Bool firstTime = True;
  cout << '[';
  Array<double>::const_iterator endIter = times.end();
  for (Array<double>::const_iterator iter= times.begin();
       iter != endIter; ++iter) {
    if (!firstTime) {
      os << ", ";
    } else {
      firstTime = False;
    }
    q.setValue (*iter);
    showTime (q, os);
  }
  os << ']';
}

// Show values representing MPositions.
void showPos (const Array<double>& pos, const Vector<String>& units, ostream& os)
{
  AlwaysAssert (pos.size() % units.size() == 0, AipsError);
  Vector<Quantity> q(units.size());
  for (uInt i=0; i< units.size(); ++i) {
    q[i] = Quantity(0., units[i]);
  }
  Bool firstTime = True;
  if (pos.size() != units.size()) {
    os << '[';
  }
  Array<double>::const_iterator endIter = pos.end();
  for (Array<double>::const_iterator iter= pos.begin(); iter != endIter;) {
    if (!firstTime) {
      os << ", ";
    } else {
      firstTime = False;
    }
    for (uInt i=0; i<units.size(); ++i) {
      q[i].setValue (*iter);
      iter++;
    }
    MVPosition pos(q);
    os << q;
  }
  if (pos.size() != units.size()) {
    os << ']';
  }
}

// Show values representing MDirections.
void showDir (const Array<double>& dir, const Vector<String>& units, ostream& os)
{
  AlwaysAssert (dir.size() % units.size() == 0, AipsError);
  Vector<Quantity> q(units.size());
  for (uInt i=0; i< units.size(); ++i) {
    q[i] = Quantity(0., units[i]);
  }
  Bool firstTime = True;
  if (dir.size() != units.size()) {
    os << '[';
  }
  Array<double>::const_iterator endIter = dir.end();
  for (Array<double>::const_iterator iter= dir.begin(); iter != endIter;) {
    if (!firstTime) {
      os << ", ";
    } else {
      firstTime = False;
    }
    os << '[';
    for (uInt i=0; i<units.size(); ++i) {
      q[i].setValue (*iter);
      MVAngle angle(q[i]);
      if (i == 0)  {
        ostringstream ostr;
        angle.print (ostr, MVAngle::Format(MVAngle::TIME, 9));
        String str(ostr.str());
        String::size_type pos = str.find(':');
        if (pos != String::npos) str[pos] = 'h';
        pos = str.find(':');
        if (pos != String::npos) str[pos] = 'm';
        os << str;
      } else {
        os << ", ";
        ostringstream ostr;
        angle.print (ostr, MVAngle::Format(MVAngle::ANGLE, 9));
        String str(ostr.str());
        String::size_type pos = str.find('.');
        if (pos != String::npos) str[pos] = 'd';
        pos = str.find('.');
        if (pos != String::npos) str[pos] = 'm';
        os << str;
      }
      iter++;
    }
    os << ']';
  }
  if (dir.size() != units.size()) {
    os << ']';
  }
}

// Show an array of values enclosed in square brackets.
// Omit square brackets if only one value.
template<typename T>
void showArray (const Array<T>& arr, ostream& os)
{
  if (arr.size() == 1) {
    os << arr.data()[0];
  } else {
    os << arr;
  }
}
template<> void showArray (const Array<MVTime>& arr, ostream& os)
{
  if (arr.size() == 1) {
    showTime (arr.data()[0], os);
  } else {
    Bool firstTime = True;
    os << '[';
    Array<MVTime>::const_iterator endIter = arr.end();
    for (Array<MVTime>::const_iterator iter= arr.begin();
         iter != endIter; ++iter) {
      if (!firstTime) {
        os << ", ";
      } else {
        firstTime = False;
      }
      showTime (*iter, os);
    }
    os << ']';
  }
}

// Show the required columns of the table.
// First test if they exist and contain scalars or arrays.
void showTable (const Table& tab, const Vector<String>& colnam,
                Bool printMeasure, const String& separator, ostream& os)
{
  uInt nrcol = 0;
  PtrBlock<TableColumn*> tableColumns(colnam.nelements());
  Block<Vector<String> > timeUnit(colnam.nelements());
  Block<Vector<String> > posUnit(colnam.nelements());
  Block<Vector<String> > dirUnit(colnam.nelements());
  Block<String> colUnits(colnam.nelements());
  Bool hasUnits = False;
  uInt i;
  for (i=0; i<colnam.nelements(); i++) {
    if (! tab.tableDesc().isColumn (colnam(i))) {
      os << "Column " << colnam(i) << " does not exist" << endl;
    }else{
      tableColumns[nrcol] = new TableColumn (tab, colnam(i));
      if (! tableColumns[nrcol]->columnDesc().isScalar()
      &&  ! tableColumns[nrcol]->columnDesc().isArray()) {
	os << "Column " << colnam(i)
           << " contains neither scalars nor arrays" << endl;
	delete tableColumns[nrcol];
        tableColumns[nrcol] = 0;
      }else{
        // Get possible units.
        const TableRecord& keys = tableColumns[nrcol]->keywordSet();
        Vector<String> units;
        if (keys.isDefined ("QuantumUnits")) {
          units = keys.asArrayString("QuantumUnits");
          if (! units.empty()) {
            colUnits[nrcol] = units[0];
            hasUnits = True;
          }
        }
        // If needed, see if it is a Measure type we know of.
        if (printMeasure) {
          if (keys.isDefined ("MEASINFO")) {
            const TableRecord& meas = keys.subRecord("MEASINFO");
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
	nrcol++;
      }
    }
  }
  if (nrcol == 0) {
    return;
  }
  // Show possible units.
  if (hasUnits) {
    os << "Unit: ";
    for (uInt j=0; j<nrcol; j++) {
      if (j > 0) {
        os << separator;
      }
      os << colUnits[j];
    }
    os << endl;
  }
  // Use TableProxy, so we can be type-agnostic.
  TableProxy proxy(tab);
  for (i=0; i<tab.nrow(); i++) {
    for (uInt j=0; j<nrcol; j++) {
      if (j > 0) {
        os << separator;
      }
      if (! tableColumns[j]->isDefined (i)) {
        os << " no_array";
      } else {
        ValueHolder vh(proxy.getCell (tableColumns[j]->columnDesc().name(), i));
        if (! timeUnit[j].empty()) {
          if (tableColumns[j]->columnDesc().isScalar()) {
            showTime (vh.asDouble(), timeUnit[j][0], os);
          } else {
            showTime (vh.asArrayDouble(), timeUnit[j][0], os);
          }
        } else if (! posUnit[j].empty()) {
          showPos (vh.asArrayDouble(), posUnit[j], os);
        } else if (! dirUnit[j].empty()) {
          showDir (vh.asArrayDouble(), dirUnit[j], os);
        } else if (vh.dataType() == TpBool) {
          // std::boolalpha seems to persist.
          os << (vh.asBool() ? "true" : "false");
        } else {
          os << vh;
        }
      }
    }
    os << endl;
  }
  
  for (i=0; i<nrcol; i++) {
    delete tableColumns[i];
  }
}

// Show the value of an expression which can be a scalar or array (part).
void showExpr(const TableExprNode& expr, ostream& os)
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
      os << "Index: " << indices.start() << endl;
    }
  }
  const Unit& unit = expr.unit();
  if (! unit.empty()) {
    os << "Unit: " << unit.getName() << endl;
  }
  Vector<uInt> rownrs (expr.nrow());
  indgen (rownrs);
  if (expr.isScalar()) {
    switch (expr.getColumnDataType()) {
    case TpBool:
      showArray (expr.getColumnBool (rownrs), os);
      break;
    case TpUChar:
      showArray (expr.getColumnuChar (rownrs), os);
      break;
    case TpShort:
      showArray (expr.getColumnShort (rownrs), os);
      break;
    case TpUShort:
      showArray (expr.getColumnuShort (rownrs), os);
      break;
    case TpInt:
      showArray (expr.getColumnInt (rownrs), os);
      break;
    case TpUInt:
      showArray (expr.getColumnuInt (rownrs), os);
      break;
    case TpInt64:
      showArray (expr.getColumnInt64 (rownrs), os);
      break;
    case TpFloat:
      showArray (expr.getColumnFloat (rownrs), os);
      break;
    case TpDouble:
      showArray (expr.getColumnDouble (rownrs), os);
      break;
    case TpComplex:
      showArray (expr.getColumnComplex (rownrs), os);
      break;
    case TpDComplex:
      showArray (expr.getColumnDComplex (rownrs), os);
      break;
    case TpString:
      showArray (expr.getColumnString (rownrs), os);
      break;
    case TpQuantity:
      {
        AlwaysAssert (expr.getNodeRep()->dataType() == TableExprNodeRep::NTDate,
                      AipsError);
        MVTime time;
        if (expr.nrow() != 1) os << '[';
        for (uInt i=0; i<expr.nrow(); i++) {
          if (i > 0) os << ", ";
          expr.get (i, time);
          showTime (time, os);
        }
        if (expr.nrow() != 1) os << ']';
      }
      break;
    default:
      os << "Unknown expression scalar type " << expr.getColumnDataType();
    }
    os << endl;
  } else {
    for (uInt i=0; i<expr.nrow(); i++) {
      if (expr.nrow() > 1) {
        os << "  row " << i << ":  ";
      }
      switch (expr.dataType()) {
      case TpBool:
        showArray (expr.getArrayBool(i), os);
        break;
      case TpInt64:
        showArray (expr.getArrayInt(i), os);
        break;
      case TpDouble:
        showArray (expr.getArrayDouble(i), os);
        break;
      case TpDComplex:
        showArray (expr.getArrayDComplex(i), os);
        break;
      case TpString:
        showArray (expr.getArrayString(i), os);
        break;
      case TpQuantity:
        {
          AlwaysAssert (expr.getNodeRep()->dataType() == TableExprNodeRep::NTDate,
                        AipsError);
          showArray (expr.getArrayDate(i), os);
        }
        break;
      default:
          os << "Unknown expression array type " << expr.dataType();
      }
      os << endl;
    }
  }
}

void showParseError (const TableParseError& x)
{
  // Try to highlight a parse error on a tty. A color init
  //# string consists of one or more of the following numeric codes:
  //# Attribute codes:
  //# 00=none 01=bold 04=underscore 05=blink 07=reverse 08=concealed
  //# Text color codes:
  //# 30=black 31=red 32=green 33=yellow 34=blue 35=magenta 36=cyan 37=white
  //# Background color codes:
  //# 40=black 41=red 42=green 43=yellow 44=blue 45=magenta 46=cyan 47=white
  // cerr has fd 2 (per C++ standard)
  const String& msg(x.what());
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


// Execute a TaQL command.
Table taqlCommand (const Options& options, const String& varName,
                   const String& command,
                   const vector<const Table*>& tempTables)
{
  // If no command is given, assume it is SELECT.
  // Only show results for SELECT, COUNT and CALC.
  Bool addComm = False;
  Bool showHelp = False;
  Bool printSelect  = options.printSelect;
  Bool printAuto    = options.printAuto;
  Bool printMeasure = options.printMeasure;
  Bool printCommand = options.printCommand;
  Bool printNRows   = options.printNRows;
  Bool printHeader  = options.printHeader;
  uInt maxNRows     = options.maxNRows;
  ostream& os = *(options.stream);
  String::size_type spos = command.find_first_not_of (' ');
  if (spos != String::npos) {
    String::size_type epos = command.find (' ', spos);
    if (epos == String::npos) {
      epos = command.size();
    }
    String s = command.substr(spos, epos-spos);
    s.downcase();
    showHelp = (s=="show" || s=="help");
    addComm = !(s=="with" || s=="select" || s=="update" || s=="insert" ||
                s=="calc" || s=="delete" || s=="count"  || 
                s=="create" || s=="createtable" ||
                s=="alter" || s=="altertable" ||
                s=="using"  || s=="usingstyle"  || s=="time" ||
                showHelp);
  }
  String strc(command);
  if (addComm) {
    strc = "SELECT " + command;
    printSelect = True;
    printHeader = False;
    printCommand = False;
    printNRows = False;
  }
  String style(options.style);
  if (style.empty()) {
    style = "glish";
  }
  strc = "using style " + style + ' ' + strc;
  Vector<String> colNames;
  String cmd;
  TaQLResult result = tableCommand (strc, tempTables, colNames, cmd);
  cmd.downcase();
  // Show result of COUNT as well.
  if (cmd == "count") {
    colNames.resize (colNames.size() + 1, True);
    colNames[colNames.size() - 1] = "_COUNT_";
  }
  if (printCommand && !showHelp) {
    if (!varName.empty()) {
      os << varName << " = ";
    }
    os << strc << endl;
    os << "    has been executed" << endl;
  }
  Table tabp;
  if (result.isTable()) {
    tabp = result.table();
    if (printNRows) {
      os << "    " << cmd << " result of " << tabp.nrow()
         << " rows" << endl;
    }
    // Only show the selected table columns for SELECT and COUNT.
    if ((cmd == "select"  ||  cmd == "count")  &&  colNames.size() > 0) {
      // Only show if explicit print or possibly if auto.
      if (printSelect || (printAuto && maxNRows>0)) {
        if (printHeader) {
          // Show the selected column names.
          os << colNames.nelements() << " selected columns: ";
          for (uInt i=0; i<colNames.nelements(); i++) {
            os << " " << colNames(i);
          }
          os << endl;
        }
        // Show the contents of the columns.
        // When printing automatically, only maxNRows are shown.
        Table tabc(tabp);
        Bool showSubset = False;
        if (!printSelect && printAuto && tabp.nrow() > maxNRows) {
          showSubset = True;
          Vector<uInt> rownrs(maxNRows);
          indgen (rownrs);
          tabc = tabp(rownrs);
        }
        showTable (tabc, colNames, printMeasure, options.separator, os);
        if (showSubset) {
          os << "  Note: only the first " << maxNRows << " rows are shown (out of "
             << tabp.nrow() << ')' << endl;
        }
      }
    }
  } else {
    showExpr (result.node(), os);
  }
  return tabp;
}


void showVersion()
{
  cerr << "taql version " << getVersionCASA() << endl;
}

void showHelp()
{
  cerr << endl;
  cerr << "TaQL is the query language for casacore tables and is described at"
       << endl;
  cerr << "  http://casacore.github.io/casacore-notes/199.html" << endl;
  cerr << "taql can be started with multiple arguments containing options and" << endl;
  cerr << "an optional TaQL command as the last argument(s)." << endl;
  cerr << "Using the -f option commands are taken from a file where commands can be" << endl;
  cerr << "split over multiple lines and a ; has to be used to delimit a command" << endl;
  cerr << "After a # a line can contain comments." << endl;
  cerr << "taql will run interactively if no command nor file is given. If possible," << endl;
  cerr << "interactive commands are kept in $HOME/.taql_history for later reuse." << endl;
  cerr << "Use q, quit, exit, or ^D to exit." << endl;
  cerr << endl;
  cerr << "Any TaQL command can be used. If no command name is given, SELECT is assumed." << endl;
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
  cerr << "Thereafter $var can be used as a table in another TaQL command like:" << endl;
  cerr << "   t1 = select from my.ms where ANTENNA1=1" << endl;
  cerr << "   t2 = select from $t1 where ANTENNA2=2" << endl;
  cerr << "A variable name followed by zero or more question marks gives info about" << endl;
  cerr << "the table. More info for more question marks (e.g. t1?)." << endl;
  cerr << "   var =" << endl;
  cerr << "clears 'var' (removes it from the saved selections)." << endl;
  cerr << "Use command ? to show all saved selections." << endl;
  cerr << endl;
  cerr << "The 'show' command gives TaQL help; subcommands give more detailed help." << endl;
  cerr << endl;
  cerr << "taql can be started with a few options:" << endl;
  cerr << " -s or --style defines the TaQL style." << endl;
  cerr << "  The default style is python; if no value is given after -s it defaults to glish" << endl;
  cerr << " -h  or --help          show this help and exit" << endl;
  cerr << " -v  or --version       show the taql version and exit" << endl;
  cerr << " -f filename            name of file containing TaQL commands to execute" << endl;
  cerr << " -d separator           separator used between printed values (default a tab (\t))" << endl;
  cerr << " -o outputfile          name of output file (or stdout or stderr)" << endl;
  cerr << " -ps or --printselect   always show the values of selected columns" << endl;
  cerr << " -pa or --printauto     automatically show values (up to maxNRows)" << endl;
  cerr << " -pm or --printmeasure  if possible, show values as formatted measures" << endl;
  cerr << " -ph or --printheader   show the header of names of the selected columns" << endl;
  cerr << " -pc or --printcommand  show the (expanded) TaQL command" << endl;
  cerr << " -pr or --printnrows    show the number of rows selected, updated, etc." << endl;
  cerr << " -p  or --printall      sets -ps, -pm, -ph, -pc and -pr" << endl;
  cerr << " -m maxNRows            max nr of rows to print automatically (default 50)" << endl;
  cerr << "A print option can be turned off by giving, for example, -nops or --noprintall" << endl;
  cerr << "The default is -nops, -pa, -pm, -ph, -nopc, -pr, -m 50, -o stdout" << endl;
  cerr << "Note that the result of CALC and an implicit SELECT is always printed" << endl;
  cerr << "";
  cerr << "These options can also be given before any TaQL command. If followed by a command," << endl;
  cerr << "the setting is used for that command only, otherwise it is permanent." << endl;
  cerr << endl;
}

void showOptions (const Options& options)
{
  const char* opts[] = {"--no", "--"};
  cerr << endl;
  cerr << "Options settings:" << endl;
  cerr << ' ' << opts[options.printSelect]   << "printselect" << endl;
  cerr << ' ' << opts[options.printAuto]     << "printauto" << endl;
  cerr << ' ' << opts[options.printMeasure]  << "printmeasure" << endl;
  cerr << ' ' << opts[options.printHeader]   << "printheader" << endl;
  cerr << ' ' << opts[options.printCommand]  << "printcommand" << endl;
  cerr << ' ' << opts[options.printNRows]    << "printnrows" << endl;
  cerr << " -m " << options.maxNRows << "   (max nr of rows to print automatically)" << endl;
  cerr << " -d " << "'" << options.separator << "'   (separator between printed columns)" << endl;
  cerr << " -o " << "'" << options.outName << "'   (output filename)" << endl;
  cerr << " --style " << "'" << options.style << "'" << endl;
  cerr << endl;
}

void showTableInfo (const String& name, const Table& tab,
                    const String& command, Int level, ostream& os)
{
  TableDesc tdesc(tab.actualTableDesc());
  os << "  " << name << " resulted from:";
  if (level >= 0) {
    os << endl;
    os << "   ";
  }
  os << ' ' << command << endl;
  if (level >= 0) {
    os << "  " << tab.nrow() << " rows, "
         << tdesc.ncolumn() << " columns" << endl;
  }
  if (level > 0) {
    Vector<String> colNames = tdesc.columnNames();
    os << "    " << colNames << endl;
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
        os << "    " << colNames[i];
        for (uInt j=colNames[i].size(); j<maxLen; ++j) {
          os << ' ';
        }
        os << ' ' << ValType::getTypeStr(cdesc.dataType());
        if (cdesc.isScalar()) {
          os << " scalar";
        } else if (cdesc.isArray()) {
          os << " array";
          if (cdesc.ndim() > 0) {
            os << " ndim=" << cdesc.ndim();
          }
          if (! cdesc.shape().empty()) {
            os << " shape=" << cdesc.shape();
          }
        }
        if (cdesc.comment().empty()) {
          os << "  " << cdesc.comment();
        }
        os << endl;
      }
    }
  }
}

// Show the variable names and tables associated to them.
void showTableMap (const TableMap& tables, ostream& os)
{
  if (tables.empty()) {
    os << "  no saved selections;    note: use h to get help info" << endl;
      } else {
    for (TableMap::const_iterator iter = tables.begin();
         iter != tables.end(); ++iter) {
      showTableInfo (iter->first, iter->second.first, iter->second.second, -1, os);
    }
  }
}

// Substitute possible table variables given like $var.
String substituteName (const String& name, const TableMap& tableMap,
                       vector<const Table*>& tabs)
{
  TableMap::const_iterator fnd = tableMap.find(name);
  if (fnd == tableMap.end()) {
    return name;    // not found
  }
  tabs.push_back (&(fnd->second.first));
  return String::toString (tabs.size());    // return seqnr as string
}

vector<const Table*> replaceVars (String& str, const TableMap& tableMap)
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
        out += substituteName(name, tableMap, tabs);
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
    out += substituteName(name, tableMap, tabs);
  }
  str = out;
  return tabs;
}


Bool execCommand (const String& command, TableMap& tableMap,
                  const Options& options)
{
  Regex varassRE("^[a-zA-Z_][a-zA-Z0-9_]*[ \t]*=");
  Regex assRE("[ \t]*=");
  Regex lwhiteRE("^[ \t]*");
  Regex rwhiteRE("[ \t]*$");
  try {
    ostream& os = *(options.stream);
    String strc(command);
    if (strc == "h") {
      showHelp();
    } else if (strc == "v") {
      showVersion();
    } else if (strc == "o") {
      showOptions (options);
    } else if (strc == "?") {
      showTableMap (tableMap, os);
    } else if (strc == "exit"  ||  strc == "quit"  ||  strc == "q") {
      return False;
    } else {
      String varName;
      String::size_type assLen = varassRE.match (strc.c_str(), strc.size());
      if (assLen != String::npos) {
        // Assignment to variable; get its name and remove from command.
        varName = strc.before(assLen);
        strc = strc.from(assLen);
        varName.del (assRE);
        if (varName.empty()) {
          throw AipsError ("Variable name before =command is empty");
        }
      }
      strc.del (lwhiteRE);
      if (strc.empty()) {
        // No command means that the variable will be removed.
        tableMap.erase (varName);
      } else {
        // No assignment, so it is a name or a command.
        // First try it as a name.
        // A name can be followed by question marks giving the level of
        // info to be printed.
        Int sz = strc.size();
        while (sz > 0  &&  strc[sz-1] == '?') {
          --sz;
        }
        Int level = strc.size() - sz;
        String name = strc.substr(0, sz);
        name.del (rwhiteRE);
        TableMap::const_iterator it = tableMap.find (name);
        if (it != tableMap.end()) {
          // It exists, so it must be a name.
          showTableInfo (name, it->second.first, it->second.second, level, os);
        } else {
          // No name, so it must be a command.
          vector<const Table*> tabs = replaceVars (strc, tableMap);
          Table tab = taqlCommand (options, varName, strc, tabs);
          if (!varName.empty()  &&  !tab.isNull()) {
            // Keep the resulting table if a variable was given.
            tableMap[varName] = make_pair(tab, command);
          }
        }
      }
    }
  } catch (const TableParseError& x) {
    showParseError (x);
  } catch (const std::exception& x) {
    cerr << x.what() << endl;
  }
  return True;
}

void execFileCommands (TableMap& tableMap, const Options& options)
{
  // Reads all commands from the file and split them at ;.
  // A command can be continued on the next line.
  vector<String> commands;
  Bool appendLast = False;
  std::ifstream ifs(options.fname.c_str());
  if (! ifs.good()) {
    throw AipsError("Cannot open file " + options.fname);
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
          appendLast = False;
        } else {
          commands.push_back (parts[i]);
        }
      }
    }
    appendLast = !parts.empty()  &&  !parts[parts.size()-1].empty();
    getline (ifs, line);
  }
  // Execute all commands.
  // Use a new scope for the options.
  Options localOptions(options);
  for (auto command : commands) {
    if (! executeArgs (splitWS(command), False, tableMap, localOptions)) {
      break;         // exit given
    }
  }
}


// Ask and execute commands till quit or ^D is given.
void askCommands (TableMap& tableMap, Options& options)
{
  // Read back history if available.
#ifdef HAVE_READLINE
  String histFile;
  String homeDir = EnvironmentVariable::get("HOME");
  if (! homeDir.empty()) {
    histFile = homeDir + "/.taql_history";
    read_history (histFile.c_str());
  }
#endif
  while (True) {
    String str;
    // Read and execute until ^D or quit is given.
    if (! (readLineSkip (str, "TaQL> ")  &&
           executeArgs (splitWS(str), False, tableMap, options))) {
      cerr << endl;
      break;
    }
  }
#ifdef HAVE_READLINE
  if (! histFile.empty()) {
    write_history (histFile.c_str());
  }
#endif
}

// Remove quotes if necessary.
// Note that it should not be done for options given in a shell command,
// but has to be done for options given at the TaQL prompt.
String removeQuotes (const String& s, Bool removeQuote)
{
  if (removeQuote  &&  s.size() >= 2) {
    if ((s[0] == '"'  ||  s[0] == '\'')  &&  s[0] == s[s.size()-1]) {
      return s.substr (1, s.size()-2);
    }
  }
  return s;
}

// Parse the given options and set flags accordingly.
// Stop at first non-option (indicated by st).
Bool parseArgs (const vector<String>& args, uInt& st, Options& options, Bool removeQuote)
{
  options.fname = String();
  for (st=0; st<args.size(); ++st) {
    String arg(args[st]);
    if (arg == "-s"  ||  arg == "--style") {
      options.style = String();
      if (st+1 < args.size()) {
        options.style = removeQuotes (args[st+1], removeQuote);
        if (options.style.size() > 0  &&  options.style[0] == '-') {
          // no style value, thus ignore.
          options.style = String();
        } else {
          // use style value.
          st++;
        }
      }
    } else if (arg == "-d") {
      if (st < args.size()-1) {
        st++;
        options.separator = removeQuotes (args[st], removeQuote);
      } else {
        throw AipsError("No separator given after -d");
      }
    } else if (arg == "-m") {
      if (st < args.size()-1) {
        st++;
        options.maxNRows = atoi(args[st].c_str());
      } else {
        throw AipsError("No value given after -m");
      }
    } else if (arg == "-f") {
      if (st < args.size()-1) {
        st++;
        // Expand possible ~ and env.vars in file name.
        options.fname = Path(removeQuotes (args[st], removeQuote)).absoluteName();
      } else {
        throw AipsError("No file name given after -f");
      }
    } else if (arg == "-o") {
      if (st < args.size()-1) {
        st++;
        String fname = removeQuotes (args[st], removeQuote);
        String outname(fname);
        outname.downcase();
        if (outname == "stdout") {
          options.stream  = CountedPtr<ostream>(&cout, False);
          options.outName = "stdout";
        } else if (outname == "stderr") {
          options.stream = CountedPtr<ostream>(&cerr, False);
          options.outName = "stderr";
        } else {
          try {
            outname = Path(fname).absoluteName();
            options.stream = new ofstream(outname);
            options.outName = outname;
          } catch (std::exception& x) {
            cerr << "Could not create output file " << fname << endl;
            cerr << "    " << x.what() << endl;
          }
        }
      } else {
        throw AipsError("No output file name given after -o");
      }
      
    } else if (arg == "-p"  ||  arg == "--printall") {
      options.printCommand = True;
      options.printSelect  = True;
      options.printMeasure = True;
      options.printNRows   = True;
      options.printHeader  = True;
    } else if (arg == "-nop"  ||  arg == "--noprintall") {
      options.printCommand = False;
      options.printSelect  = False;
      options.printMeasure = False;
      options.printNRows   = False;
      options.printHeader  = False;
    } else if (arg == "-pc"  ||  arg == "--printcommand") {
      options.printCommand = True;
    } else if (arg == "-ps"  ||  arg == "--printselect") {
      options.printSelect = True;
    } else if (arg == "-pa"  ||  arg == "--printauto") {
      options.printAuto   = True;
      options.printSelect = False;
    } else if (arg == "-pm"  ||  arg == "--printmeasure") {
      options.printMeasure = True;
    } else if (arg == "-pr"  ||  arg == "--printnrows") {
      options.printNRows = True;
    } else if (arg == "-ph"  ||  arg == "--printheader") {
      options.printHeader = True;
    } else if (arg == "-nopc"  ||  arg == "--noprintcommand") {
      options.printCommand = False;
    } else if (arg == "-nops"  ||  arg == "--noprintselect") {
      options.printSelect = False;
    } else if (arg == "-nopa"  ||  arg == "--noprintauto") {
      options.printAuto = False;
    } else if (arg == "-nopm"  ||  arg == "--noprintmeasure") {
      options.printMeasure = False;
    } else if (arg == "-nopr"  ||  arg == "--noprintnrows") {
      options.printNRows = False;
    } else if (arg == "-noph"  ||  arg == "--noprintheader") {
      options.printHeader = False;
    } else if (arg == "-v"  ||  arg == "--version") {
      showVersion();
      return False;
    } else if (arg == "-h"  ||  arg == "--help") {
      showHelp();
      return False;
    } else if (arg == "--") {
      break;      // -- signifies end of options
    } else if (arg[0] == '-') {
      cerr << arg << " is an invalid option; it will be tried as a command" << endl;
      cerr << "  Note that -- indicates the end of options (e.g., use -- " << arg << ')' << endl;
      return True;
    } else {
      break;
    }
  }
  return True;
}

// Execute a given command.
// Ask for commands if interactive and empty command.
Bool executeArgs (const vector<String> args, Bool topLevel,
                  TableMap& tableMap, Options& options)
{
  // Parse the options as given.
  Options localOptions(options);
  uInt st = 0;
  if (! parseArgs (args, st, localOptions, !topLevel)) {
    return True;
  }
  // Execute the command file if given.
  if (! localOptions.fname.empty()) {
    execFileCommands (tableMap, localOptions);
  } else if (st < args.size()) {
    // A command can be given as multiple parameters to make tab-completion
    // easier. Thus combine it all.
    String command(args[st]);
    while (++st < args.size()) {
      command += ' ' + String(args[st]);
    }
    // Execute the given command.
    return execCommand (command, tableMap, localOptions);
  } else if (topLevel) {
    // Ask the user for commands.
    cerr << "Using default TaQL style " << options.style << endl;
    askCommands (tableMap, localOptions);
  } else {
    // Make the options persistent.
    options = localOptions;
  }
  return True;
}

int main (int argc, const char* argv[])
{
  try {
    // Set default state options.
    TableMap tableMap;
    Options options;
    vector<String> args;
    for (int i=1; i<argc; ++i) {
      args.push_back (argv[i]);
    }
    executeArgs (args, True, tableMap, options);
  } catch (const std::exception& x) {
    cerr << "\nCaught an exception: " << x.what() << endl;
    return 1;
  } 
  return 0;               // successfully executed
}
