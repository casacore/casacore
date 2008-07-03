//# measuresdata.cc: Program to read IERS and other data for Measure conversion
//# Copyright (C) 2007
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
//# $Id: measuresdata.cc,v 1.1 2007/07/18 14:00:41 wbrouw Exp $


//# Includes
#include <tables/Tables/SetupNewTab.h>
#include <tables/Tables/Table.h>
#include <tables/Tables/TableDesc.h>
#include <tables/Tables/TableInfo.h>
///#include <tables/Tables/ArrayColumn.h>
#include <tables/Tables/TableRecord.h>
#include <tables/Tables/ScaColDesc.h>
#include <tables/Tables/ScalarColumn.h>
#include <tables/Tables/ArrColDesc.h>
#include <casa/Arrays/Vector.h>
#include <casa/BasicMath/Math.h>
#include <casa/BasicSL/String.h>
#include <casa/Exceptions/Error.h>
#include <casa/Inputs/Input.h>
#include <casa/Inputs/Param.h>
#include <casa/OS/Path.h>
#include <casa/OS/File.h>
#include <casa/OS/RegularFile.h>
#include <casa/Quanta/MVTime.h>
#include <casa/Quanta/Quantum.h>
#include <casa/Utilities/Assert.h>
#include <casa/Utilities/Regex.h>
#include <casa/fstream.h>
#include <casa/iomanip.h>
#include <casa/iostream.h>
#include <casa/sstream.h>
#include <map>
#include <vector>

#include <casa/namespace.h>

// Using
using std::vector;
using std::map;
  
//*************************************************************************//
// Description
//*************************************************************************//

// <synopsis>
// The measuresdata program is able to analyse ascii tables
// (or sinex files or xml files) and convert them into casa Tables which
// can be used by e.g. the Measures class.
//
// The files have to be obtained by other means (e.g. a python script,
// manually or another script (e.g. the provided measuresdata.csh tcsh script.
//
// Normal operation:
// <li>
// <item> call the program measuresdata. The program can have arguments (see
// later, but the defaults suffice. The program will produce a cout log.
// <item> the program will return with either a failure status or with
// success status. In latter case a resource file (measuresdata.link) will
// be present. This file will have format:
// <li>
// <item> status: [end|cont]
// <item> ftp: <node-address>
// <item> dir: <ftp-directory>
// <item> file: <ftp-file>
// <item> data: ascii
// <item> arg: <arguments to call back to measuresdata
// </li>
// if the status is given as 'end' the program is finished; if it is given as
// 'cont' the script should obtain the file given by ftp, dir and file and
// call measuresdata back with all the info in arg.
// <item> loop with calls to measuresdata
// </li> 
// The arguments to measuresdata are given in casa 'Inputs' format. I.e.
// as 'key=value'. In the following the key is followed by default.
// <li>
// <item> type[all] [ALL|IERS|JPL|TAI_UTC|....] : type(s) of tables to produce
// <item> in[-] [<file-name to process> | -] : input ascii file.
// Normally determined by measuresdata program
// <item> refresh[n] [y|n] : force a table refresh within the minimum
// refresh period
// <item> renew[n] [y|n] : force a table renew, rather than an update, if 
// table has to be refreshed.
// <item> dir[./] <base directory for tables>
// </li>
// </synopsis>

//*************************************************************************//
// Declarations
//*************************************************************************//
Int last_mjd(const Table *tab);
Double today_mjd();
Double today_now();
String date_string(Double date);
String version_string(Double vs);
String get_version(const Table *tab);
Int int_data(const String &in);
Bool split_data(vector<String> &out, const String &in,
		const Regex &pat=RXwhite);

//*************************************************************************//
// Data structures and constants
//*************************************************************************//
// Version
const string PROG_VS = "20070711wnb";

// Inputs description
struct inputValues {
  // Data derived from argv inputs
  // Program version
  String appVersion;
  // Table type (e.g. TAI_UTC)
  String type;
  // Table data base directory (e.g. /aips++/data)
  String dir;
  // Input file name (e.g. ./tai.in)
  String in;
  // Refresh even if not necessary for this file
  Bool refresh;
  // Renew complete table, rather than update existing
  Bool renew;
  // Current pointer in list of processes
  uInt x__n;
  // Name of output link file
  String ofile;

  // Derived data
  // Test if to be updated/renewed
  Bool testOnly;
  // List of all types to do (e.g. TAI_UTC IERSeop97)
  vector<String> types;
  // Given intype (e.g. al)
  String intype;
  // Given proper input type (e.g. ALL)
  String fulltype;
  // Skip update
  Bool noup;
  // End of cyle
  Bool end;
};

// Default inputs
const inputValues defVal = {
  PROG_VS,		// Program version
  "all", 		// Table type
  ".",			// Table base directory
  "-",	 		// Name of input file (or - if unknown)
  False,		// Force refresh
  False,		// Force renew
  0,			// Current pointer in list of processes
  "measuresdata.link",	// Output link file
  // Derived
  True,
  vector<String>(),
  "",
  "",
  False,
  True
};

// Current inputs
inputValues inVal;

// Table properties
struct tableProperties {
  // Table type (e.g. TAI_UTC)
  String type;
  // Minimum update period in days
  Double updper;
  // Always renew this table, rather than refresh (normally False)
  Bool renew;
  // Table name (e.g. ephemerides/TAI_UTC)
  String tnam;
  // Connection protocol (ftp or html)
  String connectAs;
  // Data protocol (ascii)
  String protoc;
  // Remote file address (node, directory, file name)
  String fileAddress[3];
};

// Types that can be specified
const String intypes[][2] = {
  { "ALL", 	"IERS JPL"},
  { "IERS", 	"TAI_UTC"},
  { "JPL", 	""},
  { "TAI_UTC", 	"TAI_UTC"},
  { "", 	""}
};

const tableProperties allProperties[] = {
  //Type			Update period		Always renew
  //Table name			Connection protocol	Data protocol
  //File address
  { "TAI_UTC",			31.0,			True,
    "ephemerides/TAI_UTC",  	"ftp",  		"ascii",
    { "maia.usno.navy.mil", "ser7", "tai-utc.dat" } },
  { "" }						// last table
};					

// As vectors/maps
// All input types
vector<String> types;
// Expansion per input type
map<String, vector<String> > multypes;
// Properties
map<String, tableProperties> properties;

// Check existence properties
void fillProperty(vector<String> &field, const String &in) {
  for (uInt i=0;; ++i) {		// Check if properties exist
    if (allProperties[i].type.empty()) break;
    if (allProperties[i].type == in) {
      properties[in] = allProperties[i];
      field.push_back(in);
    };
  };
}

// Expand multiple fields
void expandTypes(vector<String> &field, const String &in) {
  vector<String> tmp;
  for (uInt i=0;; ++i) {
    if (intypes[i][0].empty()) break;
    if (intypes[i][0] == in && !split_data(tmp, intypes[i][1])) tmp.resize(0);
  };
  if ((!tmp.empty() && tmp.size() == 1 && tmp[0] == in) ||
      tmp.empty()) fillProperty(field, in);
  else for (uInt j=0; j<tmp.size(); ++j) expandTypes(field, tmp[j]);
}

// Create type lists
void makeMaps() {
  // Multiple types
  for (uInt i=0;; ++i) {
    vector<String> field; 
    if (intypes[i][0].empty()) break;
    types.push_back(intypes[i][0]);
    expandTypes(field, intypes[i][0]);
    if (!field.empty()) multypes[intypes[i][0]] = field;
  };
}

//*************************************************************************//
// General support routines
//*************************************************************************//
// Match a string (non-case sensitive) to a list
 uInt minimaxNC(const String &in, const vector<String> &tname) {
  String a;
  String b;
  uInt N_name(tname.size());
  uInt i(0);
  a = upcase(in);
  // Exact fit?
  for (i=0; i<N_name; i++) {
    if (a == upcase(tname[i])) break;
  };
  // Now look for partial
  if (i >= N_name) {
    uInt ia, ib;
    ia = a.length();
    for (i=0; i<N_name; i++) {
      ib = tname[i].length();
      ib = ia < ib ? ia : ib;
      b = upcase(tname[i]);
      if (a.at(0,Int(ib)) == b.at(0,Int(ib))) {
	uInt j;
	// Look for more partials
	for (j=i+1; j<N_name; j++) {
	  ib = tname[j].length();
	  ib = ia < ib ? ia : ib;
	  b = upcase(tname[j]);
	  if (a.at(0,Int(ib)) == b.at(0,Int(ib))) break;
	};
	// Found duplicate
	if (j<N_name) i=N_name;
	break;
      };
    };
  };
  return i;
}

String boolToString(Bool yn) {
  return (yn ? String("y") : String("n"));
}

String uIntToString(uInt yn) {
  String out;
  ostringstream sout(out);
  sout << yn;
  return sout.str();
}

// Get today's MJD
Double today_mjd() {
  return (floor(today_now()));
}

// Get now as MJD
Double today_now() {
  Quantity qdat;
  if (!Quantity::read(qdat, "today")) {
    throw (AipsError("Problems obtaining current time"));
  };
  return (qdat.getValue("d"));
}

// Get string from date
String date_string(Double date) {
  return (MVTime(date).string((MVTime::formatTypes)
			      (MVTime::YMD | MVTime::CLEAN), 4));
}

// Get version string
String version_string(Double vs) {
  String out;
  ostringstream sout(out);
  sout.setf(ios::fixed, ios::floatfield);
  sout << setw(9) << setfill('0') << setprecision(4) << vs;
  return sout.str();
}

// Split line (at pattern) into vector of strings. 
Bool split_data(vector<String> &out, const String &in,
		const Regex &pat) {
  out.resize(0);
  const Int maxn = 100;
  String sout[maxn];
  Int N = split(in, sout, maxn, pat);
  for (Int i=0; i<N; ++i) if (!sout[i].empty()) out.push_back(sout[i]);
  if (out.size()==0 || N==maxn) return False;
  return True;
}

// Make Double from string
Double double_data(const String &in) {
  Double x;
  istringstream(in) >> x;
  return x;
}

// Make Int from string
Int int_data(const String &in) {
  Int x;
  istringstream(in) >> x;
  return x;
}

//*************************************************************************//
// Table related routines
//*************************************************************************//
// Test if readable table exists
Bool testr_table(const String &tnam) {
  return Table::isReadable(tnam);
}

// Open readable table
Table *openr_table(const String &tnam) {
  if (!testr_table(tnam)) {
    throw (AipsError(String("Unexpected problem finding table ") + tnam));
  };
  return (new Table(tnam));
}

// Create new table if 'renew' or if not exists; else open existing.
// tnam is full path name; td is table descriptor;
// vs is initial version (normally 1.0);
// title is description; type is short name
Table *create_table(const String &tnam, const String &dnam,
		    TableDesc &td,
		    Double vs, const String &title,
		    const String &type, Bool renew=False) {
  // Test existence and renewal
  if (testr_table(tnam)) {
    if (renew || !Table::isWritable(tnam)) {
      Table t(tnam);
      vs = int_data(get_version(&t)) + 1.0;
      String oldt(tnam + ".old");
      t.rename(oldt, Table::New);
    };
  };    

  // Open existing or new table
  Table *tab;
  if (Table::isWritable(tnam)) {
    tab = new Table(tnam, Table::Update);
  } else {
    td.rwKeywordSet().define("VS_CREATE", (date_string(today_now())));
    td.rwKeywordSet().define("VS_DATE", (date_string(today_now())));
    td.rwKeywordSet().define("VS_VERSION", version_string(vs));
    td.rwKeywordSet().define("VS_TYPE", title);;;
    SetupNewTable newtab(tnam, td, Table::New);
    tab = new Table(newtab);
    TableInfo &info = tab->tableInfo();
    info.setType("IERS");
    info.setSubType(type);
  };
  return tab;
}

// Close table tab (with name tnam) and update version (if vsup>0);
// the version date (if timup True);
// and show the table time statistics (if timshow True).
Bool close_table(const String &tnam, Table *&tab,
		 Int vsup=0, Bool timup=True, Bool timshow=True) {
  String vs = get_version(tab);
  uInt n = tab->nrow();
  Int tim(0);
  if (timshow) tim = last_mjd(tab);
  if (vsup>0) {};
  if (timup) {};
  delete tab; tab = 0;
  cout << tnam << " table " << vs << " now with " << n << " entries"; 
  if (timshow) cout << " until " << tim;
  cout << endl;
  return True;
}

// Get last MJD in table
Int last_mjd(const Table *tab) {
  uInt n = tab->nrow();
  Double mjd;
  if (n<1) mjd = 0;
  else {
    ROScalarColumn<Double> col(*tab, "MJD");
    col.get(n-1, mjd);
  };
  return Int(mjd); 
}

// Obtain VS_DATE from table
Double vsdate_mjd(const Table *tab) {
  String dat; 
  tab->keywordSet().get(String("VS_DATE"), dat);
  Quantity qdat;
  if (!Quantity::read(qdat, dat)) {
    throw (AipsError("Illegal date in VS_DATE: " + dat));
  };
  return qdat.getValue("d");
}

// Obtain VS_VERSION from table
String get_version(const Table *tab) {
  String vs; 
  tab->keywordSet().get("VS_VERSION", vs);
  return vs;
}

//*************************************************************************//
// File read/write
//*************************************************************************//

// Read data from ascii input file. The table (for reference only) is tnam;
// input file is inpath; out is vector of file lines.
Bool read_data(vector<String> &out, const String &tnam,
	       const Path &in, Bool del=True) {
  out.resize(0);
  ifstream infile(in.absoluteName().c_str());
  if (!infile) {
    throw(AipsError(String("Cannot open the input data file ") +
		    in.absoluteName()));
  };
  String line;
  while (getline(infile, line)) out.push_back(line);
  infile.clear();
  infile.close();
  /// Still do delete of in
  return True;
}

// Write the measuresdata.link file.
Bool writeLink(const tableProperties &tprop, const inputValues &inVal) {
  Path pout(inVal.ofile);
  ofstream ofile(pout.absoluteName().c_str());
  if (!ofile) return False;
  if (inVal.end) ofile << "status: end" << endl;
  else {
    ofile << "status: cont"<< endl;
    ofile << tprop.connectAs << ": " << tprop.fileAddress[0] << endl;
    ofile << "dir: " << tprop.fileAddress[1] << endl;
    ofile << "file: " << tprop.fileAddress[2] << endl;
    ofile << "data: " << tprop.protoc << endl;
    ofile << "arg: " <<
      " in=" << tprop.fileAddress[2] <<
      " refresh=y" << " renew=" << boolToString(inVal.renew) <<
      " type=" << inVal.fulltype << " x__n=" << inVal.x__n <<
      " dir=" << inVal.dir <<
      endl;
  };
  ofile.close();
  return True;
}


//*************************************************************************//
// Routines to fill tables
//*************************************************************************//

// Fill ephemerides/TAI_UTC table
Bool TAI_UTC(const tableProperties &tprop, inputValues &inVal) {
  // Table description
  ScalarColumnDesc<Double> td0("MJD",
			       "", "IncrementalStMan", "IncrementalStMan");
  ScalarColumnDesc<Double> td1("dUTC",
			       "", "IncrementalStMan", "IncrementalStMan");
  ScalarColumnDesc<Double> td2("Offset",
			       "", "IncrementalStMan", "IncrementalStMan");
  ScalarColumnDesc<Double> td3("Multiplier",
			       "", "IncrementalStMan", "IncrementalStMan");
  td0.rwKeywordSet().define("UNIT", "d");
  td1.rwKeywordSet().define("UNIT", "s");
  td2.rwKeywordSet().define("UNIT", "d");
  td3.rwKeywordSet().define("UNIT", "s");
  String tpath = inVal.dir+ "/" + tprop.tnam;
  // Test if to update
  if (!inVal.refresh && testr_table(tpath)) {
    Table *tab = openr_table(tpath);
    if (today_mjd()-vsdate_mjd(tab) < tprop.updper) {
      cout << tprop.tnam << " is up-to-date" << endl;
      close_table(tpath, tab, 0, False);
      inVal.noup = True;
      return True;
    }; 
    delete tab; tab = 0;
  };

  // Check if in present and to be used
  if (inVal.testOnly) return True;

  // Read the data file
  vector<String> lines;
  read_data(lines, tpath, Path(inVal.in), False);

  // Split data lines into fields
  vector<vector<String> > fields;
  vector<String> field;
  for (uInt i=0; i<lines.size(); ++i) {
    if (split_data(field, lines[i])) fields.push_back(field);
  };

  // Check format.
  if (fields.size() < 10 ||
      fields[0].size() < 15 ||
      fields[0].size() != fields.back().size()) {
    throw (AipsError("Incorrect input file for " + tprop.tnam));
  };

  // Create table fields
  vector<Double> mjd;
  vector<Double> dut;
  vector<Double> off;
  vector<Double> mul;
  for (uInt i=0; i<fields.size(); ++i) {
    mjd.push_back(double_data(fields[i][4]) - 2400000.5);
    dut.push_back(double_data(fields[i][6]));
    off.push_back(double_data(fields[i][11]));
    mul.push_back(double_data(fields[i][13]));
  };

  // Test looks
  if (mjd[0] != 37300 || mul[0] != 0.001296) {
    throw (AipsError("Format for data file incorrect for " + tprop.tnam));
  };

  // Create table
  TableDesc td("TAI_UTC", TableDesc::Scratch);
  td.addColumn(td0);
  td.addColumn(td1);
  td.addColumn(td2);
  td.addColumn(td3);
  Table *tab = create_table(tpath, "TAI_UTC", td, 1.0,
			    "TAI_UTC difference obtained from USNO",
			    "leapSecond", True);

  // Fill data
  tab->rwKeywordSet().define("MJD0", 37200);
  tab->rwKeywordSet().define("dMJD", 0.0);
  tab->addRow((mjd.size()-tab->nrow()));
  ScalarColumn<Double> cmjd(*tab, "MJD");
  ScalarColumn<Double> cdut(*tab, "dUTC");
  ScalarColumn<Double> coff(*tab, "Offset");
  ScalarColumn<Double> cmul(*tab, "Multiplier");
  cmjd.putColumn((Vector<Double>(mjd)));
  cdut.putColumn((Vector<Double>(dut)));
  coff.putColumn((Vector<Double>(off)));
  cmul.putColumn((Vector<Double>(mul)));

  // Ready
  close_table(tpath, tab, 0);
  
  // OK
  return True;
}

//*************************************************************************//
// Main program
//*************************************************************************//
int main (Int argc, const char* argv[])
{
  try {
    cout << " " << endl;
    cout << "Create data tables for Measures" << endl;
    cout << "-----------------------------------------------" << endl;
    
    // Make type lists
    makeMaps();
//*************************************************************************//
// Program inputs
//*************************************************************************//
    // Enable input in no-prompt mode
    Input inputs(1);

    // Define the input structure and version
    // Set defaults
    inVal = defVal;
    inputs.version(inVal.appVersion);
    inputs.create("type", inVal.type,
		  "Type of table to create", "string");
    inputs.create("dir", inVal.dir,
		  "Path to data table base directory",
		  "string");
    inputs.create("in", inVal.in,
		  "Name of input (ASCII) file", "string");
    inputs.create("x__n", uIntToString(inVal.x__n),
		  "Hidden: pointer into execution list", "int");
    inputs.create("refresh", boolToString(inVal.refresh),
		  "Force refresh, even when not needed yet", "bool");
    inputs.create("renew", boolToString(inVal.renew),
		  "force table renew when an update would suffice", "bool");

    // Fill the input structure from the command line.
    inputs.readArguments(argc, argv);

    // Create output link file ///
    Path pout(inVal.ofile);
    RegularFile fout(pout);
    fout.remove();
    if (!fout.canCreate()) {
      throw (AipsError("Cannot create in ./ the link file " +
		       pout.absoluteName()));
    };

    // Check the type
    inVal.intype = inputs.getString("type");
    cout << "The requested type is: " << inVal.intype << endl;
    uInt itype = minimaxNC(inVal.intype, types);
    if (itype >= types.size()) {
      throw (AipsError("Unknown type requested"));
    };
    inVal.fulltype = types[itype];
    if (multypes.count(inVal.fulltype)) inVal.types = multypes[inVal.fulltype];
    cout << "The processed type[s]:";
    for (uInt i=0; i<inVal.types.size(); ++i) cout << " " << inVal.types[i];
    cout << endl; 
    inVal.x__n = inputs.getInt("x__n");
    if (!inVal.types.empty() && inVal.x__n >= inVal.types.size()) {
      throw (AipsError("Program error: pointer outside processing list"));
    };

    // Get and check the table directory specification
    inVal.dir = inputs.getString("dir");
    if (inVal.dir == "") {
      throw (AipsError("The data table path must be given"));
    };
    Path dirpath(inVal.dir);
    cout << "The data table directory: " << dirpath.absoluteName() << endl;
    if (!dirpath.isValid()) {
      throw (AipsError("The table directory path is not valid"));
    };
    if (!File(dirpath).exists()) {
      throw (AipsError("The table directory path does not exist"));
    };
    if (!File(dirpath).isWritable()) {
      throw (AipsError("The table directory path is not writable"));
    };

    // Get and check the ASCII input file
    inVal.in = inputs.getString("in");
    inVal.testOnly = True;
    Path inpath;
    if (inVal.in == "-") {
      cout << "Check and request mode only" << endl;
    } else {
      inVal.testOnly = False;
      inpath = Path(inVal.in);
      cout << "The input data file: " << inpath.absoluteName() << endl;
      if (!inpath.isValid()) {
	throw (AipsError("The input data file path is not valid"));
      };
      if (!File(inpath).exists()) {
	throw (AipsError("The input data file does not exist"));
      };
      if (!File(inpath).isReadable()) {
	throw (AipsError("The input data file is not readable"));
      };
    };

    // Get and check flags
    inVal.refresh = inputs.getBool("refresh");
    if (!inVal.refresh) cout << "No f";
    else cout << "F";
    cout << "orced refresh asked" << endl;
  
    inVal.renew = inputs.getBool("renew");   
    if (!inVal.renew) cout << "No e";
    else cout << "E";
    cout << "xplicit renew asked" << endl;

  } catch (AipsError x) {
    cout << x.getMesg() << endl;
    exit(1);
  } 
   
//*************************************************************************//
//  Create proper Table[s]
//*************************************************************************//
  try {
    if (!inVal.types.empty()) {
      inVal.end = True;
      while (inVal.x__n < inVal.types.size()) {
	inVal.type = inVal.types[inVal.x__n];
	++inVal.x__n;
	cout << "Processing " << inVal.type << endl;
	if (inVal.type == "TAI_UTC") {
	  TAI_UTC(properties[inVal.type], inVal);
	} else continue;
	if (inVal.noup || !inVal.testOnly) continue;
	--inVal.x__n;
	inVal.end = False;
	break;
      };
    };
    writeLink(properties[inVal.type], inVal);

//*************************************************************************//
// Finish
//*************************************************************************//

  } catch (AipsError x) {
    cout << x.getMesg() << endl;
    exit(1);
  } 

  cout << "measuresdata ended normally" << endl;
  exit(0);
}
