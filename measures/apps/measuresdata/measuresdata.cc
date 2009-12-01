//# measuresdata.cc: Program to read IERS and other data for Measure conversion
//# Copyright (C) 2007-2008
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
//# $Id: measuresdata.cc,v 1.12 2008/11/01 19:11:28 wbrouw Exp $


//# Includes
#include <tables/Tables/SetupNewTab.h>
#include <tables/Tables/Table.h>
#include <tables/Tables/TableDesc.h>
#include <tables/Tables/TableInfo.h>
#include <tables/Tables/ArrayColumn.h>
#include <tables/Tables/ScaColDesc.h>
#include <tables/Tables/ScalarColumn.h>
#include <tables/Tables/ArrColDesc.h>
#include <tables/Tables/BaseColDesc.h>
#include <tables/Tables/TableRecord.h>
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
#include <casa/stdio.h>
#include <map>
#include <vector>

#include <casa/namespace.h>


// Version
const string PROG_VS = "20081102wnb";


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
// <item> derange[1960,now+20] [yyyy,yyyy] : the wanted range for the JPL
// planetary tables.
// <item> dir[./] <base directory for tables>
// </li>
// </synopsis>

//*************************************************************************//
// Declarations
//*************************************************************************//

// Structures
struct tableProperties;
struct inputValues;
struct columnDescr;
struct formatDescr;

// Routines
Int last_mjd(const Table *tab);
Double today_mjd();
Double today_now();
Double double_data(const String &in);
String date_string(Double date);
String version_string(Double vs, uInt w=9, uInt p=4);
Double vsdate_mjd(const Table *tab);
String get_version(const Table *tab);
Double dget_version(const Table *tab);
Double dget_tversion(const Table *tab);
void put_version(Table *tab, Double vs);
void put_vsdate(Table *tab);
Int int_data(const String &in);
Bool split_data(vector<String> &out, const String &in,
		const Regex &pat=RXwhite);
Table *openr_table(const String &tnam);
Bool close_table(const String &tnam, Table *&tab,
		 Double vsup=0, Bool timup=True, Bool timshow=True);
Bool IERSeop(tableProperties &tprop, inputValues &inVal);
Bool IERSpred(tableProperties &tprop, inputValues &inVal);
Bool JPLDE(tableProperties &tprop, inputValues &inVal);
Bool TAI_UTC(tableProperties &tprop, inputValues &inVal);
Bool IERSeop97(tableProperties &tprop, inputValues &inVal);
Bool IERSeop2000(tableProperties &tprop, inputValues &inVal);
Bool IERSpredict(tableProperties &tprop, inputValues &inVal);
Bool IERSpredict2000(tableProperties &tprop, inputValues &inVal);
Bool IGRF(tableProperties &tprop, inputValues &inVal);
Bool DE200(tableProperties &tprop, inputValues &inVal);
Bool DE405(tableProperties &tprop, inputValues &inVal);

//*************************************************************************//
// Data structures and constants
//*************************************************************************//

// Inputs description
struct inputValues {
  
  // Data derived from argv inputs
  String appVersion;	// Program version
  String type;		// Table type (e.g. TAI_UTC)
  String dir;		// Table data base directory (e.g. /aips++/data)
  String in;		// Input file name (e.g. ./tai.in)
  Bool refresh;		// Refresh, even if not necesaary for this file
  Bool renew;  		// Force renew complete table, rather than an update
  Block<Int> derange;	// Range of DE table years.
  String ofile;  	// Name of output link file
  uInt x__n;  		// Current pointer in list of processes
  Bool x__rep;		// Repeating
  Bool x__fn;		// Should be a filename given
  vector<Double> x__val;// Parameter values
  // Derived data
  Bool testOnly;  	// Test if to be updated/renewed
  vector<String> types;	// List of all types to do (e.g. TAI_UTC IERSeop97)
  String intype;  	// Given intype (e.g. al)
  String fulltype;  	// Given proper input type (e.g. ALL)
  Double lastmjd;	// Current last mjd
  Bool noup;  		// Skip update
  Bool forcedel;  	// Force delete
  Bool end;  		// End of cyle
};

// Default inputs
const inputValues defVal = {
  PROG_VS,		// Program version
  "all", 		// Table type
  ".",			// Table base directory
  "-",	 		// Name of input file (or - if unknown)
  False,		// Force refresh
  False,		// Force renew
  Block<Int>(2),	// DE table range
  "measuresdata.rc",	// Output rc file
  0,			// Current pointer in list of processes
  False,		// Repeating
  False,		// Should be a filename given
  vector<Double>(),	// Parameter values
  // Derived
  True,			// Test if to be updated/renewed
  vector<String>(),	// All types to do
  "",			// Given type in input
  "",			// Full input type name
  0.0,			// Current last mjd
  False,		// Skip update
  False,		// Force delete
  True			// End of cycle
};

// Current inputs
inputValues inVal;

//*************************************************************************//

// Format descriptor
struct formatDescr {
  // Format types
  enum FormTypes {
    X,			// Skip
    A,			// ASCII
    I,			// Int
    F,			// Float
    N_FormTypes		// Number of formats
  };
  FormTypes form;	// Format
  size_t start;		// Start address in string
  size_t n;		// Number in sub-string
};

// IERSpredict
const String IERSpredictFormat = String("") +
"i2 i2 i2 x f8 x a1 x f9 f9 x f9 f9 x2 " +
"a1 f10 f10 x f7 f7 x2 a1 x f9 f9 x f9 f9 X51";

// IERSpredict2000
const String &IERSpredict2000Format = IERSpredictFormat;

// IGRF
const String IGRFFormat = String("") +
"a1 x2 i3 i3 f9 f9 f9 f9 f9 f9 f9 f9 f9 f9 f9" +
" f9 f9 f9 f9 f9 f9 f9 f9 f9 f9 f9 f9";  // 22 coefficients and SV

//*************************************************************************//

// Column descriptor
struct columnDescr {
  // Column types
  enum ColTypes {
    CTD,		// Double
    CTI,		// Int
    CTS,		// String
    CTAD,		// Double array
    N_ColTypes		// Number of types
  };
  String colName;	// Name of column
  String unit;		// Units in column
  ColTypes colType;	// Column type
  uInt colId;		// Number input column
};

// TAI_UTC
const columnDescr TAI_UTCCol[] = {
  {"MJD", 	"d", 	columnDescr::CTD, 4},
  {"dUTC", 	"s", 	columnDescr::CTD, 6},
  {"Offset", 	"d", 	columnDescr::CTD, 11},
  {"Multiplier","s", 	columnDescr::CTD, 13},
  {"",          "",     columnDescr::N_ColTypes, 0} };

// IERSeop97
const columnDescr IERSeop97Col[] = {
  {"MJD",	"d", 		columnDescr::CTD, 3},
  {"x", 	"arcsec", 	columnDescr::CTD, 4},
  {"Dx", 	"arcsec",	columnDescr::CTD, 10},
  {"y", 	"arcsec", 	columnDescr::CTD, 5},
  {"Dy", 	"arcsec", 	columnDescr::CTD, 11},
  {"dUT1", 	"s", 		columnDescr::CTD, 6},
  {"DdUT1", 	"s", 		columnDescr::CTD, 12},
  {"LOD", 	"s", 		columnDescr::CTD, 7},
  {"DLOD", 	"s", 		columnDescr::CTD, 13},
  {"dPsi", 	"arcsec", 	columnDescr::CTD, 8},
  {"DdPsi", 	"arcsec", 	columnDescr::CTD, 14},
  {"dEps", 	"arcsec",	columnDescr::CTD, 9},
  {"DdEps", 	"arcsec", 	columnDescr::CTD, 15},
  {"",          "",             columnDescr::N_ColTypes, 0} };

// IERSeop2000
const columnDescr IERSeop2000Col[] = {
  {"MJD",	"d", 		columnDescr::CTD, 3},
  {"x", 	"arcsec", 	columnDescr::CTD, 4},
  {"Dx", 	"arcsec",	columnDescr::CTD, 10},
  {"y", 	"arcsec", 	columnDescr::CTD, 5},
  {"Dy", 	"arcsec", 	columnDescr::CTD, 11},
  {"dUT1", 	"s", 		columnDescr::CTD, 6},
  {"DdUT1", 	"s", 		columnDescr::CTD, 12},
  {"LOD", 	"s", 		columnDescr::CTD, 7},
  {"DLOD", 	"s", 		columnDescr::CTD, 13},
  {"dX", 	"arcsec", 	columnDescr::CTD, 8},
  {"DdX", 	"arcsec", 	columnDescr::CTD, 14},
  {"dY", 	"arcsec",	columnDescr::CTD, 9},
  {"DdY", 	"arcsec", 	columnDescr::CTD, 15},
  {"",          "",             columnDescr::N_ColTypes, 0} };

// IERSpredict
const columnDescr IERSpredictCol[] = {
  {"MJD",	"d", 		columnDescr::CTD, 3},
  {"x", 	"arcsec", 	columnDescr::CTD, 5},
  {"Dx", 	"arcsec",	columnDescr::CTD, 6},
  {"y", 	"arcsec", 	columnDescr::CTD, 7},
  {"Dy", 	"arcsec", 	columnDescr::CTD, 8},
  {"dUT1", 	"s", 		columnDescr::CTD, 10},
  {"DdUT1", 	"s", 		columnDescr::CTD, 11},
  {"LOD", 	"s", 		columnDescr::CTD, 12},
  {"DLOD", 	"s", 		columnDescr::CTD, 13},
  {"dPsi", 	"arcsec", 	columnDescr::CTD, 15},
  {"DdPsi", 	"arcsec", 	columnDescr::CTD, 16},
  {"dEps", 	"arcsec",	columnDescr::CTD, 17},
  {"DdEps", 	"arcsec", 	columnDescr::CTD, 18},
  {"",          "",             columnDescr::N_ColTypes, 0} };

// IERSpredict2000
const columnDescr IERSpredict2000Col[] = {
  {"MJD",	"d", 		columnDescr::CTD, 3},
  {"x", 	"arcsec", 	columnDescr::CTD, 5},
  {"Dx", 	"arcsec",	columnDescr::CTD, 6},
  {"y", 	"arcsec", 	columnDescr::CTD, 7},
  {"Dy", 	"arcsec", 	columnDescr::CTD, 8},
  {"dUT1", 	"s", 		columnDescr::CTD, 10},
  {"DdUT1", 	"s", 		columnDescr::CTD, 11},
  {"LOD", 	"s", 		columnDescr::CTD, 12},
  {"DLOD", 	"s", 		columnDescr::CTD, 13},
  {"dX", 	"arcsec", 	columnDescr::CTD, 15},
  {"DdX", 	"arcsec", 	columnDescr::CTD, 16},
  {"dY", 	"arcsec",	columnDescr::CTD, 17},
  {"DdY", 	"arcsec", 	columnDescr::CTD, 18},
  {"",          "",             columnDescr::N_ColTypes, 0} };

// IGRF
const columnDescr IGRFCol[] = {
  {"MJD",	"d", 		columnDescr::CTD,  0},
  {"COEF",	"nT/km",	columnDescr::CTAD, 0},
  {"dCOEF",	"nT/km/a",	columnDescr::CTAD, 0},
  {"",          "",             columnDescr::N_ColTypes, 0} };

// DE200
const columnDescr DE200Col[] = {
  {"MJD",	"d", 		columnDescr::CTD,  0},
  {"x",		"",		columnDescr::CTAD, 0},
  {"",          "",             columnDescr::N_ColTypes, 0} };

// DE405
const columnDescr DE405Col[] = {
  {"MJD",	"d", 		columnDescr::CTD,  0},
  {"x",		"",		columnDescr::CTAD, 0},
  {"",          "",             columnDescr::N_ColTypes, 0} };

//*************************************************************************//

// Table properties
struct tableProperties {
  String type;  	// Table type (e.g. TAI_UTC)
  Double version;  	// Double version of table
  Double updper;  	// Minimum update period in days
  Bool renew;  		// Always renew, not update this table (normally False)
  Double MJD0;  	// Start MJD of table
  Double dMJD;  	// Increment MJD in table
  String tnam;  	// Table name (e.g. geodetic/TAI_UTC)
  String connectAs;  	// Connection protocol (ftp or html)
  String protoc;  	// Data protocol (ascii)
  Bool (*rout) (tableProperties &, inputValues &); // Routine to call
  const columnDescr *cdesc; // Column descriptions
  TableDesc *td;	// Table descriptor
  vector<String> colnames; // Column names
  vector<uInt> colids;	// Input column id
  vector<TableColumn *> columns; // Table columns for access
  String title;		// Long title
  String contents;	// Contents indicator; e.g. leapSecond
  Bool repeat;		// More than one input possible
  String info;		// Information fields space separated 
  vector<String> vinfo;	// Information fields
  const String *formatString; // Format string
  vector<formatDescr> fdesc; // Input format descriptions
  String fileAddress[3];// Remote file address (node, directory, file name)
};

// Types that can be specified
const String intypes[][2] = {
  { "ALL", 		"IERS IGRF"}, //-- JPL IGRF"},
  { "IERS", 		"TAI_UTC EOP Predict"},
  { "EOP",		"IERSeop97 IERSeop2000"},
  { "Predict",		"IERSpredict IERSpredict2000"},
  { "JPL", 		"DE200 DE405"},
  { "TAI_UTC", 		"TAI_UTC"},
  { "IERSeop97",	"IERSeop97"},
  { "IERSeop2000",	"IERSeop2000"},
  { "IERSpredict",	"IERSpredict"},
  { "IERSpredict2000",	"IERSpredict2000"},
  { "IGRF", 		"IGRF"},
  { "DE200",		"DE200"},
  { "DE405",		"DE405"},
  { "", ""} };

const tableProperties allProperties[] = {
  //Type			Version			Update period
  //Always renew		MJD0			dMJD
  //Table name			Connection protocol	Data protocol
  //Routine			Columns			Table descriptor
  //Column names		Column ids		Columns
  //Title						Contents
  //Repeat			Info			Vector info
  //Format string		Input format descriptions 
  //File address

  { "TAI_UTC",			1.0,			31.0,
    True,			37300,			0.0,
    "geodetic/TAI_UTC",  	"ftp",  		"ascii",
    &TAI_UTC,			TAI_UTCCol,		0,
    vector<String>(),		vector<uInt>(),		vector<TableColumn*>(),
    "TAI_UTC difference obtained from USNO",		"leapSecond",
    False,			"",			vector<String>(),
    0, 				vector<formatDescr>(),
    { "maia.usno.navy.mil", "ser7", "tai-utc.dat" } },

  //**********************************************************************//

  { "IERSeop97",		2.0,			4.0,
    False,			37664,			1.0,
    "geodetic/IERSeop97",  	"ftp",  		"ascii",
    &IERSeop97,			IERSeop97Col,		0,
    vector<String>(),		vector<uInt>(),		vector<TableColumn*>(),
    "IERS EOPC04_05 Earth Orientation Data from IERS",	"eop97",
    True,			"",			vector<String>(),
    0, 				vector<formatDescr>(),
    { "hpiers.obspm.fr", "iers/eop/eopc04_05", "eopc04.xx" } },

  //**********************************************************************//

  { "IERSeop2000",		2.0,			4.0,
    False,			37664,			1.0,
    "geodetic/IERSeop2000", 	"ftp",  		"ascii",
    &IERSeop2000,		IERSeop2000Col,		0,
    vector<String>(),		vector<uInt>(),		vector<TableColumn*>(),
    "IERS EOP2000C04_05 Earth Orientation Data IAU2000","eop2000",
    True,			"",			vector<String>(),
    0, 				vector<formatDescr>(),
    { "hpiers.obspm.fr", "iers/eop/eopc04_05", "eopc04_IAU2000.xx" } },

  //**********************************************************************//

  { "IERSpredict",		2.0,			3.0,
    False,			0.0,			1.0,
    "geodetic/IERSpredict",  	"ftp",  		"ascii",
    &IERSpredict,		IERSpredictCol,		0,
    vector<String>(),		vector<uInt>(),		vector<TableColumn*>(),
    "IERS Earth Orientation Data predicted from NEOS",	"predict",
    False,			"",			vector<String>(),
    &IERSpredictFormat,		vector<formatDescr>(),
    { "maia.usno.navy.mil", "ser7", "finals.daily" } },

  //**********************************************************************//

  { "IERSpredict2000",		2.0,			3.0,
    False,			0.0,			1.0,
    "geodetic/IERSpredict2000",	"ftp",  		"ascii",
    &IERSpredict2000,		IERSpredict2000Col,	0,
    vector<String>(),		vector<uInt>(),		vector<TableColumn*>(),
    "IERS EOP2000C04_05 Earth Orientation Data IAU2000","predict2000",
    False,			"",			vector<String>(),
    &IERSpredict2000Format,	vector<formatDescr>(),
    { "maia.usno.navy.mil", "ser7", "finals2000A.daily" } },

  //**********************************************************************//

  { "IGRF",			2.0,			180.0,
    True,			13193.75,		1826.25,
    "geodetic/IGRF",		"http",  		"ascii",
    &IGRF,			IGRFCol,		0,
    vector<String>(),		vector<uInt>(),		vector<TableColumn*>(),
    "IGRF10 reference magnetic field",			"earthField",
    False,			"",			vector<String>(),
    &IGRFFormat,		vector<formatDescr>(),
    { "www.ngdc.noaa.gov", "IAGA/vmod", "igrf10coeffs.txt" } },

  //**********************************************************************//

  { "DE200",			2.0,			0.0,
    False,			0.0,			0.0,
    "ephemerides/DE200",	"ftp",  		"ascii",
    &DE200,			DE200Col,		0,
    vector<String>(),		vector<uInt>(),		vector<TableColumn*>(),
    "JPL Planetary ephemeris DE200",			"DE200",
    True,			"header.200 ascp****.200", vector<String>(),
    0,				vector<formatDescr>(),
    { "ssd.jpl.nasa.gov", "pub/eph/planets/ascii/de200", "" } },

  //**********************************************************************//

  { "DE405",			2.0,			0.0,
    False,			0.0,			0.0,
    "ephemerides/DE405",	"ftp",  		"ascii",
    &DE405,			DE405Col,		0,
    vector<String>(),		vector<uInt>(),		vector<TableColumn*>(),
    "JPL Planetary ephemeris DE405",			"DE405",
    True,			"header405 ascp****.405", vector<String>(),
    0,				vector<formatDescr>(),
    { "ssd.jpl.nasa.gov", "pub/eph/planets/ascii/de405", "" } },

  //**********************************************************************//

  { "",  			0.0,			0.0,
    False,			0.0,			0.0,
    "",                 	"",     		"",
    0,	        		DE405Col,		0,
    vector<String>(),		vector<uInt>(),		vector<TableColumn*>(),
    "",			        "",
    True,			"",                     vector<String>(),
    0,				vector<formatDescr>(),
    { "", "", "" } }    // last table
};					

// As vectors/maps
// All input types
vector<String> types;
// Expansion per input type
map<String, vector<String> > multypes;
// Properties
map<String, tableProperties> properties;

// Check existence of and fill properties
void fillProperty(vector<String> &field, const String &in) {
  for (uInt i=0;; ++i) {		// Check if properties exist
    if (allProperties[i].type.empty()) break;
    if (allProperties[i].type == in) {
      properties[in] = allProperties[i];
      field.push_back(in);
    };
  };
}

// Expand multiple fields and fill properties and types
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

// Create type related lists and maps
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

// Create table properties
void makeProperties() {
  // Create Table descriptor
  for (uInt i=0;; ++i) {
    if (allProperties[i].type.empty()) break;
    properties[allProperties[i].type].td =
      new TableDesc(allProperties[i].type, TableDesc::Scratch);
    for (uInt j=0;; ++j) {
      if (allProperties[i].cdesc[j].colName.empty()) break;
      properties[allProperties[i].type].colnames
	.push_back(allProperties[i].cdesc[j].colName);
      properties[allProperties[i].type].colids
	.push_back(allProperties[i].cdesc[j].colId);
      BaseColumnDesc *tcd = 0;
      switch (allProperties[i].cdesc[j].colType) {
      case columnDescr::CTD:
	tcd = new ScalarColumnDesc<Double>
	  (allProperties[i].cdesc[j].colName, "",
	   "IncrementalStMan", "IncrementalStMan");
	if (!allProperties[i].cdesc[j].unit.empty()) {
	  tcd->rwKeywordSet().define("UNIT", allProperties[i].cdesc[j].unit);
	};
	break;
      case columnDescr::CTI:
	tcd = new ScalarColumnDesc<Int>
	  (allProperties[i].cdesc[j].colName, "",
	   "IncrementalStMan", "IncrementalStMan");
	if (!allProperties[i].cdesc[j].unit.empty()) {
	  tcd->rwKeywordSet().define("UNIT", allProperties[i].cdesc[j].unit);
	};
	break;
      case columnDescr::CTS:
	tcd = new ScalarColumnDesc<String>
	  (allProperties[i].cdesc[j].colName, "",
	   "IncrementalStMan", "IncrementalStMan");
	if (!allProperties[i].cdesc[j].unit.empty()) {
	  tcd->rwKeywordSet().define("UNIT", allProperties[i].cdesc[j].unit);
	};
	break;
      case columnDescr::CTAD:
	tcd = new ArrayColumnDesc<Double>
	  (allProperties[i].cdesc[j].colName, "",
	   "IncrementalStMan", "IncrementalStMan", 1);
	if (!allProperties[i].cdesc[j].unit.empty()) {
	  tcd->rwKeywordSet().define("UNIT", allProperties[i].cdesc[j].unit);
	};
	break;
      default:
	throw (AipsError("Program error: undefined column type used"));
	break;
      };
      properties[allProperties[i].type].td->addColumn(*tcd);
      delete tcd; tcd=0;				    
    };
  };

  // Create format descriptors
  for (uInt i=0;; ++i) {
    if (allProperties[i].type.empty()) break; 		// End of list
    if (!allProperties[i].formatString) continue;	// No format given
    // Split the long format
    vector<String> tmp;
    if (!split_data(tmp, *allProperties[i].formatString)) tmp.resize(0);
    uInt off(0);
    for (uInt j=0; j<tmp.size(); ++j) {
      formatDescr fd = { formatDescr::X, off, 0 };
      if (upcase(tmp[j]) == "X") fd.n = 1;
      else if (upcase(tmp[j]) == "A") fd.n = 1;
      else if (tmp[j].size()<2) throw(AipsError("Illegal Format specifier"));
      else {
	fd.n = int_data(tmp[j].from(1));
	if (upcase(tmp[j])[0] == 'X');
	else if (upcase(tmp[j])[0] == 'A') fd.form = formatDescr::A;	
	else if (upcase(tmp[j])[0] == 'I') fd.form = formatDescr::I;	
	else if (upcase(tmp[j])[0] == 'F') fd.form = formatDescr::F;	
	else  throw(AipsError("Unknown  Format specifier"));
      };
      off += fd.n;
      if (fd.form != formatDescr::X) 
	properties[allProperties[i].type].fdesc.push_back(fd);
    };
  };

  // Create info vector
  for (uInt i=0;; ++i) {
    if (allProperties[i].type.empty()) break; 		// End of list
    if (allProperties[i].info.empty()) continue;	// No info given
    // Split the info fields
    if (!split_data(properties[allProperties[i].type].vinfo,
		    allProperties[i].info)) {
      properties[allProperties[i].type].vinfo.resize(0);
    };
  };
}

// Remove columns 
void rmColumns(Table *, tableProperties &tprop) {
    for (uInt j=0; j<tprop.columns.size(); ++j) {
      delete tprop.columns[j]; tprop.columns[j] = 0;
    };
    tprop.columns.resize(0);
}

// Create columns
void createColumns(Table *tab, tableProperties &tprop) {
  rmColumns(tab, tprop);
  for (uInt j=0; j<tprop.colnames.size(); ++j) {
    switch (tprop.cdesc[j].colType) {
    case columnDescr::CTD:
      tprop.columns.push_back
	(new ScalarColumn<Double>(*tab, tprop.colnames[j]));
      break;
    case columnDescr::CTI:
      tprop.columns.push_back
	(new ScalarColumn<Int>(*tab, tprop.colnames[j]));
      break;
    case columnDescr::CTS:
      tprop.columns.push_back
	(new ScalarColumn<String>(*tab, tprop.colnames[j]));
      break;
    case columnDescr::CTAD:
      tprop.columns.push_back
	(new ArrayColumn<Double>(*tab, tprop.colnames[j]));
      break;
    default:
      throw (AipsError("Program error: undefined column type used"));
      break;
    };
  };
}

//*************************************************************************//
// General support routines
//*************************************************************************//

// Match a string (non-case sensitive) to a list
String minimaxNC(const String &in, const vector<String> &tname) {
  String a;
  String b;
  uInt N_name(tname.size());
  uInt i(0);
  a = upcase(in);
  // Exact fit?
  for (i=0; i<N_name; i++) if (a == upcase(tname[i])) break;
  // Now look for partial
  if (i >= N_name) {
    size_t ia, ib;
    ia = a.length();
    for (i=0; i<N_name; i++) {
      ib = tname[i].length();
      ib = ia < ib ? ia : ib;
      b = upcase(tname[i]);
      if (ia==ib && a.at(0,ib) == b.at(0,ib)) {
	uInt j;
	// Look for more partials
	for (j=i+1; j<N_name; j++) {
	  ib = tname[j].length();
	  ib = ia < ib ? ia : ib;
	  b = upcase(tname[j]);
	  if (ia==ib && a.at(0,ib) == b.at(0,ib)) break;
	};
	// Found duplicate
	if (j<N_name) i=N_name;
	break;
      };
    };
  };
  if (i<N_name) return tname[i];
  return String("");
}

String boolToString(Bool yn) { return (yn ? String("y") : String("n")); }

String uIntToString(uInt yn) {
  String out;
  ostringstream sout(out);
  sout << yn;
  return sout.str();
}

String blockIntToString(Block<Int> yn) {
  String out;
  for (uInt i=0; i<yn.nelements(); ++i) {
    if (i>0) out += ",";
    out += uIntToString(uInt(yn[i]));
  };
  return out;
}

// Get today's MJD
Double today_mjd() { return (floor(today_now())); }

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
String version_string(Double vs, uInt w, uInt p) {
  String out;
  ostringstream sout(out);
  sout.setf(ios::fixed, ios::floatfield);
  sout << setw(w) << setfill('0') << setprecision(p) << vs;
  return sout.str();
}

// Split line (at pattern) into vector of strings. 
Bool split_data(vector<String> &out, const String &in, const Regex &pat) {
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
  Regex tst(RXdouble); 
  Double x;
  Int mlen;
  if (tst.find(in.c_str(), in.size(), mlen) == String::npos) x = 0;
  else {
    String tmp(in);
    if (Int(tmp.size())>mlen &&
	(tmp[mlen]=='D' || tmp[mlen]=='d')) tmp[mlen] = 'e';
    istringstream(tmp) >> x;
  };
  return x;
}

// Make Int from string
Int int_data(const String &in) {
  Regex tst(RXint); 
  Int x;
  Int mlen;
  if (tst.find(in.c_str(), in.size(), mlen) == String::npos) x = 0;
  else istringstream(in) >> x;
  return x;
}

//*************************************************************************//
// Table related routines
//*************************************************************************//

// Test if readable table exists
Bool testr_table(const String &tnam) { return Table::isReadable(tnam); }

//*************************************************************************//
// Test if table tnam has to be updated. tprop are the table properties;
// inVal the switches
Bool testu_table(const tableProperties &tprop, inputValues &inVal) {
  inVal.noup = True;			// Assume no update needed
  inVal.forcedel = False;		// Assume no forced delete
  inVal.lastmjd = 0.0;
  // Find if update needed
  if (inVal.x__fn) {			// Input file expected
    if (File(Path(inVal.in)).isReadable()) inVal.noup = False;
    else cout << "No expected input file " << inVal.in << endl; 
  } else if (!testr_table(tprop.tnam)) inVal.noup = False; // No table yet
  if (testr_table(tprop.tnam)) {
    Table *tab = openr_table(tprop.tnam);
    inVal.lastmjd = last_mjd(tab);
    if (inVal.noup) {
      if (dget_tversion(tab) < tprop.version) { // A new program version
	inVal.forcedel = True;
	inVal.noup = False;
      } else if (tab->nrow() && tprop.updper != 0.0 && 
		 today_mjd()-vsdate_mjd(tab) >= tprop.updper) {
	inVal.noup = False;		// Update period passed
      } else if (tprop.repeat && today_mjd()-inVal.lastmjd > 2*tprop.updper) {
	inVal.noup = False;		// Update since table out-of-date
      };
    };
    close_table(tprop.tnam, tab, 0, False);
    delete tab; tab = 0;
  };
  // Find if forced refresh asked
  if (inVal.noup && !inVal.x__rep && inVal.refresh) inVal.noup = False;
  // Find if forced delete necessary
  if (!inVal.forcedel && !inVal.noup && !inVal.x__rep &&
      (inVal.renew || tprop.renew)) inVal.forcedel = True;
  // Message
  if (inVal.noup) cout << tprop.tnam << " is up-to-date" << endl;
  return True;
}
 
//*************************************************************************//
// Open readable table
Table *openr_table(const String &tnam) {
  if (!testr_table(tnam)) {
    throw (AipsError(String("Unexpected problem finding table ") + tnam));
  };
  return (new Table(tnam));
}

//*************************************************************************//
// Create new table if 'renew' or if not exists; else open existing.
// tnam is full path name; td is table descriptor;
// vs is initial version (normally 1.0);
// title is description; type is short name
Table *create_table(const inputValues &inVal, tableProperties &tprop) {
  // Test existence and renewal
  Double vs = 1.0;
  if (testr_table(tprop.tnam)) {
    if (inVal.forcedel || !Table::isWritable(tprop.tnam)) {
      Table t(tprop.tnam);
      vs = int_data(get_version(&t)) + 1.0;
      String oldt(tprop.tnam + ".old");
      t.rename(oldt, Table::New);
    };
  };    

  // Open existing or new table
  Table *tab;
  TableDesc *td = properties[tprop.type].td;
  if (Table::isWritable(tprop.tnam)) tab = new Table(tprop.tnam,
						     Table::Update);
  else {
    td->rwKeywordSet().define("VS_CREATE", (date_string(today_now())));
    td->rwKeywordSet().define("VS_DATE", (date_string(today_now())));
    td->rwKeywordSet().define("VS_VERSION", version_string(vs));
    td->rwKeywordSet().define("VS_TYPE", tprop.title);
    td->rwKeywordSet().define("TAB_VERSION", version_string(tprop.version));
    td->rwKeywordSet().define("MJD0", tprop.MJD0);
    td->rwKeywordSet().define("dMJD", tprop.dMJD);
    SetupNewTable newtab(tprop.tnam, *td, Table::New);
    tab = new Table(newtab);
    TableInfo &info = tab->tableInfo();
    info.setType("IERS");
    info.setSubType(tprop.contents);
  };
  return tab;
}

//*************************************************************************//
// Close table tab (with name tnam) and update version (if vsup>0);
// the version date (if timup True);
// and show the table time statistics (if timshow True).
Bool close_table(const String &tnam, Table *&tab,
		 Double vsup, Bool timup, Bool timshow) {
  Double vs = dget_version(tab);
  uInt n = tab->nrow();
  Int tim(0);
  if (timshow) tim = last_mjd(tab);
  if (vsup>0) {
    vs += vsup;
    put_version(tab, vs);	
  };
  if (timup) put_vsdate(tab);
  delete tab; tab = 0;
  cout << tnam << " table " << version_string(vs);
  if (timup) cout << " now " << n << " entries"; 
  else cout << " has " << n << " entries"; 
  if (timshow) cout << " until " << tim;
  cout << endl;
  return True;
}

//*************************************************************************//
// Get last MJD in table
Int last_mjd(const Table *tab) {
  uInt n = tab->nrow();
  Double mjd;
  if (n<1) mjd = 0;
  else ROScalarColumn<Double>(*tab, "MJD").get(n-1, mjd);
  return Int(mjd); 
}

//*************************************************************************//
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

//*************************************************************************//
// Put VS_DATE in table
void put_vsdate(Table *tab) {
  String vs; 
  tab->rwKeywordSet().define("VS_DATE", (date_string(today_now())));
}

//*************************************************************************//
// Obtain VS_VERSION from table
String get_version(const Table *tab) {
  String vs; 
  tab->keywordSet().get("VS_VERSION", vs);
  return vs;
}

Double dget_version(const Table *tab) {
  return double_data(get_version(tab));
}

//*************************************************************************//
// Put VS_VERSION in table
void put_version(Table *tab, Double vs) { 
  tab->rwKeywordSet().define("VS_VERSION", version_string(vs));
}

//*************************************************************************//
// Obtain TAB_VERSION from table
String get_tversion(const Table *tab) {
  String vs; 
  if (tab->keywordSet().isDefined("TAB_VERSION")) {
    tab->keywordSet().get("TAB_VERSION", vs);
  } else vs = "0";   
  return vs;
}

//*************************************************************************//
Double dget_tversion(const Table *tab) {
  return double_data(get_tversion(tab));
}

//*************************************************************************//
// Put TAB_VERSION in table
void put_tversion(Table *tab, Double vs) { 
  tab->rwKeywordSet().define("TAB_VERSION", version_string(vs));
}

//*************************************************************************//
// File read/write
//*************************************************************************//

// Read data from ascii input file. The table (for reference only) is tnam;
// input file is inpath; out is vector of file lines.
Bool read_data(vector<String> &out, const String &,
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
  // Remove file if asked for
  if (del) {
    remove(in.absoluteName().c_str());	
  };
  return True;
}

Bool read_line(vector<String> &out, ifstream &infile) {
  String line;
  out.resize(0);
  if (getline(infile, line)) split_data(out, line);
  else return False;
  return True;
}

//*************************************************************************//
// Write the measuresdata rc file.
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
      " refresh=" << boolToString(inVal.refresh) <<
      " renew=" << boolToString(inVal.renew) <<
      " type=" << inVal.fulltype <<
      " derange=" << blockIntToString(inVal.derange) <<
      " x__n=" << inVal.x__n <<
      " x__fn=" << boolToString(True) <<
      " x__rep=" << boolToString(inVal.x__rep) <<
      " x__val=";
    for (uInt i=0; i<inVal.x__val.size(); ++i) {
      if (i) ofile << ",";
      ///      ofile << version_string(inVal.x__val[i], 15, 6);
      ofile << setfill('0') << setprecision(6) << inVal.x__val[i];
    };
    ofile << endl;
  };
  ofile.close();
  return True;
}


//*************************************************************************//
// Routines to fill tables
//*************************************************************************//

// Fill geodetic/TAI_UTC table
Bool TAI_UTC(tableProperties &tprop, inputValues &inVal) {

  // Test if to update
  if (testu_table(tprop, inVal) && inVal.noup) return True;

  // Check if in present and to be used
  if (inVal.testOnly || !inVal.x__fn) return True;

  // Read the data file
  vector<String> lines;
  read_data(lines, tprop.tnam, Path(inVal.in), True);

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
  vector<vector<Double> > allcol;
  for (uInt j=0; j<tprop.colnames.size(); ++j) {
    allcol.push_back(vector<Double>());
  };
  for (uInt i=0; i<fields.size(); ++i) {
    for (uInt j=0; j<tprop.colnames.size(); ++j) {
      allcol[j].push_back(double_data(fields[i][tprop.colids[j]]));
    };
    allcol[0][i] -= 2400000.5;
  };

  // Test looks
  if (allcol[0][0] != 37300 || allcol[3][0] != 0.001296) {
    throw (AipsError("Format for data file incorrect for " + tprop.tnam));
  };

  // Create table
  Table *tab = create_table(inVal, tprop);

  // Fill data
  tab->addRow((allcol[0].size()-tab->nrow()));
  createColumns(tab, tprop);
  for (uInt i=0; i<allcol[0].size(); ++i) {
    for (uInt j=0; j<tprop.columns.size(); ++j) {
      tprop.columns[j]->putScalar(i, allcol[j][i]);
    };
  };
  rmColumns(tab, tprop);

  // Ready
  close_table(tprop.tnam, tab, 0.0001);
  
  // OK
  return True;
}

//*************************************************************************//

// Fill geodetic/IERSeop97 table
Bool IERSeop97(tableProperties &tprop, inputValues &inVal) {
  return IERSeop(tprop, inVal);
}

//*************************************************************************//

// Fill geodetic/IERSeop2000 table
Bool IERSeop2000(tableProperties &tprop, inputValues &inVal) {
  return IERSeop(tprop, inVal);
}

//*************************************************************************//

// Fill eop table
Bool IERSeop(tableProperties &tprop, inputValues &inVal) {

  // Test if to update
  if (testu_table(tprop, inVal) && inVal.noup) return True;;;
  // Determine what to read next
  Double ml = max(Double(inVal.lastmjd), tprop.MJD0);
  String ytd = uIntToString(MVTime(ml+1).year() % 100);
  if (ytd.size() == 1) ytd = String("0") + ytd;
  tprop.fileAddress[2].replace(tprop.fileAddress[2].size()-2, 2, ytd);

  // Check if in present and to be used
  if (inVal.testOnly || !inVal.x__fn || 
      tprop.fileAddress[2] != inVal.in) return True;

  // Read the data file
  vector<String> lines;
  read_data(lines, tprop.tnam, Path(inVal.in), True);

  // Split data lines into fields
  vector<vector<String> > fields;
  vector<String> field;
  for (uInt i=0; i<lines.size(); ++i) {
    if (split_data(field, lines[i])) fields.push_back(field);
  };

  // Check format file
  uInt j = fields.size();
  while (j>=20 && fields[j-1].size() < 2) --j;
  if (fields.size() < 20 || fields[j-2].size() !=16) {
    throw (AipsError("Incorrect input file for " + tprop.tnam));
  };

  // Create table fields
  vector<vector<Double> > allcol;
  for (uInt j=0; j<tprop.colnames.size(); ++j) {
    allcol.push_back(vector<Double>());
  };
  for (uInt i=0; i<fields.size(); ++i) {
    if (fields[i].size() == 16 && int_data(fields[i][3]) >= 37665) {
      for (uInt j=0; j<tprop.colnames.size(); ++j) {
	allcol[j].push_back(double_data(fields[i][tprop.colids[j]]));
      };
    };
  };

  // Create table
  Table *tab = create_table(inVal, tprop);

  // Fill table
  if (allcol[0].back() - ml > 0) {
    tab->addRow(Int(allcol[0].back() - ml));
    createColumns(tab, tprop);
    for (uInt i=0; i<allcol[0].size(); ++i) {
      if (allcol[0][i] > ml) {
	uInt k = Int(allcol[0][i] - tprop.MJD0 - 1);
	for (uInt j=0; j<tprop.columns.size(); ++j) {
	  tprop.columns[j]->putScalar(k, allcol[j][i]);
	};
      };
    };
    rmColumns(tab, tprop);
  };

  // Ready
  close_table(tprop.tnam, tab, 0.0001);
 
  // OK
  return True;
}

//*************************************************************************//

// Fill geodetic/IERSpredict table
Bool IERSpredict(tableProperties &tprop, inputValues &inVal) {
  return IERSpred(tprop, inVal);
}

//*************************************************************************//

// Fill geodetic/IERSpredict2000 table
Bool IERSpredict2000(tableProperties &tprop, inputValues &inVal) {
  return IERSpred(tprop, inVal);
}

//*************************************************************************//

// Fill predict table
Bool IERSpred(tableProperties &tprop, inputValues &inVal) {

  // Test if to update
  if (testu_table(tprop, inVal) && inVal.noup) return True;

  // Check if in present and to be used
  if (inVal.testOnly || !inVal.x__fn) return True;

  // Read the data file
  vector<String> lines;
  read_data(lines, tprop.tnam, Path(inVal.in), True);

  // Split data lines into fields
  vector<vector<String> > fields;
  vector<String> field;
  for (uInt i=0; i<lines.size(); ++i) {
    if (lines[i].size() >= tprop.fdesc.back().start +  tprop.fdesc.back().n) {
      field.resize(0);
      for (uInt j=0; j<tprop.fdesc.size(); ++j) {
	field.push_back(lines[i].at(tprop.fdesc[j].start, tprop.fdesc[j].n));
      };
      fields.push_back(field);
    };
  };

  // Create table fields
  vector<vector<Double> > allcol;
  for (uInt j=0; j<tprop.colnames.size(); ++j) {
    allcol.push_back(vector<Double>());
  };
  for (uInt i=0; i<fields.size(); ++i) {
    for (uInt j=0; j<tprop.colnames.size(); ++j) {
      allcol[j].push_back(double_data(fields[i][tprop.colids[j]]));
    };
    allcol[7][i] /= 1000.0;			// Make s
    allcol[8][i] /= 1000.0;
  };

  // Create table
  Bool olddel(inVal.forcedel);			// Old force delete
  if (allcol[0][0] > inVal.lastmjd) inVal.forcedel = True; // Old one too old
  Table *tab = create_table(inVal, tprop);
  inVal.forcedel = olddel;			// Restore
  inVal.lastmjd = last_mjd(tab);

  // Fill table
  tab->keywordSet().get("MJD0", tprop.MJD0);
  if (tprop.MJD0 <= 0) tprop.MJD0 = allcol[0][0]-1;
  tab->rwKeywordSet().define("MJD0", tprop.MJD0);
  Double ml = max(Double(inVal.lastmjd), tprop.MJD0);
  if (allcol[0].back() - ml > 0) {
    tab->addRow(Int(allcol[0].back() - ml));
    createColumns(tab, tprop);
    for (uInt i=0; i<allcol[0].size(); ++i) {
      if (allcol[0][i] > ml) {
	uInt k = Int(allcol[0][i] - tprop.MJD0 - 1);
	for (uInt j=0; j<tprop.columns.size(); ++j) {
	  tprop.columns[j]->putScalar(k, allcol[j][i]);
	};
      };
    };
    rmColumns(tab, tprop);
  };

  // Ready
  close_table(tprop.tnam, tab, 0.0001);
 
  // OK
  return True;
}

//*************************************************************************//

// Fill geodetic/IGRF table
Bool IGRF(tableProperties &tprop, inputValues &inVal) {

  // Test if to update
  if (testu_table(tprop, inVal) && inVal.noup) return True;

  // Check if in present and to be used
  if (inVal.testOnly || !inVal.x__fn) return True;

  // Read the data file
  vector<String> lines;
  read_data(lines, tprop.tnam, Path(inVal.in), True);
 
  // Split data lines into fields
  vector<vector<String> > fields;
  vector<String> field;
  uInt expsize = tprop.fdesc.back().start +  tprop.fdesc.back().n; // Size
  for (uInt i=0; i<lines.size(); ++i) {
    if (lines[i].size() > 80) {
      if (lines[i].size() < expsize) lines[i].resize(expsize, ' ');
      field.resize(0);
      for (uInt j=0; j<tprop.fdesc.size(); ++j) {
	field.push_back(lines[i].at(tprop.fdesc[j].start, tprop.fdesc[j].n));
      };
      fields.push_back(field);
    };
  };

  // Create table fields
  vector<vector<Double> > allcol;
  uInt n(0);					// Number of coefficients
  uInt m(0);
  for (uInt i=0; i<fields.size(); ++i) {
    if (int_data(fields[i][1]) > 0) {		// Found field
      n = int_data(fields[i][1]);
      m = int_data(fields[i][2]);
      vector<Double> coldat;
      for (uInt j=3; j<tprop.fdesc.size(); ++j) {
	coldat.push_back(double_data(fields[i][j]));
      };
      allcol.push_back(coldat);
    };
  };
  if (n == 0 || n != m) {
    throw (AipsError("IGRF data has incorrect nm pair: " +
		     uIntToString(n) + ", " + uIntToString(m)));
  };
  if (allcol.size() != n*(n+2)) {
    throw (AipsError("IGRF data has incorrect number of entries: " +
		     uIntToString(allcol.size()) + " for nm"));
  };

  // Create table
  Table *tab = create_table(inVal, tprop);

  // Fill table
  tab->addRow((allcol[0].size()-tab->nrow()-1));
  createColumns(tab, tprop);
  for (uInt i=0; i<allcol[0].size()-1; ++i) {
    tprop.columns[0]->putScalar(i, (tprop.MJD0 + 5*(i+1)*365.25));
    vector<Double> col;
    for (uInt j=0; j<allcol.size(); ++j) col.push_back(allcol[j][i]);
    Vector<Double> Vcol(col);
    static_cast<ArrayColumn<Double>*>(tprop.columns[1])->put(i, Vcol);
    col.resize(0);
    if (i == allcol[0].size()-2) {
      for (uInt j=0; j<allcol.size(); ++j) col.push_back(allcol[j][i+1]);
    } else {
      for (uInt j=0; j<allcol.size(); ++j) {
	col.push_back(((allcol[j][i+1] - allcol[j][i])/5));
      };
    };
    Vcol = Vector<Double>(col);
    static_cast<ArrayColumn<Double>*>(tprop.columns[2])->put(i, Vcol);
  };
  rmColumns(tab, tprop);

  // Ready
  close_table(tprop.tnam, tab, 0.0001);
 
  // OK
  return True;
}

//*************************************************************************//

// Fill JPL planetary tables
Bool JPLDE(tableProperties &tprop, inputValues &inVal) {
  /// cout << "--- JPL tables cannot be created yet ----" << endl;;;
  ///return True;;;
  // Test if to update
  if (testu_table(tprop, inVal) && inVal.noup) return True;
 
  // Check if header present
  Path hpath(tprop.vinfo[0]);
  if (hpath.isValid() && File(hpath).exists() && File(hpath).isReadable()) {
    tprop.fileAddress[2] = tprop.vinfo[1];
    tprop.fileAddress[2].replace(4, 4, uIntToString(1960)); ////
  } else {
    tprop.fileAddress[2] = tprop.vinfo[0];
    return True;				// Get header first
  };

  // Dates
  Int stdat = Int(MVTime(1960, 1, 1).day());

  // Check if in present and to be used
  if (inVal.testOnly || !inVal.x__fn ||
      tprop.fileAddress[2] != inVal.in) return True;

  // Read header
  if (!(hpath.isValid() && File(hpath).exists() && File(hpath).isReadable())) {
    throw (AipsError("Cannot obtain the header file "+tprop.vinfo[0]));
  };
  uInt ksize(0);
  uInt ncoeff(0);
  Double stepo(0);
  Double endepo(0);
  uInt incepo(0);
  vector<String> kwnames;
  vector<Double> kwval;
  vector<Int> ptt;
  Vector<Int> pttA;
  vector<String> hlines;
  read_data(hlines, tprop.vinfo[0], hpath, False);

  // Split header lines into fields
  vector<vector<String> > hfields;
  vector<String> field;
  for (uInt i=0; i<hlines.size(); ++i) {
    if (split_data(field, hlines[i])) hfields.push_back(field);
  };

  // Get header info
  uInt bl = hfields.size();
  uInt bc(0);
  for (; bc<bl; ++bc) {			// Sizes
    if (hfields[bc].size()>3 && hfields[bc][0] == "KSIZE=") {
      ksize = int_data(hfields[bc][1]);
      ncoeff = int_data(hfields[bc][3]);
      break;
    };
  };
  for (++bc; bc<bl; ++bc) {			// Group 1030: epochs
    if (hfields[bc].size()<2 || hfields[bc][0] != "GROUP" ||
	hfields[bc][1] != "1030") continue;
    for (++bc; bc<bl; ++bc) {
      if (hfields[bc].size()<3) continue;
      stepo = double_data(hfields[bc][0]) -2400000.5;
      endepo = double_data(hfields[bc][1]) -2400000.5;
      incepo = int_data(hfields[bc][2]);
      break;
    };
    break;
  };
  for (++bc; bc<bl; ++bc) {			// Group 1040: kw names
    if (hfields[bc].size()<2 || hfields[bc][0] != "GROUP" ||
	hfields[bc][1] != "1040") continue;
    for (++bc; bc<bl; ++bc) {
      if (hfields[bc].size()<1 || int_data(hfields[bc][0]) == 0) continue;
      uInt n = int_data(hfields[bc][0]);
      for (++bc; bc<bl; ++bc) {
	for (uInt j=0; j<hfields[bc].size() && kwnames.size()<n; ++j) {
	  kwnames.push_back(hfields[bc][j]);
	};
	if (kwnames.size() >= n) break;
      };
      break;
    };
    break;
  };
  for (++bc; bc<bl; ++bc) {			// Group 1041: kw values
    if (hfields[bc].size()<2 || hfields[bc][0] != "GROUP" ||
	hfields[bc][1] != "1041") continue;
    for (++bc; bc<bl; ++bc) {
      if (hfields[bc].size()<1 || int_data(hfields[bc][0]) == 0) continue;
      uInt n = int_data(hfields[bc][0]);
      if (n!=kwnames.size()) throw (AipsError(tprop.vinfo[0]+
					      " format error"));
      for (++bc; bc<bl; ++bc) {
	for (uInt j=0;
	     j<hfields[bc].size() && kwval.size()<kwnames.size(); ++j) {
	  kwval.push_back(double_data(hfields[bc][j]));
	};
	if (kwval.size() >= kwnames.size()) break;
      };
      break;
    };
    break;
  };
  for (++bc; bc<bl; ++bc) {			// Group 1050: poly coeff
    if (hfields[bc].size()<2 || hfields[bc][0] != "GROUP" ||
	hfields[bc][1] != "1050") continue;
    for (++bc; bc<bl; ++bc) {
      if (hfields[bc].size()<1 || int_data(hfields[bc][0]) == 0) continue;
      for (uInt j=0; j<hfields[bc].size() && j<13; ++j) {
	ptt.push_back(int_data(hfields[bc][j]));
      };
      if (ptt.size() >= 3*13) break;
    };
    break;
  };
  if (!ksize || !incepo || kwnames.size() != kwval.size() ||
      ptt.size() != 3*13) {
    throw (AipsError("Illegal header file " + tprop.vinfo[0]));
  };
  pttA.resize(ptt.size());
  for (uInt i=0; i<ptt.size(); ++i) pttA[i] = ptt[i];

  // Read data file
  ifstream infile(Path(inVal.in).absoluteName().c_str());
  vector<String> line;
  vector<Double> allmjd;
  vector<vector<Double> > allcol;
  while (read_line(line, infile)) {
    if (line.size() < 2 || int_data(line[1]) != Int(ncoeff)) continue;
    Double st0;
    Double st1;
    vector<Double> res;
    while (read_line(line, infile)) {
      for (uInt i=0; i<line.size(); ++i) {
	if (res.size() == 0 && i<2) {
	  if (i==0) st0 = double_data(line[i])-2400000.5;
	  else st1 = double_data(line[i])-2400000.5;
	  continue;
	};
	if (st0 <= inVal.lastmjd) break;
	res.push_back(double_data(line[i]));
      };
      if (st0 <= inVal.lastmjd) break;
      if (res.size() >= ncoeff) {
	res.resize(ncoeff);
	allcol.push_back(res);
	allmjd.push_back(st0);
	break;
      };
    };
  };
  infile.clear();
  infile.close();
  ///  cout << "sz: " << allmjd[0] << ":" << allmjd.size() << ":" << allcol.size() << ":" << allcol[0].size() << endl;;;  

  // Create table
  Table *tab = create_table(inVal, tprop);

  // Fill table
  tab->keywordSet().get("MJD0", tprop.MJD0);
  if (tprop.MJD0 <= 0) tprop.MJD0 = ((stdat-stepo)/incepo-1)*incepo + stepo;
  tab->rwKeywordSet().define("MJD0", tprop.MJD0);
  tprop.dMJD = incepo;
  tab->rwKeywordSet().define("dMJD", tprop.dMJD);
  for (uInt i=0; i<kwnames.size(); ++i) 
    tab->rwKeywordSet().define(kwnames[i], kwval[i]);
  createColumns(tab, tprop);
  TableColumn tcd=TableColumn(*tab, "x");
  tcd.rwKeywordSet().define("Rows", 3);
  tcd.rwKeywordSet().define("Columns", 13);
  tcd.rwKeywordSet().define("Description", pttA); ///
  // Data
  tab->addRow(allmjd.size());
  for (uInt i=0; i<allmjd.size(); ++i) {
    tprop.columns[0]->putScalar(i, allmjd[i]);
    Vector<Double> colA(allcol[i]);
    static_cast<ArrayColumn<Double>*>(tprop.columns[1])->put(i, colA);
  };
  rmColumns(tab,tprop);

  // Ready
  close_table(tprop.tnam, tab, 0.0001);

// OK
return True;
}

//*************************************************************************//

// Fill JPL DE200 table
Bool DE200(tableProperties &tprop, inputValues &inVal) {
  return JPLDE(tprop, inVal);
}

//*************************************************************************//

// Fill JPL DE405 table
Bool DE405(tableProperties &tprop, inputValues &inVal) {
  return JPLDE(tprop, inVal);
}

//*************************************************************************//
// Main program
//*************************************************************************//

int main (int argc, const char** argv) {
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
    inVal.derange.resize(2);
    inVal.derange[0] = 1960;
    inVal.derange[1] = Time().year()+20;
    inputs.version(inVal.appVersion);
    inputs.create("type", inVal.type,
		  "Type of table to create", "string");
    inputs.create("dir", inVal.dir,
		  "Path to data table base directory",
		  "string");
    inputs.create("refresh", boolToString(inVal.refresh),
		  "Force refresh, even when not needed yet", "bool");
    inputs.create("renew", boolToString(inVal.renew),
		  "Force table renew when an update would suffice", "bool");
    inputs.create("derange", blockIntToString(inVal.derange),
		  "Range for JPL DE tables as yyyy,yyyy", "int");
    inputs.create("in", inVal.in,
		  "Hidden: name of input (ASCII) file", "string");
    inputs.create("x__n", uIntToString(inVal.x__n),
		  "Hidden: pointer into execution list", "int");
    inputs.create("x__fn", boolToString(inVal.x__fn),
		  "Hidden: filename should have been given", "bool");
    inputs.create("x__rep", boolToString(inVal.x__rep),
		  "Hidden: in repeating input cycle", "bool");
    inputs.create("param", "", "Parameter values", "string");
    inputs.create("x__val", "", "Parameter values", "string");

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
    if ((inVal.fulltype = minimaxNC(inVal.intype, types)).empty()) {
      throw (AipsError("Unknown type requested"));
    };
    if (multypes.count(inVal.fulltype)) inVal.types = multypes[inVal.fulltype];
    cout << "The processed type[s]:";
    for (uInt i=0; i<inVal.types.size(); ++i) cout << " " << inVal.types[i];
    cout << endl; 
    inVal.x__n = inputs.getInt("x__n");
    if (!inVal.types.empty() && inVal.x__n >= inVal.types.size()) {
      throw (AipsError("Program error: pointer outside processing list"));
    };

    // Get DE range
    inVal.derange = inputs.getIntArray("derange");
    if (inVal.derange.nelements() < 2 || inVal.derange[0]>=inVal.derange[1]) {
      throw (AipsError("Illegal DE table range specified"));
    };
    if (String(inVal.types[inVal.x__n], 0, 2) == String("DE")) {
      cout << "The requested DE table range is " << inVal.derange[0] << "-" <<
	inVal.derange[1] << endl;
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
  
    inVal.x__fn = inputs.getBool("x__fn");   

    inVal.x__rep = inputs.getBool("x__rep");

    // Get extra parameters
    String val = inputs.getString("x__val");
    if (val.empty()) val = inputs.getString("param");  
    vector<String> out;
    if (split_data(out, val, Regex("[(]"))) {
      val = out[0];
      if (split_data(out, val, Regex("[)]"))) {
	val = out[0];
	if (split_data(out, val, Regex("[,]"))) {
	  for (uInt i=0; i<out.size(); ++i) {
	    inVal.x__val.push_back(double_data(out[i]));
	  };
	};
      };
    };
    if (!inputs.getString("param").empty()) {
      cout << "Given parameters: ";
      for (uInt i=0; i<inVal.x__val.size();++i) {
	if (i) cout << ", ";
	cout << inVal.x__val[i];
      };
      cout << endl;
    };	 
 
    // Create the full properties
    makeProperties();

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
	if (inVal.x__fn) {
	  cout << "Processing " << inVal.type << endl;
	} else {
	  cout << "Testing " << inVal.type << endl;
	};
	properties[inVal.type].rout(properties[inVal.type], inVal);
	if (!inVal.noup) {			// Finished one
	  if (!inVal.x__fn) {
	    inVal.end = False;
	    --inVal.x__n;
	    break;
	  };
	  inVal.x__fn = False;
	  inVal.x__rep = True;
	  if (testu_table(properties[inVal.type], inVal)
	      && !inVal.noup) {			// More to do
	    inVal.end = False;
	    --inVal.x__n;
	    break;
	  };
	} else inVal.x__fn = False;			// Make sure
	inVal.x__rep = False;
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
