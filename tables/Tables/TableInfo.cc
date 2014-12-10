//# TableInfo.cc: Table type, subtype and further info
//# Copyright (C) 1996,1997,1999,2001,2002
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


//# Includes
#include <casacore/tables/Tables/TableInfo.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/casa/OS/File.h>
#include <casacore/casa/iostream.h>
#include <casacore/casa/fstream.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

TableInfo::TableInfo()
: writeIt_p (True)
{}

TableInfo::TableInfo (const String& fileName)
: writeIt_p (True)
{
    File file (Path(fileName).absoluteName());
    if (! file.exists()) {
	return;
    }
    ifstream os(file.path().absoluteName().chars(), ios::in);
    char buf[1025];
    int  len;
    if (! os.getline (buf, 1024)) {              // Type = string
	return;
    }
    len = os.gcount();
    if (len > 7) {
	type_p = String (buf + 7);
    }
    if (! os.getline (buf, 1024)) {              // SubType = string
	return;
    }
    len = os.gcount();
    if (len > 10) {
	subType_p = String (buf + 10);
    }
    if (! os.getline (buf, 1024)) {              // blank string
	return;
    }
    // Read the readme lines until a newline.
    // Add a newline at the end of each line when a newline was read
    // (in that case gcount gives length+1).
    while (os.getline (buf, 1024, '\n')) {
	len = os.gcount();
	if (buf[len-1] == '\0') {
	    buf[len-1] = '\n';
	    buf[len] = '\0';
	}
	readme_p += String (buf, len);
    }
    // The info file existed and is normal.
    // So it does not need to be written.
    writeIt_p = False;
}


// Create a TableInfo object of one of the predefined types. 
// This is a centralised way of setting the Table type/subType.
TableInfo::TableInfo (Type tableType)
  :type_p    (type(tableType)),
   subType_p (subType(tableType)),
   readme_p  (),
   writeIt_p (True)
{}

TableInfo::TableInfo (const TableInfo& that)
: type_p    (that.type_p),
  subType_p (that.subType_p),
  readme_p  (that.readme_p),
  writeIt_p (True)
{}

TableInfo& TableInfo::operator= (const TableInfo& that)
{
    if (this != &that) {
	type_p    = that.type_p;
	subType_p = that.subType_p;
	readme_p  = that.readme_p;
	writeIt_p = True;
    }
    return *this;
}

TableInfo::~TableInfo()
{}


void TableInfo::flush (const String& fileName)
{
    if (writeIt_p) {
	ofstream os(Path(fileName).absoluteName().chars(), ios::out);
	os << "Type = " << type_p << endl;
	os << "SubType = " << subType_p << endl;
	os << endl;
	os << readme_p;
	writeIt_p = False;
    }
}


void TableInfo::setType (const String& type)
{
    type_p    = type;
    writeIt_p = True;
}

void TableInfo::setSubType (const String& subType)
{
    subType_p = subType;
    writeIt_p = True;
}

void TableInfo::readmeClear()
{
    readme_p  = "";
    writeIt_p = True;
}


void TableInfo::readmeAddLine (const String& readmeLine)
{
    readme_p += readmeLine;
    readme_p += '\n';
    writeIt_p = True;
}

String TableInfo::type(Type tableType)
{
    switch (tableType) {
    case PAGEDIMAGE: return "Image";
    case PAGEDARRAY: return "Paged Array";
    case MEASUREMENTSET: return "Measurement Set";
    case ANTENNA: return "Antenna";
    case ARRAY: return "Telescope Array";
    case FEED: return "Feed Characteristics";
    case FIELD: return "Field";
    case OBSERVATION: return "Observation Information";
    case OBSLOG: return "Observation Log";
    case SOURCE: return "Source";
    case SPECTRALWINDOW: return "Spectral Window";
    case SYSCAL: return "System Calibration";
    case WEATHER: return "Weather";
    case ME_CALIBRATION: return "Calibration";
    case LOG: return "Log message";
    case COMPONENTLIST: return "Component List";
    default: return "";
    };
}

String TableInfo::subType(Type tableType)
{
    switch (tableType) {
    case PAGEDIMAGE: return "";
    case PAGEDARRAY: return "";
    case MEASUREMENTSET: return "";
    case ANTENNA: return "";
    case ARRAY: return "";
    case FEED: return "";
    case FIELD: return "";
    case OBSERVATION: return "";
    case OBSLOG: return "";
    case SOURCE: return "";
    case SPECTRALWINDOW: return "";
    case SYSCAL: return "";
    case WEATHER: return "";
    case ME_CALIBRATION: return "";
    case LOG: return "";
    case COMPONENTLIST: return "";
    default: return "";
    };
}

} //# NAMESPACE CASACORE - END

