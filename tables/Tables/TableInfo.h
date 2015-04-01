//# TableInfo.h: Table type, subtype and further info
//# Copyright (C) 1996,1997,1999
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

#ifndef TABLES_TABLEINFO_H
#define TABLES_TABLEINFO_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/casa/BasicSL/String.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

// <summary>
// Table type, subtype and further info
// </summary>

// <use visibility=local>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="tTable.cc">
// </reviewed>

//# <prerequisite>
//# Classes you should understand before using this one.
//# </prerequisite>

// <etymology>
// TableInfo holds information (like type) about a table.
// </etymology>

// <synopsis> 
// TableInfo holds information about a table. It contains the following
// information:
// <dl>
//  <dt> Type
//  <dd> the type of a table (e.g. IMAGE, LOG).
//  <dt> SubType
//  <dd> the subtype of a table (e.g. UVDATA, MAP or ANTENNAPATTERN for 
//       type IMAGE).
//  <dt> Readme
//  <dd> An arbitrary number of lines containing ancillary text
//       describing the table (or maybe its history).
// </dl>
// This information is stored
// in the file <src>table.info</src> in the table directory.
// Regular tables as well as reference tables (results of sort/select)
// have their own table.info file.
// <br>
// The initial table-info of a regular table is blank. It has to be set
// explicitly by the user.
// <br>
// The initial table-info of a reference table
// is a copy of the table-info of its parent table. The user can add
// lines to the readme information to describe the table in more detail.
// Of course, the type and/or subtype can be changed at will.
// <p>
// The type and subtype information are stored at the beginning of
// the first two lines of the file as:
// <srcblock>
//        Type = TypeString
//        SubType = SubTypeString
// </srcblock>
// These lines in the table.info file can be used by external programs
// (like the filebrowser) to determine the type of table being handled.
// <p>
// The third line in the file is blank. The line(s) thereafter contain
// the possible readme information (note that multiple readme lines are
// possible). They can be added using the function <src>readmeAddLine</src>.
// <p>
// Existing tables do not have a table.info file yet. The table system
// will handle them correctly and use a blank type, subtype
// and readme string. A table.info file will be created when the
// table is opened for update.
// <p>
// To be sure that different table types have unique names, it can be
// useful to use enum's and to define them in one common file. For
// Casacore tables this enum is defined in this file.

// <example>
// <srcblock>
// // Open a table for update.
// Table table("name", Table::Update);
// // Get its TableInfo object.
// TableInfo& info = table.tableInfo();
// // Set type and subtype.
// info.setType ("IMAGE");
// info.setSubType ("SubTypeString");
// // Add a few readme lines. The second one adds 2 lines.
// info.readmeAddLine ("the first readme string");
// info.readmeAddLine ("the second readme string\nthe third readme string");
// // Display the type, etc..
// cout << info.type() << " " << info.subType() << endl;
// cout << info.readme();
// </srcblock>
// </example>

// <motivation>
// External programs need to be able to determine the type of a table.
// </motivation>

//# <todo asof="$DATE:$">
//# A List of bugs, limitations, extensions or planned refinements.
//# </todo>


class TableInfo
{
public:
    // enum for various standard Table types.
    // Underscores in the enumerator indicate different sub-types
    enum Type {
	// a PagedImage is a PagedArray with coordinates and Masking (opt.)
	PAGEDIMAGE,
	// a PagedArray (.../Lattices/PagedArray.h)
	PAGEDARRAY,
	// MeasurementSet main Table
	MEASUREMENTSET,
	// MeasurementSet Antenna table
	ANTENNA,
	// MeasurementSet Array table
	ARRAY,
	// MeasurementSet Feed characteristics table
	FEED,
	// MeasurementSet Field table
	FIELD,
	// MeasurementSet Observation information table
	OBSERVATION,
	// MeasurementSet Oserving Log table
	OBSLOG,
	// MeasurementSet Source table
	SOURCE,
	// MeasurementSet Spectral Window table
	SPECTRALWINDOW,
	// MeasurementSet System Calibration table
	SYSCAL,
	// MeasurementSet Weather table
	WEATHER,
	// Measurement Equation Calibration table
	ME_CALIBRATION,
	// Casacore Log table
	LOG,
	// A ComponentList table contains parameterised representations of the
	// sky brightness.
	COMPONENTLIST
    };

    // Create an empty object.
    TableInfo();

    // Create the object reading it from the given file name.
    // If the file does not exist, type, subtype and readme are
    // initialized to a blank string.
    explicit TableInfo (const String& fileName);

    // Create a TableInfo object of one of the predefined types. 
    // This is a centralised way of setting the Table type only. 
    TableInfo (Type which);

    // Copy constructor (copy semantics).
    TableInfo (const TableInfo& that);

    // Assignment (copy semantics).
    TableInfo& operator= (const TableInfo& that);

    ~TableInfo();

    // Get the table (sub)type.
    // <group>
    const String& type() const;
    const String& subType() const;
    // </group>

    // Get the readme.
    const String& readme() const;

    // Set the table (sub)type.
    void setType (const String& type);
    void setSubType (const String& subType);

    // Convert the Type enumerator to a type and subType string
    // <group>
    static String type(Type tableType);
    static String subType(Type tableType);
    // </group>

    // Clear the readme.
    void readmeClear();

    // Add a line to the readme.
    // It will itself add a newline character ('\n') to the end of the line.
    void readmeAddLine (const String& readmeLine);

    // Write the TableInfo object.
    void flush (const String& fileName);

private:
    String type_p;
    String subType_p;
    String readme_p;
    Bool   writeIt_p;    // True = object has changed, so has to be written
};



inline const String& TableInfo::type() const
{
    return type_p;
}
inline const String& TableInfo::subType() const
{
    return subType_p;
}
inline const String& TableInfo::readme() const
{
    return readme_p;
}




} //# NAMESPACE CASACORE - END

#endif
