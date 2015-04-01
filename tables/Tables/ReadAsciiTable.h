//# ReadAsciiTable.h: Filling a table from an Ascii file
//# Copyright (C) 1993,1994,1995,1999,2001,2002
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

#ifndef TABLES_READASCIITABLE_H
#define TABLES_READASCIITABLE_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/casa/Arrays/IPosition.h>
#include <casacore/tables/Tables/Table.h>

//# Forward Declarations
#include <casacore/casa/iosfwd.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

class Regex;
class IPosition;
class LogIO;
class TableRecord;
class TableColumn;


// <summary>
// Filling a table from an Ascii file.
// </summary>
// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="" demos="">
// </reviewed>

// <use visibility=export>

// <prerequisite>
//  <li> <linkto class="Table:description">Table</linkto>
// </prerequisite>

// <synopsis>
// Global functions to fill a table from an Ascii file.
//
// The table columns are filled from a file containing the data values
// separated by a separator (optionally followed by whitespace). The
// default separator is a comma. Non-given values default to 0, False, or
// blank string (depending on data type). A value is not given between 2
// consecutive separators or if less values are given than needed.
// One line per table row should be given.
// The following two header lines define the columns in the table:
// <ol>
//   <li> The first line contains the names of the variables in each column.
//        These names may be enclosed in double quotes.
//   <li> The second line contains the data types of each column.
//        Valid types are:
//      <ul>
//        <li>  S     for Short Integer data
//        <li>  I     for Integer data
//        <li>  R     for Real data
//        <li>  D     for Double Precision data
//        <li>  X     for Complex data (Real, Imaginary)
//        <li>  DX    for Double Precision Complex data (R,I)
//        <li>  Z     for Complex data (Amplitude, Phase)
//        <li>  DZ    for Double Precision Complex data (A,P)
//        <li>  A     for ASCII data (must be enclosed in double
//                    quotes if it contains one or more blanks)
//        <li>  DMS   for MVAngle-format position in DMS (converted to radians)
//                    In this case a colon separated position is seen as
//                    degrees and not as hours.
//                    Blanks instead of : can be used as separator.
//        <li>  HMS   for MVAngle-format position in HMS (converted to radians)
//                    Blanks instead of : can be used as separator.
//      </ul>
//        The type can optionally be followed by one or more positive numbers
//        (separated by commas without whitespace) indicating that the column
//        contains an array. The numbers give the shape of the array.
//        E.g. <src>D2,4</src> defines a column containing arrays with
//        shape [2,4]. It "consumes" 8 numbers in each input data line.
//        The last column can contain a 0 in one of the shape numbers.
//        It indicates that the arrays are variable shaped; it "consumes"
//        all remaining numbers in each input data line. If needed,
//        the arrays are filled with default values (0, False, or blank).
//        E.g. <src>I0</src> indicates a variable shaped vector.
//        <src>I0,4</src> with a line with remaining input
//        <src>1 2 3 4 5 6 7 8 9</src> results in an array with shape [3,4]
//        (filled with with 3 zeroes).
// </ol>
// If the <src>autoHeader</src> argument is True, the column definition
// lines should not be given. It recognizes the types from the first data
// line. It gives the names 'column0', etc. to the columns.
// It can recognize integer, double, and string types.
// It is possible to give a shape argument which has the same function
// as the shape values discussed above.
// <p>
// There are two forms of the readAsciiTable function:
// <ol>
//  <li> The simplest form has two input files.
//       The second input file contains the column data.
//       The first input file contains the keywords (if any)
//       and the column definitions.
//       The keywords in the first file, if there are any, must be enclosed
//       between a line that starts with ".keywords" and a line that starts
//       with ".endkeywords". To define column keywords, .keywords should be
//       followed by whitespace and the column name. 
//       Between these two lines each line should contain the following:
//       <ul>
//        <li> The keyword name, e.g., ANYKEY
//        <li> The datatype of the keyword (cf. list of valid types above)
//        <li> The value or values for the keyword (the keyword may contain a
//             scalar or a vector of values).  e.g., 3.14159  21.78945
//      </ul>
//      After the keywords definitions, the two column definition lines
//      should follow (unless <src>autoHeader=True</src> is given).
//      <br>For example:
//      <srcblock>
//       .keywords
//       KEYI  I  10
//       KEYIV I  11 12 13 14
//       KEYF  R  1.2
//       KEYFV R  -3.2 0 5.6
//       KEYD  D  1.23456789
//       KEYDV D  1 2 3 4 5 6 7 8 9
//       KEYX  X  -1.5 -3
//       KEYXC X  0 1 2 3 4 5 6 7 8 9
//       KEYZ  Z  -3  -1.5
//       KEYZV Z  0 0.1 0.2 0.3 0.4 0.5
//       KEYS  A  "1 2 3 4 5"
//       KEYSV A  " 1 2 " "AAA" BBB bbb CCc C "@#$%^&*()"
//       .endkeywords
//       .keywords  COLDX
//       IKEYS   A "coldx ikey"
//       DKEYS   A "coldx dkey"
//       .endkeywords
//       COLI   COLF   COLD       COLX        COLZ       COLS
//        I      R      D          X           Z          A
//      </srcblock>
//      defines a table with 12 table keywords (of which 6 contain vector
//      values), 2 keywords for column COLDX, and and 6 columns.
//      The number of rows is determined by the number of
//      lines in the second input file.
//  <li> The other form is to combine the two files in one file.
//       In that case the data lines must be preceeded by the optional
//       keyword and column definitions (without an intermediate blank line).
// </ol>
// </synopsis>

// <example>
// <srcblock>
//   readAsciiTable ("file.in", "", "table.test");
// </srcblock>
// creates a table with name <src>table.test</src> from the text file
// <src>file.in</src>. The text file could look like:
// <srcblock>
//  COLI   COLF   COLD       COLX        COLZ       COLS
//   I      R      D          X           Z          A
//  1      1.1    1.11       1.12 1.13   1.14 1.15  Str1
//  10     11     12         13   14     15   16    String17
// </srcblock>
// resulting in a table with 6 columns and 2 rows.
// </example>

// <group name=readAsciiTable>


// Create a table with name as given by tableName.
// If autoHeader==True, the format is automatically derived from the
// first data line. It can recognize integer, double, and String types.
// The columns will be named column1, column2, etc..
// If the autoShape argument is given with 1 or more axes, all values are
// treated as a single column with the given shape. Note that one of the
// can have length 0 indicating a variable shaped array.
// If autoHeader==False, the layout of the table has to be defined in
// the first 2 lines of the input file. The remaining lines in the
// input file contain the data.
//
// When the tableDescName is not blank, the table description will
// be stored in a table description file with the given name.
// <br>It returns a string containing the format of the columns in
// the form COL1=R, COL2=D, ...
//
// The separator gives the character separating the values. The default
// is a blank. Note that irrespective of the separator, blanks between
// values are always ignored. A string value has to be enclosed in
// double quotes if it has to contain blanks or the separator value.
//
// Header and data lines starting with the regular expression given in the
// commentMarker are ignored. By default no comment marker is present.
// E.g. "#" ignores all lines starting with the #-sign.
// " *#" does the same, but the lines to ignore can start with whitespace.
//
// The first and last line argument give the 1-relative number of the
// first and last line to read from the file. firstLine <= 0 is the
// same as 1. lastLine <= 0 means until end-of-file.
// Note that lines matching the comment marker are also counted.
String readAsciiTable (const String& filein, const String& tableDescName,
		       const String& tableName, Bool autoHeader = False,
		       Char separator = ' ',
		       const String& commentMarkerRegex = "",
		       Int firstLine = 1, Int lastLine = -1,
		       const IPosition& autoShape = IPosition());

// This form gets the header info in the given vectors.
// Each element in the dataTypes vector has to be of the form as would
// be given in a header line.
String readAsciiTable (const String& filein, const String& tableproto,
		       const String& tablename,
		       const Vector<String>& columnNames,
		       const Vector<String>& dataTypes,
		       Char separator, const String& commentMarkerRegex,
		       Int firstLine, Int lastLine);

// This form reads TWO Ascii files. The first file may contain 
// keywords and their values as well as the two lines described above for
// the names and type of variables. The second file is intended for data only.
//
// When the tableDescName is not blank, the table description will
// be stored in a table description file with the given name.
// <br>It returns a string containing the format of the columns in
// the form COL1=R, COL2=D, ...
//
// The separator gives the character separating the values. The default
// is a blank. Note that irrespective of the separator, blanks between
// values are always ignored. A string value has to be enclosed in
// double quotes if it has to contain blanks or the separator value.
//
// Header and data lines starting with the regular expression given in the
// commentMarker are ignored. By default no comment marker is present.
// E.g. "#" ignores all lines starting with the #-sign.
// " *#" does the same, but the lines to ignore can start with whitespace.
//
// The first and last line argument give the 1-relative number of the
// first and last line to read from the data file. firstLine <= 0 is the
// same as 1. lastLine <= 0 means until end-of-file.
// Note that lines matching the comment marker are also counted.
// <group>
String readAsciiTable (const String& headerFile, const String& dataFile, 
		       const String& tableDescName, const String& tablename,
		       Char separator = ' ',
		       const String& commentMarkerRegex = "",
		       Int firstLine = 1, Int lastLine = -1);
//# Note that this char* version is needed, because of the first version
//# Taking a Bool as the 4th argument.
String readAsciiTable (const String& headerFile, const String& dataFile, 
		       const String& tableDescName, const char* tablename,
		       Char separator = ' ',
		       const String& commentMarkerRegex = "",
		       Int firstLine = 1, Int lastLine = -1);
// </group>

// Similar versions as above, but returning a Table object.
// The format string is returned in the first argument.
// The type of Table can be given (Plain or Memory).
// <group>
Table readAsciiTable (String& formatString, Table::TableType tableType,
		      const String& filein, const String& tableDescName,
		      const String& tableName, Bool autoHeader = False,
		      Char separator = ' ',
		      const String& commentMarkerRegex = "",
		      Int firstLine = 1, Int lastLine = -1,
		      const IPosition& autoShape = IPosition());
Table readAsciiTable (String& formatString, Table::TableType tableType,
		      const String& filein, const String& tableproto,
		      const String& tablename,
		      const Vector<String>& columnNames,
		      const Vector<String>& dataTypes,
		      Char separator, const String& commentMarkerRegex,
		      Int firstLine, Int lastLine);
Table readAsciiTable (String& formatString, Table::TableType tableType,
		      const String& headerFile, const String& dataFile, 
		      const String& tableDescName, const String& tablename,
		      Char separator = ' ',
		      const String& commentMarkerRegex = "",
		      Int firstLine = 1, Int lastLine = -1);
Table readAsciiTable (String& formatString, Table::TableType tableType,
		      const String& headerFile, const String& dataFile, 
		      const String& tableDescName, const char* tablename,
		      Char separator = ' ',
		      const String& commentMarkerRegex = "",
		      Int firstLine = 1, Int lastLine = -1);
// </group>

// </group>




// <summary>
// Helper class for readAsciiTable
// </summary>
// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="" demos="">
// </reviewed>

// <use visibility=local>

// <synopsis>
// This class contains static functions as helpers for readAsciiTable.
// </synopsis>

class ReadAsciiTable
{
public:
  // Run the readAsciiTable.
  static String run (const String& headerfile, const String& filein, 
		     const String& tableproto, const String& tablename,
		     Bool autoHeader, const IPosition& autoShape,
		     const Vector<String>& columnNames,
		     const Vector<String>& dataTypes,
		     Char separator,
		     const String& commentMarkerRegex,
		     Int firstLine, Int lastLine);
  static Table runt (String& formatString, Table::TableType tableType,
		     const String& headerfile, const String& filein, 
		     const String& tableproto, const String& tablename,
		     Bool autoHeader, const IPosition& autoShape,
		     const Vector<String>& columnNames,
		     const Vector<String>& dataTypes,
		     Char separator,
		     const String& commentMarkerRegex,
		     Int firstLine, Int lastLine);

  // Read a position using MVAngle.
  // If isDMS is True, a position with : is treated as DMS instead of HMS.
  // This function is a bit more relaxed than MVAngle::read.
  // It allows whitespace. Furthermore it allows whitespace as separator :.
  static double stringToPos (const String& pos, Bool isDMS);

private:
  // Define types.
  enum RATType {RATBool, RATShort, RATInt, RATFloat, RATDouble, RATString,
		RATComX, RATComZ, RATDComX, RATDComZ, RATDMS, RATHMS};


  // Do the actual run.
  static String doRun (const String& headerfile, const String& filein, 
		       const String& tableproto, const String& tablename,
		       Bool autoHeader, const IPosition& autoShape,
		       const Vector<String>& columnNames,
		       const Vector<String>& dataTypes,
		       Char separator,
		       Bool testComment, const Regex& commentMarker,
		       Int firstLine, Int lastLine);

  // Do the actual work of making and filling the table.
  static Table makeTab (String& formatString, Table::TableType tableType,
			const String& headerfile, const String& filein, 
			const String& tableproto,
			const String& tablename,
			Bool autoHeader, const IPosition& autoShape,
			const Vector<String>& columnNames,
			const Vector<String>& dataTypes,
			Char separator,
			Bool testComment, const Regex& commentMarker,
			Int firstLine, Int lastLine);

  // Get the next line. Skip lines to be ignored.
  // It returns False when no more lines are available.
  static Bool getLine (ifstream& file, Int& lineNumber,
		       char* line, Int lineSize,
		       Bool testComment, const Regex& commentMarker,
		       Int firstLine, Int lastLine);
  
  // Get the next part of the line using the separator as delimiter.
  // Leading blanks are ignored.
  static Int getNext (const Char* string, Int strlen, Char* result,
		      Int& at, Char separator);
  
  // Derive the types from the values in the first data line.
  static void getTypes (const IPosition& shape,
			const Char* in, Int leng,
			Char* string1, Char* string2, Char separator);

  // Turn the string into a Bool value.
  // Empty string, value 0 and any value starting with f, F, n or N are False.
  static Bool makeBool (const String& str);

  // Handle a keyword set.
  static void handleKeyset (Int lineSize, char* string1,
			    char* first, char* second,
			    TableRecord& keysets,
			    LogIO& logger,
			    const String& fileName,
			    ifstream& jFile,
			    Int& lineNumber,
			    Char separator,
			    Bool testComment,
			    const Regex& commentMarker,
			    Int firstLine, Int lastLine);

  // Get the shape and type from the type string.
  static Int getTypeShape (const String& typestr,
			   IPosition& shape, Int& type);
  
  // Get the next scalar value with the given type from string1.
  static Bool getValue (char* string1, Int lineSize, char* first,
			Int& at1, Char separator,
			Int type, void* value);

  // Handle the next scalar with the given type from the data line and
  // put it into the table column.
  static void handleScalar (char* string1, Int lineSize, char* first,
			    Int& at1, Char separator,
			    Int type,
			    TableColumn& tabcol, uInt rownr);

  // Get the next array with the given type from string1.
  // It returns the shape (for variable shaped arrays).
  static IPosition getArray (char* string1, Int lineSize, char* first,
			     Int& at1, Char separator,
			     const IPosition& shape, Int varAxis,
			     Int type, void* valueBlock);

  // Get the next array with the given type from the data line and
  // put it into the table column.
  static void handleArray (char* string1, Int lineSize, char* first,
			   Int& at1, Char separator,
			   const IPosition& shape, Int varAxis,
			   Int type,
			   TableColumn& tabcol, uInt rownr);
};



} //# NAMESPACE CASACORE - END

#endif
