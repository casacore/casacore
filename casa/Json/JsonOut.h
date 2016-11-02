//# JsonOut.h: Fill a file or stream in JSON format
//# Copyright (C) 2016
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

#ifndef CASA_JSONOUT_H
#define CASA_JSONOUT_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/casa/Arrays/Array.h>
#include <iostream>
#include <fstream>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

  //# Forward Declarations
  class Record;
  class ValueHolder;


  // <summary>
  // Class to fill a file or stream in JSON format.
  // </summary>

  // <use visibility=export>
  // <reviewed reviewer="" date="" tests="tJsonOut">
  // </reviewed>

  //# <prerequisite>
  //# </prerequisite>

  // <synopsis>
  // JsonOut is a class to create a JSON file. JsonParser.h can be used
  // to interpret a JSON file whereafter JsonKVMap gets out the information.
  //
  // Besides the standard JSON types (bool, int, float, string), sequences
  // and nested structs, JsonOut also supports Casacore data type (D)Complex,
  // Array, Record, and ValueHolder.
  // <br>- A complex number is written as a nested struct with fields
  //       "r" and "i".
  // <br>- An Array is written as a (possibly nested) sequence of values.
  // <br>- A Record is written as a nested struct; subrecords are supported.
  // <br>- A ValueHolder is written depending on the data type it contains.
  // <br>Note that floating point values are written with high accuracy
  // (7 digits for single precision, 16 digits for double precision).
  //
  // Although standard JSON does not support comments, many parsers do support
  // C-style and C++-style comments. JsonOut has the possibility to define
  // arbitrary comment delimiters (e.g., / * and * / for C-style).
  // If no start delimiter is given, possible comments are ignored.
  //
  // The output of JsonOut can be any iostream. If a file name is given, an
  // ofstream will be opened in the constructor and closed in the destructor.
  // The output is formatted pretty nicely. Nested structs are indented with
  // 2 spaces. Arrays are written with a single axis per line; continuation
  // lines are indented properly. String arrays have one value per line.
  // </synopsis>

  // <example>
  // The following example is read back by the example in class JsonParser.
  // <srcblock>
  // // Create the JSON file.
  // JsonOut jout(fullName + "/imageconcat.json");
  // // Start the JSON struct; possible comments will be ignored.
  // jout.start();
  // // Write some fields (one line per field).
  // jout.write ("Version", 1);
  // jout.write ("DataType", "float");
  // jout.write ("Axis", latticeConcat_p.axis());
  // jout.write ("Images", Array<String>(latticeNames));
  // // End the JSON struct.
  // jout.end();
  // </srcblock>
  // See tJsonOut.cc for more elaborate examples.
  // </example>

  // <motivation>
  // JSON is a commonly used interchange format.
  // </motivation>
  //
  //# <todo asof="1996/03/10">
  //#   <li> 
  //# </todo>

  class JsonOut
  {
  public:
    // The default constructor creates the output on stdout.
    JsonOut();

    // Create the file with the given name using an ofstream object.
    JsonOut (const String& name);

    // Create the object using the given ostream object.
    JsonOut (ostream& os);

    // Close the stream. It closes the ofstream object if created.
    ~JsonOut();

    // Start a JSON structure by writing a { and setting the indentation.
    // It checks if not inside a JSON structure.
    // It is possible to define the comment delimiters
    // (e.g., / * and * /  or  // and empty).
    // If commentStart is empty, possible comments are ignored.
    void start (const String& commentStart=String(),
                const String& commentEnd=String(),
                const String& indent="  ");

    // End a structure by clearing the indentation and writing a }.
    // It checks if inside a JSON structure.
    void end();

    // Start a nested structure; i.e., a field with a structured value.
    // It writes the name and opening brace and increments the indentation.
    // If supported, the comment is written on a line preceeding the key line.
    void startNested (const String& name, const String& comment=String());

    // End a nested structure.
    // It decrements the indentation and writes the closing brace.
    void endNested();

    // Write one or more lines defining a keyword-value pair, where value
    // can be of any type including Array, Record, and ValueHolder.
    // A non-finite floating point number and a null ValueHolder are
    // written as a null value.
    // If supported, the comment is written on a line preceeding the
    // 'key:value' line.
    template <typename T>
    void write (const String& name, T value, const String& comment=String());

    // Write a comment on a separate line.
    // If comments are not supported, an empty line is written.
    void writeComment (const String& comment);

    // Write a null value.
    void putNull();

    // Put a scalar value with sufficient accuracy.
    // A Complex value is written as a nested JSON structure
    // with fields r and i.
    // A string is enclosed in quotes and escaped where necessary.
    // <br>These functions are meant for internal use by the 'write' function.
    // <group>
    template <typename T> void put (T value);
    void put (Bool value);
    void put (Float value);
    void put (Double value);
    void put (const Complex& value);
    void put (const DComplex& value);
    void put (const char* value);
    void put (const String& value);
    // </group>

    // Put a line defining an array value. Multi-dim arrays are written as
    // nested [] lines.
    // Normally the values of the first dimension are written on a single line,
    // but for string values a line per value is used.
    // <br>These functions are meant for internal use by the 'write' function.
    template <typename T>
    void putArray (const Array<T>& value, const String& indent,
                   Bool firstLine);
    void putArray (const Array<String>& value, const String& indent,
                   Bool firstLine);
    template <typename T>
    void putArray (const Array<T>& value, const String& indent,
                   Bool firstLine, Bool valueEndl);

    // Escape a double quote and backslash in a string.
    static String escapeString (const String& in);

  private:
    // Copy constructor cannot be used.
    JsonOut (const JsonOut& other);

    // Assignment cannot be used.
    JsonOut& operator= (const JsonOut& other);

    // Write the name.
    void putName (const String& name);

    // General function to write a key and value.
    // Specializations exist for particular data types.
    template <typename T>
    void writeKV (const String& name, T value);

    // Write a key and array value.
    template <typename T>
    void writeKV (const String& name, const Array<T>& value);

    // Write a key and valueholder.
    void writeKV (const String& name, const ValueHolder& vh);

    // Put a Record which is written as a {} structure.
    // The Record can be nested.
    void put (const Record&);

    // Get the indentation after a name.
    // It indents with the length of the name (including quotes and colon)
    // with a maximum of 20 spaces.
    String indentValue (const String& indent, const String& name) const;

    //# Data fields.
    std::ofstream itsFile;
    std::ostream& itsStream;
    String        itsIndent;
    String        itsIndentStep;
    int           itsLevel;
    String        itsCommentStart;
    String        itsCommentEnd;
    vector<Bool>  itsFirstName;
  };


} //# NAMESPACE CASACORE - END

#ifndef CASACORE_NO_AUTO_TEMPLATES
#include <casacore/casa/Json/JsonOut.tcc>
#endif //# CASACORE_NO_AUTO_TEMPLATES
#endif
