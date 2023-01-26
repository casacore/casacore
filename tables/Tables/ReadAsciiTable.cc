//# ReadAsciiTable.cc: Filling a table from an Ascii file
//# Copyright (C) 1993,1994,1995,1996,1997,1999,2000,2001,2002,2003
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

#include <casacore/tables/Tables/ReadAsciiTable.h>
#include <casacore/tables/Tables/TableDesc.h>
#include <casacore/tables/Tables/ScaColDesc.h>
#include <casacore/tables/Tables/ArrColDesc.h>
#include <casacore/tables/Tables/Table.h>
#include <casacore/tables/Tables/TableRecord.h>
#include <casacore/tables/Tables/TableColumn.h>
#include <casacore/tables/Tables/ArrayColumn.h>
#include <casacore/tables/Tables/SetupNewTab.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Arrays/ArrayUtil.h>
#include <casacore/casa/Containers/Block.h>
#include <casacore/casa/OS/Path.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/casa/Quanta/MVAngle.h>
#include <casacore/casa/Utilities/Regex.h>
#include <casacore/casa/Exceptions/Error.h>

#include <casacore/casa/Logging/LogIO.h>
#include <casacore/casa/Logging/LogOrigin.h>

#include <casacore/casa/stdio.h>
#include <casacore/casa/string.h>
#include <casacore/casa/iostream.h>
#include <casacore/casa/fstream.h>             // needed for file IO
#include <casacore/casa/sstream.h>           // needed for internal IO


namespace casacore { //# NAMESPACE CASACORE - BEGIN

const int32_t lineSize = 32768;



//# Helper function.
//# Read a line and ignore lines to be skipped.
bool ReadAsciiTable::getLine (ifstream& file, int32_t& lineNumber,
			      char* line, int32_t lineSize,
			      bool testComment, const Regex& commentMarker,
			      int32_t firstLine, int32_t lastLine)
{
  int32_t dummy;
  while (true) {
    if (! file.getline (line, lineSize)) {
      return false;
    }
    int32_t nch = file.gcount();
    // Remove linefeed or newline.
    if (nch > 0) nch--;
    // Remove possible carriage return.
    if (nch > 1  &&  line[nch-1] == '\r') {
      nch--;
    }
    line[nch] = '\0';
    lineNumber++;
    if (lineNumber >= firstLine) {
      if (lastLine <= 0  ||  lineNumber <= lastLine) {
	if (! testComment) {
	  return true;
	}
	if (commentMarker.find (line, nch, dummy) != 0) {
	  return true;
	}
      }
    }
  }
}


//# Helper function 
//# It gets the next value from a line and stores it in result.
//# It updates at and returns the length of the value retrieved.
//# Quotes around strings are removed
//# -1 is returned if no more values are found.
int32_t ReadAsciiTable::getNext (const char* string, int32_t strlen, char* result,
			     int32_t& at, char separator)
{
    int32_t i = 0;
    bool found  = false;
    bool quoted = false;
    char ihave;
    // The next few lines are needed to treat e.g. a trailing comma as
    // a value.
    bool hasNext = false;
    if (at < 0) {
        at = -at;
	hasNext = true;
    }
    for (; at<strlen; at++) {
	ihave = string[at];
	if (ihave == '"') {
	    if (quoted) {
		quoted = false;
		continue;
	    }else{
		quoted = true;
		found  = true;
		continue;
	    }
	}
	if (ihave == '\0') {
	    result[i] = '\0';
	    return (found||hasNext  ?  i : -1);
	}
	if (quoted) {
	    result[i++] = ihave;
	    continue;
	}
	if (ihave == '\t') {
	    ihave = ' ';
	}
	if (ihave == separator) {
	    if (separator != ' ') {
	        found = true;
	        at++;
		at = -at;      // needed to recognize trailing comma
	    }
	    if (found) {
	        result[i] = '\0';
		return i;
	    }
	}
	if (ihave != ' ') {
	    found = true;
	}
	if (found) {
	    if (!quoted  &&  ihave != ' ') {
	        result[i++] = ihave; 
	    }
	}
    } 
    return -1;
}



void ReadAsciiTable::getTypes (const IPosition& shape,
			       const char* in, int32_t leng,
			       char* string1, char* string2, char separator)
{
    int32_t at = 0;
    int32_t i = 0;
    //# When constructing str in the while loop (in the else branch),
    //# a compiler bug appeared on RH systems.
    //# Therefore assignment is used instead.
    String str;
    while (getNext (in, leng, string2, at, separator) >= 0) {
        if (string2[0] == '\0') {
	    string1[0] = 'A';
	} else {
	    str = string2;
	    if (str.matches (RXint)) {
	        string1[0] = 'I';
	    } else if (str.matches (RXdouble)) {
	        string1[0] = 'D';
	    } else {
	        string1[0] = 'A';
	    }
	}
	string1++;
	char name[24];
	i++;
	sprintf (name, " Column%i", i);
	strcpy (string2, name);
	string2 += strlen(name);
	string2[0] = '\0';
	if (shape.nelements() > 0) {
	    ostringstream ostr;
	    for (uint32_t i=0; i<shape.nelements(); i++) {
	        if (i > 0) {
		    ostr << ',';
		}
	        ostr << shape(i);
	    }
	// There is probably a way to attach the char * to the ostringstream
	// but I'm not going to worry about it. wky 2003/02/27
	    strcpy(string1, ostr.str().data());
	    break;
	}
	string1[0] = ' ';
	string1++;
	string1[0] = '\0';
    }
}



//# Convert a string to a bool
bool ReadAsciiTable::makeBool (const String& str)
{
    if (str.length() == 0  ||  str == "0"  ||  str[0] == 'F'
    ||  str[0] == 'f'  || str[0] == 'N'  || str[0] == 'n') {
        return false;
    }
    return true;
}



//# Read a keyword set and add it to keysets.
void ReadAsciiTable::handleKeyset (int32_t lineSize, char* string1,
				   char* first, char* second,
				   TableRecord& keysets,
				   LogIO& logger,
				   const String& fileName,
				   ifstream& jFile,
				   int32_t& lineNumber,
				   char separator,
				   bool testComment,
				   const Regex& commentMarker,
				   int32_t firstLine, int32_t lastLine)
{
  TableRecord keyset;

  // Get the column name in case it is a column keywordset.
  String colName;
  int32_t atl = 0;
  getNext (string1, lineSize, first, atl, ' '); 
  int32_t d4 = getNext (string1, lineSize, second, atl, ' '); 
  if (d4 > 0) {
    colName = second;
  }
  while (true) {

// Read the next line(s)

    if (!getLine (jFile, lineNumber, string1, lineSize,
		  testComment, commentMarker,
		  firstLine, lastLine)) {
      throw AipsError ("ReadAsciiTable: no .endkey line in " + fileName);
    }

// If we are at END of KEYWORDS read the next line to get NAMES OF COLUMNS
// or to get next keyword group.

    if (strncmp(string1, ".endkey", 7) == 0) {
      if (!getLine (jFile, lineNumber, string1, lineSize,
		    testComment, commentMarker,
		    firstLine, lastLine)) {
	string1[0] = '\0';
      }
      break;
    }

    // Read the first two fields (name and type) of a KEYWORD line
    int32_t at3=0;
    int32_t done3 = getNext (string1, lineSize, first, at3, ' '); 
    int32_t done4 = getNext (string1, lineSize, second, at3, ' '); 
    if (done3<=0 || done4<=0) {
      throw AipsError ("ReadAsciiTable: no keyword name or type in line " +
		       String::toString(lineNumber)
		       + " of " + fileName);
    }
    String keyName = String(first);
    String keyType = String(second);
    keyType.upcase();
    if (keyset.isDefined (keyName)) {
      logger << LogIO::WARN <<
	"Keyword " << keyName << " skipped because defined twice in "
	     << fileName << LogIO::POST;
    } else {
      // Convert the type string to shape and type.
      IPosition keyShape;
      int32_t keyRAT;
      int32_t varAxis = getTypeShape (keyType, keyShape, keyRAT);
      // If no shape is given, the keyword can be a vector.
      bool shpDefined = keyShape.nelements() > 0;
      if (!shpDefined) {
	keyShape = IPosition(1,1);
	varAxis = 0;
      }
      // Get the keyword values from the line and store them in the set.
      switch (keyRAT) {
      case RATBool:
	{
	  Block<bool> values;
	  IPosition shp = getArray (string1, lineSize, first, at3, separator,
				    keyShape, varAxis, keyRAT, &values);
	  if (!shpDefined  &&  shp(0) == 1) {
	    keyset.define (keyName, values[0]);
	  } else {
	    Array<bool> array(shp, values.storage(), SHARE);
	    keyset.define (keyName, array);
	  }
	}
	break;
      case RATShort:
	{
	  Block<int16_t> values;
	  IPosition shp = getArray (string1, lineSize, first, at3, separator,
				    keyShape, varAxis, keyRAT, &values);
	  if (!shpDefined  &&  shp(0) == 1) {
	    keyset.define (keyName, values[0]);
	  } else {
	    Array<int16_t> array(shp, values.storage(), SHARE);
	    keyset.define (keyName, array);
	  }
	}
	break;
      case RATInt:
	{
	  Block<int32_t> values;
	  IPosition shp = getArray (string1, lineSize, first, at3, separator,
				    keyShape, varAxis, keyRAT, &values);
	  if (!shpDefined  &&  shp(0) == 1) {
	    keyset.define (keyName, values[0]);
	  } else {
	    Array<int32_t> array(shp, values.storage(), SHARE);
	    keyset.define (keyName, array);
	  }
	}
	break;
      case RATFloat:
	{
	  Block<float> values;
	  IPosition shp = getArray (string1, lineSize, first, at3, separator,
				    keyShape, varAxis, keyRAT, &values);
	  if (!shpDefined  &&  shp(0) == 1) {
	    keyset.define (keyName, values[0]);
	  } else {
	    Array<float> array(shp, values.storage(), SHARE);
	    keyset.define (keyName, array);
	  }
	}
	break;
      case RATDouble:
      case RATDMS:
      case RATHMS:
	{
	  Block<double> values;
	  IPosition shp = getArray (string1, lineSize, first, at3, separator,
				    keyShape, varAxis, keyRAT, &values);
	  if (!shpDefined  &&  shp(0) == 1) {
	    keyset.define (keyName, values[0]);
	  } else {
	    Array<double> array(shp, values.storage(), SHARE);
	    keyset.define (keyName, array);
	  }
	}
	break;
      case RATString:
	{
	  Block<String> values;
	  IPosition shp = getArray (string1, lineSize, first, at3, separator,
				    keyShape, varAxis, keyRAT, &values);
	  if (!shpDefined  &&  shp(0) == 1) {
	    keyset.define (keyName, values[0]);
	  } else {
	    Array<String> array(shp, values.storage(), SHARE);
	    keyset.define (keyName, array);
	  }
	}
	break;
      case RATComX:
      case RATComZ:
	{
	  Block<Complex> values;
	  IPosition shp = getArray (string1, lineSize, first, at3, separator,
				    keyShape, varAxis, keyRAT, &values);
	  if (!shpDefined  &&  shp(0) == 1) {
	    keyset.define (keyName, values[0]);
	  } else {
	    Array<Complex> array(shp, values.storage(), SHARE);
	    keyset.define (keyName, array);
	  }
	}
	break;
      case RATDComX:
      case RATDComZ:
	{
	  Block<DComplex> values;
	  IPosition shp = getArray (string1, lineSize, first, at3, separator,
				    keyShape, varAxis, keyRAT, &values);
	  if (!shpDefined  &&  shp(0) == 1) {
	    keyset.define (keyName, values[0]);
	  } else {
	    Array<DComplex> array(shp, values.storage(), SHARE);
	    keyset.define (keyName, array);
	  }
	}
	break;
      }
    }
  }
  if (keysets.isDefined (colName)) {
    logger << LogIO::WARN
	   << "Keywordset of column " << colName
	   << " skipped because defined twice in "
	   << fileName << LogIO::POST;
  } else {
    keysets.defineRecord (colName, keyset);
  }
}


int32_t ReadAsciiTable::getTypeShape (const String& typestr,
				  IPosition& shape, int32_t& type)
{
  shape.resize (0);
  int32_t varAxis = -1;
  // Split at each comma.
  Vector<String> vec = stringToVector (typestr);
  // The first value can be something like I10, so find first digit.
  // It should have a type before the first digit.
  uint32_t pos = vec(0).find (Regex("[0-9]"));
  if (pos == 0) {
    throw AipsError ("ReadAsciiTable: no type info in type string '" +
		     typestr + "'");
  }
  // Get type without shape info.
  // Note: need to convert pos to an int32_t because some compilers are more picky
  // about type safety, i.e. the native compilers for SGI and SUN.
  String tp = vec(0).before (int32_t(pos));
  if (pos >= vec(0).length()) {
    vec(0) = String();
    // Clear vector if no shape given at all.
    if (vec.nelements() == 1) {
      vec.resize (0);
    }
  } else {
    // Keep only length in first value.
    vec(0) = vec(0).from(int32_t(pos));
  }
  shape.resize (vec.nelements());
  Regex num("[0-9]+");
  // Check value and convert to integers.
  // One variable shaped axis is possible.
  for (uint32_t i=0; i<vec.nelements(); i++) {
    if (! vec(i).matches (num)) {
      throw AipsError ("ReadAsciiTable: invalid shape value '" + vec(i) +
		       "' in type string '" + typestr + "'");
    }
    istringstream istr(vec(i));
    istr >> shape(i);
    if (shape(i) <= 0) {
      if (varAxis >= 0) {
	throw AipsError ("ReadAsciiTable: multiple variable axes in "
			 "type string '" + typestr + "'");
      }
      varAxis = i;
      shape(i) = 1;
    }
  }
  if (tp == "B") {
    type = RATBool;
  } else if (tp == "S") {
    type = RATShort;
  } else if (tp == "I") {
    type = RATInt;
  } else if (tp == "R") {
    type = RATFloat;
  } else if (tp == "D") {
    type = RATDouble;
  } else if (tp == "DMS") {
    type = RATDMS;
  } else if (tp == "HMS") {
    type = RATHMS;
  } else if (tp == "A") {
    type = RATString;
  } else if (tp == "X") {
    type = RATComX;
  } else if (tp == "Z") {
    type = RATComZ;
  } else if (tp == "DX") {
    type = RATDComX;
  } else if (tp == "DZ") {
    type = RATDComZ;
  } else {
    throw AipsError ("ReadAsciiTable: invalid type specifier '" + tp + "'");
  }
  return varAxis;
}


double ReadAsciiTable::stringToPos (const String& str, bool isDMS)
{
  // This function is a bit more relaxed than MVAngle::read.
  // It allows whitespace. Furthermore it allows whitespace as separator.
  String strc(str);
  strc.downcase();
  // Remove blanks and insert : if only blanks.
  // Insert 0 if nothing between separators.
  String pos;
  bool foundBlanks = false;
  bool needSep = false;
  bool needNum = true;
  pos.reserve (strc.size());
  for (uint32_t i=0; i<strc.size(); ++i) {
    char ch = strc[i];
    if (ch == ' ') {
      foundBlanks = true;
    } else {
      if (ch==':' || ch=='.' || ch=='h' || ch=='d' || ch=='m') {
	if (needNum) {
	  pos += '0';
	}
	needSep = false;
	needNum = true;
      } else if (! (ch=='-' || ch=='+')) {
	if (needSep && foundBlanks) {
	  pos += ':';
	}
	needSep = true;
	needNum = false;
	foundBlanks = false;
      }
      pos += ch;
    }
  }
  Quantity res;
  bool ok = MVAngle::read (res, pos);
  if (!ok) {
    cerr << "ReadAsciiTable: " << pos
	 << " is not a valid MVAngle position value" << endl;
    return 0;
  }
  double val = res.getValue("rad");
  // If the string contains :, it was treated as HMS.
  // So divide by 15 if it is in fact DMS.
  if (isDMS  &&  pos.find(':') != String::npos) {
    val /= 15.;
  }
  return val;
}

bool ReadAsciiTable::getValue (char* string1, int32_t lineSize, char* first,
			       int32_t& at1, char separator,
			       int32_t type, void* value)
{
  float f1=0, f2=0;
  double d1=0, d2=0;
  bool more = true;
  int32_t done1 = getNext (string1, lineSize, first, at1, separator);
  if (done1 < 0) {
    more = false;
    done1 = 0;
    first[0] = '\0';
  }
  if(more){
  String dum(first, done1);
  switch (type) {
  case RATBool:
    *(bool*)value = makeBool(String(first, done1));
    break;
  case RATShort:
    if (done1 > 0) {
      istringstream(dum) >> *(int16_t*)value;
    } else {
      *(int16_t*)value = 0;
    }
    break;
  case RATInt:
    if (done1 > 0) {
      istringstream(dum) >> *(int32_t*)value;
    } else {
      *(int32_t*)value = 0;
    }
    break;
  case RATFloat:
    if (done1 > 0) {
      istringstream(dum) >> *(float*)value;
    } else {
      *(float*)value = 0;
    }
    break;
  case RATDouble:
    if (done1 > 0) {
      istringstream(dum) >> *(double*)value;
    } else {
      *(double*)value = 0;
    }
    break;
  case RATString:
    *(String*)value = String(first, done1);
    break;
  case RATDMS:
    *(double*)value = stringToPos (String(first, done1), true);
    break;
  case RATHMS:
    *(double*)value = stringToPos (String(first, done1), false);
    break;
  case RATComX:
    if (done1 > 0) {
      istringstream(dum) >> f1;
    }
    done1 = getNext (string1, lineSize, first, at1, separator);
    if (done1 > 0) {
      String dum2(first, done1);
      istringstream(dum2) >> f2;
    }
    *(Complex*)value = Complex(f1, f2);
    break;
  case RATDComX:
    if (done1 > 0) {
      istringstream(dum) >> d1;
    }
    done1 = getNext (string1, lineSize, first, at1, separator);
    if (done1 > 0) {
      String dum2(first, done1);
      istringstream(dum2) >> d2;
    }
    *(DComplex*)value = DComplex(d1, d2);
    break;
  case RATComZ:
    if (done1 > 0) {
      istringstream(dum) >> f1;
    }
    done1 = getNext (string1, lineSize, first, at1, separator);
    if (done1 > 0) {
      String dum2(first, done1);
      istringstream(dum2) >> f2;
    }
    f2 *= 3.14159265/180.0; 
    *(Complex*)value = Complex(f1*cos(f2), f1*sin(f2));
    break;
  case RATDComZ:
    if (done1 > 0) {
      istringstream(dum) >> d1;
    }
    done1 = getNext (string1, lineSize, first, at1, separator);
    if (done1 > 0) {
      String dum2(first, done1);
      istringstream(dum2) >> d2;
    }
    d2 *= 3.14159265/180.0; 
    *(DComplex*)value = DComplex(d1*cos(d2), d1*sin(d2));
    break;
  }
  }
  return more;
}


void ReadAsciiTable::handleScalar (char* string1, int32_t lineSize, char* first,
				   int32_t& at1, char separator,
				   int32_t type,
				   TableColumn& tabcol, rownr_t rownr)
{
  switch (type) {
  case RATBool:
    {
      bool value = false;
      getValue (string1, lineSize, first, at1, separator, type, &value);
      tabcol.putScalar (rownr, value);
    }
    break;
  case RATShort:
    {
      int16_t value = 0;
      getValue (string1, lineSize, first, at1, separator, type, &value);
      tabcol.putScalar (rownr, value);
    }
    break;
  case RATInt:
    {
      int32_t value = 0;
      getValue (string1, lineSize, first, at1, separator, type, &value);
      tabcol.putScalar (rownr, value);
    }
    break;
  case RATFloat:
    {
      float value = 0;
      getValue (string1, lineSize, first, at1, separator, type, &value);
      tabcol.putScalar (rownr, value);
    }
    break;
  case RATDouble:
  case RATDMS:
  case RATHMS:
    {
      double value = 0;
      getValue (string1, lineSize, first, at1, separator, type, &value);
      tabcol.putScalar (rownr, value);
    }
    break;
  case RATString:
    {
      String value;
      getValue (string1, lineSize, first, at1, separator, type, &value);
      tabcol.putScalar (rownr, value);
    }
    break;
  case RATComX:
  case RATComZ:
    {
      Complex value;
      getValue (string1, lineSize, first, at1, separator, type, &value);
      tabcol.putScalar (rownr, value);
    }
    break;
  case RATDComX:
  case RATDComZ:
    {
      DComplex value;
      getValue (string1, lineSize, first, at1, separator, type, &value);
      tabcol.putScalar (rownr, value);
    }
    break;
  }
}


IPosition ReadAsciiTable::getArray (char* string1, int32_t lineSize, char* first,
				    int32_t& at1, char separator,
				    const IPosition& shape, int32_t varAxis,
				    int32_t type, void* valueBlock)
{
  IPosition shp(shape);
  uint32_t nelem = shp.product();
  uint32_t nfound = 0;
  switch (type) {
  case RATBool:
    {
      Block<bool>& data = *(Block<bool>*)valueBlock;
      data.resize (nelem);
      data = false;
      bool value;
      while (getValue (string1, lineSize, first, at1, separator,
		       type, &value)) {
	if (nfound == data.nelements()) {
	  data.resize (2*nfound, true, true);
	}
	data[nfound++] = value;
	if (varAxis < 0  &&  nfound == data.nelements()) {
	  break;
	}
      }
      if (varAxis >= 0) {
	shp(varAxis) = (nfound + nelem - 1) / nelem;
	nelem = shp.product();
	if (nelem > nfound) {
	  if (nelem > data.nelements()) {
	    data.resize (nelem, true, true);
	  }
	  objset (&data[nfound], false, nelem-nfound);
	}
      }
    }
    break;
  case RATShort:
    {
      Block<int16_t>& data = *(Block<int16_t>*)valueBlock;
      data.resize (nelem);
      data = int16_t(0);
      int16_t value;
      while (getValue (string1, lineSize, first, at1, separator,
		       type, &value)) {
	if (nfound == data.nelements()) {
	  data.resize (2*nfound, true, true);
	}
	data[nfound++] = value;
	if (varAxis < 0  &&  nfound == data.nelements()) {
	  break;
	}
      }
      if (varAxis >= 0) {
	shp(varAxis) = (nfound + nelem - 1) / nelem;
	nelem = shp.product();
	if (nelem > nfound) {
	  if (nelem > data.nelements()) {
	    data.resize (nelem, true, true);
	  }
	  objset (&data[nfound], int16_t(0), nelem-nfound);
	}
      }
    }
    break;
  case RATInt:
    {
      Block<int32_t>& data = *(Block<int32_t>*)valueBlock;
      data.resize (nelem);
      data = false;
      int32_t value;
      while (getValue (string1, lineSize, first, at1, separator,
		       type, &value)) {
	if (nfound == data.nelements()) {
	  data.resize (2*nfound, true, true);
	}
	data[nfound++] = value;
	if (varAxis < 0  &&  nfound == data.nelements()) {
	  break;
	}
      }
      if (varAxis >= 0) {
	shp(varAxis) = (nfound + nelem - 1) / nelem;
	nelem = shp.product();
	if (nelem > nfound) {
	  if (nelem > data.nelements()) {
	    data.resize (nelem, true, true);
	  }
	  objset (&data[nfound], 0, nelem-nfound);
	}
      }
    }
    break;
  case RATFloat:
    {
      Block<float>& data = *(Block<float>*)valueBlock;
      data.resize (nelem);
      data = float(0);
      float value;
      while (getValue (string1, lineSize, first, at1, separator,
		       type, &value)) {
	if (nfound == data.nelements()) {
	  data.resize (2*nfound, true, true);
	}
	data[nfound++] = value;
	if (varAxis < 0  &&  nfound == data.nelements()) {
	  break;
	}
      }
      if (varAxis >= 0) {
	shp(varAxis) = (nfound + nelem - 1) / nelem;
	nelem = shp.product();
	if (nelem > nfound) {
	  if (nelem > data.nelements()) {
	    data.resize (nelem, true, true);
	  }
	  objset (&data[nfound], float(0), nelem-nfound);
	}
      }
    }
    break;
  case RATDouble:
  case RATDMS:
  case RATHMS:
    {
      Block<double>& data = *(Block<double>*)valueBlock;
      data.resize (nelem);
      data = double(0);
      double value;
      while (getValue (string1, lineSize, first, at1, separator,
		       type, &value)) {
	if (nfound == data.nelements()) {
	  data.resize (2*nfound, true, true);
	}
	data[nfound++] = value;
	if (varAxis < 0  &&  nfound == data.nelements()) {
	  break;
	}
      }
      if (varAxis >= 0) {
	shp(varAxis) = (nfound + nelem - 1) / nelem;
	nelem = shp.product();
	if (nelem > nfound) {
	  if (nelem > data.nelements()) {
	    data.resize (nelem, true, true);
	  }
	  objset (&data[nfound], double(0), nelem-nfound);
	}
      }
    }
    break;
  case RATString:
    {
      Block<String>& data = *(Block<String>*)valueBlock;
      data.resize (nelem);
      data = String();
      String value;
      while (getValue (string1, lineSize, first, at1, separator,
		       type, &value)) {
	if (nfound == data.nelements()) {
	  data.resize (2*nfound, true, true);
	}
	data[nfound++] = value;
	if (varAxis < 0  &&  nfound == data.nelements()) {
	  break;
	}
      }
      if (varAxis >= 0) {
	shp(varAxis) = (nfound + nelem - 1) / nelem;
	nelem = shp.product();
	if (nelem > nfound) {
	  if (nelem > data.nelements()) {
	    data.resize (nelem, true, true);
	  }
	  objset (&data[nfound], String(), nelem-nfound);
	}
      }
    }
    break;
  case RATComX:
  case RATComZ:
    {
      Block<Complex>& data = *(Block<Complex>*)valueBlock;
      data.resize (nelem);
      data = Complex();
      Complex value;
      while (getValue (string1, lineSize, first, at1, separator,
		       type, &value)) {
	if (nfound == data.nelements()) {
	  data.resize (2*nfound, true, true);
	}
	data[nfound++] = value;
	if (varAxis < 0  &&  nfound == data.nelements()) {
	  break;
	}
      }
      if (varAxis >= 0) {
	shp(varAxis) = (nfound + nelem - 1) / nelem;
	nelem = shp.product();
	if (nelem > nfound) {
	  if (nelem > data.nelements()) {
	    data.resize (nelem, true, true);
	  }
	  objset (&data[nfound], Complex(), nelem-nfound);
	}
      }
    }
    break;
  case RATDComX:
  case RATDComZ:
    {
      Block<DComplex>& data = *(Block<DComplex>*)valueBlock;
      data.resize (nelem);
      data = DComplex();
      DComplex value;
      while (getValue (string1, lineSize, first, at1, separator,
		       type, &value)) {
	if (nfound == data.nelements()) {
	  data.resize (2*nfound, true, true);
	}
	data[nfound++] = value;
	if (varAxis < 0  &&  nfound == data.nelements()) {
	  break;
	}
      }
      if (varAxis >= 0) {
	shp(varAxis) = (nfound + nelem - 1) / nelem;
	nelem = shp.product();
	if (nelem > nfound) {
	  if (nelem > data.nelements()) {
	    data.resize (nelem, true, true);
	  }
	  objset (&data[nfound], DComplex(), nelem-nfound);
	}
      }
    }
    break;
  }
  return shp;
}


void ReadAsciiTable::handleArray (char* string1, int32_t lineSize, char* first,
				  int32_t& at1, char separator,
				  const IPosition& shape, int32_t varAxis,
				  int32_t type,
				  TableColumn& tabcol, rownr_t rownr)
{
  switch (type) {
  case RATBool:
    {
      Block<bool> data;
      IPosition shp = getArray (string1, lineSize, first, at1, separator,
				shape, varAxis, type, &data);
      Array<bool> array(shp, data.storage(), SHARE);
      ArrayColumn<bool>(tabcol).put (rownr, array);
    }
    break;
  case RATShort:
    {
      Block<int16_t> data;
      IPosition shp = getArray (string1, lineSize, first, at1, separator,
				shape, varAxis, type, &data);
      Array<int16_t> array(shp, data.storage(), SHARE);
      ArrayColumn<int16_t>(tabcol).put (rownr, array);
    }
    break;
  case RATInt:
    {
      Block<int32_t> data;
      IPosition shp = getArray (string1, lineSize, first, at1, separator,
				shape, varAxis, type, &data);
      Array<int32_t> array(shp, data.storage(), SHARE);
      ArrayColumn<int32_t>(tabcol).put (rownr, array);
    }
    break;
  case RATFloat:
    {
      Block<float> data;
      IPosition shp = getArray (string1, lineSize, first, at1, separator,
				shape, varAxis, type, &data);
      Array<float> array(shp, data.storage(), SHARE);
      ArrayColumn<float>(tabcol).put (rownr, array);
    }
    break;
  case RATDouble:
  case RATDMS:
  case RATHMS:
    {
      Block<double> data;
      IPosition shp = getArray (string1, lineSize, first, at1, separator,
				shape, varAxis, type, &data);
      Array<double> array(shp, data.storage(), SHARE);
      ArrayColumn<double>(tabcol).put (rownr, array);
    }
    break;
  case RATString:
    {
      Block<String> data;
      IPosition shp = getArray (string1, lineSize, first, at1, separator,
				shape, varAxis, type, &data);
      Array<String> array(shp, data.storage(), SHARE);
      ArrayColumn<String>(tabcol).put (rownr, array);
    }
    break;
  case RATComX:
  case RATComZ:
    {
      Block<Complex> data;
      IPosition shp = getArray (string1, lineSize, first, at1, separator,
				shape, varAxis, type, &data);
      Array<Complex> array(shp, data.storage(), SHARE);
      ArrayColumn<Complex>(tabcol).put (rownr, array);
    }
    break;
  case RATDComX:
  case RATDComZ:
    {
      Block<DComplex> data;
      IPosition shp = getArray (string1, lineSize, first, at1, separator,
				shape, varAxis, type, &data);
      Array<DComplex> array(shp, data.storage(), SHARE);
      ArrayColumn<DComplex>(tabcol).put (rownr, array);
    }
    break;
  }
}


Table ReadAsciiTable::makeTab (String& formatString,
			       Table::TableType tableType,
			       const String& headerfile, const String& filein, 
			       const String& tableproto,
			       const String& tablename,
			       bool autoHeader, const IPosition& autoShape,
			       const Vector<String>& columnNames,
			       const Vector<String>& dataTypes,
			       char separator,
			       bool testComment, const Regex& commentMarker,
			       int32_t firstLine, int32_t lastLine)
{
    char  string1[lineSize], string2[lineSize], stringsav[lineSize];
    char  first[lineSize], second[lineSize];
    Block<String>  nameOfColumn(100);
    Block<String>  tstrOfColumn(100);
    String  keyName;

    LogIO logger(LogOrigin("readAsciiTable", WHERE));

// Determine if column names are already given.
    bool hdrGiven = false;
    if (columnNames.nelements() > 0  ||  dataTypes.nelements() > 0) {
        if (columnNames.nelements() != dataTypes.nelements()) {
	    throw AipsError ("ReadAsciiTable: vector of columnNames and "
			     "dataTypes should have equal length");
	}
	hdrGiven   = true;
	autoHeader = false;
    }

// Determine if header and data are in one file.
    bool oneFile = (headerfile == filein);
    int32_t firstHeaderLine = 1;
    int32_t lastHeaderLine = -1;
    if (oneFile) {
        firstHeaderLine = firstLine;
	lastHeaderLine  = lastLine;
    }

// PART ONE
// Define the TABLE description, i.e. define its columns.
// Create the description as scratch if no name is given.

    TableDesc td (tableproto,
		  (tableproto.empty() ? TableDesc::Scratch : TableDesc::New));
    ifstream jFile;
    Path headerPath(headerfile);
    String hdrName = headerPath.expandedName();
    jFile.open(hdrName.chars(), ios::in);
    if (! jFile) {
        throw AipsError ("ReadAsciiTable: file " + hdrName +
			 " not found or unreadable" );
    }

// Read the first line. It will be KEYWORDS or NAMES OF COLUMNS

    int32_t lineNumber = 0;
    if (!getLine (jFile, lineNumber, string1, lineSize,
		  testComment, commentMarker,
		  firstHeaderLine, lastHeaderLine)) {
        throw AipsError
	  ("ReadAsciiTable: cannot read first header line of " + headerfile);
    }

    // If the first line shows that we have KEYWORDS read until the
    // end of keywords while assembling the keywords.

    TableRecord keysets;
    while (strncmp(string1, ".key", 4) == 0) {
        handleKeyset (lineSize, string1, first, second,
		      keysets, logger,
		      headerfile, jFile, lineNumber, separator,
		      testComment, commentMarker,
		      firstHeaderLine, lastHeaderLine);
    }

    // Okay, all keywords have been read.
    // string1 contains the next line (if any).
    // Read the column definition lines from header file (if needed).
    // Determine the types if autoheader is given.

// Previous line should be NAMES OF COLUMNS; now get TYPE OF COLUMNS line
    if (!autoHeader  &&  !hdrGiven) {
        if (string1[0] == '\0') {
	    throw AipsError ("ReadAsciiTable: no COLUMN NAMES line in " +
			     headerfile);
	}
	if (!getLine (jFile, lineNumber, string2, lineSize,
		      testComment, commentMarker,
		      firstHeaderLine, lastHeaderLine)) {
	    throw AipsError ("ReadAsciiTable: no COLUMN TYPES line in " +
			     headerfile);
	}
    }

// Now open the actual data file (if not the same as header file).
// Read the first line if auto header.

    if (!oneFile) {
        jFile.close();
	Path filePath(filein);
	String fileName = filePath.expandedName();
	jFile.open(fileName.chars(), ios::in);
	if (! jFile) {
	    throw AipsError ("ReadAsciiTable: input file " + fileName +
			     " not found or unreadable");
	}
	lineNumber = 0;
	if (autoHeader) {
	    if (!getLine (jFile, lineNumber, string1, lineSize,
			  testComment, commentMarker,
			  firstLine, lastLine)) {
	        string1[0] = '\0';
	    }
	}
    }

    // Process the auto header.
    // Save string, because it'll be overwritten.
    stringsav[0] = '\0';
    if (autoHeader) {
        strcpy (stringsav, string1);
        getTypes (autoShape, string1, lineSize, string2, first, separator);
	strcpy (string1, first);
    } else if (hdrGiven) {
        strcpy (stringsav, string1);
    }

// Break up the NAME OF COLUMNS line and the TYPE OF COLUMNS line
// Place the results in the two arrays.
// Also put in in a single string to be returned to the caller.
// The separator in a header line is the given separator if found in it.
// Otherwise it is a blank.

    int nrcol = 0;
    if (hdrGiven) {
        nrcol = columnNames.size();
	nameOfColumn.resize (nrcol);
	tstrOfColumn.resize (nrcol);
	for (int i=0; i<nrcol; ++i) {
	    nameOfColumn[i] = columnNames[i];
	    tstrOfColumn[i] = dataTypes[i];
	}
    } else {
        char sep1 = separator;
	char sep2 = separator;
	if (String(string1).find(separator) == String::npos) sep1 = ' ';
	if (String(string2).find(separator) == String::npos) sep2 = ' ';
	int32_t done1 = 0, done2 = 0, at1 = 0, at2 = 0;
	while (done1 >= 0) {
	    done1 = getNext (string1, lineSize, first, at1, sep1);
	    done2 = getNext (string2, lineSize, second, at2, sep2);
	    if (done1>0 && done2>0) {
	        if (nrcol >= int32_t(nameOfColumn.nelements())) {
		    nameOfColumn.resize (2*nrcol, true, true);
		    tstrOfColumn.resize (2*nrcol, true, true);
		}
		nameOfColumn[nrcol] = String(first);
		tstrOfColumn[nrcol] = String(second);
		nrcol++;
	    } else if (done1>=0 || done2>=0) {
	        throw AipsError ("ReadAsciiTable: mismatching COLUMN NAMES "
				 "and TYPES lines in " + headerfile);
	    }
	}
    }
    // Generate a format string.
    String formStr;
    for (int i=0; i<nrcol; ++i) {
        tstrOfColumn[i].upcase();
	if (! formStr.empty()) {
	    formStr += ", ";
	}
	formStr += nameOfColumn[i] + "=" + tstrOfColumn[i];
    }

// Create the TABLE Columns for these variables

    Block<IPosition> shapeOfColumn(nrcol);
    Block<int32_t>       typeOfColumn(nrcol);
    int32_t              varAxis=0;

    for (int32_t i5=0; i5<nrcol; i5++) {
        varAxis = getTypeShape (tstrOfColumn[i5],
				shapeOfColumn[i5],
				typeOfColumn[i5]);
	if (varAxis >= 0  &&  i5 != nrcol-1) {
	  throw AipsError ("ReadAsciiTable: "
			   "only last column can have variable shaped arrays");
	}
	if (shapeOfColumn[i5].nelements() > 0) {
	  IPosition shape;
	  int32_t option = 0;
	  if (varAxis < 0) {
	    shape = shapeOfColumn[i5];
	    option = ColumnDesc::Direct | ColumnDesc::FixedShape;
	  }
	  switch (typeOfColumn[i5]) {
	  case RATBool:
	    td.addColumn (ArrayColumnDesc<bool> (nameOfColumn[i5],
						 shape, option));
	    break;
	  case RATShort:
	    td.addColumn (ArrayColumnDesc<int16_t> (nameOfColumn[i5],
						  shape, option));
	    break;
	  case RATInt:
	    td.addColumn (ArrayColumnDesc<int32_t> (nameOfColumn[i5],
						shape, option));
	    break;
	  case RATFloat:
	    td.addColumn (ArrayColumnDesc<float> (nameOfColumn[i5],
						  shape, option));
	    break;
	  case RATDouble:
	    td.addColumn (ArrayColumnDesc<double> (nameOfColumn[i5],
						   shape, option));
	    break;
	  case RATDMS:
	  case RATHMS:
	    {
	      td.addColumn (ArrayColumnDesc<double> (nameOfColumn[i5],
						     shape, option));
	      ColumnDesc& cd = td.rwColumnDesc(nameOfColumn[i5]);
	      cd.rwKeywordSet().define ("QuantumUnits",
					Vector<String>(1, "rad"));
	    }
	    break;
	  case RATString:
	    td.addColumn (ArrayColumnDesc<String> (nameOfColumn[i5],
						   shape, option));
	    break;
	  case RATComX:
	  case RATComZ:
	    td.addColumn (ArrayColumnDesc<Complex> (nameOfColumn[i5],
						     shape, option));
	    break;
	  case RATDComX:
	  case RATDComZ:
	    td.addColumn (ArrayColumnDesc<DComplex> (nameOfColumn[i5],
						     shape, option));
	    break;
	  }
	} else {
	  switch (typeOfColumn[i5]) {
	  case RATBool:
	    td.addColumn (ScalarColumnDesc<bool> (nameOfColumn[i5]));
	    break;
	  case RATShort:
	    td.addColumn (ScalarColumnDesc<int16_t> (nameOfColumn[i5]));
	    break;
	  case RATInt:
	    td.addColumn (ScalarColumnDesc<int32_t> (nameOfColumn[i5]));
	    break;
	  case RATFloat:
	    td.addColumn (ScalarColumnDesc<float> (nameOfColumn[i5]));
	    break;
	  case RATDouble:
	  case RATDMS:
	  case RATHMS:
	    td.addColumn (ScalarColumnDesc<double> (nameOfColumn[i5]));
	    break;
	  case RATString:
	    td.addColumn (ScalarColumnDesc<String> (nameOfColumn[i5]));
	    break;
	  case RATComX:
	  case RATComZ:
	    td.addColumn (ScalarColumnDesc<Complex> (nameOfColumn[i5]));
	    break;
	  case RATDComX:
	  case RATDComZ:
	    td.addColumn (ScalarColumnDesc<DComplex> (nameOfColumn[i5]));
	    break;
	  }
	}
    }

// PART TWO
// The TableDesc has now been created.  Start filling in the Table.
// Use the default storage manager.

    SetupNewTable newtab(tablename, td, Table::New);
    Table tab(newtab, tableType);

// Write keywordsets.

    for (uint32_t i=0; i<keysets.nfields(); i++) {
        String colnm = keysets.name(i);
        if (colnm.empty()) {
	    tab.rwKeywordSet() = keysets.subRecord (i);
	} else {
	    if (!tab.tableDesc().isColumn (colnm)) {
	        logger << LogIO::WARN
		       << "Keywordset of column " << colnm
		       << " skipped because column is not defined in "
		       << headerfile << LogIO::POST;
	    } else {
	        TableColumn tabcol (tab, colnm);
		tabcol.rwKeywordSet() = keysets.subRecord (i);
	    }
	}
    }

    TableColumn* tabcol = new TableColumn[nrcol];
    for (int32_t i=0; i<nrcol; i++) {
	tabcol[i].reference (TableColumn (tab, nameOfColumn[i]));
    }
    rownr_t rownr = 0;

// OK, Now we have real data
// stringsav may contain the first data line.

    int at1=0;
    bool cont = true;
    if (stringsav[0] == '\0') {
        cont = getLine (jFile, lineNumber, string1, lineSize,
			testComment, commentMarker,
			firstLine, lastLine);
    } else {
        strcpy (string1, stringsav);
    }
    while (cont) {
	at1 = 0; 
	tab.addRow();
	for (int32_t i6=0; i6<nrcol; i6++) {
	    if (shapeOfColumn[i6].nelements() > 0) {
	        int32_t varAx = (i6 == nrcol-1  ?  varAxis : -1);
		handleArray (string1, lineSize, first,
			     at1, separator,
			     shapeOfColumn[i6], varAx,
			     typeOfColumn[i6],
			     tabcol[i6], rownr);
	    } else {
		handleScalar (string1, lineSize, first,
			      at1, separator,
			      typeOfColumn[i6],
			      tabcol[i6], rownr);
	    }
	}
	rownr++;
        cont = getLine (jFile, lineNumber, string1, lineSize,
			testComment, commentMarker,
			firstLine, lastLine);
    }

    delete [] tabcol;
    jFile.close();
    formatString = formStr;
    return tab;
}


String ReadAsciiTable::doRun (const String& headerfile, const String& filein, 
			      const String& tableproto,
			      const String& tablename,
			      bool autoHeader, const IPosition& autoShape,
			      const Vector<String>& columnNames,
			      const Vector<String>& dataTypes,
			      char separator,
			      bool testComment, const Regex& commentMarker,
			      int32_t firstLine, int32_t lastLine)
{
  String formatString;
  Table tab = makeTab (formatString, Table::Plain,
		       headerfile, filein, tableproto, tablename,
		       autoHeader, autoShape, columnNames, dataTypes,
		       separator, testComment, commentMarker,
		       firstLine, lastLine);
  return formatString;
}

String ReadAsciiTable::run (const String& headerfile, const String& filein, 
			    const String& tableproto, const String& tablename,
			    bool autoHeader, const IPosition& autoShape,
			    const Vector<String>& columnNames,
			    const Vector<String>& dataTypes,
			    char separator,
			    const String& commentMarkerRegex,
			    int32_t firstLine, int32_t lastLine)
{
  if (firstLine < 1) {
    firstLine = 1;
  }
  //# The Regex is made here (instead of creating a temporary Regex
  //# in the doRun call).
  //# For one reason or another the temporary gives a bus error with gcc-3.3
  //# on Solaris in the tryerror calls in tReadAsciiTable.
  Regex regex;
  if (commentMarkerRegex.empty()) {
    return doRun (headerfile, filein, tableproto, tablename,
		  autoHeader, autoShape, columnNames, dataTypes,
		  separator, false, regex,
		  firstLine, lastLine);
  } else {
    regex = Regex(commentMarkerRegex);
    return doRun (headerfile, filein, tableproto, tablename,
		  autoHeader, autoShape, columnNames, dataTypes,
		  separator, true, regex,
		  firstLine, lastLine);
  }
}

Table ReadAsciiTable::runt (String& formatString, Table::TableType tableType,
			    const String& headerfile, const String& filein, 
			    const String& tableproto, const String& tablename,
			    bool autoHeader, const IPosition& autoShape,
			    const Vector<String>& columnNames,
			    const Vector<String>& dataTypes,
			    char separator,
			    const String& commentMarkerRegex,
			    int32_t firstLine, int32_t lastLine)
{
  if (firstLine < 1) {
    firstLine = 1;
  }
  //# The Regex is made here (instead of creating a temporary Regex
  //# in the doRun call).
  //# For one reason or another the temporary gives a bus error with gcc-3.3
  //# on Solaris in the tryerror calls in tReadAsciiTable.
  Regex regex;
  if (commentMarkerRegex.empty()) {
    return makeTab (formatString, tableType,
		    headerfile, filein, tableproto, tablename,
		    autoHeader, autoShape, columnNames, dataTypes, separator,
		    false, regex,
		    firstLine, lastLine);
  } else {
    regex = Regex(commentMarkerRegex);
    return makeTab (formatString, tableType,
		    headerfile, filein, tableproto, tablename,
		    autoHeader, autoShape, columnNames, dataTypes, separator,
		    true, regex,
		    firstLine, lastLine);
  }
}

String readAsciiTable (const String& filein, const String& tableproto,
		       const String& tablename, bool autoHeader,
		       char separator, const String& commentMarkerRegex,
		       int32_t firstLine, int32_t lastLine,
		       const IPosition& autoShape)
{
  Vector<String> dumvec;
  return ReadAsciiTable::run (filein, filein, tableproto, tablename,
			      autoHeader, autoShape, dumvec, dumvec,
			      separator, commentMarkerRegex,
			      firstLine, lastLine);
}

String readAsciiTable (const String& filein, const String& tableproto,
		       const String& tablename,
		       const Vector<String>& columnNames,
		       const Vector<String>& dataTypes,
		       char separator, const String& commentMarkerRegex,
		       int32_t firstLine, int32_t lastLine)
{
  return ReadAsciiTable::run (filein, filein, tableproto, tablename,
			      false, IPosition(), columnNames, dataTypes,
			      separator, commentMarkerRegex,
			      firstLine, lastLine);
}

String readAsciiTable (const String& headerfile, const String& filein,
		       const String& tableproto, const String& tablename,
		       char separator, const String& commentMarkerRegex,
		       int32_t firstLine, int32_t lastLine)
{
  Vector<String> dumvec;
  return ReadAsciiTable::run (headerfile, filein, tableproto, tablename,
			      false, IPosition(), dumvec, dumvec,
			      separator, commentMarkerRegex,
			      firstLine, lastLine);
}

String readAsciiTable (const String& headerfile, const String& filein,
		       const String& tableproto, const char* tablename,
		       char separator, const String& commentMarkerRegex,
		       int32_t firstLine, int32_t lastLine)
{
  Vector<String> dumvec;
  return ReadAsciiTable::run (headerfile, filein, tableproto,
			      String(tablename),
			      false, IPosition(), dumvec, dumvec,
			      separator, commentMarkerRegex,
			      firstLine, lastLine);
}

Table readAsciiTable (String& formatString, Table::TableType tableType,
		      const String& filein, const String& tableproto,
		      const String& tablename, bool autoHeader,
		      char separator, const String& commentMarkerRegex,
		      int32_t firstLine, int32_t lastLine,
		      const IPosition& autoShape)
{
  Vector<String> dumvec;
  return ReadAsciiTable::runt (formatString, tableType,
			       filein, filein, tableproto, tablename,
			       autoHeader, autoShape, dumvec, dumvec,
			       separator, commentMarkerRegex,
			       firstLine, lastLine);
}

Table readAsciiTable (String& formatString, Table::TableType tableType,
		      const String& filein, const String& tableproto,
		      const String& tablename,
		      const Vector<String>& columnNames,
		      const Vector<String>& dataTypes,
		      char separator, const String& commentMarkerRegex,
		      int32_t firstLine, int32_t lastLine)
{
  return ReadAsciiTable::runt (formatString, tableType,
			       filein, filein, tableproto, tablename,
			       false, IPosition(), columnNames, dataTypes,
			       separator, commentMarkerRegex,
			       firstLine, lastLine);
}

Table readAsciiTable (String& formatString, Table::TableType tableType,
		      const String& headerfile, const String& filein,
		      const String& tableproto, const String& tablename,
		      char separator, const String& commentMarkerRegex,
		      int32_t firstLine, int32_t lastLine)
{
  Vector<String> dumvec;
  return ReadAsciiTable::runt (formatString, tableType,
			       headerfile, filein, tableproto, tablename,
			       false, IPosition(), dumvec, dumvec,
			       separator, commentMarkerRegex,
			       firstLine, lastLine);
}

Table readAsciiTable (String& formatString, Table::TableType tableType,
		      const String& headerfile, const String& filein,
		      const String& tableproto, const char* tablename,
		      char separator, const String& commentMarkerRegex,
		      int32_t firstLine, int32_t lastLine)
{
  Vector<String> dumvec;
  return ReadAsciiTable::runt (formatString, tableType,
			       headerfile, filein, tableproto,
			       String(tablename),
			       false, IPosition(), dumvec, dumvec,
			       separator, commentMarkerRegex,
			       firstLine, lastLine);
}

} //# NAMESPACE CASACORE - END

