//# Json.h: Classes to read or write a Json file
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


#ifndef CASA_JSON_H
#define CASA_JSON_H

#include <casacore/casa/aips.h>

#include <casacore/casa/Json/JsonOut.h>
#include <casacore/casa/Json/JsonValue.h>
#include <casacore/casa/Json/JsonParser.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

// <module>
//
// <summary> Classes to read or write a JSON file </summary>

// <reviewed reviewer="" date="" demos="">
// </reviewed>

// <synopsis>
// This module handles JSON input and output. It supports the full Json
// syntax (including null values) with the addition of complex numbers
// represented as a nested JSON struct with fields "r" and "i".
// <br>NaN and infinite floating point numbers are written as a null value.
// <br>It supports comments in the JSON file using C, C++ or Python style
// delimiters.
//
// It consists of the following classes:
// <ul>
//   <li> <linkto class=JsonOut>JsonOut</linkto>
//    to create and populate a JSON file. Besides support of scalar values
//    and std::vector, it also supports Array, Record and ValueHolder objects.
//   <li> <linkto class=JsonOut>JsonParser</linkto>
//    to parse a JSON file and store the results in a JsonKVMap object.
//    The parser only accepts the rather strict JSON number representation.
//   <li> <linkto class=JsonOut>JsonKVMap</linkto>
//    to obtain the results from a parsed JSON file. It is possible to
//    obtain a (possible nested) sequence as an Array object.
// </ul>
// </synopsis>

//# <todo asof="1995/03/20">
//#   <li>
//# </todo>

// </module>


} //# NAMESPACE CASACORE - END

#endif

