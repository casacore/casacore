//# Converters.h: The Converters module - Boost.Python converters
//# Copyright (C) 2006
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
//# $Id: Converters.h,v 1.1 2006/12/13 01:24:15 gvandiep Exp $

#ifndef PYRAP_CONVERTERS_H
#define PYRAP_CONVERTERS_H

//# Includes
#include <pyrap/Converters/PycBasicData.h>
#include <pyrap/Converters/PycRecord.h>
#include <pyrap/Converters/PycValueHolder.h>
#include <pyrap/Converters/PycExcp.h>


namespace casa { //# NAMESPACE CASA - BEGIN

// <module>

// <summary>
// Convert casacore objects to/from Python (using Boost.Python)
// </summary>

// <use visibility=export>

// <reviewed reviewer="" date="" tests="tConvert" demos="">
// </reviewed>

// <prerequisite>
//    <li> <linkto class="Record">Record</linkto> class
//    <li> <linkto class="ValueHolder">Record</linkto> class
// </prerequisite>

// <synopsis> 
// Converters contains functions to convert the important AIPS++ objects
// to/from Python using the Boost.Python package.
// Converters for the following AIPS++ classes exist:
// <ul>
//  <li> Scalars of basic data types like Bool, Int, Float, Complex.
//  <li> casa::String and std::string.
//  <li> casa::Vector and std::vector of any type which can be converted
//       from a Python scalar, list, tuple, or 1-dim array. They are converted
//       back to a Python list.
//  <li> Record which is converted to/from a Python dict.
//  <li> ValueHolder which is converted to/from the appropriate Python type.
//       A ValueHolder is a class which can hold various value types:
//   <ul>
//    <li> Scalar of any basic data type.
//    <li> Record.
//    <li> N-dim Array of any basic data type. A casa::Array can be
//         constructed from Python types like tuple, list, and array
//         (numarray or numpy). A Py_None object results in an empty array.
//         The conversion back is done to a Python array. The type (numarray
//         or numpy) is determined by the package loaded (and supported).
//         If both or none are loaded, a numpy array is returned. An empty
//         array is returned as an empty Python array.
//         <br>Because AIPS++ arrays are in Fortran order and Python
//         arrays in C order, the axes are reversed during conversion.
//         <br>A 1-dim <src>Array<String></src> object is converted to a list,
//         while a higher dimensioned Array<String> object is converted to/from
//         a dict containing the shape and the values as a list.
//         However, conversion from a numpy string array is supported.
//   </ul>
//       A ValueHolder is for instance used by the Table System to be able
//       to get or put data in a column of any type.
//       It can be used by any Python binding to convert arrays. Class
//       ValueHolder has functions to get or set the array.
//  <li> casa::IPosition which can be converted
//       from a Python scalar, list, tuple, or 1-dim array.
//       It is converted back to a Python list.
//       An IPosition object represents an array shape or position, so its
//       values are reversed because of the different ordening of
//       AIPS++ and Python arrays.
//  <li> Exceptions, which are mapped to a Python <src>RuntimeError</src>
//       exception. Only the <src>casa::IterError</src> exception is mapped
//       to a Python <src>StopIteration</src> exception.
// </ul>
// Elements in a numpy array are called array scalars. They do not have a python
// type like <src>int</src>, but instead a type like <src>numpy.int32</src>.
// The converters can handle such types and convert them correctly to a scalar.
// Of course, the converters also can handle sequences containing such types.
//
// A numpy or numarray scalar array (e.g. <src>array(1.0)</src> is a somewhat
// peculiar object that cannot be indexed. It is handled correctly by the
// converters and handled as a 1-dim array containing one element.
// </synopsis>

// </module>

} //# NAMESPACE CASA - END

#endif
