//# Utilities.h: Bag of unrelated classes and groups for general use.
//# Copyright (C) 1995,1996,1997,2000,2001
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


#ifndef CASA_UTILITIES_H
#define CASA_UTILITIES_H

#include <casacore/casa/aips.h>

#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Utilities/BinarySearch.h>
#include <casacore/casa/Utilities/BitVector.h>
#include <casacore/casa/Utilities/Compare.h>
#include <casacore/casa/Utilities/Copy.h>
#include <casacore/casa/Utilities/CountedPtr.h>
#include <casacore/casa/Utilities/DataType.h>
#include <casacore/casa/Utilities/DynBuffer.h>
#include <casacore/casa/Utilities/Fallible.h>
#include <casacore/casa/Utilities/GenSort.h>
#include <casacore/casa/Utilities/Notice.h>
#include <casacore/casa/Utilities/PtrHolder.h>
#include <casacore/casa/Utilities/RegSequence.h>
#include <casacore/casa/Utilities/Regex.h>
#include <casacore/casa/Utilities/Register.h>
#include <casacore/casa/Utilities/Sequence.h>
#include <casacore/casa/Utilities/Sort.h>
#include <casacore/casa/Utilities/SortError.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/casa/Utilities/ValType.h>
#include <casacore/casa/Utilities/cregex.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

// <module>
//
// <summary> Classes and global functions for general use </summary>

// <reviewed reviewer="Friso Olnon" date="1995/03/20" demos="">
// </reviewed>

// <synopsis>
//
// This module is a bag of unrelated mini-modules, classes and
// global functions. The following functional groups can be recognized:
// <ul>
//  <li> Object utilities:
//  <ul>
//   <li> <linkto class=ObjCompare>ObjCompare</linkto>
//    objects with each other. A signature for comparison functions 
//    is defined (required for comparison functions used in the
//    <linkto class=Sort>Sort</linkto>
//    class), and one such function is provided.
//   <li> <linkto group="Copy.h#copy">objcopy/objmove/objset</linkto>
//    copies objects from one place to another.
//   <li> <linkto class=Fallible>Mark</linkto>
//    objects as valid or invalid.
//   <li> <linkto class=Notice>Notices</linkto>
//    provide basic support for shared access of data by various objects.
//   <li> <linkto class=Sort>Sort</linkto>
//    objects on one or more keys, in ascending or descending order.
//    <linkto class=GenSort>Fast sorting</linkto>
//    is provided for certain types of objects.
//   <li> <linkto group="BinarySearch.h#binarysearch">Binary Search</linkto>
//    templated functions for sorted containers (ascending or descending
//    order) are available.
//   <li> <linkto group="LinearSearch.h#linearsearch">Linear Search</linkto>
//    templated functions for unsorted containers are available.
//  </ul> 
//  <li> Logical utilities:
//  <ul>
//   <li> <linkto class=assert_>Assertion</linkto>
//    lets you throw an error when a condition in not fullfilled.
//   <li> <linkto class=BitVector>Bit vectors</linkto>
//    are an efficient method to keep True/False information on a set of
//    items or conditions.
//  </ul> 
//  <li> Pointer utilities
//  <ul>
//   <li> <linkto class=CountedPtr>Counted pointers</linkto>
//    provide support for reference counting.
//   <li> <linkto class=PtrHolder>Pointer holders</linkto>
//    can be used to hold allocated pointers which should be deleted
//    when an exception is thrown.
//  </ul> 
//  <li> Datatype utilities
//  <ul>
//   <li> <linkto group="DataType.h#DataType">DataType</linkto>
//    enumerates the possible data types in the table system.
//   <li> <linkto class=ValType>ValType</linkto>
//    describes the data types and their undefined values.
//   <li> <linkto group="Register.h#register">Register</linkto>
//    provides runtime typing information.
//  </ul> 
//  <li> Other utilities
//  <ul>
//   <li> <linkto class=DynBuffer>Dynamic buffers</linkto>
//    are used to store data in dynamically allocated buffers.
//   <li> <linkto class=Regex>Regular expressions</linkto>
//    are supported by the class <linkto class=Regex>Regex</linkto> and
//    the associated function library 
//    <a href="Utilities/cregex.html">cregex</a>.
//   <li> <linkto class=Sequence>Sequences</linkto>
//    of any datatype can be derived from the base class
//    <linkto class=Sequence>Sequence</linkto>.
//    One example is <linkto class=uIntSequence>uIntSequence</linkto>,
//    provided for general use. Another is
//    <linkto class=RegSequence>RegSequence</linkto>, exclusively used
//    by the <linkto group="Register.h#register">Register</linkto> function.
//   <li> <linkto class=String>Strings</linkto>.
//    for the C++ preprocessor
//  </ul>
// </ul>
//
// <note role=tip> You may want to look at the individual header files
// to see whether you might not prefer to include only the header
// files you really need; it may be more efficient to do so.
// </note>
//
// </synopsis>

//# <todo asof="1995/03/20">
//#   <li>
//# </todo>

// </module>


} //# NAMESPACE CASACORE - END

#endif

