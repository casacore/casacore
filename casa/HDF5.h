//# HDF5.h: Classes binding casa to the HDF5 C API
//# Copyright (C) 2008
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

#ifndef CASA_HDF5_H
#define CASA_HDF5_H

//# Includes
#include <casacore/casa/aips.h>

#include <casacore/casa/HDF5/HDF5File.h>
#include <casacore/casa/HDF5/HDF5Group.h>
#include <casacore/casa/HDF5/HDF5DataSet.h>
#include <casacore/casa/HDF5/HDF5Record.h>
#include <casacore/casa/HDF5/HDF5Error.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

// <module>
//
// <summary>
// Classes binding casa to the HDF5 C API
// </summary>

// <prerequisite>
//   <li> HDF5 (see http://www.hdfgroup.uiuc.edu/HDF5/doc_dev_snapshot)
// </prerequisite>

// <reviewed reviewer="" date="" demos="">
// </reviewed>

// <etymology>
// 'HDF5' is version 5 of the Hierarchical Data Format.
// </etymology>
//
// <synopsis>
// This module's main purpose is to provide limited, but convenient
// access to the HDF5 C API.
// The classes offer the following services:
// <ul>
//  <li> The burden of allocating and releasing the HDF5 resources
//       (the so-called hid-s) is handled in an automatic and
//       exception-safe way.
//  <li> The overwhelming and rather hard to use HDF5 C API is hidden.
//  <li> The standard casa data types (scalars and arrays) are fully supported.
//       Because HDF5 does not support empty strings, they are transparantly
//       replaced by the value '__empty__'.
//       Unfortunately HDF5 does not support empty arrays, thus they cannot
//       be stored.
//  <li> A Record is stored as a group. Its values (scalars and arrays)
//       are stored as attributes, while nested records are stored as
//       nested groups.
//  <li> Bool values are stored as chars.
//  <li> Complex values are stored as compounds of two real values.
//  <li> Large arrays can be stored in HDF5 DataSets with an optional tile size
//       (chunk size in HDF5 terminology). The array axes are reversed (fully
//       transparantly) because HDF5 uses C-order, while casa Arrays use
//       Fortran-order.
//  <li> Automatic printing of HDF5 messages is disabled. All errors are
//       thrown as exceptions (AipsError or derived from it).
// </ul>
//
// The following classes are available:
// <ul>
//  <li> HDF5File opens or creates an HDF5 file and closes it automatically.
//       Furthermore it has a function to test if a file is in HDF5 format.
//  <li> HDF5Record reads or writes a Record as a group in an HDF5 object.
//  <li> HDF5DataSet opens or creates a possible tiled data set in an HDF5
//       object. The array can be read or written in parts.
//       Currently the only data types supported are Bool, Int, Float, Double,
//       Complex, and DComplex.
//  <li> HDF5Group opens, creates, or removes a group in an HDF5 object.
// </ul>
// Note that HDF5Object forms the base class of HDF5File, HDF5Group, and
// HDF5DataSet. Most interfaces use HDF5Object, thus are applicable to
// all these object types.
// <br>An HDF5Object has a conversion operator to <src>hid_t</src>, thus
// can be used directly in any HDF5 function.
//
// Because of HDF5 resource management the objects (e.g. HDF5File) cannot
// be copied. However, they can be used in shared pointers (like casa's
// CountedPtr or boost's shared_ptr).
// <br>
// Internally the classes use HDF5HidMeta.h which does the resource management
// for HDF5 meta data like attributes, property lists, etc..
//
// <note>
// All HDF5 classes and all their functions are compiled, but it depends on
// the setting of HAVE_HDF5 how. If not set, all these functions are merely stubs
// and the class constructors throw an exception when used.
// The function <src>HDF5Object::hasHDF5Support()</src> tells if HDF5 is used.
// See the casacore build instructions at casacore.googlecode.com
// for more information.
// </note>
// </synopsis>

// <example>
// See the various test programs.
// </example>
//
// <motivation>
// HDF5 offers a C++ interface. However, this interface is still quite complex
// and is too much C-oriented.
// Furthermore there was the need to support the casa data types, in particular
// reversal of array axes was needed.
// </motivation>

// <todo asof="2008/03/13">
//  <li> Make it possible to store empty arrays (e.g. as a compound of a
//       scalar (defining its type) and a vector (defining its shape).
//  <li> Set the optimal data set chunk cache size from a given access pattern.
//       The current problem is that you can only set the cache size at the
//       HDF5 file level, not at the data set level. Furthermore, setting
//       the cache size requires that the file is closed first.
//       For the time being a fixed cache size of 16 MB is used instead of
//       the default 1 MB.
// </todo>

// </module>


} //# NAMESPACE CASACORE - END

#endif
