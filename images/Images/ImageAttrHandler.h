//# ImageAttrHandler.h: Abstract base class for an image attributes handler
//# Copyright (C) 2012
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

#ifndef IMAGES_IMAGEATTRHANDLER_H
#define IMAGES_IMAGEATTRHANDLER_H


//# Includes
#include <casacore/casa/aips.h>
#include <casacore/images/Images/ImageAttrGroup.h>
#include <casacore/casa/Containers/ValueHolder.h>
#include <casacore/casa/Arrays/Vector.h>

namespace casacore {

// <summary>
// Abstract base class for an image attributes handler.
// </summary>

// <use visibility=export>

// <reviewed reviewer="" date="" tests="tPagedmage.cc" demos="dPagedImage.cc">
// </reviewed>

// <prerequisite>
//   <li> <linkto class=ImageInterface>ImageInterface</linkto>
// </prerequisite>

// <etymology>
// This class makes it possible to store extra attributes with an image to
// describe atrbitrary meta information.
// </etymology>

// <synopsis> 
// For LOFAR it was needed to store extra meta information and to be able to
// convert it from casacore table format to HDF5 format and vice-versa.
// Furthermore, it must be possible to access the information in a way that
// arbitrary info can be stored and retrieved.
//
// An ImageAttrHandler object handles those attributes in an image. Specific
// handler classes exist for images stored in casacore and in HDF5 format.
// The attributes are divided into group which are handled by ImageAttrGroup.
// A group (e.g. LOFAR_SOURCES) maps to a subtable in casacore format and a
// group in HDF5 format.
// </synopsis> 

// <example>
// This example shows how to get attributes from an image.
// <srcblock>
//   // Open the image (done as read/write when having write access).
//   PagedImage<Float> myimage ("image.name");
//   // Get access to the attibute handler.
//   ImageAttrHandler& attrHandler = myimage.attrHandler();
//   // Get the names of all attribute groups.
//   Vector<String> groupNames = attrHandler.groupNames();
//   // Create a new group and define an attribute defining Freq in Hz.
//   ImageAttrGroup& newGroup = attrHandler.createGroup ("NEW_GROUP");
//   newGroup.putAttr ("Freq", ValueHolder(Vector<Double>(1, 1e7)),
//                     Vector<String>(1,"Hz"));
// </srcblock>
// </example>
//
// <motivation>
// LOFAR needed functionality to store arbitrary attributes.
// </motivation>

class ImageAttrHandler
{
public: 
  // Default constructor.
  ImageAttrHandler()
  {}

  virtual ~ImageAttrHandler();

  // Flush the attibrutes if needed.
  // The default implementation does nothing.
  virtual void flush();

  // Test if the given attribute group is present.
  // The default implementation returns False.
  virtual Bool hasGroup (const String& name);

  // Get all attribute group names.
  // The default implementation returns an empty vector.
  virtual Vector<String> groupNames() const;

  // Get access to a group.
  // The default implementation throws an exception.
  virtual ImageAttrGroup& openGroup (const String& groupName);

  // Create an attribute group with the given name.
  // The default implementation throws an exception.
  virtual ImageAttrGroup& createGroup (const String& groupName);

  // Close the group with the given name.
  // The default implementation does nothing.
  virtual void closeGroup (const String& groupName);
};

} //# NAMESPACE CASACORE - END

#endif
