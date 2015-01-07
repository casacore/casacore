//# ImageAttrHandlerHDF5.h: Attributes handler for HDF5 images
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

#ifndef IMAGES_IMAGEATTRHANDLERHDF5_H
#define IMAGES_IMAGEATTRHANDLERHDF5_H


//# Includes
#include <casacore/casa/aips.h>
#include <casacore/images/Images/ImageAttrHandler.h>
#include <casacore/images/Images/ImageAttrGroupHDF5.h>
#include <casacore/casa/HDF5/HDF5Group.h>
#include <casacore/casa/Utilities/CountedPtr.h>
#include <map>

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
// The attributes are divided into handlers. Each handler can reside in a subtable
// of the image or in a handler in HDF5. All attributes in a handler have the
// same number of values, where each value can be a scalar or (small) array.
// It is possible to define units and measure info for an attribute.
// </synopsis> 

// <example>
// This example shows how to get attributes from an image.
// make it known to the image.
// <srcblock>
//   // Open the image (as readonly for the moment).
//   PagedImage<Float> myimage ("image.name");
//   // Get access to attibute handler LOFAR_SOURCE.
//   ImageExtrAttr& = myimage.attrHandler ("LOFAR_SOURCE");
//   // Get the data for some field.
//   Vector<String> names = ImageExtrAttr->getString("NAME");
// </srcblock>
// </example>
//
// <motivation>
// LOFAR needed functionality to store arbitrary attributes.
// </motivation>

class ImageAttrHandlerHDF5 : public ImageAttrHandler
{
public: 
  // Default construct from the image table.
  ImageAttrHandlerHDF5();

  // Attach the table and return this object.
  // It looks for the table keyword ATTRGROUPS which contains the subtables
  // defining the attribute groups.
  // If the keyword does not exist, it will be added if <src>createHandler</src>
  // is set.
  // Otherwise the handler is an empty one and no groups can be added to it.
  ImageAttrHandlerHDF5& attachTable (const Table& image,
                                     Bool createHandler = False);

  virtual ~ImageAttrHandlerHDF5();

  // Attach the hid and return this object.
  // It looks for the group ATTRGROUPS which contains groups
  // defining the attribute groups.
  // If the group does not exist, it will be added if <src>createHandler</src>
  // is set.
  // Otherwise the handler is an empty one and no groups can be added to it.
  ImageAttrHandlerHDF5& attachHid (const HDF5Object& hid,
                                   Bool createHandler,
                                   Bool isWritable);

  // Flush the attibrutes if needed.
  virtual void flush();

  // Test if the given attribute group is present.
  virtual Bool hasGroup (const String& name);

  // Get all attribute group names.
  virtual Vector<String> groupNames() const;

  // Get access to a group.
  virtual ImageAttrGroup& openGroup (const String& groupName);

  // Create an attribute group with the given name.
  virtual ImageAttrGroup& createGroup (const String& groupName);

  // Close the group with the given name. It will flush its attributes.
  // Nothing is done if it is not open.
  virtual void closeGroup (const String& groupName);

private:
  Bool                                itsCanWrite;    //# writable?
  CountedPtr<HDF5Group>               itsGroup;       //# HDF5 group to add to
  std::map<String,ImageAttrGroupHDF5> itsGroupMap;    //# attribute groups
};

} //# NAMESPACE CASACORE - END

#endif
