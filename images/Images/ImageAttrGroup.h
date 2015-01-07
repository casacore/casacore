//# ImageAttrGroup.h: Abstract base class for an image attributes group
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

#ifndef IMAGES_IMAGEATTRGROUP_H
#define IMAGES_IMAGEATTRGROUP_H


//# Includes
#include <casacore/casa/aips.h>
#include <casacore/casa/Containers/ValueHolder.h>
#include <casacore/casa/Arrays/Vector.h>

namespace casacore {

// <summary>
// Abstract base class for an image attributes group.
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
// For LOFAR it is needed to store extra meta information and be possible to
// convert it from casacore table format to HDF5 format and vice-versa.
// Furthermore, it must be possible to access the information in a way that
// arbitrary attributes can be stored and retrieved in a way that is agnostic
// to the format the image is stored in. It must also work fine for an image
// stored in FITS format, be it that such an image cannot have such attributes.
//
// The attributes are divided into groups. A group resides in a subtable
// of a casacore image or in a group of an HDF5 image. This class handles
// the attributes of a group. It can get and put the attribute values, as well
// as their unit and measure info (type and reference frame type).
// For HDF5 images the unit is stored in attribute <src>attrname>_UNIT</src>
// and the measure info in <src>attrname>_MEASINFO</src>. For casacore images
// that info is stored as TableMeasure info in the column keywords.
//
// All attributes in a group must have the same number of values, where each
// value can be a scalar or (small) array. The unit and measure info have
// only one value, thus all values of an attribute have the same unit.
// </synopsis> 

// <example>
// This example shows how to get attributes from an image.
// <srcblock>
//   // Open the image.
//   PagedImage<Float> myimage ("image.name");
//   // Open the attribute handler.
//   ImageAttrHandler& attrHandler = myimage.attrHandler();
//   // Get access to attibute group LOFAR_SOURCE.
//   ImageAttrGroup& lofarSource = attrHandler.openGroup ("LOFAR_SOURCE");
//   // Get the names of all attributes in this group.
//   Vector<String> attrNames = lofarSource.attrNames();
//   // Get the value of the ATTRNAME attribute (if there).
//   if (lofarSource.hasAttr ("ATTRNAME)) {
//     ValueHolder vh (lofarSource.getData ("ATTRNAME"));
//     Vector<String> name = vh.asString();
//   }
// </srcblock>
// The following example shows how to add a group and attribute.
// <srcblock>
//   // Open the image.
//   PagedImage<Float> myimage ("image.name");
//   // Open the attribute handler.
//   ImageAttrHandler& attrHandler = myimage.attrHandler();
//   // Add attribute group LOFAR_SOURCE.
//   ImageAttrGroup& lofarSource = attrHandler.createGroup (LOFAR_SOURCE);
//   // Add an attribute which has unit Hz.
//   // The value has 2 values (e.g. for 2 frequency bands).
//   Vector<double> freqs(2);
//   freqs[0]=4.5e7; freqs[1]=5.5e7;
//   lofarSource.putData ("CENTER_FREQ", ValueHolder(freqs),
//                        Vector<String(1,"Hz"));
// </srcblock>
// </example>
//
// <motivation>
// LOFAR needed functionality to store arbitrary attributes.
// </motivation>

class ImageAttrGroup
{
public: 
  // Default constructor.
  ImageAttrGroup()
  {}

  virtual ~ImageAttrGroup();

  // Get the number of rows in the group.
  virtual uInt nrows() const = 0;

  // Test if an attribute exists.
  virtual Bool hasAttr (const String& attrName) const = 0;

  // Get all attribute names.
  virtual Vector<String> attrNames() const = 0;

  // Get the datatype of a attribute.
  // It returns TpOther if the attribute is not defined.
  virtual DataType dataType (const String& attrName) const = 0;

  // Get the data of the given attribute in the given row
  virtual ValueHolder getData (const String& attrName, uInt rownr) = 0;

  // Get the data of all attributes in a rows.
  virtual Record getDataRow (uInt rownr) = 0;

  // Get the possible units of the values.
  // An empty vector is returned if the attribute has no units.
  virtual Vector<String> getUnit (const String& attrName) = 0;

  // Get the possible measure info as type and Ref.
  // An empty vector is returned if the attribute has no MEASINFO.
  virtual Vector<String> getMeasInfo (const String& attrName) = 0;

  // Put the data of the given attribute in the given row.
  // If the row or attribute is new, it will be added. Note that the
  // new row must be directly after the last row in the group.
  // <br>If not empty, the units and MEASINFO will be put as column keywords.
  // The MEASINFO vector must be given as type,Ref.
  virtual void putData (const String& attrName, uInt rownr,
                        const ValueHolder& data,
                        const Vector<String>& units = Vector<String>(),
                        const Vector<String>& measInfo = Vector<String>()) = 0;
};

} //# NAMESPACE CASACORE - END

#endif
