//# ImageAttrGroupHDF5.h: Attribute group for a HDF5 image
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

#ifndef IMAGES_IMAGEATTRGROUPHDF5_H
#define IMAGES_IMAGEATTRGROUPHDF5_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/images/Images/ImageAttrGroup.h>
#include <casacore/tables/Tables/TableProxy.h>
#include <casacore/casa/HDF5/HDF5Group.h>

namespace casacore {

// <summary>
// Attribute group for a HDF5 image.
// </summary>
//
// <use visibility=export>
//
// <reviewed reviewer="" date="" tests="tPagedmage.cc" demos="dPagedImage.cc">
// </reviewed>
//
// <prerequisite>
//   <li> <linkto class=ImageAttrGroup>ImageAttrGroup</linkto>
// </prerequisite>
//
// <synopsis> 
// This is the implementation of base class class ImageAttrGroup for an image
// stored in the HDF5 format.
// See the base class for more information.
// </synopsis>

class ImageAttrGroupHDF5 : public ImageAttrGroup
{
public: 
  // The default constructor creates a null object.
  explicit ImageAttrGroupHDF5 (Bool isWritable=False)
    : itsChanged  (False),
      itsCanWrite (isWritable)
  {}

  // Construct the object for an attribute group in the image.
  // If present, it reads all attributes.
  ImageAttrGroupHDF5 (const HDF5Group& image, const String& attrGroupName,
                      Bool writable);

  virtual ~ImageAttrGroupHDF5();

  // Test if it is a null object.
  Bool isNull() const
    { return itsRecord.empty(); }

  // Flush the attibrutes if needed.
  void flush (HDF5Group& image, const String& attrGroupName);

  // Get the number of rows in the group.
  virtual uInt nrows() const;

  // Test if an attribute exists.
  virtual Bool hasAttr (const String& attrName) const;

  // Get all attribute names.
  virtual Vector<String> attrNames() const;

  // Get the datatype of a attribute.
  // It returns TpOther if the attribute is not defined.
  virtual DataType dataType (const String& attrName) const;

  // Get the data of the given attribute in the given row.
  virtual ValueHolder getData (const String& attrName, uInt rownr);

  // Get the data of all attributes in a rows.
  virtual Record getDataRow (uInt rownr);

  // Get the possible units of the values (stored as attrName_UNIT).
  // An empty vector is returned if the attribute has no units.
  virtual Vector<String> getUnit (const String& attrName);

  // Get the possible measure info as type,Ref (stored as attrName_MEASINFO).
  // An empty vector is returned if the attribute has no MEASINFO.
  virtual Vector<String> getMeasInfo (const String& attrName);

  // Put the data of the given attribute.
  // If the table does not contain data yet, it will be sized to the size
  // of the vector. Otherwise the vector size has to match the table size.
  // <br>If not empty, the units and MEASINFO will be put as column keywords.
  // The MEASINFO vector must be given as type,Ref.
  virtual void putData (const String& attrName, uInt rownr,
                        const ValueHolder& data,
                        const Vector<String>& units = Vector<String>(),
                        const Vector<String>& measInfo = Vector<String>());

private:
  // Check the rownr and add a row if needed.
  void checkRows (const String& attrName, uInt rownr);

  //# Data members.
  Record itsRecord;     //# Record containing all attributes (subrecord per row)
  Bool   itsChanged;    //# Has the Record changed?
  Bool   itsCanWrite;   //# Can attributes be written?
};

} //# NAMESPACE CASACORE - END

#endif
