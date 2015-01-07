//# ImageAttrGroupCasa.h: Attribute group for a CASA image
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

#ifndef IMAGES_IMAGEATTRGROUPCASA_H
#define IMAGES_IMAGEATTRGROUPCASA_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/images/Images/ImageAttrGroup.h>
#include <casacore/tables/Tables/TableProxy.h>

namespace casacore {

// <summary>
// Attribute group for a CASA image.
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
// stored in the casacore table format.
// See the base class for more information.
// </synopsis>

class ImageAttrGroupCasa : public ImageAttrGroup
{
public: 
  // The default constructor creates a null object.
  ImageAttrGroupCasa()
  {}

  // Construct the object for an attribute group in the image table.
  // Note that the group name is the name of a subtable containing the info.
  ImageAttrGroupCasa (const Table& image, const String& attrGroupName);

  virtual ~ImageAttrGroupCasa();

  // Test if it is a null object.
  Bool isNull() const
    { return itsTable.table().isNull(); }

  // Flush the attibrutes if needed.
  void flush();

  // Get the number of rows in the group.
  virtual uInt nrows() const;

  // Test if an attribute exists.
  virtual Bool hasAttr (const String& attrName) const;

  // Get all attribute names.
  virtual Vector<String> attrNames() const;

  // Get the datatype of a attribute.
  // It returns TpOther if the attribute is not defined.
  virtual DataType dataType (const String& attrName) const;

  // Get the data of the given attribute.
  virtual ValueHolder getData (const String& attrName, uInt rownr);

  // Get the data of all attributes in a rows.
  virtual Record getDataRow (uInt rownr);

  // Get the possible units of the values.
  // An empty vector is returned if the attribute has no units.
  virtual Vector<String> getUnit (const String& attrName);

  // Get the possible measure info as type and Ref.
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
  // Check if the size matches the number of rows.
  // Add rows if the table is still empty.
  void checkRows (const String& attrName, uInt size);

  // Add a new column for the given attribute for the data type in the value.
  Bool addNewColumn (const String& attrName, const ValueHolder&);

  //# Data members.
  TableProxy itsTable;
};

} //# NAMESPACE CASACORE - END

#endif
