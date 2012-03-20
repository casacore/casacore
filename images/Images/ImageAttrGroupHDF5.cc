//# ImageAttrGroupHDF5.cc: Attribute group for a HDF5 image
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

//# Includes
#include <images/Images/ImageAttrGroupHDF5.h>
#include <casa/HDF5/HDF5Group.h>
#include <casa/HDF5/HDF5Record.h>
#include <casa/Exceptions/Error.h>

namespace casa {

  ImageAttrGroupHDF5::ImageAttrGroupHDF5 (const HDF5Group& image,
                                          const String& attrName,
                                          Bool isWritable)
    : itsChanged  (False),
      itsCanWrite (isWritable),
      itsNValues  (0)
  {
    itsRecord = HDF5Record::readRecord (image, attrName);
    if (! itsRecord.empty()) {
      itsNValues = itsRecord.shape(0).last();
    }
  }

  ImageAttrGroupHDF5::~ImageAttrGroupHDF5()
  {}

  void ImageAttrGroupHDF5::flush (HDF5Group& image, const String& attrName)
  {
    if (itsChanged) {
      HDF5Record::writeRecord (image, attrName, itsRecord);
      itsChanged = False;
    }
  }

  uInt ImageAttrGroupHDF5::nvalues() const
  {
    if (itsRecord.empty()) {
      return 0;
    }
    // Return the length of the first attribute.
    // Assert it is not a UNIT or MEASINFO.
    IPosition shape (itsRecord.shape(0));
    return shape[shape.size()-1];
  }

  Bool ImageAttrGroupHDF5::hasAttr (const String& attrName) const
  {
    return itsRecord.isDefined (attrName);
  }

  Vector<String> ImageAttrGroupHDF5::attrNames() const
  {
    Vector<String> names(itsRecord.size());
    uInt nr = 0;
    for (uInt i=0; i<names.size(); ++i) {
      // Only names not ending in _UNIT or _MEASINFO
      String name = itsRecord.name(i);
      if (!((name.size() >= 5  &&  name.substr(name.size()-5) == "_UNIT")  ||
            (name.size() >= 9  &&  name.substr(name.size()-9) == "_MEASINFO"))){
        names[nr++] = itsRecord.name(i);
      }
    }
    names.resize (nr, True);
    return names;
  }

  DataType ImageAttrGroupHDF5::dataType (const String& attrName) const
  {
    if (itsRecord.isDefined (attrName)) {
      return itsRecord.dataType (attrName);
    }
    return TpOther;
  }

  ValueHolder ImageAttrGroupHDF5::getData (const String& attrName)
  {
    return itsRecord.asValueHolder (attrName);
  }

  Vector<String> ImageAttrGroupHDF5::getUnit (const String& attrName)
  {
    if (itsRecord.isDefined (attrName + "_UNIT")) {
      return itsRecord.asArrayString(attrName + "_UNIT");
    }
    return Vector<String>();
  }

  Vector<String> ImageAttrGroupHDF5::getMeasInfo (const String& attrName)
  {
    if (itsRecord.isDefined (attrName + "_MEASINFO")) {
      return itsRecord.asArrayString(attrName + "_MEASINFO");
    }
    return Vector<String>();
  }

  void ImageAttrGroupHDF5::putData (const String& attrName,
                                    const ValueHolder& data,
                                    const Vector<String>& units,
                                    const Vector<String>& measInfo)
  {
    if (!itsCanWrite) {
      throw AipsError("ImageAttrGroupHDF5: attribute data cannot be written");
    }
    itsRecord.defineFromValueHolder (attrName, data);
    checkSize (attrName);
    if (!units.empty()) {
      itsRecord.define (attrName + "_UNIT", units);
    }
    if (!measInfo.empty()) {
      AlwaysAssert (measInfo.size() == 2, AipsError);
      itsRecord.define (attrName + "_MEASINFO", measInfo);
    }
    itsChanged = True;
  }

  void ImageAttrGroupHDF5::checkSize (const String& attrName)
  {
    IPosition shape (itsRecord.shape(attrName));
    uInt size = shape(shape.size() - 1);
    if (itsNValues == 0) {
      itsNValues = size;
    } else if (size != itsNValues) {
      throw AipsError("ImageAttrGroupHDF5: cannot put " +
                      String::toString(size) +
                      " elements of attr " + attrName +
                      " into group containing " +
                      String::toString(itsNValues) + " rows");
    }
  }

} //# NAMESPACE CASA - END
