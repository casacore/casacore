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
#include <casacore/images/Images/ImageAttrGroupHDF5.h>
#include <casacore/casa/HDF5/HDF5Group.h>
#include <casacore/casa/HDF5/HDF5Record.h>
#include <casacore/casa/Exceptions/Error.h>
#include <iomanip>

namespace casacore {

  ImageAttrGroupHDF5::ImageAttrGroupHDF5 (const HDF5Group& image,
                                          const String& attrName,
                                          Bool isWritable)
    : itsChanged  (False),
      itsCanWrite (isWritable)
  {
    itsRecord = HDF5Record::readRecord (image, attrName);
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

  uInt ImageAttrGroupHDF5::nrows() const
  {
    return itsRecord.nfields();
  }

  Bool ImageAttrGroupHDF5::hasAttr (const String& attrName) const
  {
    if (itsRecord.empty()) {
      return False;
    }
    return itsRecord.subRecord(0).isDefined (attrName);
  }

  Vector<String> ImageAttrGroupHDF5::attrNames() const
  {
    if (itsRecord.empty()) {
      return Vector<String>();
    }
    const Record& subRecord = itsRecord.subRecord(0);
    Vector<String> names(subRecord.size());
    uInt nr = 0;
    for (uInt i=0; i<names.size(); ++i) {
      // Only names not ending in _UNIT or _MEASINFO
      String name = subRecord.name(i);
      if (!((name.size() >= 5  &&  name.substr(name.size()-5) == "_UNIT")  ||
            (name.size() >= 9  &&  name.substr(name.size()-9) == "_MEASINFO"))){
        names[nr++] = subRecord.name(i);
      }
    }
    names.resize (nr, True);
    return names;
  }

  DataType ImageAttrGroupHDF5::dataType (const String& attrName) const
  {
    if (itsRecord.empty()) {
      return TpOther;
    }
    const Record& subRecord = itsRecord.subRecord(0);
    if (subRecord.isDefined (attrName)) {
      return subRecord.dataType (attrName);
    }
    return TpOther;
  }

  ValueHolder ImageAttrGroupHDF5::getData (const String& attrName, uInt rownr)
  {
    if (rownr >= itsRecord.nfields()) {
      throw AipsError("ImageAttrGroupHDF5: rownr " + String::toString(rownr) +
                      " does not exist");
    }
    const Record& subRecord = itsRecord.subRecord(rownr);
    return subRecord.asValueHolder (attrName);
  }

  Record ImageAttrGroupHDF5::getDataRow (uInt rownr)
  {
    if (rownr >= itsRecord.nfields()) {
      throw AipsError("ImageAttrGroupHDF5: rownr " + String::toString(rownr) +
                      " does not exist");
    }
    return itsRecord.subRecord(rownr);
  }

  Vector<String> ImageAttrGroupHDF5::getUnit (const String& attrName)
  {
    if (! itsRecord.empty()) {
      const Record& subRecord = itsRecord.subRecord(0);
      if (subRecord.isDefined (attrName + "_UNIT")) {
        return subRecord.asArrayString(attrName + "_UNIT");
      }
    }
    return Vector<String>();
  }

  Vector<String> ImageAttrGroupHDF5::getMeasInfo (const String& attrName)
  {
    if (! itsRecord.empty()) {
      const Record& subRecord = itsRecord.subRecord(0);
      if (subRecord.isDefined (attrName + "_MEASINFO")) {
        return subRecord.asArrayString(attrName + "_MEASINFO");
      }
    }
    return Vector<String>();
  }

  void ImageAttrGroupHDF5::putData (const String& attrName,
                                    uInt rownr,
                                    const ValueHolder& data,
                                    const Vector<String>& units,
                                    const Vector<String>& measInfo)
  {
    if (!itsCanWrite) {
      throw AipsError("ImageAttrGroupHDF5: attribute data cannot be written");
    }
    checkRows(attrName, rownr);
    Record& subRecord = itsRecord.rwSubRecord(rownr);
    subRecord.defineFromValueHolder (attrName, data);
    if (!units.empty()) {
      subRecord.define (attrName + "_UNIT", units);
    }
    if (!measInfo.empty()) {
      AlwaysAssert (measInfo.size() == 2, AipsError);
      subRecord.define (attrName + "_MEASINFO", measInfo);
    }
    itsChanged = True;
  }

  String makeRowName (uInt rownr)
  {
    ostringstream ostr;
    ostr << std::setfill('0') << std::setw(5) << rownr;
    return ostr.str();
  }

  void ImageAttrGroupHDF5::checkRows (const String& attrName, uInt rownr)
  {
    uInt nrow = itsRecord.nfields();
    // A new row can only be added right after the last row.
    if (rownr > nrow) {
      throw AipsError("ImageAttrGroupHDF5: row " + String::toString(rownr) +
                      " of attribute " + attrName +
                      " cannot be added; beyond current #rows " +
                      String::toString(nrow));
    }
    if (rownr == nrow) {
      itsRecord.defineRecord (makeRowName(rownr), Record());
    }
  }

} //# NAMESPACE CASACORE - END
