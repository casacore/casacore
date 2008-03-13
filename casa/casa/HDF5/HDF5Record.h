//# HDF5Record.h: A class to write/read a record into HDF5
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

#ifndef CASA_HDF5RECORD_H
#define CASA_HDF5RECORD_H

#ifdef HAVE_HDF5

//# Includes
#include <casa/HDF5/HDF5Object.h>
#include <casa/HDF5/HDF5DataType.h>
#include <casa/Containers/Record.h>

namespace casa { //# NAMESPACE CASA - BEGIN

  // <summary>
  // A class representing an HDF5 file.
  // </summary>

  // <use visibility=export>

  // <reviewed reviewer="" date="" tests="tHDF5Record.cc">
  // </reviewed>

  // <prerequisite>
  //   <li> <a href="http://hdf.ncsa.uiuc.edu">HDF5 system</a>
  //   <li> <linkto class=Record>class Record</linkto>
  // </prerequisite>

  // <synopsis>
  // This class has a static function to write a Record (or TableRecord)
  // into an HDF5 file by storing it as attributes for the given group.
  // It can handle all types of fields in a record. The only exception
  // is an empty array which cannot be held in HDF5.
  // <br>
  // When writing the record, it first deletes all attributes of the group
  // to be sure that the group's attributes only contain the record.
  // <br>
  // It also has a function to read back the attributes back as a Record.
  // <p>
  // An AIPS++ Record is a recursive structure, while attributes are flat.
  // Therefore the fields of a nested record have the name of the nested record
  // (followed by a colon) as the attribute name. A nested record is preceeded
  // by a special integer attribute <tt>__isrec__</tt> containing the
  // number of fields in the nested record.
  // Each level of nesting adds another prefix.
  // <p>
  // Storing an array of strings could not be dome straightforwardly.
  // Although HDF5 supports variable length strings and arrays thereof,
  // it seems they are only supported for data sets, not for attributes.
  // Therefore an array of strings is stored in two attributes with a
  // special prefix.
  // <ul>
  // <li> <tt>Array<String>_</tt> is the prefix for the attribute holding
  //      a single string containing all strings.
  // <li> <tt>Sizes<String>_</tt> is the prefix for the attribute holding
  //      an array of integers containing the size of each string.
  // </ul>
  // <p>
  // Finally HDF5 cannot hold empty strings. This is solved by storing an empty
  // string with the special value <tt>__empty__</tt>.
  // </synopsis> 

  // <motivation>
  // Record is a very important class in AIPS++ images, so it had to be
  // possible to read and write them.
  // </motivation>

  class HDF5Record
  {
  public: 
    // Read a record from the attributes of the given group.
    // Nested records are read back correctly.
    // An empty record is returned if the group does not exist.
    static Record readRecord (const HDF5Object& parentHid,
			      const String& groupName);

    // Write the record as attributes of a group of the given parent.
    // Nested records are written as nested groups.
    // The group is deleted first if it already exists.
    static void writeRecord (const HDF5Object& parentHid,
			     const String& recordName,
			     const RecordInterface& rec);

    // Remove the record (i.e. group) from the given parent.
    // Nothing is done if the record does not exist.
    static void remove (const HDF5Object& parentHid,
			const String& recordName);

  private:
    // Read back a (nested) record.
    static Record doReadRecord (hid_t parentHid);

    // Read a subrecord. This is a callback function for H5Literate.
    static herr_t readSubRecord (hid_t gid, const char* name,
				 const H5L_info_t*, void* voidRec);

    // Read a scalar value and add it to the record.
    static void readScalar (hid_t attrId, hid_t dtid,
			    const String& name, RecordInterface& rec);

    // Read an array value and add it to the record.
    static void readArray (hid_t attrId, hid_t dtid, const IPosition&,
			   const String& name, RecordInterface& rec);

    // Read a scalar string from an attribute and add it to the record.
    static void readScaString (hid_t attrId, Int sz,
			       const String& name, RecordInterface& rec);

    // Read a array of strings from an atrribute and add it to the record.
    static void readArrString (hid_t attrId, const IPosition&,
			       const String& name, RecordInterface& rec);

    // Read a field containing a scalar of fixed length.
    template<typename T>
    static void readSca (hid_t attrId, const String& name,
			 RecordInterface& rec)
    {
      T value;
      HDF5DataType dtype((T*)0);
      read (attrId, &value, dtype);
      rec.define (name, value);
    }

    // Read a field containing an array of fixed length elements.
    template<typename T>
    static void readArr (hid_t attrId, const IPosition& shape,
			 const String& name,
			 RecordInterface& rec)
    {
      Array<T> value(shape);
      HDF5DataType dtype((T*)0);
      read (attrId, value.data(), dtype);
      rec.define (name, value);
    }

    // Read fixed length values from an attribute (scalar and array).
    static void read (hid_t attrId, void* value,
		      const HDF5DataType& dtype);

    // Write a (nested) record.
    static void doWriteRecord (const HDF5Object& groupHid,
			       const RecordInterface& rec);

    // Write a fixed length scalar value as attribute.
    static void writeScalar (hid_t parentHid, const String& name,
			     const void* value,
			     const HDF5DataType& dtype);

    // Write an array of fixed length values as attribute.
    static void writeArray (hid_t parentHid, const String& name,
			    const void* value, const IPosition& shape,
			    const HDF5DataType& dtype);

    // Write a scalar string as attribute.
    // HDF5 cannot handle empty strings, so for empty strings a special
    // value is written.
    static void writeScaString (hid_t parentHid, const String& name,
				const String& value);

    // Write an array of strings as attribute.
    // HDF5 cannot handle empty strings, so for empty strings a special
    // value is written.
    static void writeArrString (hid_t parentHid, const String& name,
				const Array<String>& value);

    // Write a field containing a fixed length scalar value.
    template<typename T>
    static void writeSca (hid_t parentHid, const String& name,
			  const RecordInterface& rec, Int i)
    {
      T value;
      rec.get (i, value);
      HDF5DataType dtype((T*)0);
      writeScalar (parentHid, name, &value, dtype);
    }

    // Write a field containing an array of fixed length elements.
    template<typename T>
    static void writeArr (hid_t parentHid, const String& name,
			  const RecordInterface& rec, Int i)
    {
      Array<T> value;
      rec.get (i, value);
      HDF5DataType dtype((T*)0);
      writeArray (parentHid, name, value.data(), value.shape(), dtype);
    }

  };

}

#endif
#endif
