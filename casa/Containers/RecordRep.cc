//# RecordRep.cc: A hierarchical collection of named fields of various types
//# Copyright (C) 1996,1997,1999,2000,2001,2002,2005
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

#include <casacore/casa/Containers/RecordRep.h>
#include <casacore/casa/Containers/BlockIO.h>
#include <casacore/casa/Containers/Record.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Arrays/Slice.h>
#include <casacore/casa/Arrays/ArrayError.h>
#include <casacore/casa/Arrays/IPosition.h>
#include <casacore/casa/IO/AipsIO.h>
#include <casacore/casa/IO/ArrayIO.h>
#include <casacore/casa/Utilities/ValType.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Exceptions/Error.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

  RecordRep::RecordRep ()
  {}
	
  RecordRep::RecordRep (const RecordDesc& description)
  {
    restructure (description, True);
  }

  RecordRep::RecordRep (const RecordRep& other)
  {
    restructure (other.desc_p, False);
    copy_other (other);
  }

  RecordRep& RecordRep::operator= (const RecordRep& other)
  {
    if (this != &other) {
      restructure (other.desc_p, False);
      copy_other (other);
    }
    return *this;
  }

  void RecordRep::restructure (const RecordDesc& newDescription, Bool recursive)
  {
    desc_p = newDescription;
    data_p.clear();
    data_p.reserve (desc_p.nfields());
    for (uInt i=0; i<desc_p.nfields(); ++i) {
      DataType dtype = desc_p.type(i);
      if (dtype == TpRecord) {
        if (recursive) {
          data_p.push_back (std::unique_ptr<RecordData<Record>>
                            (new RecordData<Record>(Record(this, desc_p.subRecord(i)))));
        } else {
          data_p.push_back (std::unique_ptr<RecordData<Record>>
                            (new RecordData<Record>(Record(this, RecordDesc()))));
        }
      } else {
        data_p.push_back (RecordDataBase::makeData (dtype, desc_p.shape(i), True));
      }
    }
  }

  void RecordRep::removeField (Int whichField)
  {
    data_p.erase (data_p.begin()+whichField);
    desc_p.removeField (whichField);
  }

  void RecordRep::addDataField (const String& name, DataType type,
                                const IPosition& shape, Bool fixedShape,
                                const void* data)
  {
    AlwaysAssert (type == TpBool      ||  type == TpArrayBool
                  ||  type == TpUChar     ||  type == TpArrayUChar
                  ||  type == TpShort     ||  type == TpArrayShort
                  ||  type == TpInt       ||  type == TpArrayInt
                  ||  type == TpUInt      ||  type == TpArrayUInt
                  ||  type == TpInt64     ||  type == TpArrayInt64
                  ||  type == TpFloat     ||  type == TpArrayFloat
                  ||  type == TpDouble    ||  type == TpArrayDouble
                  ||  type == TpComplex   ||  type == TpArrayComplex
                  ||  type == TpDComplex  ||  type == TpArrayDComplex
                  ||  type == TpString    ||  type == TpArrayString
                  , AipsError);
    if (fixedShape) {
      desc_p.addField (name, type, shape);
    } else {
      desc_p.addField (name, type);
    }
    data_p.push_back (RecordDataBase::makeData (type, shape, False));
    data_p.back()->copyData (data);
  }

  void RecordRep::addField (const String& name, const Record& rec,
                            RecordInterface::RecordType type)
  {
    // If the record is empty, it is variable structured.
    if (rec.nfields() == 0) {
      type = RecordInterface::Variable;
    }
    // If the new field is fixed, add its description too.
    if (type == RecordInterface::Fixed) {
      desc_p.addField (name, rec.description());
    } else {
      desc_p.addField (name, TpRecord);
    }
    // Use default ctor and assignment to be sure that the
    // new record gets the correct record type.
    data_p.push_back (std::unique_ptr<RecordData<Record>>
                      (new RecordData<Record>(Record(this, type))));
    data_p.back()->copyData (&rec);
  }

  void RecordRep::checkShape (const IPosition& shape, const void* value,
                              const String& fieldName)
  {
    if (! shape.isEqual (static_cast<const ArrayBase*>(value)->shape())) {
      throw (ArrayConformanceError
             ("Record::define - fixed array conformance error for field " +
              fieldName));
    }
  }

  void RecordRep::defineDataField (Int whichField, DataType type,
                                   const void* value)
  {
    const void* valPtr = value;
    AlwaysAssert (whichField >= 0  &&  whichField < Int(data_p.size()), AipsError);
    DataType descDtype = desc_p.type(whichField);
    if (type == descDtype) {
      if (desc_p.isArray(whichField)) {
        const IPosition& shape = desc_p.shape(whichField);
        if (shape.nelements() > 0  &&  shape[0] > 0) {
          checkShape (shape, value, desc_p.name(whichField));
        }
      }
    } else if (isArray(type)  &&  asScalar(type) == descDtype) {
      // A scalar can be defined using a single element vector.
      // Get the data pointer in the Vector.
      checkShape (IPosition(1,1), value, desc_p.name(whichField));
      Bool deleteIt;
      valPtr = static_cast<const ArrayBase*>(value)->getVStorage(deleteIt);
      DebugAssert (!deleteIt, AipsError);
    } else {
      throw (AipsError ("RecordRep::defineDataField - "
                        "incorrect data type used for field " +
                        desc_p.name(whichField)));
    }
    data_p[whichField]->copyData (valPtr);
  }


  Bool RecordRep::conform (const RecordRep& other) const
  {
    // First check (non-recursively) if the descriptions conform.
    if (! desc_p.conform (other.desc_p)) {
      return False;
    }
    // Now check for each fixed sub-record if it conforms.
    for (uInt i=0; i<data_p.size(); ++i) {
      if (desc_p.type(i) == TpRecord) {
        const Record& thisRecord = *static_cast<const Record*>(data_p[i]->data());
        if (thisRecord.isFixed()) {
          const Record& thatRecord =
            *static_cast<const Record*>(other.data_p[i]->data());
          if (! thisRecord.conform (thatRecord)) {
            return False;
          }
        }
      }
    }
    return True;
  }

  void RecordRep::copyData (const RecordRep& other)
  {
    // Assume conform has already been called
    DebugAssert (conform (other), AipsError);
    copy_other (other);
  }

  void RecordRep::copy_other (const RecordRep& other)
  {
    for (uInt i=0; i<desc_p.nfields(); ++i) {
      data_p[i]->copyData (*other.data_p[i]);
    }
  }
  
  void* RecordRep::get_pointer (Int whichField, DataType type,
                                const String& recordType) const
  {
    AlwaysAssert (recordType == "Record", AipsError);
    return get_pointer (whichField, type);
  }
  void* RecordRep::get_pointer (Int whichField, DataType type) const
  {
    AlwaysAssert (whichField >= 0  &&  whichField < Int(data_p.size()), AipsError);
    DataType descDtype = desc_p.type(whichField);
    if (type == descDtype) {
      return data_p[whichField]->data();
    }
    // A scalar can be returned as an array.
    if (! (isArray(type)  &&  asScalar(type) == descDtype)) {
      throw (AipsError ("RecordRep::get_pointer - "
                        "incorrect data type " + ValType::getTypeStr(type) +
                        " used for field " + desc_p.name(whichField) +
                        " with type " + ValType::getTypeStr(descDtype)));
    }
    if (! data_p[whichField]->dataArr()) {
      data_p[whichField]->makeDataArr();
    }
    return data_p[whichField]->dataArr();
  }



  void RecordRep::mergeField (const RecordRep& other, Int whichFieldFromOther,
                              RecordInterface::DuplicatesFlag flag)
  {
    // If the field exists and if flag tells to overwrite,
    // the field is removed first.
    if (flag == RecordInterface::OverwriteDuplicates) {
      Int fld = desc_p.fieldNumber (other.desc_p.name(whichFieldFromOther));
      if (fld >= 0) {
        removeField (fld);
      }
    }
    // Try to add the field to the description.
    Int nr = desc_p.nfields();
    Int nrnew = desc_p.mergeField (other.desc_p, whichFieldFromOther, flag);
    // It succeeded if nfields increased.
    // Then the value can be defined.
    if (nrnew > nr) {
      DataType type = desc_p.type (nr);
      const void* otherPtr = other.get_pointer (whichFieldFromOther, type);
      doMergeField (type, otherPtr, desc_p.shape(nr));
    }
  }

  void RecordRep::doMergeField (DataType type, const void* otherPtr,
                                const IPosition& shape)
  {
    if (type == TpRecord) {
      data_p.push_back (std::unique_ptr<RecordData<Record>>
                        (new RecordData<Record>
                         (*static_cast<const Record*>(otherPtr))));
    } else {
      data_p.push_back (RecordDataBase::makeData (type, shape, False));
    }
    data_p.back()->copyData (otherPtr);
  }

  void RecordRep::merge (const RecordRep& other,
                         RecordInterface::DuplicatesFlag flag)
  {
    Int n = other.desc_p.nfields();
    for (Int i=0; i<n; ++i) {
      mergeField (other, i, flag);
    }
  }

  void RecordRep::print (std::ostream& os,
                         Int maxNrValues, const String& indent) const
  {
    for (uInt i=0; i<desc_p.nfields(); ++i) {
      os << indent << desc_p.name(i) << ": ";
      data_p[i]->printData (os, indent, maxNrValues);
      os << endl;
    }
  }

  void RecordRep::putRecord (AipsIO& os, int recordType) const
  {
    os.putstart ("Record", 1);              // version 1
    os << desc_p;
    os << recordType;
    putData (os);
    os.putend();
  }

  void RecordRep::putData (AipsIO& os) const
  {
    for (uInt i=0; i<desc_p.nfields(); ++i) {
      if (desc_p.type(i) == TpRecord) {
        data_p[i]->putData (os, desc_p.subRecord(i).nfields()==0, nullptr);
      } else {
        data_p[i]->putData (os, False, nullptr);
      }
    }
  }

  void RecordRep::getRecord (AipsIO& os, Int& recordType)
  {
    // Support reading scalar and array keyword sets as records.
    // They are the very old way of storing keywords, since long replaced
    // by Record. The code does not exist anymore, but theoretically such data
    // can exist in a very old table. Therefore it is still supported here.
    uInt version;
    String type = os.getNextType();
    if (type == "ScalarKeywordSet") {
      version = os.getstart ("ScalarKeywordSet");
      getKeySet (os, version, 0);
    } else if (type == "ArrayKeywordSet") {
      version = os.getstart ("ArrayKeywordSet");
      getKeySet (os, version, 1);
    } else {
      uInt version = os.getstart ("Record");
      // Get the description and restructure the record.
      RecordDesc desc;
      os >> desc;
      os >> recordType;
      restructure (desc, True);
      // Read the data.
      getData (os, version);
    }
    os.getend();
  }

  void RecordRep::getData (AipsIO& os, uInt version)
  {
    for (uInt i=0; i<desc_p.nfields(); ++i) {
      if (desc_p.type(i) == TpRecord) {
        data_p[i]->getData (os, version, desc_p.subRecord(i).nfields()==0, nullptr);
      } else {
        data_p[i]->getData (os, version, False, nullptr);
      }
    }
  }

  void RecordRep::getKeySet (AipsIO& os, uInt version, uInt type)
  {
    // First build the description from the map of keyword names and
    // attributes.
    RecordDesc desc;
    getKeyDesc (os, desc);
    // Define the record from the description.
    // Read the keyword values and define the corresponding record value.
    restructure (desc, True);
    getScalarKeys (os);
    if (type == 1) {
      getArrayKeys (os);
    }
    // Newer keyword sets may contain nested keyword sets.
    // We do not support reading those, so throw an exception when they exist.
    if (version > 1) {
      uInt n;
      os >> n;
      AlwaysAssert (n==0, AipsError);
    }
  }

  void RecordRep::getKeyDesc (AipsIO& os, RecordDesc& desc)
  {
    // Start reading the Map of keyword names and attributes.
    os.getstart ("Map<String,void>");
    int dt;
    String name, comment;
    // Get #names and the default attribute (datatype + comment).
    uInt i, n;
    os >> n;
    os >> dt;
    os >> comment;
    // Get each keyword name and attribute.
    // Add them to the record description.
    for (i=0; i<n; ++i) {
      os >> name;
      os >> dt;
      os >> comment;
      desc.addField (name, DataType(dt));
    }
    os.getend();
    // Get the excluded data types and names.
    // Note that exNames was written as a Block<Regex>, but can be
    // read as a Block<String>. This is a template instantiation less.
    Block<int>    exDtype;
    Block<String> exNames;
    os >> exDtype;
    os >> exNames;
  }

  void RecordRep::getScalarKeys (AipsIO& os)
  {
    uInt i, n;
    String name;
    // Read the values for 8 supported scalar types.
    // These are Bool, Int, uInt, float, double, Complex, DComplex, String.
    for (int j=0; j<8; ++j) {
      os >> n;
      for (i=0; i<n; ++i) {
        os >> name;
        data_p[fieldNumber(name)]->getData(os, 0, False, nullptr);
      }
    }
  }

  void RecordRep::getArrayKeys (AipsIO& os)
  {
    uInt i, n;
    String name;
    // Read the values for 8 supported array types.
    // These are Bool, Int, uInt, float, double, Complex, DComplex, String.
    for (int j=0; j<8; ++j) {
      os >> n;
      for (i=0; i<n; ++i) {
        os >> name;
        data_p[fieldNumber(name)]->getData(os, 0, False, nullptr);
      }
    }
  }

} //# NAMESPACE CASACORE - END
