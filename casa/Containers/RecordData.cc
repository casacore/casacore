//# RecordData.cc: The representation of a Record data field
//# Copyright (C) 2023
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

#include <casacore/casa/Containers/RecordData.h>
#include <casacore/casa/Containers/Record.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Exceptions/Error.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN


  std::unique_ptr<RecordDataBase> RecordDataBase::makeData
  (DataType type, const IPosition& shape, Bool initialize)
  {
    IPosition arrayShape;
    if (shape.nelements() > 0  &&  shape[0] > 0) {
      arrayShape = shape;
    }
    switch (type) {
    case TpBool:
      return std::unique_ptr<RecordDataBase> (new RecordData<Bool>(False));
    case TpUChar:
      return std::unique_ptr<RecordDataBase> (new RecordData<uChar>(False));
    case TpShort:
      return std::unique_ptr<RecordDataBase> (new RecordData<Short>(0));
    case TpInt:
      return std::unique_ptr<RecordDataBase> (new RecordData<Int>(0));
    case TpUInt:
      return std::unique_ptr<RecordDataBase> (new RecordData<uInt>(0));
    case TpInt64:
      return std::unique_ptr<RecordDataBase> (new RecordData<Int64>(0));
    case TpFloat:
      return std::unique_ptr<RecordDataBase> (new RecordData<float>(0));
    case TpDouble:
      return std::unique_ptr<RecordDataBase> (new RecordData<double>(0));
    case TpComplex:
      return std::unique_ptr<RecordDataBase> (new RecordData<Complex>(Complex()));
    case TpDComplex:
      return std::unique_ptr<RecordDataBase> (new RecordData<DComplex>(DComplex()));
    case TpString:
      return std::unique_ptr<RecordDataBase> (new RecordData<String>(String()));
    case TpArrayBool:
      {
        std::unique_ptr<RecordData<Array<Bool>>> ptr
          (new RecordData<Array<Bool>> (Array<Bool>(arrayShape)));
        if (initialize) ptr->value() = False;
        return ptr;
      }
    case TpArrayUChar:
      {
        std::unique_ptr<RecordData<Array<uChar>>> ptr
          (new RecordData<Array<uChar>> (Array<uChar>(arrayShape)));
        if (initialize) ptr->value() = 0;
        return ptr;
      }
    case TpArrayShort:
      {
        std::unique_ptr<RecordData<Array<Short>>> ptr
          (new RecordData<Array<Short>> (Array<Short>(arrayShape)));
        if (initialize) ptr->value() = 0;
        return ptr;
      }
    case TpArrayInt:
      {
        std::unique_ptr<RecordData<Array<Int>>> ptr
          (new RecordData<Array<Int>> (Array<Int>(arrayShape)));
        if (initialize) ptr->value() = 0;
        return ptr;
      }
    case TpArrayUInt:
      {
        std::unique_ptr<RecordData<Array<uInt>>> ptr
          (new RecordData<Array<uInt>> (Array<uInt>(arrayShape)));
        if (initialize) ptr->value() = 0;
        return ptr;
      }
    case TpArrayInt64:
      {
        std::unique_ptr<RecordData<Array<Int64>>> ptr
          (new RecordData<Array<Int64>> (Array<Int64>(arrayShape)));
        if (initialize) ptr->value() = 0;
        return ptr;
      }
    case TpArrayFloat:
      {
        std::unique_ptr<RecordData<Array<float>>> ptr
          (new RecordData<Array<float>> (Array<float>(arrayShape)));
        if (initialize) ptr->value() = 0;
        return ptr;
      }
    case TpArrayDouble:
      {
        std::unique_ptr<RecordData<Array<double>>> ptr
          (new RecordData<Array<double>> (Array<double>(arrayShape)));
        if (initialize) ptr->value() = 0;
        return ptr;
      }
    case TpArrayComplex:
      return std::unique_ptr<RecordDataBase>
        (new RecordData<Array<Complex>> (Array<Complex>(arrayShape)));
    case TpArrayDComplex:
      return std::unique_ptr<RecordDataBase>
        (new RecordData<Array<DComplex>> (Array<DComplex>(arrayShape)));
    case TpArrayString:
      return std::unique_ptr<RecordDataBase>
        (new RecordData<Array<String>> (Array<String>(arrayShape)));
    default:
      throw (AipsError ("RecordDataBase::makeData: unknown data type " +
                        String::toString(int(type))));
    }
  }

  void RecordDataBase::makeDataArr()
  {
    throw AipsError("RecordDataBase::makeDataArr not possible for data type " +
                    String::toString(itsType));
  }
  
  // Copy the data from the other Data object to this one.
  // It is checked if the data types match and if other has data.
  void RecordDataBase::copyData (const RecordDataBase& other)
  {
    AlwaysAssert (itsType==other.itsType && itsDataPtr && other.itsDataPtr, AipsError);
    doCopyData (other.data());
  }

  void RecordDataBase::clear()
  {
    doClear();
    itsType    = TpOther;
    itsDataPtr = nullptr;
    itsArrPtr  = nullptr;
  }

  String RecordDataBase::adjustTypeStr (const String& str) const
  {
    // Remove trailing spaces and capitalize except for uChar/uInt.
    // Capitalization is needed for backward compatibility.
    // Dcomplex needs a capital c.
    String s(str);
    s.trim();
    if (s[0] != 'u') {
      s.capitalize();
      if (s == "Dcomplex") {
        s[1] = 'C';
      }
    }
    return s;
  }


  template<>
  void RecordData<uChar>::printData (std::ostream& os, const String&, Int)
    { os << "uChar " << Int(itsValue); }
  template<>
  void RecordData<String>::printData (std::ostream& os, const String&, Int)
    { os << "String \"" << itsValue << '"'; }



  RecordData<Record>::RecordData (const Record& rec)
    : RecordDataBase (TpRecord),
      itsRecPtr      (new Record(rec))
  {
    setData (itsRecPtr.get());
  }

  void RecordData<Record>::doCopyData (const void* rec)
  {
    *itsRecPtr = *static_cast<const Record*>(rec);
  }
  
  void RecordData<Record>::printData (std::ostream& os, const String& indent,
                                      Int maxNrValues)
  {
    os << '{' << endl;
    itsRecPtr->print (os, maxNrValues, indent+"  ");
    os << indent << '}';
  }
  
  void RecordData<Record>::putData (AipsIO& os, Bool empty, const TableAttr*)
  {
    if (empty) {
      os << *itsRecPtr;
    } else {
      itsRecPtr->putData (os);
    }
  }

  void RecordData<Record>::getData (AipsIO& os, uInt version, Bool empty, const TableAttr*)
  {
    if (empty) {
      os >> *itsRecPtr;
    } else {
      itsRecPtr->getData (os, version);
    }
  }

  void RecordData<Record>::doClear()
  {
    itsRecPtr.reset();
  }


} //# NAMESPACE CASACORE - END
