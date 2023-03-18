//# RecordData.h: The representation of the data in a Record field
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


#ifndef CASA_RECORDDATA_H
#define CASA_RECORDDATA_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/casa/Utilities/ValType.h>
#include <casacore/casa/Arrays/Array.h>
#include <casacore/casa/IO/ArrayIO.h>
#include <memory>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

  //# Forward Declarations
  class Record;
  class TableAttr;

  // <summary>
  // The representation of the data in a Record field
  // </summary>

  // <use visibility=local>
  // <reviewed reviewer="Mark Wieringa" date="1996/04/15" tests="tRecord">
  // </reviewed>

  // <prerequisite>
  //   <li> <linkto class="Record">Record</linkto>.
  // </prerequisite>
  //
  // <etymology>
  // RecordDataBase is the base class for RecordData
  // </etymology>
  //
  // <synopsis>
  // RecordDataBase is the abstract base class for the templated RecordData
  // class which contains the data for each possible data type in a Record.
  // It defines the virtual functions to store and handle the data.
  // They are defined such that they are also suitable for class TableRecord.
  //
  // It also has a function to create a RecordData object for all supported
  // scalar and array types.
  // </synopsis>
  //
  // <motivation>
  // The RecordData classes make it possible to handle the data in a typed way.
  // </motivation>

  class RecordDataBase
  {
  public:
    // Construct with the given data type. TpOther means an invalid type.
    explicit RecordDataBase (DataType dtype=TpOther, void* dataPtr=nullptr,
                             ArrayBase* arrPtr=nullptr)
      : itsType   (dtype),
        itsDataPtr(dataPtr),
        itsArrPtr (arrPtr)
    {}

    // Copies are ni=ot possible.
    RecordDataBase (const RecordDataBase&) = delete;
    RecordDataBase& operator= (const RecordDataBase&) = delete;

    virtual ~RecordDataBase() = default;

    // Make the data object for type_p with the possible array shape.
    // Only initialize an array if needed.
    static std::unique_ptr<RecordDataBase> makeData
    (DataType type, const IPosition& shape=IPosition(), Bool initialize=False);

    // Make an Array object for a scalar value, so a scalar can be accessed
    // as an array.
    // The default implementation throws a "not possible" exception.
    virtual void makeDataArr();

    // Copy the data from the other Data object to this one.
    // It is checked if the data types match and if other has data.
    void copyData (const RecordDataBase& other);
    void copyData (const void* data)
      { doCopyData (data); }

    // Print the data. For an array a maximum nr of values is printed
    // and continuation lines are indented as given.
    virtual void printData (std::ostream& os, const String& indent,
                            Int maxNrValues) = 0;

    // Put the data into the AipsIO stream.
    // The last argument is only of importance for subrecords.
    virtual void putData (AipsIO& os, Bool empty, const TableAttr*) = 0;

    // Get the data from the AipsIO stream.
    // The last 2 arguments are only of importance for subrecords.
    virtual void getData (AipsIO& os, uInt version, Bool empty, const TableAttr*) = 0;
  
    // Clear the contents of the field.
    void clear();
    
    // Get the data or dataArr pointer.
    // <group>
    const void* data() const
      { return itsDataPtr; }
    void* data()
      { return itsDataPtr; }
    const ArrayBase* dataArr() const
      { return itsArrPtr; }
    ArrayBase* dataArr()
      { return itsArrPtr; }
    // </group>
    
  protected:
    // Set the data pointer.
    void setData (void* dataPtr)
      { itsDataPtr = dataPtr; }
    // Set the data array pointer.
    void setDataArr (ArrayBase* arrPtr)
      { itsArrPtr = arrPtr; }
    // Adjust the data type string as needed (for backward compatibility).
    String adjustTypeStr (const String& str) const;
    
  private:
    virtual void doCopyData (const void*) = 0;
    virtual void doClear() = 0;
    
    // Data type
    DataType itsType;
    // Pointer to the data (scalar, Array, (Table)Record or Table)
    // This pointer is only filled as the address of underlying data.
    void* itsDataPtr;
    // Array object sharing a scalar to allow handling a scalar as an array
    // This pointer is only filled as the address of underlying Array objects.
    ArrayBase* itsArrPtr;
  };


  // <summary>
  // The representation of scalar data in a Record field
  // </summary>
  // <use visibility=local>
  template<typename T>
  class RecordData : public RecordDataBase
  {
  public:
    RecordData (const T& value)
      : RecordDataBase (whatType<T>(), &itsValue),
        itsValue       (value)
    {}
    ~RecordData() override = default;
    void makeDataArr() override
      {
        itsAsArray.reset (new Array<T>(IPosition(1,1), &itsValue, SHARE));
        setDataArr (itsAsArray.get());
      }
    void doCopyData (const void* data) override
      { itsValue = *static_cast<const T*>(data); }
    void printData (std::ostream& os, const String&, Int) override
      { os << adjustTypeStr(ValType::getTypeStr(static_cast<T*>(0))) << ' ' << itsValue; }
    void putData (AipsIO& os, Bool, const TableAttr*) override
      { os << itsValue; }
    void getData (AipsIO& os, uInt, Bool, const TableAttr*) override
      { os >> itsValue; }
    void doClear() override
      { itsAsArray.reset(); }
  private:
    T itsValue;
    std::unique_ptr<Array<T>> itsAsArray;
  };
  //# uChar does not print nicely, so specialize it.
  template<>
  void RecordData<uChar>::printData (std::ostream& os, const String&, Int);
  //# String needs enclosing double quotes.
  template<>
  void RecordData<String>::printData (std::ostream& os, const String&, Int);


  // <summary>
  // The representation of array data in a Record field
  // </summary>
  // <use visibility=local>
  template<typename T>
  class RecordData<Array<T>> : public RecordDataBase
  {
  public:
    RecordData (const Array<T>& value)
      : RecordDataBase (whatType<T>(), &itsArray, &itsArray),
        itsArray       (value.copy())
    {}
    ~RecordData() override = default;
    void doCopyData (const void* arr) override
      { itsArray.assign (*static_cast<const Array<T>*>(arr)); }
    void printData (std::ostream& os, const String& indent,
                    Int maxNrValues) override
    {
      os << adjustTypeStr(ValType::getTypeStr(static_cast<T*>(0)))
         << " array with shape " << itsArray.shape();
      if (maxNrValues < 0) {
        os << endl << itsArray;
      } else if (maxNrValues > 0) {
        Vector<T> vec = itsArray.reform (IPosition(1, itsArray.size()));
        if (size_t(maxNrValues+1) >= vec.nelements()) {
          os << endl << indent << "  " << vec;
        } else {
          os << ", first values:" << endl << indent << "  "
             << vec(Slice(0,maxNrValues-1));
        }
      }
    }
    void putData (AipsIO& os, Bool, const TableAttr*) override
    //# Note that the old RecordRep::putDataField wrote type names such as
    //# "Array<Int>", while operator<< writes "Array".
    //# See ArrayIO.h why this is fine.
      { os << itsArray; }
    void getData (AipsIO& os, uInt, Bool, const TableAttr*) override
      { os >> itsArray; }
    void doClear() override
      { itsArray.resize(); }
    Array<T>& value()
      { return itsArray; }
  private:
    Array<T> itsArray;
  };


  // <summary>
  // The representation of a nested Record object in a Record field
  // </summary>
  // <use visibility=local>
  template<>
  class RecordData<Record> : public RecordDataBase
  {
  public:
    RecordData (const Record&);
    ~RecordData() override = default;
    void doCopyData (const void* rec) override;
    void printData (std::ostream& os, const String& indent,
                    Int maxNrValues) override;
    void putData (AipsIO& os, Bool empty, const TableAttr*) override;
    void getData (AipsIO& os, uInt version, Bool empty, const TableAttr*) override;
    void doClear() override;
  private:
    //# Cannot include Record.h, so a pointer is needed
    std::unique_ptr<Record> itsRecPtr;
  };


} //# NAMESPACE CASACORE - END

#endif
