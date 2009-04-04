//# BitFlagsEngine.h: Templated virtual column engine to map bit flags to a Bool
//# Copyright (C) 2009
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

#ifndef TABLES_BITFLAGSENGINE_H
#define TABLES_BITFLAGSENGINE_H

//# Includes
#include <tables/Tables/BaseMappedArrayEngine.h>

namespace casa { //# NAMESPACE CASA - BEGIN


  // <summary> Non-templated Helper class to handle the mask. </summary>
  // <visibility=local>
  class BFEngineMask
  {
  public:
    // Form the mask as given.
    explicit BFEngineMask (uInt mask=0xffffffff);

    // Form the mask from the given keywords defining the bits.
    BFEngineMask (const Array<String>& keys, uInt defaultMask);

    // Make the mask from the given keywords defining the bits.
    void makeMask (const ROTableColumn& column);

    // Form the read mask from the specification.
    // If keywords are given, the mask is formed from them.
    void fromRecord (const RecordInterface& spec, const ROTableColumn& column,
                     const String& prefix);

    // Store the info in a Record.
    void toRecord (RecordInterface& spec, const String& prefix) const;

    // Get the mask.
    uInt getMask() const
      { return itsMask; }

    // Get the mask keywords.
    const Array<String>& getKeys() const
      { return itsMaskKeys; }

  private:
    Array<String> itsMaskKeys;
    uInt          itsMask;
  };


  // <summary>
  // Templated virtual column engine to map bit flags to a Bool.
  // </summary>

  // <use visibility=export>

  // <reviewed reviewer="Gareth Hunt" date="94Nov17" tests="">
  // </reviewed>

  // <prerequisite>
  //# Classes you should understand before using this one.
  //   <li> VirtualColumnEngine
  //   <li> VirtualArrayColumn
  // </prerequisite>

  // <synopsis> 
  // BitFlagsEngine is a virtual column engine which scales an array
  // of one type to another type to save disk storage.
  // This resembles the classic AIPS compress method which scales the
  // data from float to short.
  // The scale factor and offset value can be given in two ways:
  // <ul>
  //  <li> As a fixed value which is used for all arrays in the column.
  //  <li> As the name of a column. In this way each array in a
  //         column can have its own scale and offset value.
  //         The scale and offset value in a row must be put before
  //         the array is put and should not be changed anymore.
  // </ul>
  // It is also possible to have a variable scale factor with a fixed offset
  // value.
  // As in FITS the scale and offset values are used as:
  // <br><src> True_value = Stored_value * scale + offset; </src>
  //
  // An engine object should be used for one column only, because the stored
  // column name is part of the engine. If it would be used for more than
  // one column, they would all share the same stored column.
  // When the engine is bound to a column, it is checked if the name
  // of that column matches the given virtual column name.
  //
  // The engine can be used for a column containing any kind of array
  // (thus direct or indirect, fixed or variable shaped)) as long as the
  // virtual array can be stored in the stored array. Thus a fixed shaped
  // virtual can use a variable shaped stored, but not vice versa.
  // A fixed shape indirect virtual can use a stored with direct arrays.
  //
  // This class can also serve as an example of how to implement
  // a virtual column engine.
  // </synopsis> 

  // <motivation>
  // This class allows to store data in a smaller representation.
  // It is needed to resemble the classic AIPS compress option.
  // It adds the scale and offset value on a per row basis.
  //
  // Because the engine can serve only one column, it was possible to
  // combine the engine and the column functionality in one class.
  // This has been achieved using multiple inheritance.
  // The advantage of this is that only one templated class is used,
  // so less template instantiations are needed.
  // </motivation>

  // <example>
  // <srcblock>
  // // Create the table description and 2 columns with indirect arrays in it.
  // // The Int column will be stored, while the double will be
  // // used as virtual.
  // TableDesc tableDesc ("", TableDesc::Scratch);
  // tableDesc.addColumn (ArrayColumnDesc<Int> ("storedArray"));
  // tableDesc.addColumn (ArrayColumnDesc<double> ("virtualArray"));
  //
  // // Create a new table using the table description.
  // SetupNewTable newtab (tableDesc, "tab.data", Table::New);
  //
  // // Create the array scaling engine to scale from double to Int
  // // and bind it to the double column.
  // // Create the table.
  // BitFlagsEngine<Int> scalingEngine("virtualArray",
  //                                             "storedArray", 10);
  // newtab.bindColumn ("virtualArray", scalingEngine);
  // Table table (newtab);
  //
  // // Store a 3-D array (with dim. 2,3,4) into each row of the column.
  // // The shape of each array in the column is implicitly set by the put
  // // function. This will also set the shape of the underlying Int array.
  // ArrayColumn data (table, "virtualArray");
  // Array<double> someArray(IPosition(4,2,3,4));
  // someArray = 0;
  // for (uInt i=0, i<10; i++) {          // table will have 10 rows
  //     table.addRow();
  //     data.put (i, someArray)
  // }
  // </srcblock>
  // </example>

  // <templating arg=VirtualType>
  //  <li> only suited for built-in numerics data types
  // </templating>
  // <templating arg=StoredType>
  //  <li> only suited for built-in numerics data types
  // </templating>

  template<typename StoredType> class BitFlagsEngine : public BaseMappedArrayEngine<Bool, StoredType>
  {
    //# Make members of parent class known.
  public:
    using BaseMappedArrayEngine<Bool,StoredType>::virtualName;
  protected:
    using BaseMappedArrayEngine<Bool,StoredType>::storedName;
    using BaseMappedArrayEngine<Bool,StoredType>::table;
    using BaseMappedArrayEngine<Bool,StoredType>::roColumn;
    using BaseMappedArrayEngine<Bool,StoredType>::rwColumn;
    using BaseMappedArrayEngine<Bool,StoredType>::setNames;

  public:
    // Construct an engine to map integer arrays in a column to Bool arrays.
    // StoredColumnName is the name of the column where the scaled
    // data will be put and must have data type StoredType.
    // The virtual column using this engine must have data type Bool.
    // A mask can be given that specifies which bits to use in the mapping
    // from StoredType to bool. Similarly a mask can be given defining which
    // bits to set when mapping from Bool to StoredType.
    BitFlagsEngine (const String& virtualColumnName,
                    const String& storedColumnName,
                    StoredType readMask=StoredType(0xffffffff),
                    StoredType writeMask=1);

    // Construct an engine to map integer arrays in a column to Bool arrays.
    // StoredColumnName is the name of the column where the scaled
    // data will be put and must have data type StoredType.
    // The virtual column using this engine must have data type Bool.
    // A mask can be given that specifies which bits to use in the mapping
    // from StoredType to bool. Similarly a mask can be given defining which
    // bits to set when mapping from Bool to StoredType.
    // The masks are given using the values of keywords in the stored column.
    // Each keyword should be an integer defining one or more bits and can be
    // seen as a symbolic name. The keyword values are or-ed to form the mask.
    BitFlagsEngine (const String& virtualColumnName,
                    const String& storedColumnName,
                    const Array<String>& readMaskKeys,
                    const Array<String>& writeMaskKeys);

    // Construct from a record specification as created by dataManagerSpec().
    BitFlagsEngine (const Record& spec);

    // Destructor is mandatory.
    ~BitFlagsEngine();

    // Return the type name of the engine (i.e. its class name).
    virtual String dataManagerType() const;

    // Get the name given to the engine (is the virtual column name).
    virtual String dataManagerName() const;
  
    // Record a record containing data manager specifications.
    virtual Record dataManagerSpec() const;

    // Get data manager properties that can be modified.
    // These are ReadMask, WriteMask, ReadMaskKeys, and WriteMaskKeys.
    // It is a subset of the data manager specification.
    virtual Record getProperties() const;

    // Modify data manager properties.
    // These are ReadMask, WriteMask, ReadMaskKeys, and/or WriteMaskKeys.
    // Mask keys should be given as an array of strings giving the keyword
    // names defining mask bits (similar to the constructor). Mask keys are
    // only used if not empty.
    virtual void setProperties (const Record& spec);

    // Return the name of the class.
    // This includes the names of the template arguments.
    static String className();

    // Register the class name and the static makeObject "constructor".
    // This will make the engine known to the table system.
    // The automatically invoked registration function in DataManReg.cc
    // contains BitFlagsEngine<Int>.
    // Any other instantiation of this class must be registered "manually"
    // (or added to DataManReg.cc).
    static void registerClass();

  private:
    // Copy constructor is only used by clone().
    // (so it is made private).
    BitFlagsEngine (const BitFlagsEngine<StoredType>&);

    // Assignment is not needed and therefore forbidden
    // (so it is made private and not implemented).
    BitFlagsEngine<StoredType>& operator= (const BitFlagsEngine<StoredType>&);

    // Clone the engine object.
    DataManager* clone() const;

    // Initialize the object for a new table.
    // It defines the keywords containing the engine parameters.
    void create (uInt initialNrrow);

    // Preparing consists of setting the writable switch and
    // adding the initial number of rows in case of create.
    // Furthermore it reads the keywords containing the engine parameters.
    void prepare();

    // Get an array in the given row.
    // This will scale and offset from the underlying array.
    void getArray (uInt rownr, Array<Bool>& array);

    // Put an array in the given row.
    // This will scale and offset to the underlying array.
    void putArray (uInt rownr, const Array<Bool>& array);

    // Get a section of the array in the given row.
    // This will scale and offset from the underlying array.
    void getSlice (uInt rownr, const Slicer& slicer, Array<Bool>& array);

    // Put into a section of the array in the given row.
    // This will scale and offset to the underlying array.
    void putSlice (uInt rownr, const Slicer& slicer,
		   const Array<Bool>& array);

    // Get an entire column.
    // This will scale and offset from the underlying array.
    void getArrayColumn (Array<Bool>& array);

    // Put an entire column.
    // This will scale and offset to the underlying array.
    void putArrayColumn (const Array<Bool>& array);

    // Get some array values in the column.
    // This will scale and offset from the underlying array.
    virtual void getArrayColumnCells (const RefRows& rownrs,
				      Array<Bool>& data);

    // Put some array values in the column.
    // This will scale and offset to the underlying array.
    virtual void putArrayColumnCells (const RefRows& rownrs,
				      const Array<Bool>& data);

    // Get a section of all arrays in the column.
    // This will scale and offset from the underlying array.
    void getColumnSlice (const Slicer& slicer, Array<Bool>& array);

    // Put a section of all arrays in the column.
    // This will scale and offset to the underlying array.
    void putColumnSlice (const Slicer& slicer, const Array<Bool>& array);

    // Get a section of some arrays in the column.
    // This will scale and offset from the underlying array.
    virtual void getColumnSliceCells (const RefRows& rownrs,
				      const Slicer& slicer,
				      Array<Bool>& data);

    // Put into a section of some arrays in the column.
    // This will scale and offset to the underlying array.
    virtual void putColumnSliceCells (const RefRows& rownrs,
				      const Slicer& slicer,
				      const Array<Bool>& data);

    // Map bit flags array to Bool array.
    // This is meant when reading an array from the stored column.
    void mapOnGet (Array<Bool>& array,
                   const Array<StoredType>& stored);

    // Map Bool array to bit flags array.
    // This is meant when writing an array into the stored column.
    void mapOnPut (const Array<Bool>& array,
                   Array<StoredType>& stored);

    // Functor to and an array and mask and convert to Bool.
    struct FlagsToBool : public std::unary_function<StoredType,Bool>
    {
      explicit FlagsToBool(StoredType readMask) : itsMask(readMask) {}
      Bool operator() (StoredType value) const
        { return (value & itsMask) != 0; }
    private:
      StoredType itsMask;
    };
    // Functor to convert Bools to flags using a mask.
    // By default only bit 0 is set.
    // Flag bits not affected are kept.
    struct BoolToFlags : public std::binary_function<Bool,StoredType,StoredType>
    {
      explicit BoolToFlags(StoredType writeMask) : itsMask(writeMask) {}
      StoredType operator() (Bool flag, StoredType value) const
        { return (flag ? value&itsMask : value); }
    private:
      StoredType itsMask;
    };

  public:
    // Define the "constructor" to construct this engine when a
    // table is read back.
    // This "constructor" has to be registered by the user of the engine.
    // If the engine is commonly used, its registration can be added
    // to the registerAllCtor function in DataManReg.cc. 
    // That function gets automatically invoked by the table system.
    static DataManager* makeObject (const String& dataManagerType,
				    const Record& spec);

  private:
    BFEngineMask itsBFEReadMask;
    BFEngineMask itsBFEWriteMask;
    StoredType   itsReadMask;
    StoredType   itsWriteMask;
    Bool         itsIsNew;         //# True = new table
  };


} //# NAMESPACE CASA - END

#ifndef CASACORE_NO_AUTO_TEMPLATES
#include <tables/Tables/BitFlagsEngine.tcc>
#endif //# CASACORE_NO_AUTO_TEMPLATES
#endif
