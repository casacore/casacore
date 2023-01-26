//# BitFlagsEngine.h: Templated virtual column engine to map bit flags to a bool
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

#ifndef TABLES_BITFLAGSENGINE_H
#define TABLES_BITFLAGSENGINE_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/tables/DataMan/BaseMappedArrayEngine.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN


  // <summary> Non-templated Helper class to handle the mask. </summary>
  // <use visibility=local>
  class BFEngineMask
  {
  public:
    // Form the mask as given.
    explicit BFEngineMask (uint32_t mask=0xffffffff);

    // Form the mask from the given keywords defining the bits.
    BFEngineMask (const Array<String>& keys, uint32_t defaultMask);

    // Make the mask from the given keywords defining the bits.
    void makeMask (const TableColumn& column);

    // Form the read mask from the specification.
    // If keywords are given, the mask is formed from them.
    void fromRecord (const RecordInterface& spec, const TableColumn& column,
                     const String& prefix);

    // Store the info in a Record.
    void toRecord (RecordInterface& spec, const String& prefix) const;

    // Get the mask.
    uint32_t getMask() const
      { return itsMask; }

    // Get the mask keywords.
    const Array<String>& getKeys() const
      { return itsMaskKeys; }

  private:
    Array<String> itsMaskKeys;
    uint32_t          itsMask;
  };


  // <summary>
  // Templated virtual column engine to map bit flags to a bool.
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
  // BitFlagsEngine is a virtual column engine which maps an integer column
  // containing flag bits to a bool column. It can be used in a MeasurementSet
  // to have multiple flag categories, yet use all existing software that
  // deals with the bool FLAG column.
  //
  // The engine support read as well as write access.
  // For both cases a mask can be defined telling which bits have to be taken
  // into account. For example, when writing to the bool FLAG column, the data
  // in the bitflags column twill be or-ed with the bits as defined in the
  // writemask. Similary when reading FLAG, only the bits of the readmask are
  // taken into account.
  //
  // The masks can be defined in two ways:
  // <ul>
  //  <li> The mask can be given directly as an integer value.
  //       The default write mask is 1 (thus only bit 0), while the default
  //       read mask is all bits.
  //  <li> Symbolic names for mask bits can be defined as keywords in the
  //       flagbits column. They define the bit value, not the bit number.
  //       It makes it possible to combine bits in a keyword.
  //       The keywords are stored in a subrecord of keyword FLAGSETS.
  //       Example of keyword and their values could be:
  //       <br>RFI=1, CAL=2, CLIP=4, OTHER=8, RFICAL=3
  //       <br>Note that in this example RFICAL is defined such that it
  //       contains RFI and CAL.
  // </ul>
  // A mask can be set at construction time, but it can be changed at runtime
  // using the <src>setProperties</src> function.
  // The masks are kept in special keywords (which are different from the
  // keywords defining the flag bits), so it is possible to change a mask
  // by changing those keywords before opening a table. However, that is
  // not recommended.
  //
  // BitFlagsEngine is known to the table system for data types unsigned char, int16_t,
  // and int32_t.
  // </synopsis>

  // <motivation>
  // The FLAG_CATEGORY defined the Measurement does not work because adding
  // an extra flag means resizing the entire array which is slow.
  // This class makes it possible to use an integer column to store flags
  // and map it directly to a bool column.
  // </motivation>

  // <example>
  // <srcblock>
  // // Create the table description and 2 columns with indirect arrays in it.
  // // The int32_t column will be stored, while the bool will be used as virtual.
  // TableDesc tableDesc ("", TableDesc::Scratch);
  // tableDesc.addColumn (ArrayColumnDesc<int32_t> ("BitBlags"));
  // tableDesc.addColumn (ArrayColumnDesc<bool> ("FLAG"));
  //
  // // Create a new table using the table description.
  // SetupNewTable newtab (tableDesc, "tab.data", Table::New);
  //
  // // Create the engine and bind the FLAG column to it.
  // BitFlagsEngine<int32_t> flagsEngine("FLAG", "BitFlags");
  // newtab.bindColumn ("FLAG", flagsEngine);
  // // Create the table.
  // Table table (newtab);
  //
  // // Store a 3-D array (with dim. 2,3,4) into each row of the column.
  // // The shape of each array in the column is implicitly set by the put
  // // function. This will also set the shape of the underlying int32_t array.
  // ArrayColumn data (table, "virtualArray");
  // Array<bool> someArray(IPosition(4,2,3,4));
  // someArray = true;
  // for (rownr_t i=0, i<10; i++) {          // table will have 10 rows
  //     table.addRow();
  //     data.put (i, someArray)
  // }
  // </srcblock>
  // The underlying integer array will be stored according to the writemask
  // which defaults to 1.
  // </example>

  // <templating arg=StoredType>
  //  <li> only suited for built-in integer data types
  // </templating>

  template<typename StoredType> class BitFlagsEngine : public BaseMappedArrayEngine<bool, StoredType>
  {
    //# Make members of parent class known.
  public:
    using BaseMappedArrayEngine<bool,StoredType>::virtualName;
  protected:
    using BaseMappedArrayEngine<bool,StoredType>::storedName;
    using BaseMappedArrayEngine<bool,StoredType>::table;
    using BaseMappedArrayEngine<bool,StoredType>::column;
    using BaseMappedArrayEngine<bool,StoredType>::setNames;

  public:
    // Construct an engine to map integer arrays in a column to bool arrays.
    // StoredColumnName is the name of the column where the integer
    // data will be put and must have data type StoredType.
    // The virtual column using this engine must have data type bool.
    // <br>A mask can be given that specifies which bits to use in the mapping
    // from StoredType to bool. Similarly a mask can be given defining which
    // bits to set when mapping from bool to StoredType.
    BitFlagsEngine (const String& virtualColumnName,
                    const String& storedColumnName,
                    StoredType readMask=StoredType(0xffffffff),
                    StoredType writeMask=1);

    // Construct an engine to map integer arrays in a column to bool arrays.
    // StoredColumnName is the name of the column where the scaled
    // data will be put and must have data type StoredType.
    // The virtual column using this engine must have data type bool.
    // <br>A mask can be given that specifies which bits to use in the mapping
    // from StoredType to bool. Similarly a mask can be given defining which
    // bits to set when mapping from bool to StoredType.
    // The masks are given using the values of keywords in the stored column.
    // Each keyword should be an integer defining one or more bits and can be
    // seen as a symbolic name. The keyword values are or-ed to form the mask.
    // The keywords are stored in a subrecord of keyword FLAGSETS.
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
    // contains BitFlagsEngine<int32_t>.
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
    void create64 (rownr_t initialNrrow);

    // Preparing consists of setting the writable switch and
    // adding the initial number of rows in case of create.
    // Furthermore it reads the keywords containing the engine parameters.
    void prepare();

    // Get an array in the given row.
    // This will scale and offset from the underlying array.
    void getArray (rownr_t rownr, Array<bool>& array);

    // Put an array in the given row.
    // This will scale and offset to the underlying array.
    void putArray (rownr_t rownr, const Array<bool>& array);

    // Get a section of the array in the given row.
    // This will scale and offset from the underlying array.
    void getSlice (rownr_t rownr, const Slicer& slicer, Array<bool>& array);

    // Put into a section of the array in the given row.
    // This will scale and offset to the underlying array.
    void putSlice (rownr_t rownr, const Slicer& slicer,
		   const Array<bool>& array);

    // Get an entire column.
    // This will scale and offset from the underlying array.
    void getArrayColumn (Array<bool>& array);

    // Put an entire column.
    // This will scale and offset to the underlying array.
    void putArrayColumn (const Array<bool>& array);

    // Get some array values in the column.
    // This will scale and offset from the underlying array.
    virtual void getArrayColumnCells (const RefRows& rownrs,
				      Array<bool>& data);

    // Put some array values in the column.
    // This will scale and offset to the underlying array.
    virtual void putArrayColumnCells (const RefRows& rownrs,
				      const Array<bool>& data);

    // Get a section of all arrays in the column.
    // This will scale and offset from the underlying array.
    void getColumnSlice (const Slicer& slicer, Array<bool>& array);

    // Put a section of all arrays in the column.
    // This will scale and offset to the underlying array.
    void putColumnSlice (const Slicer& slicer, const Array<bool>& array);

    // Get a section of some arrays in the column.
    // This will scale and offset from the underlying array.
    virtual void getColumnSliceCells (const RefRows& rownrs,
				      const Slicer& slicer,
				      Array<bool>& data);

    // Put into a section of some arrays in the column.
    // This will scale and offset to the underlying array.
    virtual void putColumnSliceCells (const RefRows& rownrs,
				      const Slicer& slicer,
				      const Array<bool>& data);

    // Map bit flags array to bool array.
    // This is meant when reading an array from the stored column.
    void mapOnGet (Array<bool>& array,
                   const Array<StoredType>& stored);

    // Map bool array to bit flags array.
    // This is meant when writing an array into the stored column.
    void mapOnPut (const Array<bool>& array,
                   Array<StoredType>& stored);

    // Functor to and an array and mask and convert to bool.
    struct FlagsToBool
    {
      explicit FlagsToBool(StoredType readMask) : itsMask(readMask) {}
      bool operator() (StoredType value) const
        { return (value & itsMask) != 0; }
    private:
      StoredType itsMask;
    };
    // Functor to convert Bools to flags using a mask.
    // By default only bit 0 is set.
    // Flag bits not affected are kept.
    struct BoolToFlags
    {
      explicit BoolToFlags(StoredType writeMask) : itsMask(writeMask) {}
      StoredType operator() (bool flag, StoredType value) const
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
    bool         itsIsNew;         //# true = new table
  };


} //# NAMESPACE CASACORE - END

#ifndef CASACORE_NO_AUTO_TEMPLATES
#include <casacore/tables/DataMan/BitFlagsEngine.tcc>
#endif //# CASACORE_NO_AUTO_TEMPLATES
#endif
