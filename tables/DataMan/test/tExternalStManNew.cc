//# tExternalStManNew.cc: Test program for an external stman with new interface
//# Copyright (C) 2019
//# Associated Universities, Inc. Washington DC, USA.
//#
//# This program is free software; you can redistribute it and/or modify it
//# under the terms of the GNU General Public License as published by the Free
//# Software Foundation; either version 2 of the License, or (at your option)
//# any later version.
//#
//# This program is distributed in the hope that it will be useful, but WITHOUT
//# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
//# FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
//# more details.
//#
//# You should have received a copy of the GNU General Public License along
//# with this program; if not, write to the Free Software Foundation, Inc.,
//# 675 Massachusetts Ave, Cambridge, MA 02139, USA.
//#
//# Correspondence concerning AIPS++ should be addressed as follows:
//#        Internet email: aips2-request@nrao.edu.
//#        Postal address: AIPS++ Project Office
//#                        National Radio Astronomy Observatory
//#                        520 Edgemont Road
//#                        Charlottesville, VA 22903-2475 USA

// This test is a very simplified clone of ASTRON's LofarStMan
// using the new DataManager interface.
// It can be compared with tExternalStMan.cc to show the differences
// between the old and new DataManager interface.
//
// The results are written to stdout. The script executing this program,
// compares the results with the reference output file.


//# Includes
#include <casacore/tables/DataMan/DataManager.h>
#include <casacore/tables/DataMan/StManColumnBase.h>
#include <casacore/tables/DataMan/DataManError.h>
#include <casacore/tables/Tables/Table.h>
#include <casacore/tables/Tables/TableRecord.h>
#include <casacore/casa/Containers/Record.h>
#include <casacore/casa/Arrays/Array.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/iostream.h>


namespace casacore {


  // Define the constants as used in the Column classes.
  const uint32_t ntime=10;
  const uint32_t nant=3;
  const uint32_t npol=4;
  const uint32_t nchan=8;


  //# Forward Declarations.
  class LofarColumn;


  class LofarStMan : public DataManager
  {
  public:
    // Create a Lofar storage manager with the given name.
    // If no name is used, it is set to "LofarStMan"
    explicit LofarStMan (const String& dataManagerName = "LofarStMan");

    // Create a Lofar storage manager with the given name.
    // The specifications are part of the record (as created by dataManagerSpec).
    LofarStMan (const String& dataManagerName, const Record& spec);
  
    ~LofarStMan();

    // Clone this object.
    virtual DataManager* clone() const;
  
    // Get the type name of the data manager (i.e. LofarStMan).
    virtual String dataManagerType() const;
  
    // Get the name given to the storage manager (in the constructor).
    virtual String dataManagerName() const;
  
    // Record a record containing data manager specifications.
    virtual Record dataManagerSpec() const;

    // The storage manager is not a regular one.
    virtual bool isRegular() const;
  
    // The storage manager cannot add rows.
    virtual bool canAddRow() const;
  
    // The storage manager cannot delete rows.
    virtual bool canRemoveRow() const;
  
    // The storage manager can add columns, which does not really do something.
    virtual bool canAddColumn() const;
  
    // Columns can be removed, but it does not do anything at all.
    virtual bool canRemoveColumn() const;
  
    // Make the object from the type name string.
    // This function gets registered in the DataManager "constructor" map.
    // The caller has to delete the object.
    static DataManager* makeObject (const String& aDataManType,
                                    const Record& spec);

    // Register the class name and the static makeObject "constructor".
    // This will make the engine known to the table system.
    static void registerClass();

    // Get the nr of rows.
    rownr_t getNRow() const
      { return ntime * nant * nant; }

  private:
    // Copy constructor cannot be used.
    LofarStMan (const LofarStMan& that);

    // Assignment cannot be used.
    LofarStMan& operator= (const LofarStMan& that);
  
    // Flush and optionally fsync the data.
    // It does nothing, and returns false.
    virtual bool flush (AipsIO&, bool doFsync);
  
    // Let the storage manager create files as needed for a new table.
    // This allows a column with an indirect array to create its file.
    virtual void create64 (rownr_t nrrow);
  
    // Open the storage manager file for an existing table.
    // Return the number of rows in the data file.
    virtual rownr_t open64 (rownr_t nrrow, AipsIO&);

    // Prepare the columns.
    virtual void prepare();

    // Resync the storage manager with the new file contents.
    // It does nothing.
    virtual rownr_t resync64 (rownr_t nrrow);
  
    // Reopen the storage manager files for read/write.
    // It does nothing.
    virtual void reopenRW();
  
    // The data manager will be deleted (because all its columns are
    // requested to be deleted).
    // So clean up the things needed (e.g. delete files).
    virtual void deleteManager();

    // Add rows to the storage manager.
    // It cannot do it, so throws an exception.
    virtual void addRow64 (rownr_t nrrow);
  
    // Delete a row from all columns.
    // It cannot do it, so throws an exception.
    virtual void removeRow64 (rownr_t rowNr);
  
    // Do the final addition of a column.
    // It won't do anything.
    virtual void addColumn (DataManagerColumn*);
  
    // Remove a column from the data file.
    // It won't do anything.
    virtual void removeColumn (DataManagerColumn*);
  
    // Create a column in the storage manager on behalf of a table column.
    // The caller has to delete the newly created object.
    // <group>
    // Create a scalar column.
    virtual DataManagerColumn* makeScalarColumn (const String& aName,
                                                 int aDataType,
                                                 const String& aDataTypeID);
    // Create a direct array column.
    virtual DataManagerColumn* makeDirArrColumn (const String& aName,
                                                 int aDataType,
                                                 const String& aDataTypeID);
    // Create an indirect array column.
    virtual DataManagerColumn* makeIndArrColumn (const String& aName,
                                                 int aDataType,
                                                 const String& aDataTypeID);
    // </group>

    //# Declare member variables.
    // Name of data manager.
    String itsDataManName;
    // The column objects.
    vector<LofarColumn*> itsColumns;
  };


  class LofarColumn : public StManColumnBase
  {
  public:
    explicit LofarColumn (LofarStMan* parent, int dtype)
      : StManColumnBase (dtype),
        itsParent       (parent)
    {}
    virtual ~LofarColumn();
    // Most columns are not writable (only DATA is writable).
    virtual bool isWritable() const;
    // Set column shape of fixed shape columns; it does nothing.
    virtual void setShapeColumn (const IPosition& shape);
    // Prepare the column. By default it does nothing.
    virtual void prepareCol();
  protected:
    LofarStMan* itsParent;
  };

  // <summary>ANTENNA1 column in the LOFAR Storage Manager.</summary>
  // <use visibility=local>
  class Ant1Column : public LofarColumn
  {
  public:
    explicit Ant1Column (LofarStMan* parent, int dtype)
      : LofarColumn(parent, dtype) {}
    virtual ~Ant1Column();
    virtual void getInt (rownr_t rowNr, int32_t* dataPtr);
  };

  // <summary>ANTENNA2 column in the LOFAR Storage Manager.</summary>
  // <use visibility=local>
  class Ant2Column : public LofarColumn
  {
  public:
    explicit Ant2Column (LofarStMan* parent, int dtype)
      : LofarColumn(parent, dtype) {}
    virtual ~Ant2Column();
    virtual void getInt (rownr_t rowNr, int32_t* dataPtr);
  };

  // <summary>TIME and TIME_CENTROID column in the LOFAR Storage Manager.</summary>
  // <use visibility=local>
  class TimeColumn : public LofarColumn
  {
  public:
    explicit TimeColumn (LofarStMan* parent, int dtype)
      : LofarColumn(parent, dtype) {}
    virtual ~TimeColumn();
    virtual void getdouble (rownr_t rowNr, double* dataPtr);
  };

  // <summary>INTERVAL and EXPOSURE column in the LOFAR Storage Manager.</summary>
  // <use visibility=local>
  class IntervalColumn : public LofarColumn
  {
  public:
    explicit IntervalColumn (LofarStMan* parent, int dtype)
      : LofarColumn(parent, dtype) {}
    virtual ~IntervalColumn();
    virtual void getdouble (rownr_t rowNr, double* dataPtr);
  };

  // <summary>All columns in the LOFAR Storage Manager with value 0.</summary>
  // <use visibility=local>
  class ZeroColumn : public LofarColumn
  {
  public:
    explicit ZeroColumn (LofarStMan* parent, int dtype)
      : LofarColumn(parent, dtype) {}
    virtual ~ZeroColumn();
    virtual void getInt (rownr_t rowNr, int32_t* dataPtr);
  private:
    int32_t itsValue;
  };

  // <summary>All columns in the LOFAR Storage Manager with value false.</summary>
  // <use visibility=local>
  class FalseColumn : public LofarColumn
  {
  public:
    explicit FalseColumn (LofarStMan* parent, int dtype)
      : LofarColumn(parent, dtype) {}
    virtual ~FalseColumn();
    virtual void getBool (rownr_t rowNr, bool* dataPtr);
  private:
    bool itsValue;
  };

  // <summary>UVW column in the LOFAR Storage Manager.</summary>
  // <use visibility=local>
  class UvwColumn : public LofarColumn
  {
  public:
    explicit UvwColumn (LofarStMan* parent, int dtype)
      : LofarColumn(parent, dtype) {}
    virtual ~UvwColumn();
    virtual IPosition shape (rownr_t rownr);
    virtual void getArrayV (rownr_t rowNr, ArrayBase& dataPtr);
  };

  // <summary>DATA column in the LOFAR Storage Manager.</summary>
  // <use visibility=local>
  class DataColumn : public LofarColumn
  {
  public:
    explicit DataColumn (LofarStMan* parent, int dtype)
      : LofarColumn(parent, dtype) {}
    virtual ~DataColumn();
    virtual bool isWritable() const;
    virtual IPosition shape (rownr_t rownr);
    virtual void getArrayV (rownr_t rowNr, ArrayBase& dataPtr);
    virtual void putArrayV (rownr_t rowNr, const ArrayBase& dataPtr);
  };

  // <summary>FLAG column in the LOFAR Storage Manager.</summary>
  // <use visibility=local>
  class FlagColumn : public LofarColumn
  {
  public:
    explicit FlagColumn (LofarStMan* parent, int dtype)
      : LofarColumn(parent, dtype) {}
    virtual ~FlagColumn();
    virtual IPosition shape (rownr_t rownr);
    virtual void getArrayV (rownr_t rowNr, ArrayBase& dataPtr);
  };

  // <summary>WEIGHT column in the LOFAR Storage Manager.</summary>
  // <use visibility=local>
  class WeightColumn : public LofarColumn
  {
  public:
    explicit WeightColumn (LofarStMan* parent, int dtype)
      : LofarColumn(parent, dtype) {}
    virtual ~WeightColumn();
    virtual IPosition shape (rownr_t rownr);
    virtual void getArrayV (rownr_t rowNr, ArrayBase& dataPtr);
  };

  // <summary>SIGMA column in the LOFAR Storage Manager.</summary>
  // <use visibility=local>
  class SigmaColumn : public LofarColumn
  {
  public:
    explicit SigmaColumn (LofarStMan* parent, int dtype)
      : LofarColumn(parent, dtype) {}
    virtual ~SigmaColumn();
    virtual IPosition shape (rownr_t rownr);
    virtual void getArrayV (rownr_t rowNr, ArrayBase& dataPtr);
  };

  // <summary>WEIGHT_SPECTRUM column in the LOFAR Storage Manager.</summary>
  // <use visibility=local>
  class WSpectrumColumn : public LofarColumn
  {
  public:
    explicit WSpectrumColumn (LofarStMan* parent, int dtype)
      : LofarColumn(parent, dtype) {}
    virtual ~WSpectrumColumn();
    virtual IPosition shape (rownr_t rownr);
    virtual void getArrayV (rownr_t rowNr, ArrayBase& dataPtr);
  };

  // <summary>FLAG_CATEGORY column in the LOFAR Storage Manager.</summary>
  // <use visibility=local>
  class FlagCatColumn : public LofarColumn
  {
  public:
    explicit FlagCatColumn (LofarStMan* parent, int dtype)
      : LofarColumn(parent, dtype) {}
    virtual ~FlagCatColumn();
    virtual bool isShapeDefined (rownr_t rownr);
    virtual IPosition shape (rownr_t rownr);
  };


  LofarColumn::~LofarColumn()
  {}
  bool LofarColumn::isWritable() const
  {
    return false;
  }
  void LofarColumn::setShapeColumn (const IPosition&)
  {}
  void LofarColumn::prepareCol()
  {}

  Ant1Column::~Ant1Column()
  {}
  void Ant1Column::getInt (rownr_t rownr, int32_t* dataPtr)
  {
    // Use 3 antennae (baselines 0-0, 0-1, 0-2, 1-0, 1-1, 1-2, 2-0, 2-1, 2-2).
    *dataPtr = (rownr%(nant*nant)) / nant;
  }

  Ant2Column::~Ant2Column()
  {}
  void Ant2Column::getInt (rownr_t rownr, int32_t* dataPtr)
  {
    // Use 3 antennae (baselines 0-0, 0-1, 0-2, 1-0, 1-1, 1-2, 2-0, 2-1, 2-2).
    *dataPtr = rownr%nant;
  }

  TimeColumn::~TimeColumn()
  {}
  void TimeColumn::getdouble (rownr_t rownr, double* dataPtr)
  {
    *dataPtr = 1 + 2 * (rownr/(nant*nant));
  }

  IntervalColumn::~IntervalColumn()
  {}
  void IntervalColumn::getdouble (rownr_t, double* dataPtr)
  {
    *dataPtr = 2;
  }

  ZeroColumn::~ZeroColumn()
  {}
  void ZeroColumn::getInt (rownr_t, int32_t* dataPtr)
  {
    itsValue = 0;
    columnCache().setIncrement (0);
    if (itsParent->getNRow() > 0) {
      columnCache().set (0, itsParent->getNRow()-1, &itsValue);
    }
    *dataPtr = 0;
  }

  FalseColumn::~FalseColumn()
  {}
  void FalseColumn::getBool (rownr_t, bool* dataPtr)
  {
    itsValue = false;
    columnCache().setIncrement (0);
    if (itsParent->getNRow() > 0) {
      columnCache().set (0, itsParent->getNRow()-1, &itsValue);
    }
    *dataPtr = 0;
  }

  UvwColumn::~UvwColumn()
  {}
  IPosition UvwColumn::shape (rownr_t)
  {
    return IPosition(1,3);
  }
  void UvwColumn::getArrayV (rownr_t rownr, ArrayBase& dataPtr)
  {
    DebugAssert (dtype() == TpDouble, AipsError);
    bool deleteIt;
    void* ptr = dataPtr.getVStorage (deleteIt);
    double* p = static_cast<double*>(ptr);
    p[0] = rownr * 0.1;
    p[1] = rownr * 0.1 + 0.03;
    p[2] = rownr * 0.1 + 0.06;
    dataPtr.putVStorage (ptr, deleteIt);
  }

  DataColumn::~DataColumn()
  {}
  bool DataColumn::isWritable() const
  {
    return true;
  }
  IPosition DataColumn::shape (rownr_t)
  {
    return IPosition(2, npol, nchan);
  }
  void DataColumn::getArrayV (rownr_t rownr, ArrayBase& dataPtr)
  {
    DebugAssert (dtype() == TpComplex, AipsError);
    Array<Complex>& arr = static_cast<Array<Complex>&>(dataPtr);
    indgen (arr, Complex(rownr, rownr+0.5));
  }
  void DataColumn::putArrayV (rownr_t rownr, const ArrayBase& dataPtr)
  {
    DebugAssert (dtype() == TpComplex, AipsError);
    cout << "Ignored DataColumn::putArrayComplexV " << dataPtr.shape()
         << " for row " << rownr << endl;
  }

  FlagColumn::~FlagColumn()
  {}
  IPosition FlagColumn::shape (rownr_t)
  {
    return IPosition(2, npol, nchan);
  }
  void FlagColumn::getArrayV (rownr_t rownr, ArrayBase& dataPtr)
  {
    DebugAssert (dtype() == TpBool, AipsError);
    Array<bool>& arr = static_cast<Array<bool>&>(dataPtr);
    arr = false;
    arr(IPosition(2,rownr%npol, rownr%nchan)) = true;
  }


  WeightColumn::~WeightColumn()
  {}
  IPosition WeightColumn::shape (rownr_t)
  {
    return IPosition(1, 4);
  }
  void WeightColumn::getArrayV (rownr_t, ArrayBase& dataPtr)
  {
    DebugAssert (dtype() == TpFloat, AipsError);
    Array<float>& arr = static_cast<Array<float>&>(dataPtr);
    arr = float(1);
  }

  SigmaColumn::~SigmaColumn()
  {}
  IPosition SigmaColumn::shape (rownr_t)
  {
    return IPosition(1, 4);
  }
  void SigmaColumn::getArrayV (rownr_t, ArrayBase& dataPtr)
  {
    DebugAssert (dtype() == TpFloat, AipsError);
    Array<float>& arr = static_cast<Array<float>&>(dataPtr);
    arr = float(1);
  }

  WSpectrumColumn::~WSpectrumColumn()
  {}
  IPosition WSpectrumColumn::shape (rownr_t)
  {
    return IPosition(2, npol, nchan);
  }
  void WSpectrumColumn::getArrayV (rownr_t rownr, ArrayBase& dataPtr)
  {
    DebugAssert (dtype() == TpFloat, AipsError);
    Array<float>& arr = static_cast<Array<float>&>(dataPtr);
    arr = float(rownr);
  }

  FlagCatColumn::~FlagCatColumn()
  {}
  bool FlagCatColumn::isShapeDefined (rownr_t)
  {
    return false;
  }
  IPosition FlagCatColumn::shape (rownr_t)
  {
    throw DataManError ("LofarStMan: no data in column FLAG_CATEGORY");
  }


  LofarStMan::LofarStMan (const String& dataManName)
    : DataManager    (),
      itsDataManName (dataManName)
  {}

  LofarStMan::LofarStMan (const String& dataManName,
                          const Record&)
    : DataManager    (),
      itsDataManName (dataManName)
  {}

  LofarStMan::LofarStMan (const LofarStMan& that)
    : DataManager    (),
      itsDataManName (that.itsDataManName)
  {}

  LofarStMan::~LofarStMan()
  {
    for (uint32_t i=0; i<ncolumn(); i++) {
      delete itsColumns[i];
    }
  }

  DataManager* LofarStMan::clone() const
  {
    return new LofarStMan (*this);
  }

  String LofarStMan::dataManagerType() const
  {
    return "LofarStMan";
  }

  String LofarStMan::dataManagerName() const
  {
    return itsDataManName;
  }

  Record LofarStMan::dataManagerSpec() const
  {
    return Record();
  }

  DataManagerColumn* LofarStMan::makeScalarColumn (const String& name,
                                                   int dtype,
                                                   const String&)
  {
    LofarColumn* col;
    if (name == "TIME"  ||  name == "TIME_CENTROID") {
      col = new TimeColumn(this, dtype);
    } else if (name == "ANTENNA1") {
      col = new Ant1Column(this, dtype);
    } else if (name == "ANTENNA2") {
      col = new Ant2Column(this, dtype);
    } else if (name == "INTERVAL"  ||  name == "EXPOSURE") {
      col = new IntervalColumn(this, dtype);
    } else if (name == "FLAG_ROW") {
      col = new FalseColumn(this, dtype);
    } else {
      col = new ZeroColumn(this, dtype);
    }
    itsColumns.push_back (col);
    return col;
  }

  DataManagerColumn* LofarStMan::makeDirArrColumn (const String& name,
                                                   int dataType,
                                                   const String& dataTypeId)
  {
    return makeIndArrColumn (name, dataType, dataTypeId);
  }

  DataManagerColumn* LofarStMan::makeIndArrColumn (const String& name,
                                                   int dtype,
                                                   const String&)
  {
    LofarColumn* col;
    if (name == "UVW") {
      col = new UvwColumn(this, dtype);
    } else if (name == "DATA") {
      col = new DataColumn(this, dtype);
    } else if (name == "FLAG") {
      col = new FlagColumn(this, dtype);
    } else if (name == "FLAG_CATEGORY") {
      col = new FlagCatColumn(this, dtype);
    } else if (name == "WEIGHT") {
      col = new WeightColumn(this, dtype);
    } else if (name == "SIGMA") {
      col = new SigmaColumn(this, dtype);
    } else if (name == "WEIGHT_SPECTRUM") {
      col = new WSpectrumColumn(this, dtype);
    } else {
      throw DataManError (name + " is unknown column for LofarStMan");
    }
    itsColumns.push_back (col);
    return col;
  }

  DataManager* LofarStMan::makeObject (const String& group, const Record& spec)
  {
    // This function is called when reading a table back.
    return new LofarStMan (group, spec);
  }

  void LofarStMan::registerClass()
  {
    DataManager::registerCtor ("LofarStMan", makeObject);
  }

  bool LofarStMan::isRegular() const
  {
    return false;
  }
  bool LofarStMan::canAddRow() const
  {
    return false;
  }
  bool LofarStMan::canRemoveRow() const
  {
    return false;
  }
  bool LofarStMan::canAddColumn() const
  {
    return true;
  }
  bool LofarStMan::canRemoveColumn() const
  {
    return true;
  }

  void LofarStMan::addRow64 (rownr_t)
  {
    throw DataManError ("LofarStMan cannot add rows");
  }
  void LofarStMan::removeRow64 (rownr_t)
  {
    throw DataManError ("LofarStMan cannot remove rows");
  }
  void LofarStMan::addColumn (DataManagerColumn*)
  {}
  void LofarStMan::removeColumn (DataManagerColumn*)
  {}

  bool LofarStMan::flush (AipsIO&, bool)
  {
    return false;
  }

  void LofarStMan::create64 (rownr_t)
  {}

  rownr_t LofarStMan::open64 (rownr_t, AipsIO&)
  {
    return getNRow();
  }

  void LofarStMan::prepare()
  {}

  rownr_t LofarStMan::resync64 (rownr_t)
  {
    return getNRow();
  }

  void LofarStMan::reopenRW()
  {}

  void LofarStMan::deleteManager()
  {}

  
}   // end namespace


#include <casacore/tables/Tables/TableDesc.h>
#include <casacore/tables/Tables/SetupNewTab.h>
#include <casacore/tables/Tables/Table.h>
#include <casacore/tables/Tables/TableLock.h>
#include <casacore/tables/Tables/ScaColDesc.h>
#include <casacore/tables/Tables/ArrColDesc.h>
#include <casacore/tables/Tables/ScalarColumn.h>
#include <casacore/tables/Tables/ArrayColumn.h>
#include <casacore/casa/Arrays/Array.h>
#include <casacore/casa/IO/ArrayIO.h>
#include <casacore/casa/Arrays/ArrayLogical.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/iostream.h>
#include <casacore/casa/sstream.h>

using namespace casacore;


void createTable()
{
  // Build the table description.
  // Add all mandatory columns of the MS main table.
  TableDesc td("", "1", TableDesc::Scratch);
  td.comment() = "A test of class Table";
  td.addColumn (ScalarColumnDesc<double>("TIME"));
  td.addColumn (ScalarColumnDesc<int32_t>("ANTENNA1"));
  td.addColumn (ScalarColumnDesc<int32_t>("ANTENNA2"));
  td.addColumn (ScalarColumnDesc<int32_t>("FEED1"));
  td.addColumn (ScalarColumnDesc<int32_t>("FEED2"));
  td.addColumn (ScalarColumnDesc<int32_t>("DATA_DESC_ID"));
  td.addColumn (ScalarColumnDesc<int32_t>("PROCESSOR_ID"));
  td.addColumn (ScalarColumnDesc<int32_t>("FIELD_ID"));
  td.addColumn (ScalarColumnDesc<int32_t>("ARRAY_ID"));
  td.addColumn (ScalarColumnDesc<int32_t>("OBSERVATION_ID"));
  td.addColumn (ScalarColumnDesc<int32_t>("STATE_ID"));
  td.addColumn (ScalarColumnDesc<int32_t>("SCAN_NUMBER"));
  td.addColumn (ScalarColumnDesc<double>("INTERVAL"));
  td.addColumn (ScalarColumnDesc<double>("EXPOSURE"));
  td.addColumn (ScalarColumnDesc<double>("TIME_CENTROID"));
  td.addColumn (ScalarColumnDesc<bool>("FLAG_ROW"));
  td.addColumn (ArrayColumnDesc<double>("UVW",IPosition(1,3),
                                        ColumnDesc::Direct));
  td.addColumn (ArrayColumnDesc<Complex>("DATA"));
  td.addColumn (ArrayColumnDesc<float>("SIGMA"));
  td.addColumn (ArrayColumnDesc<float>("WEIGHT"));
  td.addColumn (ArrayColumnDesc<float>("WEIGHT_SPECTRUM"));
  td.addColumn (ArrayColumnDesc<bool>("FLAG"));
  td.addColumn (ArrayColumnDesc<bool>("FLAG_CATEGORY"));
  // Now create a new table from the description.
  SetupNewTable newtab("tLofarStMan_tmp.data", td, Table::New);
  // Create the storage manager and bind all columns to it.
  LofarStMan sm1;
  newtab.bindAll (sm1);
  // Finally create the table. The destructor writes it.
  Table tab(newtab);
}


// maxWeight tells maximum weight before it wraps
// (when nbytesPerSample is small).
void readTable()
{
  // Open the table and check if #rows is as expected.
  Table tab("tLofarStMan_tmp.data");
  rownr_t nrow = tab.nrow();
  uint32_t nbasel = nant*nant;
  AlwaysAssertExit (nrow = ntime*nbasel);
  AlwaysAssertExit (!tab.canAddRow());
  AlwaysAssertExit (!tab.canRemoveRow());
  AlwaysAssertExit (tab.canRemoveColumn(Vector<String>(1, "DATA")));
  // Create objects for all mandatory MS columns.
  ArrayColumn<Complex> dataCol(tab, "DATA");
  ArrayColumn<float> weightCol(tab, "WEIGHT");
  ArrayColumn<float> wspecCol(tab, "WEIGHT_SPECTRUM");
  ArrayColumn<float> sigmaCol(tab, "SIGMA");
  ArrayColumn<double> uvwCol(tab, "UVW");
  ArrayColumn<bool> flagCol(tab, "FLAG");
  ArrayColumn<bool> flagcatCol(tab, "FLAG_CATEGORY");
  ScalarColumn<double> timeCol(tab, "TIME");
  ScalarColumn<double> centCol(tab, "TIME_CENTROID");
  ScalarColumn<double> intvCol(tab, "INTERVAL");
  ScalarColumn<double> expoCol(tab, "EXPOSURE");
  ScalarColumn<int32_t> ant1Col(tab, "ANTENNA1");
  ScalarColumn<int32_t> ant2Col(tab, "ANTENNA2");
  ScalarColumn<int32_t> feed1Col(tab, "FEED1");
  ScalarColumn<int32_t> feed2Col(tab, "FEED2");
  ScalarColumn<int32_t> ddidCol(tab, "DATA_DESC_ID");
  ScalarColumn<int32_t> pridCol(tab, "PROCESSOR_ID");
  ScalarColumn<int32_t> fldidCol(tab, "FIELD_ID");
  ScalarColumn<int32_t> arridCol(tab, "ARRAY_ID");
  ScalarColumn<int32_t> obsidCol(tab, "OBSERVATION_ID");
  ScalarColumn<int32_t> stidCol(tab, "STATE_ID");
  ScalarColumn<int32_t> scnrCol(tab, "SCAN_NUMBER");
  ScalarColumn<bool> flagrowCol(tab, "FLAG_ROW");
  // Create and initialize expected data and weight.
  Array<Complex> dataExp(IPosition(2,npol,nchan));
  indgen (dataExp, Complex(0, 0.5));
  Array<float> weightExp(IPosition(2,1,nchan), 0.0f);
  // Loop through all rows in the table and check the data.
  rownr_t row=0;
  for (uint32_t i=0; i<ntime; ++i) {
    for (uint32_t j=0; j<nant; ++j) {
      for (uint32_t k=0; k<nant; ++k) {
        // Contents must be present except for FLAG_CATEGORY.
	AlwaysAssertExit (dataCol.isDefined (row));
	AlwaysAssertExit (weightCol.isDefined (row));
        AlwaysAssertExit (wspecCol.isDefined (row));
        AlwaysAssertExit (sigmaCol.isDefined (row));
        AlwaysAssertExit (flagCol.isDefined (row));
        AlwaysAssertExit (!flagcatCol.isDefined (row));
        // Check data, weight, sigma, weight_spectrum, flag
        AlwaysAssertExit (allNear (dataCol(row), dataExp, 1e-7));
        AlwaysAssertExit (weightCol.shape(row) == IPosition(1,npol));
        AlwaysAssertExit (allEQ (weightCol(row), float(1)));
        AlwaysAssertExit (sigmaCol.shape(row) == IPosition(1,npol));
        AlwaysAssertExit (allEQ (sigmaCol(row), float(1)));
        Array<float> weights = wspecCol(row);
        AlwaysAssertExit (weights.shape() == IPosition(2,npol,nchan));
        Array<bool> flagExp (weights.shape(), false);
        flagExp(IPosition(2,row%npol, row%nchan)) = true;
        AlwaysAssertExit (allEQ (flagCol(row), flagExp));
        // Check ANTENNA1 and ANTENNA2
        AlwaysAssertExit (ant1Col(row) == int32_t(j));
        AlwaysAssertExit (ant2Col(row) == int32_t(k));
        dataExp += Complex(1, 1);
        weightExp += float(1);
        ++row;
      }
    }
  }
  // Check values in TIME column.
  const double interval = 2;
  Vector<double> times = timeCol.getColumn();
  AlwaysAssertExit (times.size() == nrow);
  row=0;
  double startTime = 1;
  for (uint32_t i=0; i<ntime; ++i) {
    for (uint32_t j=0; j<nbasel; ++j) {
      AlwaysAssertExit (near(times[row], startTime));
      ++row;
    }
    startTime += interval;
  }
  // Check the other columns.
  AlwaysAssertExit (allNear(centCol.getColumn(), times, 1e-13));
  AlwaysAssertExit (allNear(intvCol.getColumn(), interval, 1e-13));
  AlwaysAssertExit (allNear(expoCol.getColumn(), interval, 1e-13));
  AlwaysAssertExit (allEQ(feed1Col.getColumn(), 0));
  AlwaysAssertExit (allEQ(feed2Col.getColumn(), 0));
  AlwaysAssertExit (allEQ(ddidCol.getColumn(), 0));
  AlwaysAssertExit (allEQ(pridCol.getColumn(), 0));
  AlwaysAssertExit (allEQ(fldidCol.getColumn(), 0));
  AlwaysAssertExit (allEQ(arridCol.getColumn(), 0));
  AlwaysAssertExit (allEQ(obsidCol.getColumn(), 0));
  AlwaysAssertExit (allEQ(stidCol.getColumn(), 0));
  AlwaysAssertExit (allEQ(scnrCol.getColumn(), 0));
  AlwaysAssertExit (allEQ(flagrowCol.getColumn(), false));
  // Check the UVW coordinates.
  Array<double> uvwExp(IPosition(1,3));
  indgen (uvwExp, 0., 0.03);
  for (rownr_t i=0; i<nrow; ++i) {
    AlwaysAssertExit (allNear(uvwCol(i), uvwExp, 1e-13));
    uvwExp += 0.1;
  }
  // Check if getColumnCells works.
  RefRows rownrs(0,2,1);
  Slicer slicer(IPosition(2,0,0), IPosition(2,1,1));
  Array<float> wg  = wspecCol.getColumnCells (rownrs);
  Array<float> wgs = wspecCol.getColumnCells (rownrs, slicer);
  cout << wspecCol(0).shape() << ' ' << wg.shape() << ' ' << wgs.shape() << endl;
}

void updateTable()
{
  // Open the table for write.
  Table tab("tLofarStMan_tmp.data", Table::Update);
  // Create object for DATA column.
  ArrayColumn<Complex> dataCol(tab, "DATA");
  // Check we can write the column, but not change the shape.
  AlwaysAssertExit (tab.isColumnWritable ("DATA"));
  AlwaysAssertExit (!dataCol.canChangeShape());
  // Create and initialize data.
  Array<Complex> data(IPosition(2,npol,nchan));
  // Write the data (which only writes a message).
  dataCol.put (0, data);
}

void copyTable()
{
  Table tab("tLofarStMan_tmp.data");
  // Deep copy the table.
  tab.deepCopy ("tLofarStMan_tmp.datcp", Table::New, true);
}


int main()
{
  try {
    // Register LofarStMan to be able to read it back.
    LofarStMan::registerClass();
    // Create the table.
    createTable();
    readTable();
    // Update the table and check again.
    updateTable();
    readTable();
    // Check the copying the table works well.
    copyTable();
  } catch (AipsError& x) {
    cout << "Caught an exception: " << x.getMesg() << endl;
    return 1;
  } 
  return 0;                           // exit with success status
}
