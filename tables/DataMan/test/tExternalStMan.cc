// # tExternalStMan.cc: Test program for an external stman with old interface
// # Copyright (C) 2019
// # Associated Universities, Inc. Washington DC, USA.
// #
// # This program is free software; you can redistribute it and/or modify it
// # under the terms of the GNU General Public License as published by the Free
// # Software Foundation; either version 2 of the License, or (at your option)
// # any later version.
// #
// # This program is distributed in the hope that it will be useful, but WITHOUT
// # ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
// # FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
// # more details.
// #
// # You should have received a copy of the GNU General Public License along
// # with this program; if not, write to the Free Software Foundation, Inc.,
// # 675 Massachusetts Ave, Cambridge, MA 02139, USA.
// #
// # Correspondence concerning AIPS++ should be addressed as follows:
// #        Internet email: casa-feedback@nrao.edu.
// #        Postal address: AIPS++ Project Office
// #                        National Radio Astronomy Observatory
// #                        520 Edgemont Road
// #                        Charlottesville, VA 22903-2475 USA

// This test is a very simplified clone of ASTRON's LofarStMan.
// using the old DataManager interface.
// It tests if the old DataManager interface works properly.
//
// The results are written to stdout. The script executing this program,
// compares the results with the reference output file.

// # Includes
#include <casacore/tables/DataMan/DataManager.h>
#include <casacore/tables/DataMan/StManColumn.h>
#include <casacore/tables/DataMan/DataManError.h>
#include <casacore/tables/Tables/Table.h>
#include <casacore/tables/Tables/TableRecord.h>
#include <casacore/casa/Containers/Record.h>
#include <casacore/casa/Arrays/Array.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/iostream.h>

namespace casacore {

// Define the constants as used in the Column classes.
const unsigned int ntime = 10;
const unsigned int nant = 3;
const unsigned int npol = 4;
const unsigned int nchan = 8;

// # Forward Declarations.
class LofarColumn;

class LofarStMan final : public DataManager {
 public:
  // Create a Lofar storage manager with the given name.
  // If no name is used, it is set to "LofarStMan"
  explicit LofarStMan(const String& dataManagerName = "LofarStMan");

  // Create a Lofar storage manager with the given name.
  // The specifications are part of the record (as created by dataManagerSpec).
  LofarStMan(const String& dataManagerName, const Record& spec);

  ~LofarStMan() override;

  // Clone this object.
  DataManager* clone() const override;

  // Get the type name of the data manager (i.e. LofarStMan).
  String dataManagerType() const override;

  // Get the name given to the storage manager (in the constructor).
  String dataManagerName() const override;

  // Record a record containing data manager specifications.
  Record dataManagerSpec() const override;

  // The storage manager is not a regular one.
  bool isRegular() const override;

  // The storage manager cannot add rows.
  bool canAddRow() const override;

  // The storage manager cannot delete rows.
  bool canRemoveRow() const override;

  // The storage manager can add columns, which does not really do something.
  bool canAddColumn() const override;

  // Columns can be removed, but it does not do anything at all.
  bool canRemoveColumn() const override;

  // Make the object from the type name string.
  // This function gets registered in the DataManager "constructor" map.
  // The caller has to delete the object.
  static DataManager* makeObject(const String& aDataManType, const Record& spec);

  // Register the class name and the static makeObject "constructor".
  // This will make the engine known to the table system.
  static void registerClass();

  // Get the nr of rows.
  unsigned int getNRow() const { return ntime * nant * nant; }

 private:
  // Copy constructor cannot be used.
  LofarStMan(const LofarStMan& that);

  // Assignment cannot be used.
  LofarStMan& operator=(const LofarStMan& that);

  // Flush and optionally fsync the data.
  // It does nothing, and returns false.
  bool flush(AipsIO&, bool doFsync) override;

  // Let the storage manager create files as needed for a new table.
  // This allows a column with an indirect array to create its file.
  void create(unsigned int nrrow) override;

  // Open the storage manager file for an existing table.
  // Return the number of rows in the data file.
  // <group>
  void open(unsigned int nrrow, AipsIO&) override;  // # should never be called
  unsigned int open1(unsigned int nrrow, AipsIO&) override;
  // </group>

  // Prepare the columns.
  void prepare() override;

  // Resync the storage manager with the new file contents.
  // It does nothing.
  // <group>
  void resync(unsigned int nrrow) override;  // # should never be called
  unsigned int resync1(unsigned int nrrow) override;
  // </group>

  // Reopen the storage manager files for read/write.
  // It does nothing.
  void reopenRW() override;

  // The data manager will be deleted (because all its columns are
  // requested to be deleted).
  // So clean up the things needed (e.g. delete files).
  void deleteManager() override;

  // Add rows to the storage manager.
  // It cannot do it, so throws an exception.
  void addRow(unsigned int nrrow) override;

  // Delete a row from all columns.
  // It cannot do it, so throws an exception.
  void removeRow(unsigned int rowNr) override;

  // Do the final addition of a column.
  // It won't do anything.
  void addColumn(DataManagerColumn*) override;

  // Remove a column from the data file.
  // It won't do anything.
  void removeColumn(DataManagerColumn*) override;

  // Create a column in the storage manager on behalf of a table column.
  // The caller has to delete the newly created object.
  // <group>
  // Create a scalar column.
  DataManagerColumn* makeScalarColumn(const String& aName, int aDataType,
                                      const String& aDataTypeID) override;
  // Create a direct array column.
  DataManagerColumn* makeDirArrColumn(const String& aName, int aDataType,
                                      const String& aDataTypeID) override;
  // Create an indirect array column.
  DataManagerColumn* makeIndArrColumn(const String& aName, int aDataType,
                                      const String& aDataTypeID) override;
  // </group>

  // # Declare member variables.
  //  Name of data manager.
  String itsDataManName;
  // The column objects.
  vector<LofarColumn*> itsColumns;
};

class LofarColumn : public StManColumn {
 public:
  explicit LofarColumn(LofarStMan* parent, int dtype) : StManColumn(dtype), itsParent(parent) {}
  ~LofarColumn() override;
  // Most columns are not writable (only DATA is writable).
  bool isWritable() const override;
  // Set column shape of fixed shape columns; it does nothing.
  void setShapeColumn(const IPosition& shape) final override;
  // Prepare the column. By default it does nothing.
  virtual void prepareCol();

 protected:
  LofarStMan* itsParent;
};

// <summary>ANTENNA1 column in the LOFAR Storage Manager.</summary>
// <use visibility=local>
class Ant1Column : public LofarColumn {
 public:
  explicit Ant1Column(LofarStMan* parent, int dtype) : LofarColumn(parent, dtype) {}
  ~Ant1Column() override;
  void getIntV(unsigned int rowNr, int* dataPtr) override;
};

// <summary>ANTENNA2 column in the LOFAR Storage Manager.</summary>
// <use visibility=local>
class Ant2Column : public LofarColumn {
 public:
  explicit Ant2Column(LofarStMan* parent, int dtype) : LofarColumn(parent, dtype) {}
  ~Ant2Column() override;
  void getIntV(unsigned int rowNr, int* dataPtr) override;
};

// <summary>TIME and TIME_CENTROID column in the LOFAR Storage Manager.</summary>
// <use visibility=local>
class TimeColumn : public LofarColumn {
 public:
  explicit TimeColumn(LofarStMan* parent, int dtype) : LofarColumn(parent, dtype) {}
  ~TimeColumn() override;
  void getdoubleV(unsigned int rowNr, double* dataPtr) override;
};

// <summary>INTERVAL and EXPOSURE column in the LOFAR Storage Manager.</summary>
// <use visibility=local>
class IntervalColumn : public LofarColumn {
 public:
  explicit IntervalColumn(LofarStMan* parent, int dtype) : LofarColumn(parent, dtype) {}
  ~IntervalColumn() override;
  void getdoubleV(unsigned int rowNr, double* dataPtr) override;
};

// <summary>All columns in the LOFAR Storage Manager with value 0.</summary>
// <use visibility=local>
class ZeroColumn : public LofarColumn {
 public:
  explicit ZeroColumn(LofarStMan* parent, int dtype) : LofarColumn(parent, dtype) {}
  ~ZeroColumn() override;
  void getIntV(unsigned int rowNr, int* dataPtr) override;

 private:
  int itsValue;
};

// <summary>All columns in the LOFAR Storage Manager with value false.</summary>
// <use visibility=local>
class FalseColumn : public LofarColumn {
 public:
  explicit FalseColumn(LofarStMan* parent, int dtype) : LofarColumn(parent, dtype) {}
  ~FalseColumn() override;
  void getBoolV(unsigned int rowNr, bool* dataPtr) override;

 private:
  bool itsValue;
};

// <summary>UVW column in the LOFAR Storage Manager.</summary>
// <use visibility=local>
class UvwColumn : public LofarColumn {
 public:
  explicit UvwColumn(LofarStMan* parent, int dtype) : LofarColumn(parent, dtype) {}
  ~UvwColumn() override;
  using LofarColumn::shape;
  IPosition shape(unsigned int rownr) override;
  void getArraydoubleV(unsigned int rowNr, Array<double>* dataPtr) override;
};

// <summary>DATA column in the LOFAR Storage Manager.</summary>
// <use visibility=local>
class DataColumn : public LofarColumn {
 public:
  explicit DataColumn(LofarStMan* parent, int dtype) : LofarColumn(parent, dtype) {}
  ~DataColumn() override;
  bool isWritable() const override;
  using LofarColumn::shape;
  IPosition shape(unsigned int rownr) override;
  void getArrayComplexV(unsigned int rowNr, Array<Complex>* dataPtr) override;
  void putArrayComplexV(unsigned int rowNr, const Array<Complex>* dataPtr) override;
};

// <summary>FLAG column in the LOFAR Storage Manager.</summary>
// <use visibility=local>
class FlagColumn : public LofarColumn {
 public:
  explicit FlagColumn(LofarStMan* parent, int dtype) : LofarColumn(parent, dtype) {}
  ~FlagColumn() override;
  using LofarColumn::shape;
  IPosition shape(unsigned int rownr) override;
  void getArrayBoolV(unsigned int rowNr, Array<bool>* dataPtr) override;
};

// <summary>WEIGHT column in the LOFAR Storage Manager.</summary>
// <use visibility=local>
class WeightColumn : public LofarColumn {
 public:
  explicit WeightColumn(LofarStMan* parent, int dtype) : LofarColumn(parent, dtype) {}
  ~WeightColumn() override;
  using LofarColumn::shape;
  IPosition shape(unsigned int rownr) override;
  void getArrayfloatV(unsigned int rowNr, Array<float>* dataPtr) override;
};

// <summary>SIGMA column in the LOFAR Storage Manager.</summary>
// <use visibility=local>
class SigmaColumn : public LofarColumn {
 public:
  explicit SigmaColumn(LofarStMan* parent, int dtype) : LofarColumn(parent, dtype) {}
  ~SigmaColumn() override;
  using LofarColumn::shape;
  IPosition shape(unsigned int rownr) override;
  void getArrayfloatV(unsigned int rowNr, Array<float>* dataPtr) override;
};

// <summary>WEIGHT_SPECTRUM column in the LOFAR Storage Manager.</summary>
// <use visibility=local>
class WSpectrumColumn : public LofarColumn {
 public:
  explicit WSpectrumColumn(LofarStMan* parent, int dtype) : LofarColumn(parent, dtype) {}
  ~WSpectrumColumn() override;
  using LofarColumn::shape;
  IPosition shape(unsigned int rownr) override;
  void getArrayfloatV(unsigned int rowNr, Array<float>* dataPtr) override;
};

// <summary>FLAG_CATEGORY column in the LOFAR Storage Manager.</summary>
// <use visibility=local>
class FlagCatColumn : public LofarColumn {
 public:
  explicit FlagCatColumn(LofarStMan* parent, int dtype) : LofarColumn(parent, dtype) {}
  ~FlagCatColumn() override;
  using LofarColumn::isShapeDefined;
  bool isShapeDefined(unsigned int rownr) override;
  using LofarColumn::shape;
  IPosition shape(unsigned int rownr) override;
};

LofarColumn::~LofarColumn() {}
bool LofarColumn::isWritable() const { return false; }
void LofarColumn::setShapeColumn(const IPosition&) {}
void LofarColumn::prepareCol() {}

Ant1Column::~Ant1Column() {}
void Ant1Column::getIntV(unsigned int rownr, int* dataPtr) {
  // Use 3 antennae (baselines 0-0, 0-1, 0-2, 1-0, 1-1, 1-2, 2-0, 2-1, 2-2).
  *dataPtr = (rownr % (nant * nant)) / nant;
}

Ant2Column::~Ant2Column() {}
void Ant2Column::getIntV(unsigned int rownr, int* dataPtr) {
  // Use 3 antennae (baselines 0-0, 0-1, 0-2, 1-0, 1-1, 1-2, 2-0, 2-1, 2-2).
  *dataPtr = rownr % nant;
}

TimeColumn::~TimeColumn() {}
void TimeColumn::getdoubleV(unsigned int rownr, double* dataPtr) {
  *dataPtr = 1 + 2 * (rownr / (nant * nant));
}

IntervalColumn::~IntervalColumn() {}
void IntervalColumn::getdoubleV(unsigned int, double* dataPtr) { *dataPtr = 2; }

ZeroColumn::~ZeroColumn() {}
void ZeroColumn::getIntV(unsigned int, int* dataPtr) {
  itsValue = 0;
  columnCache().setIncrement(0);
  if (itsParent->getNRow() > 0) {
    columnCache().set(0, itsParent->getNRow() - 1, &itsValue);
  }
  *dataPtr = 0;
}

FalseColumn::~FalseColumn() {}
void FalseColumn::getBoolV(unsigned int, bool* dataPtr) {
  itsValue = false;
  columnCache().setIncrement(0);
  if (itsParent->getNRow() > 0) {
    columnCache().set(0, itsParent->getNRow() - 1, &itsValue);
  }
  *dataPtr = 0;
}

UvwColumn::~UvwColumn() {}
IPosition UvwColumn::shape(unsigned int) { return IPosition(1, 3); }
void UvwColumn::getArraydoubleV(unsigned int rownr, Array<double>* dataPtr) {
  (*dataPtr)[0] = rownr * 0.1;
  (*dataPtr)[1] = rownr * 0.1 + 0.03;
  (*dataPtr)[2] = rownr * 0.1 + 0.06;
}

DataColumn::~DataColumn() {}
bool DataColumn::isWritable() const { return true; }
IPosition DataColumn::shape(unsigned int) { return IPosition(2, npol, nchan); }
void DataColumn::getArrayComplexV(unsigned int rownr, Array<Complex>* dataPtr) {
  indgen(*dataPtr, Complex(rownr, rownr + 0.5));
}
void DataColumn::putArrayComplexV(unsigned int rownr, const Array<Complex>* dataPtr) {
  cout << "Ignored DataColumn::putArrayComplexV " << dataPtr->shape() << " for row " << rownr
       << endl;
}

FlagColumn::~FlagColumn() {}
IPosition FlagColumn::shape(unsigned int) { return IPosition(2, npol, nchan); }
void FlagColumn::getArrayBoolV(unsigned int rownr, Array<bool>* dataPtr) {
  *dataPtr = false;
  (*dataPtr)(IPosition(2, rownr % npol, rownr % nchan)) = true;
}

WeightColumn::~WeightColumn() {}
IPosition WeightColumn::shape(unsigned int) { return IPosition(1, 4); }
void WeightColumn::getArrayfloatV(unsigned int, Array<float>* dataPtr) { *dataPtr = float(1); }

SigmaColumn::~SigmaColumn() {}
IPosition SigmaColumn::shape(unsigned int) { return IPosition(1, 4); }
void SigmaColumn::getArrayfloatV(unsigned int, Array<float>* dataPtr) { *dataPtr = float(1); }

WSpectrumColumn::~WSpectrumColumn() {}
IPosition WSpectrumColumn::shape(unsigned int) { return IPosition(2, npol, nchan); }
void WSpectrumColumn::getArrayfloatV(unsigned int rownr, Array<float>* dataPtr) {
  *dataPtr = float(rownr);
}

FlagCatColumn::~FlagCatColumn() {}
bool FlagCatColumn::isShapeDefined(unsigned int) { return false; }
IPosition FlagCatColumn::shape(unsigned int) {
  throw DataManError("LofarStMan: no data in column FLAG_CATEGORY");
}

LofarStMan::LofarStMan(const String& dataManName) : DataManager(), itsDataManName(dataManName) {}

LofarStMan::LofarStMan(const String& dataManName, const Record&)
    : DataManager(), itsDataManName(dataManName) {}

LofarStMan::LofarStMan(const LofarStMan& that)
    : DataManager(), itsDataManName(that.itsDataManName) {}

LofarStMan::~LofarStMan() {
  for (unsigned int i = 0; i < ncolumn(); i++) {
    delete itsColumns[i];
  }
}

DataManager* LofarStMan::clone() const { return new LofarStMan(*this); }

String LofarStMan::dataManagerType() const { return "LofarStMan"; }

String LofarStMan::dataManagerName() const { return itsDataManName; }

Record LofarStMan::dataManagerSpec() const { return Record(); }

DataManagerColumn* LofarStMan::makeScalarColumn(const String& name, int dtype, const String&) {
  LofarColumn* col;
  if (name == "TIME" || name == "TIME_CENTROID") {
    col = new TimeColumn(this, dtype);
  } else if (name == "ANTENNA1") {
    col = new Ant1Column(this, dtype);
  } else if (name == "ANTENNA2") {
    col = new Ant2Column(this, dtype);
  } else if (name == "INTERVAL" || name == "EXPOSURE") {
    col = new IntervalColumn(this, dtype);
  } else if (name == "FLAG_ROW") {
    col = new FalseColumn(this, dtype);
  } else {
    col = new ZeroColumn(this, dtype);
  }
  itsColumns.push_back(col);
  return col;
}

DataManagerColumn* LofarStMan::makeDirArrColumn(const String& name, int dataType,
                                                const String& dataTypeId) {
  return makeIndArrColumn(name, dataType, dataTypeId);
}

DataManagerColumn* LofarStMan::makeIndArrColumn(const String& name, int dtype, const String&) {
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
    throw DataManError(name + " is unknown column for LofarStMan");
  }
  itsColumns.push_back(col);
  return col;
}

DataManager* LofarStMan::makeObject(const String& group, const Record& spec) {
  // This function is called when reading a table back.
  return new LofarStMan(group, spec);
}

void LofarStMan::registerClass() { DataManager::registerCtor("LofarStMan", makeObject); }

bool LofarStMan::isRegular() const { return false; }
bool LofarStMan::canAddRow() const { return false; }
bool LofarStMan::canRemoveRow() const { return false; }
bool LofarStMan::canAddColumn() const { return true; }
bool LofarStMan::canRemoveColumn() const { return true; }

void LofarStMan::addRow(unsigned int) { throw DataManError("LofarStMan cannot add rows"); }
void LofarStMan::removeRow(unsigned int) { throw DataManError("LofarStMan cannot remove rows"); }
void LofarStMan::addColumn(DataManagerColumn*) {}
void LofarStMan::removeColumn(DataManagerColumn*) {}

bool LofarStMan::flush(AipsIO&, bool) { return false; }

void LofarStMan::create(unsigned int) {}

void LofarStMan::open(unsigned int, AipsIO&) {
  throw DataManError("LofarStMan::open should never be called");
}
unsigned int LofarStMan::open1(unsigned int, AipsIO&) { return getNRow(); }

void LofarStMan::prepare() {}

void LofarStMan::resync(unsigned int) {
  throw DataManError("LofarStMan::resync should never be called");
}
unsigned int LofarStMan::resync1(unsigned int) { return getNRow(); }

void LofarStMan::reopenRW() {}

void LofarStMan::deleteManager() {}

}  // namespace casacore

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

void createTable() {
  // Build the table description.
  // Add all mandatory columns of the MS main table.
  TableDesc td("", "1", TableDesc::Scratch);
  td.comment() = "A test of class Table";
  td.addColumn(ScalarColumnDesc<double>("TIME"));
  td.addColumn(ScalarColumnDesc<int>("ANTENNA1"));
  td.addColumn(ScalarColumnDesc<int>("ANTENNA2"));
  td.addColumn(ScalarColumnDesc<int>("FEED1"));
  td.addColumn(ScalarColumnDesc<int>("FEED2"));
  td.addColumn(ScalarColumnDesc<int>("DATA_DESC_ID"));
  td.addColumn(ScalarColumnDesc<int>("PROCESSOR_ID"));
  td.addColumn(ScalarColumnDesc<int>("FIELD_ID"));
  td.addColumn(ScalarColumnDesc<int>("ARRAY_ID"));
  td.addColumn(ScalarColumnDesc<int>("OBSERVATION_ID"));
  td.addColumn(ScalarColumnDesc<int>("STATE_ID"));
  td.addColumn(ScalarColumnDesc<int>("SCAN_NUMBER"));
  td.addColumn(ScalarColumnDesc<double>("INTERVAL"));
  td.addColumn(ScalarColumnDesc<double>("EXPOSURE"));
  td.addColumn(ScalarColumnDesc<double>("TIME_CENTROID"));
  td.addColumn(ScalarColumnDesc<bool>("FLAG_ROW"));
  td.addColumn(ArrayColumnDesc<double>("UVW", IPosition(1, 3), ColumnDesc::Direct));
  td.addColumn(ArrayColumnDesc<Complex>("DATA"));
  td.addColumn(ArrayColumnDesc<float>("SIGMA"));
  td.addColumn(ArrayColumnDesc<float>("WEIGHT"));
  td.addColumn(ArrayColumnDesc<float>("WEIGHT_SPECTRUM"));
  td.addColumn(ArrayColumnDesc<bool>("FLAG"));
  td.addColumn(ArrayColumnDesc<bool>("FLAG_CATEGORY"));
  // Now create a new table from the description.
  SetupNewTable newtab("tLofarStMan_tmp.data", td, Table::New);
  // Create the storage manager and bind all columns to it.
  LofarStMan sm1;
  newtab.bindAll(sm1);
  // Finally create the table. The destructor writes it.
  Table tab(newtab);
}

// maxWeight tells maximum weight before it wraps
// (when nbytesPerSample is small).
void readTable() {
  // Open the table and check if #rows is as expected.
  Table tab("tLofarStMan_tmp.data");
  unsigned int nrow = tab.nrow();
  unsigned int nbasel = nant * nant;
  AlwaysAssertExit(ntime * nbasel == nrow);
  AlwaysAssertExit(!tab.canAddRow());
  AlwaysAssertExit(!tab.canRemoveRow());
  AlwaysAssertExit(tab.canRemoveColumn(Vector<String>(1, "DATA")));
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
  ScalarColumn<int> ant1Col(tab, "ANTENNA1");
  ScalarColumn<int> ant2Col(tab, "ANTENNA2");
  ScalarColumn<int> feed1Col(tab, "FEED1");
  ScalarColumn<int> feed2Col(tab, "FEED2");
  ScalarColumn<int> ddidCol(tab, "DATA_DESC_ID");
  ScalarColumn<int> pridCol(tab, "PROCESSOR_ID");
  ScalarColumn<int> fldidCol(tab, "FIELD_ID");
  ScalarColumn<int> arridCol(tab, "ARRAY_ID");
  ScalarColumn<int> obsidCol(tab, "OBSERVATION_ID");
  ScalarColumn<int> stidCol(tab, "STATE_ID");
  ScalarColumn<int> scnrCol(tab, "SCAN_NUMBER");
  ScalarColumn<bool> flagrowCol(tab, "FLAG_ROW");
  // Create and initialize expected data and weight.
  Array<Complex> dataExp(IPosition(2, npol, nchan));
  indgen(dataExp, Complex(0, 0.5));
  Array<float> weightExp(IPosition(2, 1, nchan), 0.f);
  // Loop through all rows in the table and check the data.
  unsigned int row = 0;
  for (unsigned int i = 0; i < ntime; ++i) {
    for (unsigned int j = 0; j < nant; ++j) {
      for (unsigned int k = 0; k < nant; ++k) {
        // Contents must be present except for FLAG_CATEGORY.
        AlwaysAssertExit(dataCol.isDefined(row));
        AlwaysAssertExit(weightCol.isDefined(row));
        AlwaysAssertExit(wspecCol.isDefined(row));
        AlwaysAssertExit(sigmaCol.isDefined(row));
        AlwaysAssertExit(flagCol.isDefined(row));
        AlwaysAssertExit(!flagcatCol.isDefined(row));
        // Check data, weight, sigma, weight_spectrum, flag
        AlwaysAssertExit(allNear(dataCol(row), dataExp, 1e-7));
        AlwaysAssertExit(weightCol.shape(row) == IPosition(1, npol));
        AlwaysAssertExit(allEQ(weightCol(row), float(1)));
        AlwaysAssertExit(sigmaCol.shape(row) == IPosition(1, npol));
        AlwaysAssertExit(allEQ(sigmaCol(row), float(1)));
        Array<float> weights = wspecCol(row);
        AlwaysAssertExit(weights.shape() == IPosition(2, npol, nchan));
        Array<bool> flagExp(weights.shape(), false);
        flagExp(IPosition(2, row % npol, row % nchan)) = true;
        AlwaysAssertExit(allEQ(flagCol(row), flagExp));
        // Check ANTENNA1 and ANTENNA2
        AlwaysAssertExit(ant1Col(row) == int(j));
        AlwaysAssertExit(ant2Col(row) == int(k));
        dataExp += Complex(1, 1);
        weightExp += float(1);
        ++row;
      }
    }
  }
  // Check values in TIME column.
  const double interval = 2;
  Vector<double> times = timeCol.getColumn();
  AlwaysAssertExit(times.size() == nrow);
  row = 0;
  double startTime = 1;
  for (unsigned int i = 0; i < ntime; ++i) {
    for (unsigned int j = 0; j < nbasel; ++j) {
      AlwaysAssertExit(near(times[row], startTime));
      ++row;
    }
    startTime += interval;
  }
  // Check the other columns.
  AlwaysAssertExit(allNear(centCol.getColumn(), times, 1e-13));
  AlwaysAssertExit(allNear(intvCol.getColumn(), interval, 1e-13));
  AlwaysAssertExit(allNear(expoCol.getColumn(), interval, 1e-13));
  AlwaysAssertExit(allEQ(feed1Col.getColumn(), 0));
  AlwaysAssertExit(allEQ(feed2Col.getColumn(), 0));
  AlwaysAssertExit(allEQ(ddidCol.getColumn(), 0));
  AlwaysAssertExit(allEQ(pridCol.getColumn(), 0));
  AlwaysAssertExit(allEQ(fldidCol.getColumn(), 0));
  AlwaysAssertExit(allEQ(arridCol.getColumn(), 0));
  AlwaysAssertExit(allEQ(obsidCol.getColumn(), 0));
  AlwaysAssertExit(allEQ(stidCol.getColumn(), 0));
  AlwaysAssertExit(allEQ(scnrCol.getColumn(), 0));
  AlwaysAssertExit(allEQ(flagrowCol.getColumn(), false));
  // Check the UVW coordinates.
  Array<double> uvwExp(IPosition(1, 3));
  indgen(uvwExp, 0., 0.03);
  for (unsigned int i = 0; i < nrow; ++i) {
    AlwaysAssertExit(allNear(uvwCol(i), uvwExp, 1e-13));
    uvwExp += 0.1;
  }
  // Check if getColumnCells works.
  RefRows rownrs(0, 2, 1);
  Slicer slicer(IPosition(2, 0, 0), IPosition(2, 1, 1));
  Array<float> wg = wspecCol.getColumnCells(rownrs);
  Array<float> wgs = wspecCol.getColumnCells(rownrs, slicer);
  cout << wspecCol(0).shape() << ' ' << wg.shape() << ' ' << wgs.shape() << endl;
}

void updateTable() {
  // Open the table for write.
  Table tab("tLofarStMan_tmp.data", Table::Update);
  // Create object for DATA column.
  ArrayColumn<Complex> dataCol(tab, "DATA");
  // Check we can write the column, but not change the shape.
  AlwaysAssertExit(tab.isColumnWritable("DATA"));
  AlwaysAssertExit(!dataCol.canChangeShape());
  // Create and initialize data.
  Array<Complex> data(IPosition(2, npol, nchan));
  // Write the data (which only writes a message).
  dataCol.put(0, data);
}

void copyTable() {
  Table tab("tLofarStMan_tmp.data");
  // Deep copy the table.
  tab.deepCopy("tLofarStMan_tmp.datcp", Table::New, true);
}

int main() {
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
  return 0;  // exit with success status
}
