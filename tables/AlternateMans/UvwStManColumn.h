#ifndef CASACORE_STOKES_I_ST_MAN_COLUMN_H_
#define CASACORE_STOKES_I_ST_MAN_COLUMN_H_

#include <casacore/tables/DataMan/StManColumn.h>
#include <casacore/tables/Tables/ScalarColumn.h>

#include <casacore/casa/Arrays/Array.h>
#include <casacore/casa/Arrays/IPosition.h>

#include "UvwFile.h"

#include <optional>

namespace casacore {

class UvwIStMan;

class UvwStManColumn final : public StManColumn {
 public:
  /**
   * Constructor, to be overloaded by subclass.
   * @param parent The parent stman to which this column belongs.
   * @param dtype The column's type as defined by Casacore.
   */
  explicit UvwStManColumn(UvwFile &file)
      : StManColumn(DataType::TpDouble), file_(file) {}

  /**
   * Whether this column is writable
   * @returns @c true
   */
  Bool isWritable() const final { return true; }

  /** Set the dimensions of values in this column. */
  void setShapeColumn(const IPosition &shape) final {
    if (shape.size() != 1 || shape[0] != 3) {
      throw std::runtime_error(
          "UvwStMan can only be used for array columns with 1 dimension of "
          "size 3");
    }
  }

  /** Get the dimensions of the values in a particular row.
   * @param rownr The row to get the shape for. */
  IPosition shape(uInt) final { return IPosition{3}; }
  IPosition shape(rownr_t) final { return IPosition{3}; }

  void getArrayV(rownr_t row, ArrayBase &dataPtr) final {
    Array<double> &array = static_cast<Array<double> &>(dataPtr);
    bool ownership;
    double *storage = array.getStorage(ownership);
    const int antenna1 = antenna1_column_(row);
    const int antenna2 = antenna2_column_(row);
    file_.ReadUvw(row, antenna1, antenna2, storage);
    array.putStorage(storage, ownership);
  }

  /**
   * Write values into a particular row.
   * @param rowNr The row number to write the values to.
   * @param dataPtr The data pointer.
   */
  void putArrayV(rownr_t row, const ArrayBase &dataPtr) final {
    const Array<double> &array = static_cast<const Array<double> &>(dataPtr);
    bool ownership;
    const double *storage = array.getStorage(ownership);
    const int antenna1 = antenna1_column_(row);
    const int antenna2 = antenna2_column_(row);
    file_.WriteUvw(row, antenna1, antenna2, storage);
    array.freeStorage(storage, ownership);
  }

  void Prepare(Table &table) {
    antenna1_column_ = ScalarColumn<int>(table, "ANTENNA1");
    antenna2_column_ = ScalarColumn<int>(table, "ANTENNA2");
  }

 private:
  UvwStManColumn(const UvwStManColumn &source) = delete;
  void operator=(const UvwStManColumn &source) = delete;

  UvwFile &file_;
  ScalarColumn<int> antenna1_column_;
  ScalarColumn<int> antenna2_column_;
};
}  // namespace casacore

#endif
