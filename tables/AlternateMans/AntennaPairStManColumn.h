#ifndef CASACORE_STOKES_I_ST_MAN_COLUMN_H_
#define CASACORE_STOKES_I_ST_MAN_COLUMN_H_

#include <casacore/tables/DataMan/StManColumn.h>
#include <casacore/tables/Tables/ScalarColumn.h>

#include <casacore/casa/Arrays/Array.h>
#include <casacore/casa/Arrays/IPosition.h>

#include "AntennaPairFile.h"

#include <optional>

namespace casacore {

class AntennaPairStManColumn final : public StManColumn {
 public:
  /**
   * Constructor, to be overloaded by subclass.
   * @param parent The parent stman to which this column belongs.
   * @param dtype The column's type as defined by Casacore.
   */
  explicit AntennaPairStManColumn(AntennaPairFile &file, bool second)
      : StManColumn(DataType::TpInt), file_(file), second_(second) {}

  /**
   * Whether this column is writable
   * @returns @c true
   */
  Bool isWritable() const final { return true; }

  void getInt(rownr_t row, Int *dataPtr) final {
    if (second_)
      *dataPtr = file_.ReadAntenna2(row);
    else
      *dataPtr = file_.ReadAntenna1(row);
  }

  /**
   * Write values into a particular row.
   * @param rowNr The row number to write the values to.
   * @param dataPtr The data pointer.
   */
  void putInt(rownr_t row, const Int *dataPtr) final {
    if (second_)
      file_.WriteAntenna2(row, *dataPtr);
    else
      file_.WriteAntenna1(row, *dataPtr);
  }

 private:
  AntennaPairStManColumn(const AntennaPairStManColumn &source) = delete;
  void operator=(const AntennaPairStManColumn &source) = delete;

  AntennaPairFile &file_;
  bool second_;
};
}  // namespace casacore

#endif
