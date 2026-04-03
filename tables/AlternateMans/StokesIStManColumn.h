#ifndef CASACORE_STOKES_I_ST_MAN_COLUMN_H_
#define CASACORE_STOKES_I_ST_MAN_COLUMN_H_

#include <casacore/tables/DataMan/StManColumn.h>

#include <casacore/casa/Arrays/Array.h>
#include <casacore/casa/Arrays/IPosition.h>

#include "BufferedColumnarFile.h"
#include "MorphingArray.h"
#include "StokesIConversions.h"

#include <optional>

namespace casacore {

class StokesIStMan;

/**
 * Base class for columns of the StokesIStMan.
 * @author André Offringa
 */
class StokesIStManColumn final : public casacore::StManColumn {
 public:
  /**
   * Constructor, to be overloaded by subclass.
   * @param parent The parent stman to which this column belongs.
   * @param dtype The column's type as defined by Casacore.
   */
  explicit StokesIStManColumn(StokesIStMan &parent, BufferedColumnarFile &file,
                              casacore::DataType dtype)
      : casacore::StManColumn(dtype), parent_(parent), file_(file) {}

  /**
   * Whether this column is writable
   * @returns @c true
   */
  casacore::Bool isWritable() const final { return true; }

  /** Set the dimensions of values in this column. */
  void setShapeColumn(const casacore::IPosition &shape) final {
    if (shape.size() != 2) {
      throw std::runtime_error("StokesIStMan is used for a column with " +
                               std::to_string(shape.size()) +
                               " dimensions, but it can only be used for "
                               "columns with exactly 2 dimensions");
    }
    shape_ = shape;
    updateStride();
  }

  /** Get the dimensions of the values in a particular row.
   * @param rownr The row to get the shape for. */
  casacore::IPosition shape(casacore::uInt) final { return shape_; }
  casacore::IPosition shape(casacore::rownr_t) final { return shape_; }
  const casacore::IPosition &shape() const { return shape_; }

  /**
   * Read the values for a particular row.
   * @param rowNr The row number to get the values for.
   * @param dataPtr The array of values.
   */
  void getArrayComplexV(casacore::uInt rowNr,
                        casacore::Array<casacore::Complex> *dataPtr) final {
    getArrayGeneric(rowNr, dataPtr);
  }

  void getArrayfloatV(casacore::uInt rowNr,
                      casacore::Array<float> *dataPtr) final {
    getArrayGeneric(rowNr, dataPtr);
  }

  void getArrayBoolV(casacore::uInt rowNr,
                     casacore::Array<casacore::Bool> *dataPtr) final {
    getArrayGeneric(rowNr, dataPtr);
  }

  /**
   * Write values into a particular row.
   * @param rowNr The row number to write the values to.
   * @param dataPtr The data pointer.
   */
  void putArrayComplexV(
      casacore::uInt rowNr,
      const casacore::Array<casacore::Complex> *dataPtr) final {
    putArrayGeneric(rowNr, dataPtr);
  }
  void putArrayDComplexV(
      casacore::uInt rowNr,
      const casacore::Array<casacore::DComplex> *dataPtr) final {
    putArrayGeneric(rowNr, dataPtr);
  }
  void putArraydoubleV(casacore::uInt rowNr,
                       const casacore::Array<double> *dataPtr) final {
    putArrayGeneric(rowNr, dataPtr);
  }
  void putArrayfloatV(casacore::uInt rowNr,
                      const casacore::Array<float> *dataPtr) final {
    putArrayGeneric(rowNr, dataPtr);
  }
  void putArrayBoolV(casacore::uInt rowNr,
                     const casacore::Array<casacore::Bool> *dataPtr) final {
    putArrayGeneric(rowNr, dataPtr);
  }

  void setOffset(uint64_t column_offset) { column_offset_ = column_offset; }
  uint64_t getStoredSizeInBytes() const {
    if (dtype() == casacore::TpBool) {
      return (shape_[1] + 7) / 8;
    } else {
      const uint64_t type_size = SizeOfType(dtype());
      return shape_[1] * type_size;
    }
  }

 private:
  StokesIStManColumn(const StokesIStManColumn &source) = delete;
  void operator=(const StokesIStManColumn &source) = delete;

  template <typename T>
  void getArrayGeneric(casacore::uInt rowNr, casacore::Array<T> *dataPtr) {
    bool ownership;
    T *storage = dataPtr->getStorage(ownership);
    const size_t n_values = shape_[1];
    file_.Read(rowNr, column_offset_, storage, n_values);
    ExpandFromStokesI(storage, n_values);
    dataPtr->putStorage(storage, ownership);
  }

  template <typename T>
  void putArrayGeneric(casacore::uInt rowNr,
                       const casacore::Array<T> *dataPtr) {
    bool ownership;
    const T *storage = dataPtr->getStorage(ownership);
    const size_t n_values = shape_[1];
    buffer_.Resize<T>(n_values);
    const T *buffer = TransformToStokesI(storage, buffer_.Data<T>(), n_values);
    file_.Write(rowNr, column_offset_, buffer, n_values);
    dataPtr->freeStorage(storage, ownership);
  }
  void updateStride();

  StokesIStMan &parent_;
  BufferedColumnarFile &file_;
  IPosition shape_;
  uint64_t column_offset_;
  MorphingArray buffer_;
};
}  // namespace casacore

#include "StokesIStMan.h"

namespace casacore {

inline void StokesIStManColumn::updateStride() {
  parent_.CalculateAndUpdateStride();
}

}  // namespace casacore

#endif
