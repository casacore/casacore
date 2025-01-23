#ifndef CASACORE_STOKES_I_ST_MAN_COLUMN_H_
#define CASACORE_STOKES_I_ST_MAN_COLUMN_H_

#include <casacore/tables/DataMan/StManColumn.h>

#include <casacore/casa/Arrays/Array.h>
#include <casacore/casa/Arrays/IPosition.h>

#include "BufferedColumnarFile.h"

#include <optional>

namespace casacore {

class StokesIStMan;

/**
 * Expands @p n values from single Stokes I values
 * to have 4 values, in place. This implies the array
 * should have place to store n*4 values.
 */
template <typename T>
void ExpandFromStokesI(T *data, size_t n) {
  for (size_t i = 0; i != n; ++i) {
    const size_t index = n - i - 1;
    const T value = data[index];
    data[index * 4] = value;
    data[index * 4 + 1] = T();
    data[index * 4 + 2] = T();
    data[index * 4 + 3] = value;
  }
}

/**
 * Calculates for every set of 4 input values the Stokes-I values by
 * doing out = 0.5 * (in_pp + in_qq), where pp/qq can be xx/yy or 
 * ll/rr. If pq or qp is non-zero, an exception is thrown. 
 * 
 * If type T is bool, then out = in_pp || in_qq.
 */
template <typename T>
inline T *TransformToStokesI(const T *input, char *buffer, size_t n) {
  for (size_t i = 0; i != n; ++i) {
    // Placement new is used, because the lifetime of type T needs
    // to be started.
    new (&buffer[i * sizeof(T)]) T((input[i * 4] + input[i * 4 + 3]) * T(0.5));
    if(input[i*4+1] != T(0.0) || input[i*4+2] != T(0.0))
      throw std::runtime_error(
          "Stokes-I stman cannot store data for which the 2nd and 3rd "
          "correlation are non-zero");
    // While we could also check whether pp == qq, this is a bit more
    // complicated because of rounding inaccuracies. The above check should
    // catch the most crucial misuse of the stman, so a pp == qq check is not
    // performed.
  }
  return reinterpret_cast<T *>(buffer);
}

template <>
inline bool *TransformToStokesI(const bool *input, char *buffer, size_t n) {
  for (size_t i = 0; i != n; ++i) {
    new (&buffer[i]) bool(input[i * 4] || input[i * 4 + 3]);
  }
  return reinterpret_cast<bool *>(buffer);
}

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
      : casacore::StManColumn(dtype), parent_(parent), file_(file) {
    // TODO handle size of boolean columns
  }

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
    buffer_.resize(n_values * sizeof(T));
    T *buffer = TransformToStokesI(storage, buffer_.data(), n_values);
    file_.Write(rowNr, column_offset_, buffer, n_values);
    dataPtr->freeStorage(storage, ownership);
  }
  void updateStride();

  StokesIStMan &parent_;
  BufferedColumnarFile &file_;
  IPosition shape_;
  uint64_t column_offset_;
  std::vector<char> buffer_;
};
}  // namespace casacore

#include "StokesIStMan.h"

namespace casacore {

inline void StokesIStManColumn::updateStride() {
  parent_.CalculateAndUpdateStride();
}

}  // namespace casacore

#endif
