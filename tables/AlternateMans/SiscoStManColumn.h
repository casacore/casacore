#ifndef CASACORE_SISCO_ST_MAN_COLUMN_H_
#define CASACORE_SISCO_ST_MAN_COLUMN_H_

#include <casacore/tables/DataMan/StManColumn.h>

#include <casacore/casa/Arrays/Array.h>
#include <casacore/casa/Arrays/IPosition.h>

#include <casacore/tables/Tables/ScalarColumn.h>

#include "SiscoReader.h"
#include "SiscoWriter.h"
#include "ShapesFileReader.h"
#include "ShapesFileWriter.h"

#include <filesystem>
#include <optional>

namespace casacore {

class SiscoStMan;

/**
 * Base class for columns of the StokesIStMan.
 * @author André Offringa
 */
class SiscoStManColumn final : public StManColumn {
 public:
  /**
   * Constructor, to be overloaded by subclass.
   * @param parent The parent stman to which this column belongs.
   * @param dtype The column's type as defined by Casacore.
   */
  explicit SiscoStManColumn(SiscoStMan &parent, DataType dtype)
      : StManColumn(dtype), parent_(parent) {
    if (dtype != casacore::TpComplex) {
      throw std::runtime_error(
          "Sisco storage manager column can only be used for a data column "
          "with single precision complex values");
    }
  }

  /**
   * Whether this column is writable
   * @returns @c true
   */
  bool isWritable() const final { return true; }

  bool canChangeShape() const final { return true; }

  void setShape(rownr_t, const IPosition &) final {
    // Shape is implied from the array; explicit setting of the shape is not
    // required.
  }
  void setShape(unsigned, const IPosition &) final {}

  bool isShapeDefined(rownr_t) final { return false; }
  bool isShapeDefined(unsigned) final { return false; }

  /** Set the dimensions of values in this column. */
  void setShapeColumn(const IPosition &shape) final {
    if (shape.size() != 2) {
      throw std::runtime_error(
          "Sisco storage manager is used for a column with " +
          std::to_string(shape.size()) +
          " dimensions, but it can only be used for "
          "columns with exactly 2 dimensions");
    }
    current_shape_ = shape;
  }

  /** Get the dimensions of the values in a particular row.
   * @param rownr The row to get the shape for. */
  IPosition shape(rownr_t row) final {
    if (writer_ && row >= current_row_) {
      return IPosition();
    } else {
      if (!reader_ || row < current_row_) {
        if (std::filesystem::exists(parent_.fileName()))
          OpenReader();
        else
          return IPosition();
      }
      while (current_row_ < row) {
        SkipRow();
      }
      return current_shape_;
    }
  }
  IPosition shape(unsigned row) final {
    return shape(static_cast<rownr_t>(row));
  }

  /**
   * Read the values for a particular row.
   * @param row The row number to get the values for.
   * @param dataPtr The array of values.
   */
  void getArrayV(rownr_t row, ArrayBase &dataPtr) final {
    Array<std::complex<float>> &array =
        static_cast<Array<std::complex<float>> &>(dataPtr);
    if (!reader_ || row < current_row_) {
      OpenReader();
    }
    while (current_row_ < row) {
      SkipRow();
    }

    const int field_id = field_id_column_(current_row_);
    const int data_desc_id = data_desc_id_column_(current_row_);
    const int antenna1 = antenna1_column_(current_row_);
    const int antenna2 = antenna2_column_(current_row_);
    const int n_polarizations = current_shape_[0];
    const size_t n_channels = current_shape_[1];

    bool ownership;
    Complex *storage = array.getStorage(ownership);
    buffer_.resize(n_channels);
    for (int polarization = 0; polarization != n_polarizations;
         ++polarization) {
      const size_t baseline_id = GetBaselineId(field_id, data_desc_id, antenna1,
                                               antenna2, polarization);
      reader_->Read(baseline_id, buffer_);
      for (size_t channel = 0; channel != n_channels; ++channel) {
        storage[channel * n_polarizations + polarization] = buffer_[channel];
      }
    }
    array.putStorage(storage, ownership);

    current_shape_ = shapes_reader_->Read();
    ++current_row_;
  }

  /**
   * Write values into a particular row.
   * @param row The row number to write the values to.
   * @param dataPtr The data pointer.
   */
  void putArrayV(rownr_t row, const ArrayBase &dataPtr) final {
    const Array<std::complex<float>> &array =
        static_cast<const Array<std::complex<float>> &>(dataPtr);
    if (!writer_ || row < current_row_) {
      OpenWriter();
    }
    while (current_row_ < row) {
      WriteEmptyRow();
    }

    const int field_id = field_id_column_(current_row_);
    const int data_desc_id = data_desc_id_column_(current_row_);
    const int antenna1 = antenna1_column_(current_row_);
    const int antenna2 = antenna2_column_(current_row_);
    const int n_polarizations = array.shape()[0];
    const size_t n_channels = array.shape()[1];

    bool ownership;
    const std::complex<float> *storage = array.getStorage(ownership);
    buffer_.resize(n_channels);
    for (int polarization = 0; polarization != n_polarizations;
         ++polarization) {
      const size_t baseline_id = GetBaselineId(field_id, data_desc_id, antenna1,
                                               antenna2, polarization);
      for (size_t channel = 0; channel != n_channels; ++channel) {
        buffer_[channel] = storage[channel * n_polarizations + polarization];
      }
      writer_->Write(baseline_id, buffer_);
    }
    array.freeStorage(storage, ownership);

    current_shape_ = array.shape();
    ++current_row_;
    shapes_writer_->Write(current_shape_);
  }

  void Prepare();

 private:
  SiscoStManColumn(const SiscoStManColumn &source) = delete;
  void operator=(const SiscoStManColumn &source) = delete;

  void Reset() {
    reader_.reset();
    shapes_reader_.reset();
    writer_.reset();
    shapes_writer_.reset();
  }

  void OpenWriter() {
    Reset();
    writer_.emplace(parent_.fileName(), parent_.PredictLevel(),
                    parent_.DeflateLevel());
    char header_buffer[kHeaderSize];
    std::fill_n(header_buffer, kHeaderSize, 0);
    std::copy_n(kMagic, kMagicSize, &header_buffer[0]);
    std::copy_n(reinterpret_cast<const char *>(&kVersionMajor), 2,
                &header_buffer[kMagicSize]);
    std::copy_n(reinterpret_cast<const char *>(&kVersionMinor), 2,
                &header_buffer[kMagicSize + 2]);
    std::span<const std::byte> header(
        reinterpret_cast<const std::byte *>(header_buffer), kHeaderSize);
    writer_->Open(header);

    shapes_writer_.emplace(parent_.fileName() + kShapesExtension);

    current_row_ = 0;
    current_shape_ = IPosition();
    baseline_ids_.clear();
    baseline_count_ = 0;
  }

  void OpenReader() {
    Reset();
    reader_.emplace(parent_.fileName());
    char header_buffer[kHeaderSize];
    std::span<std::byte> header(reinterpret_cast<std::byte *>(header_buffer),
                                kHeaderSize);
    reader_->Open(header);
    char magic_tag[kMagicSize];
    short version_major;
    short version_minor;
    std::copy_n(&header_buffer[0], kMagicSize, magic_tag);
    std::copy_n(&header_buffer[kMagicSize], 2,
                reinterpret_cast<char *>(&version_major));
    std::copy_n(&header_buffer[kMagicSize + 2], 2,
                reinterpret_cast<char *>(&version_minor));
    shapes_reader_.emplace(parent_.fileName() + kShapesExtension);

    current_row_ = 0;
    current_shape_ = shapes_reader_->Read();
    baseline_ids_.clear();
    baseline_count_ = 0;
  }

  size_t GetBaselineId(int field_id, int data_desc_id, int antenna1,
                       int antenna2, int polarization) {
    const std::array<int, 5> baseline{field_id, data_desc_id, antenna1,
                                      antenna2, polarization};
    std::map<std::array<int, 5>, size_t>::const_iterator iterator =
        baseline_ids_.find(baseline);
    if (iterator == baseline_ids_.end()) {
      iterator = baseline_ids_.emplace(baseline, baseline_count_).first;
      ++baseline_count_;
    }
    return iterator->second;
  }

  void WriteEmptyRow() {
    shapes_writer_->Write(IPosition());
    ++current_row_;
  }

  void SkipRow() {
    const int field_id = field_id_column_(current_row_);
    const int data_desc_id = data_desc_id_column_(current_row_);
    const int antenna1 = antenna1_column_(current_row_);
    const int antenna2 = antenna2_column_(current_row_);
    const int n_polarizations = current_shape_[0];
    const int n_channels = current_shape_[1];
    buffer_.resize(n_channels);
    for (int polarization = 0; polarization != n_polarizations;
         ++polarization) {
      const size_t baseline_id = GetBaselineId(field_id, data_desc_id, antenna1,
                                               antenna2, polarization);
      reader_->Read(baseline_id, buffer_);
    }
    current_shape_ = shapes_reader_->Read();
    ++current_row_;
  }

  static constexpr size_t kHeaderSize = 20;
  static constexpr char kMagic[] = "Sisco\0\0\0";
  static constexpr size_t kMagicSize = 8;
  static constexpr uint16_t kVersionMajor = 1;
  static constexpr uint16_t kVersionMinor = 0;
  static constexpr char kShapesExtension[] = "-shapes";

  ScalarColumn<int> field_id_column_;
  ScalarColumn<int> data_desc_id_column_;
  ScalarColumn<int> antenna1_column_;
  ScalarColumn<int> antenna2_column_;

  SiscoStMan &parent_;
  std::optional<sisco::SiscoWriter> writer_;
  std::optional<sisco::SiscoReader> reader_;
  std::optional<ShapesFileWriter> shapes_writer_;
  std::optional<ShapesFileReader> shapes_reader_;
  rownr_t current_row_ = 0;
  std::vector<std::complex<float>> buffer_;
  IPosition current_shape_;
  std::map<std::array<int, 5>, size_t> baseline_ids_;
  size_t baseline_count_;
};

}  // namespace casacore

#include "SiscoStMan.h"

namespace casacore {

void SiscoStManColumn::Prepare() {
  Table &table = parent_.table();
  field_id_column_ = ScalarColumn<int>(table, "FIELD_ID");
  data_desc_id_column_ = ScalarColumn<int>(table, "DATA_DESC_ID");
  antenna1_column_ = ScalarColumn<int>(table, "ANTENNA1");
  antenna2_column_ = ScalarColumn<int>(table, "ANTENNA2");
}

}  // namespace casacore

#endif
