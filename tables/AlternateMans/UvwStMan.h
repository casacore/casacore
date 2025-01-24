#ifndef CASACORE_UVW_STORAGE_MANAGER_H_
#define CASACORE_UVW_STORAGE_MANAGER_H_

#include <casacore/tables/DataMan/DataManager.h>

#include <casacore/casa/Containers/Record.h>

#include "UvwFile.h"

#include <memory>
#include <stdexcept>

namespace casacore {

class UvwStManColumn;

/**
 * A storage manager that saves the UVW with (lossless) compression.
 */
class UvwStMan final : public DataManager {
 public:
  UvwStMan(const casacore::String &, const casacore::Record &);

  /**
   * The columns are not copied: the new manager will be empty.
   */
  UvwStMan(const UvwStMan &source);

  ~UvwStMan() noexcept;

  UvwStMan &operator=(const UvwStMan &source) = delete;

  casacore::DataManager *clone() const final { return new UvwStMan(*this); }

  static casacore::DataManager *makeObject(const casacore::String &name,
                                           const casacore::Record &spec) {
    return new UvwStMan(name, spec);
  }

  casacore::String dataManagerType() const final { return "UvwStMan"; }

  casacore::Record dataManagerSpec() const final { return Record(); }

 private:
  casacore::Bool flush(casacore::AipsIO &, casacore::Bool) final {
    return false;
  }

  void create64(casacore::rownr_t nRow) final;

  casacore::rownr_t open64(casacore::rownr_t nRow, casacore::AipsIO &) final;

  casacore::DataManagerColumn *makeScalarColumn(
      const casacore::String &, int, const casacore::String &) final {
    throw std::runtime_error(
        "makeScalarColumn() called on a UvwStMan. UvwStMan can only make array "
        "columns");
  }

  casacore::DataManagerColumn *makeDirArrColumn(
      const casacore::String &name, int dataType,
      const casacore::String &dataTypeID) final;

  casacore::DataManagerColumn *makeIndArrColumn(
      const casacore::String &, int, const casacore::String &) final {
    throw std::runtime_error(
        "makeIndArrColumn() called on a UvwStMan. UvwStMan can only make "
        "direct columns");
  }

  casacore::rownr_t resync64(casacore::rownr_t nRow) final { return nRow; }

  void deleteManager() final;

  // Prepare the columns, let the data manager initialize itself further.
  // Prepare is called after create/open has been called for all
  // columns. In this way one can be sure that referenced columns
  // are read back and partly initialized.
  void prepare() final {}

  // Reopen the storage manager files for read/write.
  void reopenRW() final {}

  // Add rows to the storage manager.
  void addRow64(casacore::rownr_t nrrow) final;

  // Delete a row from all columns.
  void removeRow64(casacore::rownr_t row_nr) final;

  // Do the final addition of a column.
  void addColumn(casacore::DataManagerColumn *) final;

  // Remove a column from the data file.
  void removeColumn(casacore::DataManagerColumn *) final;

  std::string name_;
  std::unique_ptr<UvwStManColumn> column_;
  UvwFile file_;
};

}  // namespace casacore

#endif
