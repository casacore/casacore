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
  UvwStMan(const String &, const Record &);

  /**
   * The columns are not copied: the new manager will be empty.
   */
  UvwStMan(const UvwStMan &source);

  ~UvwStMan() noexcept;

  UvwStMan &operator=(const UvwStMan &source) = delete;

  DataManager *clone() const final { return new UvwStMan(*this); }

  static DataManager *makeObject(const String &name, const Record &spec) {
    return new UvwStMan(name, spec);
  }

  String dataManagerType() const final { return "UvwStMan"; }

  Record dataManagerSpec() const final { return Record(); }

 private:
  Bool flush(AipsIO &, Bool) final { return false; }

  void create64(rownr_t nRow) final;

  rownr_t open64(rownr_t nRow, AipsIO &) final;

  DataManagerColumn *makeScalarColumn(const String &, int,
                                      const String &) final {
    throw std::runtime_error(
        "makeScalarColumn() called on a UvwStMan. UvwStMan can only make array "
        "columns");
  }

  DataManagerColumn *makeDirArrColumn(const String &name, int dataType,
                                      const String &dataTypeID) final;

  DataManagerColumn *makeIndArrColumn(const String &, int,
                                      const String &) final {
    throw std::runtime_error(
        "makeIndArrColumn() called on a UvwStMan. UvwStMan can only make "
        "direct columns");
  }

  rownr_t resync64(rownr_t nRow) final { return nRow; }

  void deleteManager() final;

  // Prepare is called after create/open has been called for all
  // columns. In this way one can be sure that referenced columns
  // are read back and partly initialized.
  void prepare() final;

  // Reopen the storage manager files for read/write.
  void reopenRW() final {}

  // Add rows to the storage manager.
  void addRow64(rownr_t nrrow) final;

  // Delete a row from all columns.
  void removeRow64(rownr_t row_nr) final;

  // Do the final addition of a column.
  void addColumn(DataManagerColumn *) final;

  // Remove a column from the data file.
  void removeColumn(DataManagerColumn *) final;

  std::string name_;
  std::unique_ptr<UvwStManColumn> column_;
  UvwFile file_;
};

}  // namespace casacore

#endif
