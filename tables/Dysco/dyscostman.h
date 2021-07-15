#ifndef DYSCO_STORAGE_MANAGER_H
#define DYSCO_STORAGE_MANAGER_H

#include <casacore/tables/DataMan/DataManager.h>

#include <casacore/casa/Containers/Record.h>

#include <fstream>
#include <vector>

#include "dyscodistribution.h"
#include "dysconormalization.h"
#include "threadgroup.h"
#include "uvector.h"

/**
 * @file
 * Contains DyscoStMan and its global register function
 * register_dyscostman().
 *
 * @defgroup Globals Global functions
 * Contains the register_dyscostman() function.
 */

#ifndef DOXYGEN_SHOULD_SKIP_THIS
extern "C" {
#endif
void register_dyscostman();
#ifndef DOXYGEN_SHOULD_SKIP_THIS
}
#endif

/**
 * @author André Offringa
 */
namespace dyscostman {

class DyscoStManColumn;

/**
 * The main class for the Dysco storage manager.
 */
class DyscoStMan : public casacore::DataManager {
public:
  /**
   * Convience constructor to create a new storage manager with some settings
   * without having to fill a 'spec' Record. The storage manager will be
   * initialized to AF normalization with a truncated Gaussian distribution for
   * the quantization, and a truncation of sigma = 2.5. To change the settings,
   * use one of the Set...Distribution() methods and SetNormalization().
   * @param databitRate The number of bits per float used for visibilities.
   * @param weightBitRate The number of bits per float used for the weight
   * column.
   * @param name Storage manager name.
   */
  DyscoStMan(unsigned dataBitRate, unsigned weightBitRate,
             const casacore::String &name = "DyscoStMan");

  /**
   * Initialize the storage manager to use a Gaussian distribution for the
   * quantization. This method should only be called directly after creating
   * DyscoStMan, before adding columns, and reading/writing data.
   *
   * In tests with MWA and LOFAR data, the Gaussian distribution showed lesser
   * compression accuracy compared to the truncated Gaussian and uniform
   * distributions.
   * @see SetUniformDistribution(), SetTruncatedGaussianDistribution()
   */
  void SetGaussianDistribution() { _distribution = GaussianDistribution; }

  /**
   * Initialize the storage manager to use a Uniform distribution for the
   * quantization (i.e., use a linear quantizer). This method should only be
   * called directly after creating DyscoStMan, before adding columns, and
   * reading/writing data.
   *
   * In tests with MWA and LOFAR data, the Uniform distribution showed very good
   * results, only the truncated Gaussian distribution showed better results for
   * some cases.
   */
  void SetUniformDistribution() { _distribution = UniformDistribution; }

  /**
   * Initialize the storage manager to use a Student T distribution for the
   * quantization (i.e., use a linear quantizer). This method should only be
   * called directly after creating DyscoStMan, before adding columns, and
   * reading/writing data.
   *
   * The Student T distribution performed not very well on test sets, and was
   * mainly added for testing.
   */
  void SetStudentsTDistribution(double nu) {
    _distribution = StudentsTDistribution;
    _studentTNu = nu;
  }

  /**
   * Initialize the storage manager to use a Uniform distribution for the
   * quantization (i.e., use a linear quantizer). This method should only be
   * called directly after creating DyscoStMan, before adding columns, and
   * reading/writing data.
   *
   * In tests with MWA and LOFAR data, the truncated Gaussian distribution with
   * a sigma of 1.5 to 2.5 is the recommended distribution.
   * @param truncationSigma At which point the distribution is truncated. Good
   * values are 1.5 to 2.5.
   */
  void SetTruncatedGaussianDistribution(double truncationSigma) {
    _distribution = TruncatedGaussianDistribution;
    _distributionTruncation = truncationSigma;
  }

  /**
   * Set the type of normalization.
   * This method should only be called directly after creating DyscoStMan,
   * before adding columns, and reading/writing data.
   */
  void SetNormalization(Normalization normalization) {
    _normalization = normalization;
  }

  void SetStaticSeed(bool staticSeed) { _staticSeed = staticSeed; }

  /**
   * This constructor is called by Casa when it needs to create a DyscoStMan.
   * Casa will call makeObject() that will call this constructor.
   * When it loads an DyscoStMan for an existing MS, the "spec" parameter
   * will be empty, thus the class should initialize its properties
   * by reading them from the file.
   * The @p spec is used to make a new storage manager with specs similar to
   * another one.
   * @param name Name of this storage manager.
   * @param spec Specs to initialize this class with.
   */
  DyscoStMan(const casacore::String &name, const casacore::Record &spec);

  /**
   * Copy constructor that initializes a storage manager with similar specs.
   * The columns are not copied: the new manager will be empty.
   */
  DyscoStMan(const DyscoStMan &source);

  /** Destructor. */
  ~DyscoStMan();

  /** Assignment -- new dyscostman takes the settings of the source (but not the
   * columns and/or data).
   * @param source Source manager.
   */
  DyscoStMan &operator=(const DyscoStMan &source) = delete;

  /** Polymorphical copy constructor, equal to DyscoStMan(const DyscoStMan&).
   * @returns Empty manager with specs as the source.
   */
  virtual casacore::DataManager *clone() const final override {
    return new DyscoStMan(*this);
  }

  /** Type of manager
   * @returns "DyscoStMan". */
  virtual casacore::String dataManagerType() const final override {
    return "DyscoStMan";
  }

  /** Returns the name of this manager as specified during construction. */
  virtual casacore::String dataManagerName() const final override {
    return _name;
  }

  /** Get manager specifications. Includes method settings, etc. Can be used
   * to make a second storage manager with
   * @returns Record containing data manager specifications.
   */
  virtual casacore::Record dataManagerSpec() const final override;

  /**
   * Get the number of rows in the measurement set.
   * @returns Number of rows in the measurement set.
   */
  uint getNRow() const { return _nRow; }

  /**
   * Whether rows can be added.
   * @returns @c true
   */
  virtual casacore::Bool canAddRow() const final override { return true; }

  /**
   * Whether rows can be removed.
   * @returns @c true (but only rows at the end can actually be removed)
   */
  virtual casacore::Bool canRemoveRow() const final override { return true; }

  /**
   * Whether columns can be added.
   * @returns @c true (but restrictions apply; columns can only be added as long
   * as no writes have been performed on the set).
   */
  virtual casacore::Bool canAddColumn() const final override { return true; }

  /**
   * Whether columns can be removed.
   * @return @c true (but restrictions apply -- still to be checked)
   * @todo Describe restrictons
   */
  virtual casacore::Bool canRemoveColumn() const final override { return true; }

  /**
   * Create an object with given name and spec.
   * This methods gets registered in the DataManager "constructor" map.
   * The caller has to delete the object. New class will be
   * initialized via @ref DyscoStMan(const casacore::String& name, const
   * casacore::Record& spec).
   * @returns A DyscoStMan with given specs.
   */
  static casacore::DataManager *makeObject(const casacore::String &name,
                                           const casacore::Record &spec) {
    return new DyscoStMan(name, spec);
  }

  /**
   * This function makes the DyscoStMan known to casacore. The function
   * is necessary for loading the storage manager from a shared library. It
   * should have this specific name ("register_" + storage manager's name in
   * lowercase) to be able to be automatically called when the library is
   * loaded. That function will forward the
   * call here.
   */
  static void registerClass();

protected:
  /**
   * The number of rows that are actually stored in the file.
   * This method is synchronized (i.e., thread-safe).
   */
  uint64_t nBlocksInFile() const {
    std::lock_guard<std::mutex> lock(_mutex);
    return _nBlocksInFile;
  }

  /**
   * Number of rows in one "time-block", i.e. a sequence of rows that
   * belong to the same timestep, spw and field.
   * This value is only available after a first time block was written
   * (see areOffsetsInitialized()).
   * @returns Number of measurement set rows in one time block.
   */
  size_t nRowsInBlock() const { return _rowsPerBlock; }

  /**
   * Number of antennae used in a time block. This does not have to be equal
   * to the number of antennae stored in the measurement set.
   * This value is only available after a first time block was written
   * (see areOffsetsInitialized()).
   * @returns Number of antennae.
   */
  size_t nAntennae() const { return _antennaCount; }

  /**
   * Return index of block that contains the given measurement set row.
   * This can only be calculated after a first time block was written
   * (see areOffsetsInitialized()).
   * @param row A measurement set row.
   * @returns Block index.
   */
  size_t getBlockIndex(uint64_t row) const { return row / _rowsPerBlock; }

  /**
   * Return the offset of the row within the block.
   * This can only be calculated after a first time block was written
   * (see areOffsetsInitialized()).
   * @see getBlockIndex().
   * @param row A measurement set row.
   * @returns offset of row within block.
   */
  size_t getRowWithinBlock(uint64_t row) const { return row % _rowsPerBlock; }

  /**
   * Calculate first measurement set row index of a given block index.
   * @param block A block index
   * @returns First measurement set row index of given block.
   */
  uint64_t getRowIndex(size_t block) const {
    return uint64_t(block) * uint64_t(_rowsPerBlock);
  }

  /**
   * This method returns @c true when the number of rows per block and the
   * number of antennae per block are known. This is only the case once the
   * first time- block was written to the file.
   * @returns True when the nr of rows per block and antennae are available.
   */
  bool areOffsetsInitialized() const { return _rowsPerBlock != 0; }

  /**
   * To be called by a column once it determines rowsPerBlock and antennaCount.
   * @param rowsPerBlock Number of measurement set rows in one time block.
   * @param antennaCount Highest antenna index+1 used in a time block.
   */
  void initializeRowsPerBlock(size_t rowsPerBlock, size_t antennaCount,
                              bool writeToHeader);

private:
  friend class DyscoStManColumn;

  const static unsigned short VERSION_MAJOR, VERSION_MINOR;

  void readCompressedData(size_t blockIndex, const DyscoStManColumn *column,
                          unsigned char *dest, size_t size);

  void writeCompressedData(size_t blockIndex, const DyscoStManColumn *column,
                           const unsigned char *data, size_t size);

  void readHeader();

  void writeHeader();

  void makeEmpty();

  void setFromSpec(const casacore::Record &spec);

  size_t getFileOffset(size_t blockIndex) const {
    return _blockSize * blockIndex + _headerSize;
  }

  // Flush and optionally fsync the data.
  // The AipsIO stream represents the main table file and can be
  // used by virtual column engines to store SMALL amounts of data.
  virtual casacore::Bool flush(casacore::AipsIO &,
                               casacore::Bool doFsync) final override;

  // Let the storage manager create files as needed for a new table.
  // This allows a column with an indirect array to create its file.
  virtual void create(casacore::uInt nRow) final override;

  // Open the storage manager file for an existing table.
  // Return the number of rows in the data file.
  virtual void open(casacore::uInt nRow, casacore::AipsIO &) final override;

  // Create a column in the storage manager on behalf of a table column.
  // The caller will NOT delete the newly created object.
  // Create a scalar column.
  virtual casacore::DataManagerColumn *
  makeScalarColumn(const casacore::String &name, int dataType,
                   const casacore::String &dataTypeID) final override;

  // Create a direct array column.
  virtual casacore::DataManagerColumn *
  makeDirArrColumn(const casacore::String &name, int dataType,
                   const casacore::String &dataTypeID) final override;

  // Create an indirect array column.
  virtual casacore::DataManagerColumn *
  makeIndArrColumn(const casacore::String &name, int dataType,
                   const casacore::String &dataTypeID) final override;

  virtual void resync(casacore::uInt nRow) final override;

  virtual void deleteManager() final override;

  // Prepare the columns, let the data manager initialize itself further.
  // Prepare is called after create/open has been called for all
  // columns. In this way one can be sure that referenced columns
  // are read back and partly initialized.
  virtual void prepare() final override;

  // Reopen the storage manager files for read/write.
  virtual void reopenRW() final override;

  // Add rows to the storage manager.
  virtual void addRow(casacore::uInt nrrow) final override;

  // Delete a row from all columns.
  virtual void removeRow(casacore::uInt rowNr) final override;

  // Do the final addition of a column.
  virtual void addColumn(casacore::DataManagerColumn *) final override;

  // Remove a column from the data file.
  virtual void removeColumn(casacore::DataManagerColumn *) final override;

  uint64_t _nRow;
  uint64_t _nBlocksInFile;
  uint32_t _rowsPerBlock;
  uint32_t _antennaCount;
  uint32_t _blockSize;

  unsigned _headerSize;
  mutable std::mutex _mutex;
  std::unique_ptr<std::fstream> _fStream;

  std::string _name;
  unsigned _dataBitCount;
  unsigned _weightBitCount;
  DyscoDistribution _distribution;
  Normalization _normalization;
  double _studentTNu, _distributionTruncation;
  bool _staticSeed;

  std::vector<std::unique_ptr<DyscoStManColumn>> _columns;
};

} // namespace dyscostman

#endif
