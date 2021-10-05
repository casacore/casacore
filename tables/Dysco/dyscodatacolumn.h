#ifndef DYSCO_DATA_COLUMN_H
#define DYSCO_DATA_COLUMN_H

#include "threadeddyscocolumn.h"

#include "stochasticencoder.h"
#include "timeblockencoder.h"

namespace dyscostman {

class DyscoStMan;

/**
 * A column for storing compressed complex values with an approximate Gaussian
 * distribution.
 * @author André Offringa
 */
class DyscoDataColumn final : public ThreadedDyscoColumn<std::complex<float>> {
 public:
  /**
   * Create a new column. Internally called by DyscoStMan when creating a
   * new column.
   */
  DyscoDataColumn(DyscoStMan *parent, int dtype)
      : ThreadedDyscoColumn(parent, dtype),
        _rnd(std::random_device{}()),
        _gausEncoder(),
        _distribution(GaussianDistribution),
        _normalization(Normalization::kRF),
        _randomize(true) {}

  DyscoDataColumn(const DyscoDataColumn &source) = delete;

  void operator=(const DyscoDataColumn &source) = delete;

  /** Destructor. */
  virtual ~DyscoDataColumn() { shutdown(); }

  virtual void Prepare(DyscoDistribution distribution,
                       Normalization normalization, double studentsTNu,
                       double distributionTruncation) override;

  void SetStaticRandomizationSeed() {
    std::cout
        << "Warning: Initializing random number generator with static seed!\n";
    _rnd = std::mt19937();
    _randomize = false;
  }

 protected:
  virtual void initializeDecode(TimeBlockBuffer<data_t> *buffer,
                                const float *metaBuffer, size_t nRow,
                                size_t nAntennae) override;

  virtual void decode(TimeBlockBuffer<data_t> *buffer, const symbol_t *data,
                      size_t blockRow, size_t a1, size_t a2) override;

  virtual std::unique_ptr<ThreadDataBase> initializeEncodeThread() override;

  virtual void encode(ThreadDataBase *threadData,
                      TimeBlockBuffer<data_t> *buffer, float *metaBuffer,
                      symbol_t *symbolBuffer, size_t nAntennae) override;

  virtual size_t metaDataFloatCount(size_t nRow, size_t nPolarizations,
                                    size_t nChannels,
                                    size_t nAntennae) const override;

  virtual size_t symbolCount(size_t nRowsInBlock, size_t nPolarizations,
                             size_t nChannels) const override;

  virtual size_t defaultThreadCount() const override;

 private:
  struct ThreadData final : public ThreadDataBase {
    ThreadData(std::unique_ptr<TimeBlockEncoder> timeBlockEncoder)
        : encoder(std::move(timeBlockEncoder)) {}
    std::unique_ptr<TimeBlockEncoder> encoder;
    std::mt19937 rnd;
  };

  std::mt19937 _rnd;
  std::unique_ptr<StochasticEncoder<float>> _gausEncoder;
  std::unique_ptr<TimeBlockEncoder> _decoder;
  DyscoDistribution _distribution;
  Normalization _normalization;
  double _studentsTNu;
  bool _randomize;
};

}  // namespace dyscostman

#endif
