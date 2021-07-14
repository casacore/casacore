#ifndef DYSCO_ROW_TIME_BLOCK_ENCODER_H
#define DYSCO_ROW_TIME_BLOCK_ENCODER_H

#include "stochasticencoder.h"
#include "timeblockbuffer.h"
#include "uvector.h"

#include <complex>
#include <random>
#include <vector>

#include "timeblockencoder.h"

class RowTimeBlockEncoder : public TimeBlockEncoder {
public:
  RowTimeBlockEncoder(size_t nPol, size_t nChannels);

  virtual ~RowTimeBlockEncoder() override {}

  virtual void
  EncodeWithDithering(const dyscostman::StochasticEncoder<float> &gausEncoder,
                      FBuffer &buffer, float *metaBuffer,
                      symbol_t *symbolBuffer, size_t antennaCount,
                      std::mt19937 &rnd) final override {
    encode<true>(gausEncoder, buffer, metaBuffer, symbolBuffer, antennaCount,
                 &rnd);
  }

  virtual void EncodeWithoutDithering(
      const dyscostman::StochasticEncoder<float> &gausEncoder, FBuffer &buffer,
      float *metaBuffer, symbol_t *symbolBuffer,
      size_t antennaCount) final override {
    encode<false>(gausEncoder, buffer, metaBuffer, symbolBuffer, antennaCount,
                  0);
  }

  virtual void InitializeDecode(const float *metaBuffer, size_t nRow,
                                size_t nAntennae) final override;

  virtual void Decode(const dyscostman::StochasticEncoder<float> &gausEncoder,
                      FBuffer &buffer, const symbol_t *symbolBuffer,
                      size_t blockRow, size_t antenna1,
                      size_t antenna2) final override;

  virtual size_t SymbolCount(size_t nRow, size_t nPol,
                             size_t nChannels) const final override {
    return nRow * nChannels * nPol * 2 /*complex*/;
  }

  virtual size_t SymbolCount(size_t nRow) const final override {
    return nRow * _nChannels * _nPol * 2 /*complex*/;
  }

  virtual size_t SymbolsPerRow() const final override {
    return _nChannels * _nPol * 2 /*complex*/;
  }

  virtual size_t MetaDataCount(size_t nRow, size_t nPol, size_t nChannels,
                               size_t nAntennae) const final override {
    return nRow;
  }

private:
  template <bool UseDithering>
  void encode(const dyscostman::StochasticEncoder<float> &gausEncoder,
              const FBuffer &buffer, float *metaBuffer, symbol_t *symbolBuffer,
              size_t antennaCount, std::mt19937 *rnd);

  size_t _nPol, _nChannels;

  std::uniform_int_distribution<unsigned> _ditherDist;
  ao::uvector<double> _rowFactors;
};

#endif
