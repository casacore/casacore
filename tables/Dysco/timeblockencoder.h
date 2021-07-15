#ifndef DYSCO_TIME_BLOCK_ENCODER_H
#define DYSCO_TIME_BLOCK_ENCODER_H

#include "stochasticencoder.h"
#include "timeblockbuffer.h"
#include "uvector.h"

#include <complex>
#include <random>
#include <vector>

class RMSMeasurement {
public:
  RMSMeasurement() : _count(0), _value(0.0) {}

  void Include(const std::complex<double> &val) {
    if (isfinite(val)) {
      _count++;
      _value += val.real() * val.real() + val.imag() * val.imag();
    }
  }

  double RMS() const { return sqrt(_value / (_count * 2)); }

private:
  static bool isfinite(const std::complex<double> &val) {
    return std::isfinite(val.real()) && std::isfinite(val.imag());
  }

  size_t _count;
  double _value;
};

class TimeBlockEncoder {
public:
  typedef TimeBlockBuffer<std::complex<float>> FBuffer;
  typedef typename TimeBlockBuffer<std::complex<float>>::DataRow FBufferRow;
  typedef TimeBlockBuffer<std::complex<double>> DBuffer;
  typedef typename TimeBlockBuffer<std::complex<double>>::DataRow DBufferRow;

  typedef unsigned symbol_t;

  virtual ~TimeBlockEncoder() {}

  virtual void
  EncodeWithDithering(const dyscostman::StochasticEncoder<float> &gausEncoder,
                      FBuffer &buffer, float *metaBuffer,
                      symbol_t *symbolBuffer, size_t antennaCount,
                      std::mt19937 &rnd) = 0;

  virtual void EncodeWithoutDithering(
      const dyscostman::StochasticEncoder<float> &gausEncoder, FBuffer &buffer,
      float *metaBuffer, symbol_t *symbolBuffer, size_t antennaCount) = 0;

  virtual void InitializeDecode(const float *metaBuffer, size_t nRow,
                                size_t nAntennae) = 0;

  virtual void Decode(const dyscostman::StochasticEncoder<float> &gausEncoder,
                      FBuffer &buffer, const symbol_t *symbolBuffer,
                      size_t blockRow, size_t antenna1, size_t antenna2) = 0;

  virtual size_t SymbolCount(size_t nRow, size_t nPol,
                             size_t nChannels) const = 0;

  virtual size_t SymbolCount(size_t nRow) const = 0;

  virtual size_t SymbolsPerRow() const = 0;

  virtual size_t MetaDataCount(size_t nRow, size_t nPol, size_t nChannels,
                               size_t nAntennae) const = 0;

protected:
  TimeBlockEncoder() {}
};

#endif
