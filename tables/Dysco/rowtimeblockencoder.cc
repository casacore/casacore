#include "rowtimeblockencoder.h"
#include "stochasticencoder.h"

using namespace dyscostman;

RowTimeBlockEncoder::RowTimeBlockEncoder(size_t nPol, size_t nChannels)
    : _nPol(nPol),
      _nChannels(nChannels),
      _ditherDist(StochasticEncoder<float>::GetDitherDistribution()),
      _rowFactors() {}

void RowTimeBlockEncoder::InitializeDecode(const float *metaBuffer, size_t nRow,
                                           size_t /*nAntennae*/) {
  _rowFactors.assign(metaBuffer, metaBuffer + nRow);
}

void RowTimeBlockEncoder::Decode(const StochasticEncoder<float> &gausEncoder,
                                 FBuffer &buffer, const symbol_t *symbolBuffer,
                                 size_t blockRow, size_t antenna1,
                                 size_t antenna2) {
  FBufferRow &row = buffer[blockRow];
  row.antenna1 = antenna1;
  row.antenna2 = antenna2;
  row.visibilities.resize(_nChannels * _nPol);
  std::complex<float> *destination = row.visibilities.data();
  const symbol_t *srcRowPtr = symbolBuffer + blockRow * SymbolsPerRow();
  const size_t visPerRow = _nPol * _nChannels;
  for (size_t i = 0; i != visPerRow; ++i) {
    double factor = _rowFactors[blockRow];
    destination->real(double(gausEncoder.Decode(*srcRowPtr)) * factor);
    ++srcRowPtr;
    destination->imag(double(gausEncoder.Decode(*srcRowPtr)) * factor);
    ++srcRowPtr;
    ++destination;
  }
}

template <bool UseDithering>
void RowTimeBlockEncoder::encode(const StochasticEncoder<float> &gausEncoder,
                                 const FBuffer &buffer, float *metaBuffer,
                                 symbol_t *symbolBuffer,
                                 size_t /*antennaCount*/, std::mt19937 *rnd) {
  // Note that encoding is performed with doubles
  std::vector<DBufferRow> data;
  buffer.ConvertVector<std::complex<double>>(data);
  const size_t visPerRow = _nPol * _nChannels;

  // Scale every maximum per row to the max level
  const double maxLevel = gausEncoder.MaxQuantity();
  for (size_t rowIndex = 0; rowIndex != data.size(); ++rowIndex) {
    DBufferRow &row = data[rowIndex];
    double maxVal = 0.0;
    for (size_t i = 0; i != visPerRow; ++i) {
      std::complex<double> v = row.visibilities[i];
      double m = std::max(std::fabs(v.real()), std::fabs(v.imag()));
      if (std::isfinite(m)) maxVal = std::max(maxVal, m);
    }
    const double factor = (maxVal == 0.0) ? 1.0 : maxLevel / maxVal;
    for (size_t i = 0; i != visPerRow; ++i) row.visibilities[i] *= factor;
    metaBuffer[rowIndex] = maxVal / maxLevel;
  }

  symbol_t *symbolBufferPtr = symbolBuffer;
  for (const DBufferRow &row : data) {
    for (size_t i = 0; i != visPerRow; ++i) {
      if (UseDithering) {
        symbolBufferPtr[i * 2] = gausEncoder.EncodeWithDithering(
            row.visibilities[i].real(), _ditherDist(*rnd));
        symbolBufferPtr[i * 2 + 1] = gausEncoder.EncodeWithDithering(
            row.visibilities[i].imag(), _ditherDist(*rnd));
      } else {
        symbolBufferPtr[i * 2] = gausEncoder.Encode(row.visibilities[i].real());
        symbolBufferPtr[i * 2 + 1] =
            gausEncoder.Encode(row.visibilities[i].imag());
      }
    }
    symbolBufferPtr += visPerRow * 2;
  }
}
