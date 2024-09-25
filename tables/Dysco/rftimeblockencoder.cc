#include "rftimeblockencoder.h"
#include "stochasticencoder.h"

#include <random>

RFTimeBlockEncoder::RFTimeBlockEncoder(size_t nPol, size_t nChannels)
    : _nPol(nPol),
      _nChannels(nChannels),
      _channelFactors(_nChannels * nPol),
      _rowFactors(),
      _ditherDist(
          dyscostman::StochasticEncoder<double>::GetDitherDistribution()) {}

RFTimeBlockEncoder::~RFTimeBlockEncoder() = default;

void RFTimeBlockEncoder::maximizeRows(std::vector<DBufferRow> &data, float *metaBuffer,
    const dyscostman::StochasticEncoder<float> &gausEncoder) {
  // Scale rows: Scale every row maximum to the max level.
  // Polarizations are processed separately: every polarization
  // has its own row-scaling factor.
  const double maxLevel = gausEncoder.MaxQuantity();
  const size_t visPerRow = _nPol * _nChannels;
  for (size_t rowIndex = 0; rowIndex != data.size(); ++rowIndex) {
    DBufferRow &row = data[rowIndex];
    for (size_t polIndex = 0; polIndex != _nPol; ++polIndex) {
      double max_val = 0.0;
      for (size_t channel = 0; channel != _nChannels; ++channel) {
        std::complex<double> v = row.visibilities[channel * _nPol + polIndex];
        double m = std::max(std::fabs(v.real()), std::fabs(v.imag()));
        if (std::isfinite(m))
          max_val = std::max(max_val, m);
      }
      const double factor = max_val == 0.0
                                ? 1.0
                                : maxLevel / max_val;
      for (size_t channel = 0; channel != _nChannels; ++channel) {
       row.visibilities[channel * _nPol + polIndex] *= factor;
      }

      metaBuffer[visPerRow + rowIndex * _nPol + polIndex] =
          (maxLevel == 0.0) ? 1.0 : max_val / maxLevel;
    }
  }
}

void RFTimeBlockEncoder::maximizeChannels(std::vector<DBufferRow> &data, float *metaBuffer,
    const dyscostman::StochasticEncoder<float> &gausEncoder) {
  const size_t visPerRow = _nPol * _nChannels;
  // Scale channels: channels are scaled such that the maximum
  // value equals the maximum encodable value
  for (size_t visIndex = 0; visIndex != visPerRow; ++visIndex) {
    double largest_component = 0.0;
    for (const DBufferRow &row : data) {
      const std::complex<double> *ptr = &row.visibilities[visIndex];
      const double local_max = std::max(std::fabs(ptr->real()), std::fabs(ptr->imag()));
      if (std::isfinite(local_max) && local_max > largest_component)
        largest_component = local_max;
    }
    const double factor =
        (gausEncoder.MaxQuantity() == 0.0 || largest_component == 0.0)
            ? 1.0
            : gausEncoder.MaxQuantity() / largest_component;
    metaBuffer[visIndex] = 1.0 / factor;
    for (RFTimeBlockEncoder::DBufferRow &row : data) {
      row.visibilities[visIndex] *= factor;
    }
  }
}

template <bool UseDithering>
void RFTimeBlockEncoder::encode(
    const dyscostman::StochasticEncoder<float> &gausEncoder,
    const TimeBlockEncoder::FBuffer &buffer, float *metaBuffer,
    TimeBlockEncoder::symbol_t *symbolBuffer, size_t /*antennaCount*/,
    std::mt19937 *rnd) {
  // Note that encoding is performed with doubles
  std::vector<DBufferRow> data;
  buffer.ConvertVector<std::complex<double>>(data);
  const size_t visPerRow = _nPol * _nChannels;

  // Rows are processed before
  // channels, because auto-correlations might have much
  // higher values compared to cross-correlations. By first
  // scaling the rows, the auto-correlations and cross-correlations
  // are brought to the same level.
  maximizeRows(data, metaBuffer, gausEncoder);

  maximizeChannels(data, metaBuffer, gausEncoder);

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

void RFTimeBlockEncoder::InitializeDecode(const float *metaBuffer, size_t nRow,
                                          size_t /*nAntennae*/) {
  _channelFactors.assign(metaBuffer, metaBuffer + _nPol * _nChannels);
  metaBuffer += _nPol * _nChannels;
  _rowFactors.assign(metaBuffer, metaBuffer + _nPol * nRow);
}

void RFTimeBlockEncoder::Decode(
    const dyscostman::StochasticEncoder<float> &gausEncoder,
    TimeBlockEncoder::FBuffer &buffer,
    const TimeBlockEncoder::symbol_t *symbolBuffer, size_t blockRow,
    size_t antenna1, size_t antenna2) {
  FBufferRow &row = buffer[blockRow];
  row.antenna1 = antenna1;
  row.antenna2 = antenna2;
  row.visibilities.resize(_nChannels * _nPol);
  std::complex<float> *destination = row.visibilities.data();
  const symbol_t *srcRowPtr = symbolBuffer + blockRow * SymbolsPerRow();
  const size_t visPerRow = _nPol * _nChannels;
  for (size_t i = 0; i != visPerRow; ++i) {
    double chFactor = _channelFactors[i];
    double factor = chFactor * _rowFactors[blockRow * _nPol + i % _nPol];
    destination->real(double(gausEncoder.Decode(*srcRowPtr)) * factor);
    ++srcRowPtr;
    destination->imag(double(gausEncoder.Decode(*srcRowPtr)) * factor);
    ++srcRowPtr;
    ++destination;
  }
}
