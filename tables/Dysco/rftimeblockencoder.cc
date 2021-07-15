#include "rftimeblockencoder.h"
#include "stochasticencoder.h"

#include <random>

RFTimeBlockEncoder::RFTimeBlockEncoder(size_t nPol, size_t nChannels)
    : _nPol(nPol), _nChannels(nChannels), _channelFactors(_nChannels * nPol),
      _rowFactors(),
      _ditherDist(
          dyscostman::StochasticEncoder<double>::GetDitherDistribution()) {}

RFTimeBlockEncoder::~RFTimeBlockEncoder() {}

template <bool UseDithering>
void RFTimeBlockEncoder::encode(
    const dyscostman::StochasticEncoder<float> &gausEncoder,
    const TimeBlockEncoder::FBuffer &buffer, float *metaBuffer,
    TimeBlockEncoder::symbol_t *symbolBuffer, size_t antennaCount,
    std::mt19937 *rnd) {
  // Note that encoding is performed with doubles
  std::vector<DBufferRow> data;
  buffer.ConvertVector<std::complex<double>>(data);
  const size_t visPerRow = _nPol * _nChannels;

  // Normalize the channels
  std::vector<RMSMeasurement> channelRMSes(visPerRow);
  for (const DBufferRow &row : data) {
    for (size_t i = 0; i != visPerRow; ++i)
      channelRMSes[i].Include(row.visibilities[i]);
  }
  for (DBufferRow &row : data) {
    for (size_t i = 0; i != visPerRow; ++i) {
      double rms = channelRMSes[i].RMS();
      row.visibilities[i] /= rms;
    }
  }

  // Scale every maximum per row to the max level
  const double maxLevel = gausEncoder.MaxQuantity();
  for (size_t rowIndex = 0; rowIndex != data.size(); ++rowIndex) {
    DBufferRow &row = data[rowIndex];
    ao::uvector<double> maxValPerPol(_nPol, 0.0);
    for (size_t i = 0; i != visPerRow; ++i) {
      std::complex<double> v = row.visibilities[i];
      double m = std::max(std::fabs(v.real()), std::fabs(v.imag()));
      if (std::isfinite(m))
        maxValPerPol[i % _nPol] = std::max(maxValPerPol[i % _nPol], m);
    }
    for (size_t i = 0; i != visPerRow; ++i) {
      const double factor = maxValPerPol[i % _nPol] == 0.0
                                ? 1.0
                                : maxLevel / maxValPerPol[i % _nPol];
      row.visibilities[i] *= factor;
    }
    for (size_t polIndex = 0; polIndex != _nPol; ++polIndex)
      metaBuffer[visPerRow + rowIndex * _nPol + polIndex] =
          maxValPerPol[polIndex] / maxLevel;
  }

  // Maximize channels
  ao::uvector<double> maxima(visPerRow, 0.0);
  for (const DBufferRow &row : data) {
    for (size_t i = 0; i != visPerRow; ++i) {
      std::complex<double> v = row.visibilities[i];
      double m = std::max(std::fabs(v.real()), std::fabs(v.imag()));
      if (std::isfinite(m))
        maxima[i] = std::max(maxima[i], m);
    }
  }
  // Convert the maxima to factors
  for (size_t i = 0; i != visPerRow; ++i) {
    if (maxima[i] == 0.0)
      maxima[i] = 1.0;
    else
      maxima[i] = maxLevel / maxima[i];
    metaBuffer[i] = channelRMSes[i].RMS() / maxima[i];
  }
  for (DBufferRow &row : data) {
    for (size_t i = 0; i != visPerRow; ++i)
      row.visibilities[i] *= maxima[i];
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

void RFTimeBlockEncoder::InitializeDecode(const float *metaBuffer, size_t nRow,
                                          size_t nAntennae) {
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
