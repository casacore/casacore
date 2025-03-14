#include "aftimeblockencoder.h"

#include <random>

namespace {
void changeChannelFactor(std::vector<AFTimeBlockEncoder::DBufferRow> &data,
                                            float *metaBuffer, size_t visIndex,
                                            double factor) {
  metaBuffer[visIndex] /= factor;
  for (AFTimeBlockEncoder::DBufferRow &row : data) row.visibilities[visIndex] *= factor;
}
}

AFTimeBlockEncoder::AFTimeBlockEncoder(size_t nPol, size_t nChannels,
                                       bool fitToMaximum)
    : _nPol(nPol),
      _nChannels(nChannels),
      _fitToMaximum(fitToMaximum),
      _rmsPerChannel(_nChannels * nPol),
      _ditherDist(
          dyscostman::StochasticEncoder<double>::GetDitherDistribution()) {}

AFTimeBlockEncoder::~AFTimeBlockEncoder() {}

void AFTimeBlockEncoder::Normalize(
    const dyscostman::StochasticEncoder<float> &gausEncoder,
    TimeBlockBuffer<std::complex<float>> &buffer, size_t antennaCount) {
  if (_rmsPerAntenna.size() < antennaCount) _rmsPerAntenna.resize(antennaCount);
  std::vector<DBufferRow> data;
  buffer.ConvertVector<std::complex<double>>(data);
  const size_t visPerRow = _nPol * _nChannels;

  // Normalize the channels
  std::vector<RMSMeasurement> channelRMSes(_nChannels * _nPol);
  for (const DBufferRow &row : data) {
    for (size_t i = 0; i != visPerRow; ++i) {
      channelRMSes[i].Include(row.visibilities[i]);
    }
  }
  for (DBufferRow &row : data) {
    for (size_t i = 0; i != visPerRow; ++i) {
      double rms = channelRMSes[i].RMS();
      row.visibilities[i] /= rms;
    }
  }

  for (size_t p = 0; p != _nPol; ++p) {
    // Normalize the antennae
    calculateAntennaeRMS(data, p, antennaCount);
    for (DBufferRow &row : data) {
      double mul =
          (_rmsPerAntenna[row.antenna1] * _rmsPerAntenna[row.antenna2]);
      double fac = (mul == 0.0) ? 0.0 : 1.0 / mul;
      for (size_t ch = 0; ch != _nChannels; ++ch) {
        row.visibilities[ch * _nPol + p] *= fac;
      }
    }
  }

  if (_fitToMaximum) {
    for (size_t visIndex = 0; visIndex != visPerRow; ++visIndex) {
      double factor = 1.0;
      for (const DBufferRow &row : data) {
        if (row.antenna1 != row.antenna2) {
          const std::complex<double> *ptr = &row.visibilities[visIndex];
          double complMax = std::max(ptr->real(), ptr->imag());
          double complMin = std::min(ptr->real(), ptr->imag());

          if (complMax * factor > gausEncoder.MaxQuantity()) {
            factor = complMax / gausEncoder.MaxQuantity();
          } else if (complMin * factor < gausEncoder.MinQuantity()) {
            factor = complMin / gausEncoder.MinQuantity();
          }
        }
      }
      for (DBufferRow &row : data) row.visibilities[visIndex] *= factor;
    }
  }
}

void AFTimeBlockEncoder::changeAntennaFactor(std::vector<DBufferRow> &data,
                                             float *metaBuffer,
                                             size_t antennaIndex,
                                             size_t antennaCount,
                                             size_t polIndex, double factor) {
  const size_t visPerRow = _nPol * _nChannels;
  size_t metaIndex = visPerRow + antennaCount * polIndex;
  metaBuffer[metaIndex + antennaIndex] /= factor;
  for (DBufferRow &row : data) {
    unsigned count = 0;
    if (row.antenna1 == antennaIndex) ++count;
    if (row.antenna2 == antennaIndex) ++count;
    for (unsigned repeat = 0; repeat != count; ++repeat) {
      for (size_t i = 0; i != _nChannels; ++i)
        row.visibilities[i * _nPol + polIndex] *= factor;
    }
  }
}

//
// There are 3 axes to maximize over; antenna1, antenna2, channel
// We want to maximize all values (average abs value as large as possible)
// Example: (10=max)
//  ch1     ch2
//  a1a2a3   a1a2a3
//  ------   ------
//   1 1 1    1 1 1
//  10 1 1    1 1 1
//   1 1 1    1 1 1
// ant3 * 10 -> sum = (3 * 1 + 10 + 5 * 10) + (4 * 1 + 5 * 10) = 117
//  ch2 * 10 -> sum = 18 + 90 = 108
// both * s10 -> sum = (13 + 5*s10) + (s10*4 + 10*5) = 91.5
//  ch1     ch2
//   1 1 1    1 1 1
//   1 1 1    1 1 1
//   1 2 1    1 1.5 1
// ant3 * 10 -> sum = (3 * 1 + 10 + 5 * 10) + (4 * 1 + 5 * 10) = 117
//  ch2 * 10 -> sum = 18 + 90 = 108
// both * s10 -> sum = (13 + 5*s10) + (s10*4 + 10*5) = 91.5
//
// Approach: iterate over all antenna and channels, and find the antenna/channel
// that can increase the sum the most.
void AFTimeBlockEncoder::fitToMaximum(
    std::vector<DBufferRow> &data, float *metaBuffer,
    const dyscostman::StochasticEncoder<float> &gausEncoder,
    size_t antennaCount) {
  // First, the channels and polarizations are scaled such that the maximum
  // value equals the maximum encodable value
  const size_t visPerRow = _nPol * _nChannels;
  for (size_t visIndex = 0; visIndex != visPerRow; ++visIndex) {
    double largest_component = 0.0;
    for (const DBufferRow &row : data) {
      if (row.antenna1 != row.antenna2) {
        const std::complex<double> *ptr = &row.visibilities[visIndex];
        double local_max = std::max(std::max(ptr->real(), ptr->imag()),
                                   -std::min(ptr->real(), ptr->imag()));
        if (std::isfinite(local_max) && local_max > largest_component)
          largest_component = local_max;
      }
    }
    const double factor =
        (gausEncoder.MaxQuantity() == 0.0 || largest_component == 0.0)
            ? 1.0
            : gausEncoder.MaxQuantity() / largest_component;
    changeChannelFactor(data, metaBuffer, visIndex, factor);
  }

  for (size_t polIndex = 0; polIndex != _nPol; ++polIndex) {
    bool isProgressing;
    do {
      // Find the factor that increasest the sum of absolute values the most
      double bestChannelIncrease = 0.0, channelFactor = 1.0;
      size_t bestChannel = 0;
      for (size_t channel = 0; channel != _nChannels; ++channel) {
        // By how much can we increase this channel?
        double largest_component = 0.0;
        for (const DBufferRow &row : data) {
          if (row.antenna1 != row.antenna2) {
            const std::complex<double> *ptr =
                &row.visibilities[channel * _nPol + polIndex];
            double local_max = std::max(std::max(ptr->real(), ptr->imag()),
                                       -std::min(ptr->real(), ptr->imag()));
            if (std::isfinite(local_max) && local_max > largest_component)
              largest_component = local_max;
          }
        }
        double factor = (largest_component == 0.0)
                            ? 0.0
                            : (gausEncoder.MaxQuantity() / largest_component - 1.0);
        // How much does this increase the total?
        double thisIncrease = 0.0;
        for (DBufferRow &row : data) {
          if (row.antenna1 != row.antenna2) {
            std::complex<double> v =
                row.visibilities[channel * _nPol + polIndex] * double(factor);
            const double absoluteValue =
                std::fabs(v.real()) + std::fabs(v.imag());
            if (std::isfinite(absoluteValue)) thisIncrease += absoluteValue;
          }
        }
        if (thisIncrease > bestChannelIncrease) {
          bestChannelIncrease = thisIncrease;
          bestChannel = channel;
          channelFactor = factor + 1.0;
        }
      }

      ao::uvector<double> maxCompPerAntenna(antennaCount, 0.0);
      for (const DBufferRow &row : data) {
        if (row.antenna1 != row.antenna2) {
          for (size_t channel = 0; channel != _nChannels; ++channel) {
            const std::complex<double> *ptr =
                &row.visibilities[channel * _nPol + polIndex];
            double complMax = std::max(std::max(ptr->real(), ptr->imag()),
                                       -std::min(ptr->real(), ptr->imag()));
            if (std::isfinite(complMax)) {
              if (complMax > maxCompPerAntenna[row.antenna1])
                maxCompPerAntenna[row.antenna1] = complMax;
              if (complMax > maxCompPerAntenna[row.antenna2])
                maxCompPerAntenna[row.antenna2] = complMax;
            }
          }
        }
      }
      ao::uvector<double> increasePerAntenna(antennaCount, 0.0);
      for (const DBufferRow &row : data) {
        if (row.antenna1 != row.antenna2) {
          double factor1 = (maxCompPerAntenna[row.antenna1] == 0.0)
                               ? 0.0
                               : (gausEncoder.MaxQuantity() /
                                      maxCompPerAntenna[row.antenna1] -
                                  1.0);
          double factor2 = (maxCompPerAntenna[row.antenna2] == 0.0)
                               ? 0.0
                               : (gausEncoder.MaxQuantity() /
                                      maxCompPerAntenna[row.antenna2] -
                                  1.0);
          for (size_t channel = 0; channel != _nChannels; ++channel) {
            std::complex<double> v1 =
                row.visibilities[channel * _nPol + polIndex] * factor1;
            double av1 = std::fabs(v1.real()) + std::fabs(v1.imag());
            if (std::isfinite(av1)) increasePerAntenna[row.antenna1] += av1;
            std::complex<double> v2 =
                row.visibilities[channel * _nPol + polIndex] * factor2;
            double av2 = std::fabs(v2.real()) + std::fabs(v2.imag());
            if (std::isfinite(av2)) increasePerAntenna[row.antenna2] += av2;
          }
        }
      }
      size_t bestAntenna = 0;
      double bestAntennaIncrease = 0.0;
      for (size_t a = 0; a != antennaCount; ++a) {
        if (increasePerAntenna[a] > bestAntennaIncrease) {
          bestAntennaIncrease = increasePerAntenna[a];
          bestAntenna = a;
        }
      }
      // The benefit was calculated for increasing an antenna and increasing a
      // channel. Select which of those two has the largest benefit and apply:
      if (bestAntennaIncrease > bestChannelIncrease) {
        double factor =
            (maxCompPerAntenna[bestAntenna] == 0.0)
                ? 1.0
                : (gausEncoder.MaxQuantity() / maxCompPerAntenna[bestAntenna]);
        if (factor < 1.0)
          isProgressing = false;
        else {
          isProgressing = factor > 1.01;
          changeAntennaFactor(data, metaBuffer, bestAntenna, antennaCount,
                              polIndex, factor);
        }
      } else {
        if (channelFactor < 1.0) {
          isProgressing = false;
        } else {
          isProgressing = channelFactor > 1.001;
          changeChannelFactor(data, metaBuffer, bestChannel * _nPol + polIndex,
                              channelFactor);
        }
      }
    } while (isProgressing);
  }
}

template <bool UseDithering>
void AFTimeBlockEncoder::encode(
    const dyscostman::StochasticEncoder<float> &gausEncoder,
    const TimeBlockBuffer<std::complex<float>> &buffer, float *metaBuffer,
    symbol_t *symbolBuffer, size_t antennaCount, std::mt19937 *rnd) {
  if (_rmsPerAntenna.size() < antennaCount) _rmsPerAntenna.resize(antennaCount);
  // Note that encoding is performed with doubles
  std::vector<DBufferRow> data;
  buffer.ConvertVector<std::complex<double>>(data);
  const size_t visPerRow = _nPol * _nChannels;

  // Normalize the RMS of the channels
  std::vector<RMSMeasurement> channelRMSes(_nChannels * _nPol);
  for (const DBufferRow &row : data) {
    for (size_t i = 0; i != visPerRow; ++i) {
      channelRMSes[i].Include(row.visibilities[i]);
    }
  }
  for (DBufferRow &row : data) {
    for (size_t i = 0; i != visPerRow; ++i) {
      double rms = channelRMSes[i].RMS();
      if (rms != 0.0) {
        row.visibilities[i] /= rms;
      }
      metaBuffer[i] = rms;
    }
  }

  for (size_t p = 0; p != _nPol; ++p) {
    // Normalize the RMS of the antennae
    calculateAntennaeRMS(data, p, antennaCount);
    for (DBufferRow &row : data) {
      double mul =
          (_rmsPerAntenna[row.antenna1] * _rmsPerAntenna[row.antenna2]);
      double fac = (mul == 0.0) ? 0.0 : 1.0 / mul;
      for (size_t ch = 0; ch != _nChannels; ++ch) {
        row.visibilities[ch * _nPol + p] *= fac;
      }
    }

    size_t metaIndex = visPerRow + antennaCount * p;
    for (size_t a = 0; a != antennaCount; ++a)
      metaBuffer[metaIndex + a] = _rmsPerAntenna[a];
  }

  if (_fitToMaximum) {
    fitToMaximum(data, metaBuffer, gausEncoder, antennaCount);
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

template void AFTimeBlockEncoder::encode<true>(
    const dyscostman::StochasticEncoder<float> &gausEncoder,
    const TimeBlockBuffer<std::complex<float>> &buffer, float *metaBuffer,
    symbol_t *symbolBuffer, size_t antennaCount, std::mt19937 *rnd);
template void AFTimeBlockEncoder::encode<false>(
    const dyscostman::StochasticEncoder<float> &gausEncoder,
    const TimeBlockBuffer<std::complex<float>> &buffer, float *metaBuffer,
    symbol_t *symbolBuffer, size_t antennaCount, std::mt19937 *rnd);

void AFTimeBlockEncoder::calculateAntennaeRMS(
    const std::vector<DBufferRow> &data, size_t polIndex, size_t antennaCount) {
  std::vector<RMSMeasurement> matrixMeas(antennaCount * antennaCount);
  for (const DBufferRow &row : data) {
    size_t a1 = row.antenna1, a2 = row.antenna2;
    if (a1 != a2) {
      if (a1 > a2) std::swap(a1, a2);
      const size_t index = a1 * antennaCount + a2;
      for (size_t ch = 0; ch != _nChannels; ++ch)
        matrixMeas[index].Include(row.visibilities[ch * _nPol + polIndex]);
    }
  }

  ao::uvector<double> matrix(matrixMeas.size());
  for (size_t i = 0; i != antennaCount; ++i) {
    for (size_t j = i; j != antennaCount; ++j) {
      matrix[i * antennaCount + j] = matrixMeas[i * antennaCount + j].RMS();
      matrix[j * antennaCount + i] = matrixMeas[i * antennaCount + j].RMS();
    }
  }

  // (note that _rmsPerAntenna is larger, so don't use assign())
  for (size_t i = 0; i != antennaCount; ++i) _rmsPerAntenna[i] = 1.0;

  double precision = 1.0;
  for (size_t iteration = 0; iteration != 100 && precision > 1e-6;
       ++iteration) {
    ao::uvector<double> nextRMS(antennaCount, 0.0);
    for (size_t i = 0; i != antennaCount; ++i) {
      double weightSum = 0.0;
      for (size_t j = 0; j != antennaCount; ++j) {
        if (i != j && _rmsPerAntenna[j] != 0.0) {
          double w = _rmsPerAntenna[j];
          if (std::isfinite(matrix[i * antennaCount + j])) {
            // matrix / estVec, but since we weight, just:
            nextRMS[i] += matrix[i * antennaCount + j];
            weightSum += w;
          }
        }
      }
      if (weightSum == 0.0)
        nextRMS[i] = 0.0;
      else
        nextRMS[i] /= weightSum;
    }

    double maxVal = 0.0;
    for (size_t i = 0; i != antennaCount; ++i) {
      _rmsPerAntenna[i] = nextRMS[i] * 0.8 + _rmsPerAntenna[i] * 0.2;
      maxVal = std::max(_rmsPerAntenna[i], maxVal);
    }
    precision = 0.0;
    for (size_t i = 0; i != antennaCount; ++i) {
      if (_rmsPerAntenna[i] < maxVal * 1e-5) _rmsPerAntenna[i] = 0.0;
      precision = std::max(precision,
                           std::fabs(_rmsPerAntenna[i] - nextRMS[i]) / maxVal);
    }
  }
}

void AFTimeBlockEncoder::InitializeDecode(const float *metaBuffer,
                                          size_t /*nRow*/, size_t nAntennae) {
  if (_rmsPerAntenna.size() < nAntennae * _nPol)
    _rmsPerAntenna.resize(nAntennae * _nPol);
  const size_t visPerRow = _nPol * _nChannels;
  for (size_t i = 0; i != visPerRow; ++i) _rmsPerChannel[i] = metaBuffer[i];
  metaBuffer += visPerRow;
  for (size_t p = 0; p != _nPol; ++p) {
    for (size_t a = 0; a != nAntennae; ++a)
      _rmsPerAntenna[a * _nPol + p] = metaBuffer[a + p * nAntennae];
  }
}

void AFTimeBlockEncoder::Decode(
    const dyscostman::StochasticEncoder<float> &gausEncoder, FBuffer &buffer,
    const AFTimeBlockEncoder::symbol_t *symbolBuffer, size_t blockRow,
    size_t antenna1, size_t antenna2) {
  ao::uvector<double> antFactors(_nPol);
  for (size_t p = 0; p != _nPol; ++p)
    antFactors[p] = _rmsPerAntenna[antenna1 * _nPol + p] *
                    _rmsPerAntenna[antenna2 * _nPol + p];

  FBufferRow &row = buffer[blockRow];
  row.antenna1 = antenna1;
  row.antenna2 = antenna2;
  row.visibilities.resize(_nChannels * _nPol);
  std::complex<float> *destination = row.visibilities.data();
  const symbol_t *srcRowPtr = symbolBuffer + blockRow * SymbolsPerRow();
  for (size_t ch = 0; ch != _nChannels; ++ch) {
    for (size_t p = 0; p != _nPol; ++p) {
      double chRMS = _rmsPerChannel[ch * _nPol + p];
      double factor = chRMS * antFactors[p];
      destination->real(double(gausEncoder.Decode(*srcRowPtr)) * factor);
      ++srcRowPtr;
      destination->imag(double(gausEncoder.Decode(*srcRowPtr)) * factor);
      ++srcRowPtr;
      ++destination;
    }
  }
}
