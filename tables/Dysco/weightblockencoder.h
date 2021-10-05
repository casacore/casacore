#ifndef DYSCO_WEIGHT_BLOCK_ENCODER_H
#define DYSCO_WEIGHT_BLOCK_ENCODER_H

#include <cmath>
#include <cstring>

#include "timeblockbuffer.h"

class WeightBlockEncoder {
 public:
  WeightBlockEncoder(size_t nPolarizations, size_t nChannels, size_t quantCount)
      : _nPolarizations(nPolarizations),
        _nChannels(nChannels),
        _quantCount(quantCount) {}

  size_t MetaDataFloatCount() const { return 1.0; }

  size_t SymbolCount(size_t nRowsInBlock) const {
    return nRowsInBlock * _nChannels;
  }

  void InitializeDecode(const float *metaBuffer) {
    _decodeMaxValue = metaBuffer[0];
  }

  void Decode(TimeBlockBuffer<float> &buffer, const unsigned int *symbolBuffer,
              size_t blockRow) const {
    double scaleValue = _decodeMaxValue / (double(_quantCount - 1));
    TimeBlockBuffer<float>::DataRow &row = buffer[blockRow];
    const unsigned int *rowBuffer = &symbolBuffer[blockRow * _nChannels];
    for (size_t ch = 0; ch != _nChannels; ++ch) {
      float value = *rowBuffer * scaleValue;
      row.visibilities.resize(_nChannels * _nPolarizations);
      float *chPtr = &row.visibilities[ch * _nPolarizations];
      for (size_t p = 0; p != _nPolarizations; ++p) chPtr[p] = value;
      ++rowBuffer;
    }
  }

  void Encode(TimeBlockBuffer<float> &buffer, float *metaBuffer,
              unsigned int *symbolBuffer) const {
    float maxValue = 0.0;
    for (const TimeBlockBuffer<float>::DataRow &row : buffer.GetVector()) {
      for (size_t ch = 0; ch != _nChannels; ++ch) {
        const float *visPtr = &row.visibilities[ch * _nPolarizations];
        float weight = *visPtr;
        for (size_t p = 1; p != _nPolarizations; ++p)
          if (visPtr[p] < weight) weight = visPtr[p];
        if (weight > maxValue) maxValue = weight;
      }
    }
    if (maxValue == 0.0) maxValue = 1.0;
    metaBuffer[0] = maxValue;

    double scaleValue = double(_quantCount - 1) / maxValue;

    for (const TimeBlockBuffer<float>::DataRow &row : buffer.GetVector()) {
      for (size_t ch = 0; ch != _nChannels; ++ch) {
        const float *visPtr = &row.visibilities[ch * _nPolarizations];
        float weight = *visPtr;
        for (size_t p = 1; p != _nPolarizations; ++p)
          if (visPtr[p] < weight) weight = visPtr[p];

        *symbolBuffer = roundf(double(weight) * scaleValue);
        ++symbolBuffer;
      }
    }
  }

 private:
  const size_t _nPolarizations;
  const size_t _nChannels;
  const size_t _quantCount;
  float _decodeMaxValue;
};

#endif
