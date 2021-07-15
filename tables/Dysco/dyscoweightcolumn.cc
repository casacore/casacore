#include "dyscoweightcolumn.h"

namespace dyscostman {

void DyscoWeightColumn::Prepare(DyscoDistribution distribution,
                                Normalization normalization, double studentsTNu,
                                double distributionTruncation) {
  ThreadedDyscoColumn::Prepare(distribution, normalization, studentsTNu,
                               distributionTruncation);
  const size_t nPolarizations = shape()[0], nChannels = shape()[1];
  _encoder.reset(new WeightBlockEncoder(nPolarizations, nChannels,
                                        1 << getBitsPerSymbol()));
}

void DyscoWeightColumn::initializeDecode(TimeBlockBuffer<data_t> */*buffer*/,
                                         const float *metaBuffer, size_t /*nRow*/,
                                         size_t /*nAntennae*/) {
  _encoder->InitializeDecode(metaBuffer);
}

void DyscoWeightColumn::decode(TimeBlockBuffer<data_t> *buffer,
                               const unsigned int *data, size_t blockRow,
                               size_t /*a1*/, size_t /*a2*/) {
  _encoder->Decode(*buffer, data, blockRow);
}

void DyscoWeightColumn::encode(ThreadDataBase */*threadData*/,
                               TimeBlockBuffer<data_t> *buffer,
                               float *metaBuffer, symbol_t *symbolBuffer,
                               size_t /*nAntennae*/) {
  _encoder->Encode(*buffer, metaBuffer, symbolBuffer);
}

} // namespace dyscostman
