#include "../aftimeblockencoder.h"
#include "../dysconormalization.h"
#include "../rftimeblockencoder.h"
#include "../rowtimeblockencoder.h"
#include "../stochasticencoder.h"

#include <random>

#include <boost/test/unit_test.hpp>

using namespace dyscostman;

BOOST_AUTO_TEST_SUITE(timeblock_encoder)

namespace {

std::unique_ptr<TimeBlockEncoder> CreateEncoder(
    Normalization blockNormalization, size_t nPol, size_t nChan) {
  switch (blockNormalization) {
    default:
    case Normalization::kRF:
      return std::unique_ptr<TimeBlockEncoder>(
          new RFTimeBlockEncoder(nPol, nChan));
    case Normalization::kAF:
      return std::unique_ptr<TimeBlockEncoder>(
          new AFTimeBlockEncoder(nPol, nChan, true));
    case Normalization::kRow:
      return std::unique_ptr<TimeBlockEncoder>(
          new RowTimeBlockEncoder(nPol, nChan));
  }
}

TimeBlockBuffer<std::complex<float>> Decode(
    Normalization blockNormalization, StochasticEncoder<float>& gausEncoder,
    size_t nAnt, size_t nChan, size_t nPol, size_t nRow,
    const float* metaBuffer, const TimeBlockEncoder::symbol_t* symbolBuffer) {
  TimeBlockBuffer<std::complex<float>> out(nPol, nChan);
  out.resize(nRow);
  std::unique_ptr<TimeBlockEncoder> decoder =
      CreateEncoder(blockNormalization, nPol, nChan);
  decoder->InitializeDecode(metaBuffer, nRow, nAnt);
  size_t rIndex = 0;
  for (size_t ant1 = 0; ant1 != nAnt; ++ant1) {
    for (size_t ant2 = ant1; ant2 != nAnt; ++ant2) {
      decoder->Decode(gausEncoder, out, symbolBuffer, rIndex, ant1, ant2);
      ++rIndex;
    }
  }
  return out;
}

void TestSimpleExample(Normalization blockNormalization) {
  const size_t nAnt = 4, nChan = 1, nPol = 2, nRow = (nAnt * (nAnt + 1) / 2);

  TimeBlockBuffer<std::complex<float>> buffer(nPol, nChan);

  std::complex<float> data[nChan * nPol];
  data[0] = 99.0;
  data[1] = 99.0;
  buffer.SetData(0, 0, 0, data);
  data[0] = 10.0;
  data[1] = std::complex<double>(9.0, 1.0);
  buffer.SetData(1, 0, 1, data);
  data[0] = 8.0;
  data[1] = std::complex<double>(7.0, 2.0);
  buffer.SetData(2, 0, 2, data);
  data[0] = 6.0;
  data[1] = std::complex<double>(5.0, 3.0);
  buffer.SetData(3, 0, 3, data);
  data[0] = 99.0;
  data[1] = 99.0;
  buffer.SetData(4, 1, 1, data);
  data[0] = 4.0;
  data[1] = std::complex<double>(3.0, 4.0);
  buffer.SetData(5, 1, 2, data);
  data[0] = 2.0;
  data[1] = std::complex<double>(1.0, 5.0);
  buffer.SetData(6, 1, 3, data);
  data[0] = 99.0;
  data[1] = 99.0;
  buffer.SetData(7, 2, 2, data);
  data[0] = 0.0;
  data[1] = std::numeric_limits<float>::quiet_NaN();
  buffer.SetData(8, 2, 3, data);
  data[0] = 99.0;
  data[1] = 99.0;
  buffer.SetData(9, 3, 3, data);
  const TimeBlockBuffer<std::complex<float>> input(buffer);

  StochasticEncoder<float> gausEncoder(256, 1.0, false);
  std::unique_ptr<TimeBlockEncoder> encoder =
      CreateEncoder(blockNormalization, nPol, nChan);

  size_t metaDataCount = encoder->MetaDataCount(nRow, nPol, nChan, nAnt);
  size_t symbolCount = encoder->SymbolCount(nRow);
  ao::uvector<float> metaBuffer(metaDataCount);
  ao::uvector<TimeBlockEncoder::symbol_t> symbolBuffer(symbolCount);

  encoder->EncodeWithoutDithering(gausEncoder, buffer, metaBuffer.data(),
                                  symbolBuffer.data(), nAnt);
  TimeBlockBuffer<std::complex<float>> out =
      Decode(blockNormalization, gausEncoder, nAnt, nChan, nPol, nRow,
             metaBuffer.data(), symbolBuffer.data());
  std::complex<float> dataFromOut[nChan * nPol], dataFromIn[nChan * nPol];
  for (size_t row = 0; row != nRow; row++) {
    // skip auto-correlations of AF, since these are not saved.
    out.GetData(row, dataFromOut);
    input.GetData(row, dataFromIn);
    if (blockNormalization != Normalization::kAF ||
        (row != 0 && row != 4 && row != 7 && row != 9)) {
      for (size_t ch = 0; ch != nChan; ++ch) {
        BOOST_CHECK_MESSAGE(
            std::norm(dataFromOut[ch] - dataFromIn[ch]) < 0.1,
            "Output{" << dataFromOut[ch] << "} is close to input{"
                      << dataFromIn[ch] << "} of row " << row
                      << " with normalization " << int(blockNormalization));
      }
    }
  }
}

void TestTimeBlockEncoder(Normalization blockNormalization) {
  const size_t nAnt = 50, nChan = 64, nPol = 4, nRow = (nAnt * (nAnt + 1) / 2);

  TimeBlockBuffer<std::complex<float>> buffer(nPol, nChan);
  std::mt19937 rnd;
  std::normal_distribution<float> dist;
  StochasticEncoder<float> gausEncoder(256, 1.0, false);
  std::uniform_int_distribution<unsigned> dither =
      gausEncoder.GetDitherDistribution();

  dist(rnd);

  ao::uvector<std::complex<float>> data(nChan * nPol);
  std::vector<ao::uvector<std::complex<float>>> allData;

  RMSMeasurement unscaledRMS;
  double factorSum = 0.0;
  size_t factorCount = 0;
  size_t blockRow = 0;
  for (size_t a1 = 0; a1 != nAnt; ++a1) {
    for (size_t a2 = a1; a2 != nAnt; ++a2) {
      for (size_t ch = 0; ch != nChan; ++ch) {
        for (size_t p = 0; p != nPol; ++p) {
          std::complex<float> rndVal(dist(rnd), dist(rnd));
          // std::complex<float> rndVal(1.0, 0.0);
          float f = 1.0;
          // float f = float(a1+1) * float(a2+1) * float(p+1);
          factorSum += f;
          factorCount++;
          data[p + ch * nPol] = rndVal * f;

          std::complex<float> encodedVal(
              gausEncoder.Decode(
                  gausEncoder.EncodeWithDithering(rndVal.real(), dither(rnd))),
              gausEncoder.Decode(
                  gausEncoder.EncodeWithDithering(rndVal.imag(), dither(rnd))));
          unscaledRMS.Include(encodedVal - rndVal);
        }
      }
      buffer.SetData(blockRow, a1, a2, data.data());
      allData.push_back(data);
      ++blockRow;
    }
  }

  std::unique_ptr<TimeBlockEncoder> encoder =
      CreateEncoder(blockNormalization, nPol, nChan);

  const size_t nIter = 25;
  ao::uvector<float> metaBuffer(
      encoder->MetaDataCount(nRow, nPol, nChan, nAnt));
  ao::uvector<unsigned> symbolBuffer(
      encoder->SymbolCount(nAnt * (nAnt + 1) / 2));

  for (size_t i = 0; i != nIter; ++i)
    encoder->EncodeWithDithering(gausEncoder, buffer, metaBuffer.data(),
                                 symbolBuffer.data(), nAnt, rnd);

  for (size_t i = 0; i != nIter; ++i) {
    encoder->InitializeDecode(metaBuffer.data(), nRow, nAnt);
    blockRow = 0;
    for (size_t a1 = 0; a1 != nAnt; ++a1) {
      for (size_t a2 = a1; a2 != nAnt; ++a2) {
        ao::uvector<std::complex<float>> dataOut(nChan * nPol);
        encoder->Decode(gausEncoder, buffer, symbolBuffer.data(), blockRow, a1,
                        a2);
        buffer.GetData(blockRow, dataOut.data());
        ++blockRow;
      }
    }
  }

  RMSMeasurement mEncodingError, mData;
  size_t index = 0;
  encoder->InitializeDecode(metaBuffer.data(), nRow, nAnt);
  blockRow = 0;
  for (size_t a1 = 0; a1 != nAnt; ++a1) {
    for (size_t a2 = a1; a2 != nAnt; ++a2) {
      ao::uvector<std::complex<float>> dataOut(nChan * nPol);
      encoder->Decode(gausEncoder, buffer, symbolBuffer.data(), blockRow, a1,
                      a2);
      buffer.GetData(blockRow, dataOut.data());

      ao::uvector<std::complex<float>>& dataIn = allData[index];
      for (size_t i = 0; i != nPol * nChan; ++i) {
        mEncodingError.Include(dataOut[i] - dataIn[i]);
        mData.Include(dataIn[i]);
      }

      ++index;
      ++blockRow;
    }
  }
  BOOST_CHECK_LT(mEncodingError.RMS(), mData.RMS() * 0.1);
  /*std::cout << "Gaussian encoding error for unscaled values: " <<
  unscaledRMS.RMS() << '\n'; std::cout << "Average factor: " << factorSum /
  factorCount << '\n';
  std::cout << "Effective RMS of error: " << mEncoded.RMS() / (factorSum /
  factorCount) << " ( " << (mEncoded.RMS() / (factorSum / factorCount)) /
  unscaledRMS.RMS() << " x theoretical)\n";*/
}

TimeBlockBuffer<std::complex<float>> EncodeDecode(Normalization block_normalization, TimeBlockBuffer<std::complex<float>> &buffer, size_t n_pol, size_t n_chan, size_t n_ant) {
  const size_t n_row = buffer.NRows();
  StochasticEncoder<float> gausEncoder(256, 1.0, false);
  std::unique_ptr<TimeBlockEncoder> encoder =
      CreateEncoder(block_normalization, n_pol, n_chan);
  size_t metaDataCount = encoder->MetaDataCount(n_row, n_pol, n_chan, n_ant);
  size_t symbolCount = encoder->SymbolCount(n_row);
  ao::uvector<float> metaBuffer(metaDataCount);
  ao::uvector<TimeBlockEncoder::symbol_t> symbolBuffer(symbolCount);

  std::mt19937 mt;
  encoder->EncodeWithDithering(gausEncoder, buffer, metaBuffer.data(),
                               symbolBuffer.data(), n_ant, mt);
  return Decode(block_normalization, gausEncoder, n_ant, n_chan, n_pol, n_row,
             metaBuffer.data(), symbolBuffer.data());
}

}

BOOST_AUTO_TEST_CASE(row_normalization_per_row_accuracy) {
  TestSimpleExample(Normalization::kRow);
}

BOOST_AUTO_TEST_CASE(row_normalization_global_rms_accuracy) {
  TestTimeBlockEncoder(Normalization::kRow);
}

BOOST_AUTO_TEST_CASE(af_normalization_per_row_accuracy) {
  TestSimpleExample(Normalization::kAF);
}

BOOST_AUTO_TEST_CASE(af_normalization_global_rms_accuracy) {
  TestTimeBlockEncoder(Normalization::kAF);
}

BOOST_AUTO_TEST_CASE(rf_normalization_per_row_accuracy) {
  TestSimpleExample(Normalization::kRF);
}

BOOST_AUTO_TEST_CASE(rf_normalization_global_rms_accuracy) {
  TestTimeBlockEncoder(Normalization::kRF);
}

void TestZeroEncoding(Normalization block_normalization) {
  const size_t n_ant = 4, n_chan = 1, n_pol = 2;

  TimeBlockBuffer<std::complex<float>> buffer(n_pol, n_chan);

  std::vector<std::complex<float>> data(n_chan * n_pol, 0.0);
  size_t index = 0;
  for (size_t a1 = 0; a1 != n_ant; ++a1) {
    for (size_t a2 = a1; a2 != n_ant; ++a2) {
      buffer.SetData(index, a1, a2, data.data());
      ++index;
    }
  }

  const TimeBlockBuffer<std::complex<float>> out = EncodeDecode(block_normalization, buffer, n_pol, n_chan, n_ant);

  std::complex<float> dataFromOut[n_chan * n_pol];
  for (size_t row = 0; row != out.NRows(); row++) {
    // skip auto-correlations of AF, since these are not saved.
    out.GetData(row, dataFromOut);
    if (block_normalization != Normalization::kAF ||
        (row != 0 && row != 4 && row != 7 && row != 9)) {
      for (size_t ch = 0; ch != n_chan; ++ch) {
        BOOST_CHECK_MESSAGE(std::isfinite(dataFromOut[ch].real()),
                            "Real output{" << dataFromOut[ch]
                                           << "} is finite, row " << row
                                           << " with normalization "
                                           << int(block_normalization));
        BOOST_CHECK_MESSAGE(std::isfinite(dataFromOut[ch].imag()),
                            "Imaginary output{" << dataFromOut[ch]
                                                << "} is finite, row " << row
                                                << " with normalization "
                                                << int(block_normalization));
        BOOST_CHECK_EQUAL(dataFromOut[ch].real(), 0.0);
        BOOST_CHECK_EQUAL(dataFromOut[ch].imag(), 0.0);
      }
    }
  }
}

BOOST_AUTO_TEST_CASE(af_normalization_with_zeros) {
  TestZeroEncoding(Normalization::kAF);
}

BOOST_AUTO_TEST_CASE(rf_normalization_with_zeros) {
  TestZeroEncoding(Normalization::kRF);
}

BOOST_AUTO_TEST_CASE(row_normalization_with_zeros) {
  TestZeroEncoding(Normalization::kRow);
}

BOOST_AUTO_TEST_CASE(rf_dynamic_range) {
  constexpr size_t n_ant = 3;
  constexpr size_t n_chan = 3;
  constexpr size_t n_pol = 1;
  constexpr Normalization block_normalization = Normalization::kRF;

  TimeBlockBuffer<std::complex<float>> buffer(n_pol, n_chan);
  // row, ant1, ant2
  constexpr std::complex<float> normal_row_values[n_chan] = {{0.1, 0.1}, {1.0, 1.0}, {1.0, 1.0}};
  constexpr std::complex<float> weird_row_values[n_chan] = {{1.0, 1.0}, {1e8, 1e8}, {1e8, 1e8}};
  constexpr size_t weird_antenna = 1;
  size_t row = 0;
  for(size_t a1=0; a1!=n_ant; ++a1) {
    for(size_t a2=a1; a2!=n_ant; ++a2) {
      if(a1 == weird_antenna && a2 == weird_antenna)
        buffer.SetData(row, a1, a2, weird_row_values);
      else
        buffer.SetData(row, a1, a2, normal_row_values);
      ++row;
    }
  }

  const TimeBlockBuffer<std::complex<float>> out = EncodeDecode(block_normalization, buffer, n_pol, n_chan, n_ant);
  row = 0;
  for(size_t a1=0; a1!=n_ant; ++a1) {
    for(size_t a2=a1; a2!=n_ant; ++a2) {
      // skip auto-correlations of AF, since these are not saved.
      std::complex<float> out_data[n_chan * n_pol];
      out.GetData(row, out_data);
      for (size_t ch = 0; ch != n_chan; ++ch) {
        BOOST_CHECK_MESSAGE(std::isfinite(out_data[ch].real()),
                            "Real output{" << out_data[ch]
                                            << "} is finite, row " << row);
        BOOST_CHECK_MESSAGE(std::isfinite(out_data[ch].imag()),
                            "Imaginary output{" << out_data[ch]
                                                << "} is finite, row " << row);
        if(a1 != weird_antenna || a2 != weird_antenna) {
          const double expected = (ch == 0) ? 0.1 : 1.0;
          BOOST_CHECK_CLOSE_FRACTION(out_data[ch].real(), expected, 1e-4);
          BOOST_CHECK_CLOSE_FRACTION(out_data[ch].imag(), expected, 1e-4);
        }
        else if(ch != 0) {
          // We don't test channel 0 because it can (and may) have a large error, because
          // it is the only low value in this row.
          BOOST_CHECK_CLOSE_FRACTION(out_data[ch].real(), 1e8, 1e-4);
          BOOST_CHECK_CLOSE_FRACTION(out_data[ch].imag(), 1e8, 1e-4);
        }
      }
      ++row;
    }
  }
}

BOOST_AUTO_TEST_SUITE_END()
