#include "../aftimeblockencoder.h"

#include <random>

#include <boost/test/unit_test.hpp>

using namespace dyscostman;

BOOST_AUTO_TEST_SUITE(timeblock_encoder)

BOOST_AUTO_TEST_CASE( row_normalization_per_row_accuracy )
{
  size_t nPol = 4, nChan = 10, nAntenna = 100, nRow = (nAntenna*(nAntenna+1))/2;
  std::mt19937 rnd;
	AFTimeBlockEncoder encoder(nPol, nChan, true);
	StochasticEncoder<float> gausEncoder(1024, 1.0, false);
  TimeBlockBuffer<std::complex<float>> buffer(nPol, nChan);
  std::vector<float> metaBuffer(encoder.MetaDataCount(nRow, nPol, nChan, nAntenna));
  std::vector<AFTimeBlockEncoder::symbol_t> symbolBuffer;
  
  for(size_t ant1=0; ant1!=nAntenna; ++ant1)
  {
    for(size_t ant2=ant1; ant2!=nAntenna; ++ant2)
    {
      buffer[row].visibilities;
    }
  }
  
  encoder.EncodeWithDithering(gausEncoder, buffer, metaBuffer.data(), symbolBuffer.data(), nAntenna, rnd);
}

BOOST_AUTO_TEST_SUITE_END()
