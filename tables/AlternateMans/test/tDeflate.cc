#include <casacore/tables/AlternateMans/Deflate.h>

#include <random>

#include <boost/test/unit_test.hpp>

namespace casacore {

BOOST_AUTO_TEST_SUITE(distribution_example)

BOOST_AUTO_TEST_CASE(compress_and_uncompress) {
  std::mt19937_64 rnd;
  std::uniform_int_distribution<unsigned char> distribution(0, 255);
  constexpr size_t kBufferSize = 100*1024;
  std::vector<std::byte> input(kBufferSize);
  for(std::byte& value : input)
    value = std::byte(distribution(rnd));

  deflate::Compressor compressor(9);
  std::vector<std::byte> output(compressor.CompressBound(kBufferSize));
  const size_t result = compressor.Compress(input, output);
  BOOST_CHECK_LE(result, output.size());

  deflate::Decompressor decompressor;
  std::vector<std::byte> decompressed(kBufferSize);
  const size_t decompressed_size = decompressor.Decompress(output, decompressed);
  BOOST_CHECK_EQUAL(decompressed_size, kBufferSize);
  BOOST_CHECK(input == decompressed);
}

BOOST_AUTO_TEST_CASE(compression_test) {
  std::mt19937_64 rnd;
  std::uniform_int_distribution<unsigned char> distribution(0, 3);
  std::normal_distribution<double> gaus_distribution(127.5, 25);
  constexpr size_t kBufferSize = 1024 * 1024;
  std::vector<std::byte> buffer(kBufferSize);
  for(std::byte& value : buffer) {
    value = std::byte(distribution(rnd)*16);
  }
  deflate::Compressor compressor(9);
  std::vector<std::byte> output(compressor.CompressBound(kBufferSize));
  const size_t result = compressor.Compress(buffer, output);
  BOOST_CHECK_LT(result, kBufferSize/2);
}

/**
 * This is a demonstration of the effectiviness of predictive compression, using
 * the sin function as input.
 */
BOOST_AUTO_TEST_CASE(sin_compression_test, *boost::unit_test::disabled()) {
  constexpr size_t kBufferSize = 1024 * 1024;
  std::vector<int16_t> value_buffer(kBufferSize);
  std::vector<int16_t> delta_buffer(kBufferSize);
  std::vector<int16_t> predicted_buffer(kBufferSize);
  for(int i=0; i!=kBufferSize; ++i) {
    constexpr double f = 0.01;
    const int value = std::sin(i*f)*32767.5;
    const int previous = std::sin((i-1)*f)*32767.5;
    const int prevprev = std::sin((i-2)*f)*32767.5;
    const int predicted = 2 * previous - prevprev;
    value_buffer[i] = value;
    delta_buffer[i] = value - previous;
    predicted_buffer[i] = value - predicted;
    if(i < 30)
      std::cout << value_buffer[i] << " , " << delta_buffer[i] << " , " << predicted_buffer[i] << '\n';
  }
  deflate::Compressor compressor(9);
  const size_t n_bytes = kBufferSize*sizeof(uint16_t);
  std::vector<std::byte> output(compressor.CompressBound(n_bytes));

  const std::byte* input = reinterpret_cast<const std::byte*>(value_buffer.data());
  size_t result = compressor.Compress(std::span<const std::byte>(input, n_bytes), output);
  std::cout << "Value: " << result << " bytes, " << std::round(result*1000.0/n_bytes)/10.0 << " %\n";

  input = reinterpret_cast<const std::byte*>(delta_buffer.data());
  result = compressor.Compress(std::span<const std::byte>(input, n_bytes), output);
  std::cout << "Delta: " << result << " bytes, " << std::round(result*1000.0/n_bytes)/10.0 << " %\n";

  input = reinterpret_cast<const std::byte*>(predicted_buffer.data());
  result = compressor.Compress(std::span<const std::byte>(input, n_bytes), output);
  std::cout << "Predicted: " << result << " bytes, " << std::round(result*1000.0/n_bytes)/10.0 << " %\n";
}

BOOST_AUTO_TEST_SUITE_END()

} // namespace casacore
