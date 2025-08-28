#include <casacore/tables/AlternateMans/SiscoReader.h>
#include <casacore/tables/AlternateMans/SiscoWriter.h>

#include <boost/test/unit_test.hpp>

#include <filesystem>

namespace casacore::sisco {

namespace {
const std::string kFilename = "test-tmp.tmp";
} // namespace

BOOST_AUTO_TEST_SUITE(sisco_writer)

struct FileFixture {
  FileFixture() {
    std::filesystem::remove(kFilename);
  }
  ~FileFixture() {
    //std::filesystem::remove(kFilename);
  }
};

BOOST_FIXTURE_TEST_CASE(construct, FileFixture) {
  {
    SiscoWriter writer(kFilename, 2, 9);
    writer.Open(std::span<std::byte>());
  }
  BOOST_CHECK(std::filesystem::exists(kFilename));
  {
    // Recreate file
    SiscoWriter writer(kFilename, 1, 1);
    writer.Open(std::span<std::byte>());
  }
  BOOST_CHECK(std::filesystem::exists(kFilename));
}

BOOST_FIXTURE_TEST_CASE(write, FileFixture) {
  constexpr size_t kHeaderSize = 20;
  // 20 bytes string, nullptr not included
  constexpr char kHeader[] = "This is a testfile  ";
  const std::vector<std::complex<float>> data_0(100, 3.14f);
  const std::vector<std::complex<float>> data_1(75, 1982.0f);
  const std::vector<std::complex<float>> data_2(1, 42.0f);
  {
    SiscoWriter writer(kFilename, 2, 9);
    writer.Open(std::span(reinterpret_cast<const std::byte*>(kHeader), kHeaderSize));
    writer.Write(0, data_0);
    writer.Write(1, data_1);
    writer.Write(0, data_0);
    writer.Write(0, data_0);
    writer.Write(2, data_2);
    writer.Write(1, data_1);
    writer.Write(0, data_0);
    writer.Write(2, data_2);
    writer.Write(2, data_2);
  }

  SiscoReader reader(kFilename);
  char read_header[kHeaderSize];
  reader.Open(std::span(reinterpret_cast<std::byte*>(read_header), kHeaderSize));
  BOOST_CHECK_EQUAL_COLLECTIONS(read_header, read_header+kHeaderSize, kHeader, kHeader+kHeaderSize);
  
  std::vector<std::complex<float>> result_0(100);
  std::vector<std::complex<float>> result_1(75);
  std::vector<std::complex<float>> result_2(1);
  reader.Read(0, result_0);
  BOOST_CHECK_EQUAL_COLLECTIONS(data_0.begin(), data_0.end(), result_0.begin(), result_0.end());
  reader.Read(1, result_1);
  BOOST_CHECK_EQUAL_COLLECTIONS(data_1.begin(), data_1.end(), result_1.begin(), result_1.end());
  reader.Read(0, result_0);
  BOOST_CHECK_EQUAL_COLLECTIONS(data_0.begin(), data_0.end(), result_0.begin(), result_0.end());
  reader.Read(0, result_0);
  BOOST_CHECK_EQUAL_COLLECTIONS(data_0.begin(), data_0.end(), result_0.begin(), result_0.end());
  reader.Read(2, result_2);
  BOOST_CHECK_EQUAL_COLLECTIONS(data_2.begin(), data_2.end(), result_2.begin(), result_2.end());
  reader.Read(1, result_1);
  BOOST_CHECK_EQUAL_COLLECTIONS(data_1.begin(), data_1.end(), result_1.begin(), result_1.end());
  reader.Read(0, result_0);
  BOOST_CHECK_EQUAL_COLLECTIONS(data_0.begin(), data_0.end(), result_0.begin(), result_0.end());
  reader.Read(2, result_2);
  BOOST_CHECK_EQUAL_COLLECTIONS(data_2.begin(), data_2.end(), result_2.begin(), result_2.end());
  reader.Read(2, result_2);
  BOOST_CHECK_EQUAL_COLLECTIONS(data_2.begin(), data_2.end(), result_2.begin(), result_2.end());
}

BOOST_AUTO_TEST_SUITE_END()

} // namespace casacore::sisco
