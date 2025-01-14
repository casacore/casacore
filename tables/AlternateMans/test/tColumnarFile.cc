#include <boost/test/unit_test.hpp>

#include "../StokesIStMan/SimpleColumnarFile.h"
#include "../StokesIStMan/BufferedColumnarFile.h"

using casacore::VarBufferedColumnarFile;
using casacore::SimpleColumnarFile;

template<typename ColumnarFile>
void TestEmptyConstructor() {
  ColumnarFile file_a;
  BOOST_CHECK_EQUAL(file_a.NRows(), 0);
  BOOST_CHECK_EQUAL(file_a.Filename(), "");
  BOOST_CHECK(!file_a.IsOpen());

  const std::string filename = "columnar_file_test.tmp";
  ColumnarFile file_b = ColumnarFile::CreateNew(filename, 0, 0);
  file_b = std::move(file_a);
  BOOST_CHECK_EQUAL(file_b.NRows(), 0);
  BOOST_CHECK_EQUAL(file_b.Filename(), "");
  BOOST_CHECK(!file_b.IsOpen());

  unlink(filename.c_str());
}

template<typename ColumnarFile>
void TestCreateAndOpen() {
  constexpr size_t kStrideA = 10;
  const std::string filename = "columnar_file_test.tmp";
  {
    ColumnarFile file = ColumnarFile::CreateNew(filename, 0, kStrideA);
    BOOST_CHECK_EQUAL(file.Stride(), kStrideA);
    BOOST_CHECK_EQUAL(file.NRows(), 0);
    BOOST_CHECK_EQUAL(file.Filename(), filename);
    BOOST_CHECK(file.IsOpen());
  }

  // See if re-creating works
  constexpr size_t kStrideB = 20;
  ColumnarFile file = ColumnarFile::CreateNew(filename, 0, kStrideB);
  BOOST_CHECK_EQUAL(file.Stride(), kStrideB);
  BOOST_CHECK_EQUAL(file.NRows(), 0);
  BOOST_CHECK_EQUAL(file.Filename(), filename);
  BOOST_CHECK(file.IsOpen());
  file.AddRows(43);
  BOOST_CHECK_EQUAL(file.NRows(), 43);
  file.DeleteRow();
  BOOST_CHECK_EQUAL(file.NRows(), 42);
  BOOST_CHECK(file.IsOpen());
  // Close by assigning to empty
  file = ColumnarFile();

  file = ColumnarFile::OpenExisting(filename, 0);
  BOOST_CHECK_EQUAL(file.Stride(), kStrideB);
  BOOST_CHECK_EQUAL(file.NRows(), 42);
  BOOST_CHECK_EQUAL(file.Filename(), filename);
  BOOST_CHECK(file.IsOpen());
  file.AddRows(8);
  BOOST_CHECK_EQUAL(file.NRows(), 50);
  file.Close();
  BOOST_CHECK(!file.IsOpen());

  // Check if update worked
  file = ColumnarFile::OpenExisting(filename, 0);
  BOOST_CHECK_EQUAL(file.Stride(), kStrideB);
  BOOST_CHECK_EQUAL(file.NRows(), 50);
  file.Close();

  unlink(filename.c_str());
}

template<typename ColumnarFile>
void TestReadAndWrite() {
  // Test a file with two columns: one is 14 complex floats, the other 3.
  std::array<std::complex<float>, 14> data_a;
  std::array<std::complex<float>, 3> data_b;
  constexpr size_t kStride = sizeof(data_a) + sizeof(data_b);
  const std::string filename = "columnar_file_test.tmp";
  const std::array<unsigned char, 4> header = {1, 9, 8, 2};
  ColumnarFile file = ColumnarFile::CreateNew(filename, header.size(), kStride);
  file.WriteHeader(header.data());
  BOOST_CHECK_EQUAL(file.Stride(), 17*8);

  // Generate random data
  for(size_t i=0; i!=data_a.size(); ++i)
      data_a[i] = std::complex<float>(i+3, i*2);
  for(size_t i=0; i!=data_b.size(); ++i)
      data_b[i] = std::complex<float>(i*3+41, i*-2.0);

  file.Write(2, 0, data_a.data(), data_a.size());
  file.Write(2, sizeof(data_a), data_b.data(), data_b.size());
  BOOST_CHECK_EQUAL(file.NRows(), 3);

  std::array<unsigned char, 4> header_buffer;
  std::fill(header_buffer.begin(), header_buffer.end(), 0);
  file.ReadHeader(header_buffer.data());
  BOOST_CHECK_EQUAL_COLLECTIONS(header_buffer.begin(), header_buffer.end(), header.begin(), header.end());
  
  std::array<std::complex<float>, data_a.size()> read_buffer;
  file.Read(2, 0, read_buffer.data(), data_a.size());
  BOOST_CHECK_EQUAL_COLLECTIONS(read_buffer.begin(), read_buffer.end(), data_a.begin(), data_a.end());
  file.Read(2, sizeof(data_a), read_buffer.data(), data_b.size());
  BOOST_CHECK_EQUAL_COLLECTIONS(read_buffer.begin(), read_buffer.begin() + data_b.size(), data_b.begin(), data_b.end());

  file.Read(1, 0, read_buffer.data(), data_a.size());
  BOOST_CHECK(std::all_of(read_buffer.begin(), read_buffer.end(), [](std::complex<float> v) {return v == 0.0f;} ));

  file.Close();
  
  file = ColumnarFile::OpenExisting(filename, 4);
  std::fill(header_buffer.begin(), header_buffer.end(), 0);
  file.ReadHeader(header_buffer.data());
  BOOST_CHECK_EQUAL_COLLECTIONS(header_buffer.begin(), header_buffer.end(), header.begin(), header.end());
  
  file.Read(0, 0, read_buffer.data(), data_a.size());
  BOOST_CHECK(std::all_of(read_buffer.begin(), read_buffer.end(), [](std::complex<float> v) {return v == 0.0f;} ));
  file.Read(2, 0, read_buffer.data(), data_a.size());
  BOOST_CHECK_EQUAL_COLLECTIONS(read_buffer.begin(), read_buffer.end(), data_a.begin(), data_a.end());
  file.Read(2, sizeof(data_a), read_buffer.data(), data_b.size());
  BOOST_CHECK_EQUAL_COLLECTIONS(read_buffer.begin(), read_buffer.begin() + data_b.size(), data_b.begin(), data_b.end());

  std::fill(read_buffer.begin(), read_buffer.end(), 0.0f);
  file.Write(2, sizeof(data_a), read_buffer.data(), data_b.size());
  file.Read(2, 0, read_buffer.data(), data_a.size());
  BOOST_CHECK_EQUAL_COLLECTIONS(read_buffer.begin(), read_buffer.end(), data_a.begin(), data_a.end());
  std::fill(read_buffer.begin(), read_buffer.end(), 1.0f);
  file.Read(2, sizeof(data_a), read_buffer.data(), data_b.size());
  BOOST_CHECK(std::all_of(read_buffer.begin(), read_buffer.begin() + data_b.size(), [](std::complex<float> v) {return v == 0.0f;} ));
  BOOST_CHECK(std::all_of(read_buffer.begin() + data_b.size(), read_buffer.end(), [](std::complex<float> v) {return v == 1.0f;} ));
  unlink(filename.c_str());
}

BOOST_AUTO_TEST_SUITE(simple_columnar_file)

BOOST_AUTO_TEST_CASE(empty_constructor) {
  TestEmptyConstructor<SimpleColumnarFile>();
}

BOOST_AUTO_TEST_CASE(create_and_open_file) {
  TestCreateAndOpen<SimpleColumnarFile>();
}

BOOST_AUTO_TEST_CASE(read_and_write) {
  TestReadAndWrite<SimpleColumnarFile>();
}

BOOST_AUTO_TEST_SUITE_END()


BOOST_AUTO_TEST_SUITE(buffered_columnar_file)

BOOST_AUTO_TEST_CASE(empty_constructor) {
  TestEmptyConstructor<VarBufferedColumnarFile<1>>();
  TestEmptyConstructor<VarBufferedColumnarFile<200>>();
}

BOOST_AUTO_TEST_CASE(create_and_open_file) {
  TestCreateAndOpen<VarBufferedColumnarFile<1>>();
  TestCreateAndOpen<VarBufferedColumnarFile<200>>();
}

BOOST_AUTO_TEST_CASE(read_and_write) {
  TestReadAndWrite<VarBufferedColumnarFile<1>>();
  TestReadAndWrite<VarBufferedColumnarFile<200>>();
}

BOOST_AUTO_TEST_SUITE_END()
