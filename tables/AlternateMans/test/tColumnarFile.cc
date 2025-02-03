#define BOOST_TEST_MODULE alternate_mans
#define BOOST_TEST_DYN_LINK

#include <boost/test/unit_test.hpp>

#include <casacore/tables/AlternateMans/SimpleColumnarFile.h>
#include <casacore/tables/AlternateMans/BufferedColumnarFile.h>

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

  BOOST_CHECK_THROW(ColumnarFile::OpenExisting("This-is-not-an-existing-filename.nope", 0), std::runtime_error);

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

template<typename ColumnarFile>
void TestReadOnlyOpen() {
  using std::filesystem::permissions;
  using std::filesystem::perms;
  using std::filesystem::perm_options;
  
  constexpr size_t kColumnOffset = 6;
  const std::array<int32_t, 4> kRowData{1, 9, 8, 2};
  constexpr size_t kStride = kColumnOffset + kRowData.size() * sizeof(int32_t);
  constexpr size_t kHeader = 33;
  const std::string kFilename = "columnar_file_test_ro.tmp";
  // If an earlier test failed, there might still be an RO file with this
  // name on disk; make sure to remove it, otherwise CreateNew() fails.
  if(std::filesystem::exists(kFilename)) {
    permissions(kFilename, 
      perms::owner_write|perms::others_write|perms::group_write,
      perm_options::add);
    unlink(kFilename.c_str());
  }
  // Write a simple test file
  {
    ColumnarFile file = ColumnarFile::CreateNew(kFilename, kHeader, kStride);
    file.AddRows(37);
    file.Write(3, kColumnOffset, kRowData.data(), kRowData.size());
    file.Close();
  }
  permissions(kFilename, 
    perms::owner_write|perms::others_write|perms::group_write,
    perm_options::remove);
  
  // Check if we can read the RO file
  ColumnarFile file = ColumnarFile::OpenExisting(kFilename, kHeader);
  BOOST_CHECK_EQUAL(file.Stride(), kStride);
  BOOST_CHECK_EQUAL(file.NRows(), 37);
  std::array<int32_t, 4> data{0, 0, 0, 0};
  file.Read(3, kColumnOffset, data.data(), data.size());
  BOOST_CHECK_EQUAL_COLLECTIONS(kRowData.begin(), kRowData.end(), data.begin(), data.end());
  file.Close();
  
  // Overwriting an RO file should report an error
  BOOST_CHECK_THROW(ColumnarFile::CreateNew(kFilename, kHeader, kStride), std::runtime_error);
  
  // Updating an RO file should report an error. The error might not be throwed before calling close,
  // because the write actions might be buffered.
  const auto Update = [kFilename]()->void {
    ColumnarFile file = ColumnarFile::OpenExisting(kFilename, kHeader);
    std::array<int32_t, 4> data{1, 2, 3, 4};
    file.Write(3, kColumnOffset, data.data(), data.size());
    file.Close();
  };
  BOOST_CHECK_THROW(Update(), std::runtime_error);
  
  permissions(kFilename, 
    perms::owner_write|perms::others_write|perms::group_write,
    perm_options::add);
  unlink(kFilename.c_str());
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

BOOST_AUTO_TEST_CASE(read_only) {
  TestReadOnlyOpen<SimpleColumnarFile>();
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

BOOST_AUTO_TEST_CASE(read_only) {
  TestReadOnlyOpen<VarBufferedColumnarFile<1>>();
  TestReadOnlyOpen<VarBufferedColumnarFile<200>>();
}

BOOST_AUTO_TEST_CASE(buffered_file_edge_case) {
  constexpr size_t kColumnSize = sizeof(float) * 2;
  constexpr size_t kStride = kColumnSize * 2;
  const std::string filename = "columnar_file_test.tmp";
  casacore::VarBufferedColumnarFile file = casacore::VarBufferedColumnarFile<kStride*2>::CreateNew(filename, 0, kStride);
  const float values[2] = { 3, 4 };
  // Polute the buffer with some values
  file.Write(2, 0, values, 2);
  file.Write(3, 0, values, 2);
  file.Write(2, kColumnSize, values, 2);
  file.Write(3, kColumnSize, values, 2);
  // At this point, block 1 (rows 2-3) is activated.
  file.Write(5, 0, values, 2);
  // Now, block 2 (rows 4-5) is activated
  float result[2];
  file.Read(5, kColumnSize, result, 2);
  BOOST_CHECK_EQUAL(result[0], 0);
  BOOST_CHECK_EQUAL(result[1], 0);
  file.Read(5, 0, result, 2);
  BOOST_CHECK_EQUAL(result[0], 3);
  BOOST_CHECK_EQUAL(result[1], 4);
  file.Close();
  unlink(filename.c_str());
}

BOOST_AUTO_TEST_SUITE_END()
