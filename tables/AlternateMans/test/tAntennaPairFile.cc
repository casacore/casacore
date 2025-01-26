#include <boost/test/unit_test.hpp>

#include <casacore/tables/AlternateMans/AntennaPairFile.h>

using casacore::AntennaPairFile;

namespace {
const std::string kFilename = "antenna_pair_file_test.tmp";
} // namespace

BOOST_AUTO_TEST_SUITE(antenna_pair_file)

BOOST_AUTO_TEST_CASE(empty_file) {
  AntennaPairFile file;
  BOOST_CHECK_EQUAL(file.Filename(), "");
  BOOST_CHECK_EQUAL(file.NRowsInPattern(), 0);

  file = AntennaPairFile::CreateNew(kFilename);
  BOOST_CHECK_EQUAL(file.Filename(), kFilename);
  BOOST_CHECK_EQUAL(file.NRowsInPattern(), 0);
  file.Close();

  file = AntennaPairFile::OpenExisting(kFilename);
  BOOST_CHECK_EQUAL(file.Filename(), kFilename);
  BOOST_CHECK_EQUAL(file.NRowsInPattern(), 0);
  file.Close();

  unlink(kFilename.c_str());
}

BOOST_AUTO_TEST_CASE(conisistency_checking) {
  AntennaPairFile file = AntennaPairFile::CreateNew(kFilename);

  BOOST_CHECK_THROW(file.ReadAntenna1(0), std::runtime_error);
  BOOST_CHECK_THROW(file.ReadAntenna2(0), std::runtime_error);

  // If we don't start writing at the start, an exception should be thrown
  BOOST_CHECK_THROW(file.WriteAntenna1(2, 0), std::runtime_error);
  BOOST_CHECK_THROW(file.ReadAntenna1(0), std::runtime_error);
  BOOST_CHECK_THROW(file.ReadAntenna2(0), std::runtime_error);
  BOOST_CHECK_THROW(file.ReadAntenna1(2), std::runtime_error);
  BOOST_CHECK_THROW(file.ReadAntenna2(2), std::runtime_error);

  file.WriteAntenna1(0, 3);
  BOOST_CHECK_EQUAL(file.ReadAntenna1(0), 3);
  BOOST_CHECK_THROW(file.ReadAntenna2(0), std::runtime_error);

  // Rewriting a value with the same value should be allowed
  file.WriteAntenna1(0, 3);
  BOOST_CHECK_EQUAL(file.ReadAntenna1(0), 3);
  BOOST_CHECK_THROW(file.ReadAntenna2(0), std::runtime_error);

  // ...but not with a different value
  BOOST_CHECK_THROW(file.WriteAntenna1(0, 4), std::runtime_error);
  BOOST_CHECK_EQUAL(file.ReadAntenna1(0), 3);
  BOOST_CHECK_THROW(file.ReadAntenna2(0), std::runtime_error);

  // Writing the next row without finishing this one is not allowed
  // (this is because the pattern can not be discovered without both
  // values written -- one should not e.g. write first all ANTENNA1
  // values, because then all values would have to be buffered).
  BOOST_CHECK_THROW(file.WriteAntenna1(1, 3), std::runtime_error);
  BOOST_CHECK_THROW(file.ReadAntenna1(1), std::runtime_error);
  BOOST_CHECK_THROW(file.ReadAntenna2(1), std::runtime_error);

  file.WriteAntenna2(0, 0);

  // This time write antenna 2 for the next row first
  file.WriteAntenna2(1, 1);
  BOOST_CHECK_EQUAL(file.ReadAntenna2(1), 1);
  BOOST_CHECK_THROW(file.ReadAntenna1(1), std::runtime_error);

  // Test rewriting antenna 2
  file.WriteAntenna2(1, 1);
  BOOST_CHECK_EQUAL(file.ReadAntenna2(1), 1);
  BOOST_CHECK_THROW(file.ReadAntenna1(1), std::runtime_error);

  BOOST_CHECK_THROW(file.WriteAntenna2(1, 0), std::runtime_error);
  BOOST_CHECK_EQUAL(file.ReadAntenna2(1), 1);
  BOOST_CHECK_THROW(file.ReadAntenna1(1), std::runtime_error);

  // Writing next row is not allowed until antenna 1 is written
  BOOST_CHECK_THROW(file.WriteAntenna2(2, 2), std::runtime_error);
  BOOST_CHECK_THROW(file.ReadAntenna2(2), std::runtime_error);
  BOOST_CHECK_THROW(file.ReadAntenna1(2), std::runtime_error);

  file.WriteAntenna1(1, 3);

  // Check all values written so far
  BOOST_CHECK_EQUAL(file.ReadAntenna1(0), 3);
  BOOST_CHECK_EQUAL(file.ReadAntenna2(0), 0);
  BOOST_CHECK_EQUAL(file.ReadAntenna1(1), 3);
  BOOST_CHECK_EQUAL(file.ReadAntenna2(1), 1);

  // Finish the pattern by starting a new pattern with the same first row
  file.WriteAntenna2(2, 0);
  BOOST_CHECK_EQUAL(file.ReadAntenna2(2), 0);
  file.WriteAntenna1(2, 3);
  BOOST_CHECK_EQUAL(file.ReadAntenna1(2), 3);
  BOOST_CHECK_EQUAL(file.NRowsInPattern(), 2);

  // Writing further patterns should be allowed (as long as they are consistent)
  file.WritePair(3, 3, 1);
  file.WritePair(4, 3, 0);
  BOOST_CHECK_EQUAL(file.NRowsInPattern(), 2);

  // Writing inconsistent patterns should throw
  BOOST_CHECK_THROW(file.WriteAntenna1(5, 1), std::runtime_error); // should be 3
  BOOST_CHECK_THROW(file.WriteAntenna2(5, 3), std::runtime_error); // should be 1

  // Reading any further rows should return the same pattern
  BOOST_CHECK_EQUAL(file.ReadAntenna1(6), 3);
  BOOST_CHECK_EQUAL(file.ReadAntenna2(6), 0);
  BOOST_CHECK_EQUAL(file.ReadAntenna1(7), 3);
  BOOST_CHECK_EQUAL(file.ReadAntenna2(7), 1);

  file.Close();
  unlink(kFilename.c_str());
}

BOOST_AUTO_TEST_CASE(write_and_read) {
  // Define pirate language
  using Arr = std::array<int32_t, 2>;
  const std::array<Arr, 11> data = {
    Arr{ 0, 1 },
    Arr{ 0, 2 },
    Arr{ 0, 3 },
    Arr{ 0, 5 },
    Arr{ 1, 2 },
    Arr{ 1, 3 },
    Arr{ 1, 5 },
    Arr{ 2, 3 },
    Arr{ 2, 5 },
    Arr{ 3, 5 }
  };

  {
    AntennaPairFile file = AntennaPairFile::CreateNew(kFilename);
    // Only write 4 lines, to see if we can continue an unfinished file
    for(size_t i=0; i!=4; ++i) {
      file.WritePair(i, data[i][0], data[i][1]);
    }
  }

  {
    AntennaPairFile file = AntennaPairFile::OpenExisting(kFilename);
    for(size_t i=0; i!=4; ++i) {
      BOOST_CHECK_EQUAL(file.ReadAntenna1(i), data[i][0]);
      BOOST_CHECK_EQUAL(file.ReadAntenna2(i), data[i][1]);
    }
    BOOST_CHECK_THROW(file.ReadAntenna1(4), std::runtime_error);
    BOOST_CHECK_THROW(file.ReadAntenna2(4), std::runtime_error);
  }

  {
    AntennaPairFile file = AntennaPairFile::OpenExisting(kFilename);
    uint64_t row = 0;
    const size_t repeat_count = 3;
    for(size_t repeat = 0; repeat != repeat_count; ++repeat) {
      for(const Arr& row_data : data) {
        file.WritePair(row, row_data[0], row_data[1]);
        ++row;
      }
    }
    BOOST_CHECK_EQUAL(file.NRowsInPattern(), data.size());
  }

  {
    AntennaPairFile file = AntennaPairFile::OpenExisting(kFilename);
    BOOST_CHECK_EQUAL(file.NRowsInPattern(), data.size());
    uint64_t row = 0;
    const size_t repeat_count = 4;
    for(size_t repeat = 0; repeat != repeat_count; ++repeat) {
      for(const Arr& row_data : data) {
        BOOST_CHECK_EQUAL(file.ReadAntenna1(row), row_data[0]);
        BOOST_CHECK_EQUAL(file.ReadAntenna2(row), row_data[1]);
        ++row;
      }
    }
  }

  unlink(kFilename.c_str());
}


BOOST_AUTO_TEST_SUITE_END()
