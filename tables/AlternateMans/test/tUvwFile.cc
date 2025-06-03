#include <boost/test/unit_test.hpp>

#include <casacore/tables/AlternateMans/UvwFile.h>

using casacore::UvwFile;

namespace {
const std::string kFilename = "uvw_file_test.tmp";
const std::array<double, 3> kAnts[3] ={ { 7, 13, 17 }, { 19, 23, 29 }, { 5, 3, 31 } };
constexpr size_t kNBaselines = 6;
const std::array<std::size_t, 2> kBaselines[kNBaselines] = {
  {0, 0},
  {0, 1},
  {0, 2},
  {1, 1},
  {1, 2},
  {2, 2}
};
const std::array<std::size_t, 2> kBaselinesSwapped[kNBaselines] = {
  {0, 0},
  {1, 0},
  {2, 0},
  {1, 1},
  {2, 1},
  {2, 2}
};
const std::array<double, 3> kUvwPerBaseline[kNBaselines] = {
  {0.0, 0.0, 0.0},
  {kAnts[1][0] - kAnts[0][0], kAnts[1][1] - kAnts[0][1], kAnts[1][2] - kAnts[0][2] },
  {kAnts[2][0] - kAnts[0][0], kAnts[2][1] - kAnts[0][1], kAnts[2][2] - kAnts[0][2] },
  {0.0, 0.0, 0.0},
  {kAnts[2][0] - kAnts[1][0], kAnts[2][1] - kAnts[1][1], kAnts[2][2] - kAnts[1][2] },
  {0.0, 0.0, 0.0}
};


const std::array<std::size_t, 2> kBaselinesMissingAntenna[kNBaselines] = {
  {1, 1},
  {1, 3},
  {1, 4},
  {3, 3},
  {3, 4},
  {4, 4}
};
}

BOOST_AUTO_TEST_SUITE(uvw_file)

BOOST_AUTO_TEST_CASE(create_empty) {
  UvwFile file_a = UvwFile::CreateNew(kFilename);
  BOOST_CHECK_EQUAL(file_a.NRows(), 0);
  BOOST_CHECK_EQUAL(file_a.Filename(), kFilename);
  file_a.Close();

  UvwFile file_b = UvwFile::CreateNew(kFilename);
  file_b = std::move(file_a);
  BOOST_CHECK_EQUAL(file_b.NRows(), 0);
  BOOST_CHECK_EQUAL(file_b.Filename(), "");
  file_b.Close();

  unlink(kFilename.c_str());
}

BOOST_AUTO_TEST_CASE(open_empty) {
  UvwFile file = UvwFile::CreateNew(kFilename);
  BOOST_CHECK_EQUAL(file.NRows(), 0);
  BOOST_CHECK_EQUAL(file.Filename(), kFilename);
  file.Close();

  file = UvwFile::OpenExisting(kFilename);
  BOOST_CHECK_EQUAL(file.NRows(), 0);
  BOOST_CHECK_EQUAL(file.Filename(), kFilename);
  file.Close();

  unlink(kFilename.c_str());
}

void WriteTwoTimesteps(UvwFile& file, const std::array<std::size_t, 2>* baselines, bool with_autos) {
  uint64_t row = 0;
  for(uint64_t i=0; i!=kNBaselines; ++i) {
    if(with_autos || baselines[i][0] != baselines[i][1]) {
      const std::array<double, 3>& uvw = kUvwPerBaseline[i];
      file.WriteUvw(row, baselines[i][0], baselines[i][1], uvw.data());
      BOOST_CHECK_EQUAL(file.NRows(), row+1);
      ++row;
    }
  }
  for(uint64_t i=0; i!=kNBaselines; ++i) {
    if(with_autos || baselines[i][0] != baselines[i][1]) {
      std::array<double, 3> uvw = kUvwPerBaseline[i];
      uvw[0] *= 2.0;
      uvw[1] *= 3.0;
      uvw[2] *= 4.0;
      file.WriteUvw(row, baselines[i][0], baselines[i][1], uvw.data());
      BOOST_CHECK_EQUAL(file.NRows(), row+1);
      ++row;
    }
  }
}

void ReadTwoTimesteps(UvwFile& file, const std::array<std::size_t, 2>* baselines, bool with_autos) {
  // Read the second timestep first to see if seeking works ok.
  uint64_t row = with_autos ? kNBaselines : 3;
  for(uint64_t i=0; i!=kNBaselines; ++i) {
    if(with_autos || baselines[i][0] != baselines[i][1]) {
      std::array<double, 3> uvw;
      file.ReadUvw(row, baselines[i][0], baselines[i][1], uvw.data());
      std::array<double, 3> expected_uvw = kUvwPerBaseline[i];
      expected_uvw[0] *= 2.0;
      expected_uvw[1] *= 3.0;
      expected_uvw[2] *= 4.0;
      for(size_t z=0; z!=3; ++z)
        BOOST_CHECK_CLOSE_FRACTION(expected_uvw[z], uvw[z], 1e-6);
      ++row;
    }
  }
  row = 0;
  for(uint64_t i=0; i!=kNBaselines; ++i) {
    if(with_autos || baselines[i][0] != baselines[i][1]) {
      std::array<double, 3> uvw;
      file.ReadUvw(row, baselines[i][0], baselines[i][1], uvw.data());
      for(size_t z=0; z!=3; ++z)
        BOOST_CHECK_CLOSE_FRACTION(kUvwPerBaseline[i][z], uvw[z], 1e-6);
      ++row;
    }
  }
}

BOOST_AUTO_TEST_CASE(write_and_direct_read) {
  UvwFile file = UvwFile::CreateNew(kFilename);
  WriteTwoTimesteps(file, kBaselines, true);
  ReadTwoTimesteps(file, kBaselines, true);
  file.Close();
  unlink(kFilename.c_str());
}

BOOST_AUTO_TEST_CASE(write_and_read_after_reopen) {
  {
    UvwFile file = UvwFile::CreateNew(kFilename);
    WriteTwoTimesteps(file, kBaselines, true);
  }

  UvwFile file = UvwFile::OpenExisting(kFilename);
  ReadTwoTimesteps(file, kBaselines, true);

  file.Close();
  unlink(kFilename.c_str());
}

BOOST_AUTO_TEST_CASE(without_autocorrelations) {
  UvwFile file = UvwFile::CreateNew(kFilename);
  WriteTwoTimesteps(file, kBaselines, false);
  ReadTwoTimesteps(file, kBaselines, false);
  file.Close();
  unlink(kFilename.c_str());
}

BOOST_AUTO_TEST_CASE(missing_antenna) {
  UvwFile file = UvwFile::CreateNew(kFilename);
  WriteTwoTimesteps(file, kBaselinesMissingAntenna, true);
  ReadTwoTimesteps(file, kBaselinesMissingAntenna, true);
  file.Close();
  file = UvwFile::CreateNew(kFilename);
  WriteTwoTimesteps(file, kBaselinesMissingAntenna, false);
  ReadTwoTimesteps(file, kBaselinesMissingAntenna, false);
  file.Close();
  unlink(kFilename.c_str());
}

BOOST_AUTO_TEST_CASE(invalid_baselines) {
  UvwFile file = UvwFile::CreateNew(kFilename);
  file.WriteUvw(0, 0, 1, kUvwPerBaseline[0].data());
  BOOST_CHECK_THROW(
    file.WriteUvw(1, 2, 3, kUvwPerBaseline[1].data()),
    std::runtime_error);
  file.Close();
  unlink(kFilename.c_str());
}

BOOST_AUTO_TEST_CASE(non_standard_ordering) {
  UvwFile file = UvwFile::CreateNew(kFilename);
  WriteTwoTimesteps(file, kBaselinesSwapped, true);
  ReadTwoTimesteps(file, kBaselinesSwapped, true);
  file.Close();
  file = UvwFile::CreateNew(kFilename);
  WriteTwoTimesteps(file, kBaselinesSwapped, false);
  ReadTwoTimesteps(file, kBaselinesSwapped, false);
  file.Close();
  unlink(kFilename.c_str());
}

BOOST_AUTO_TEST_CASE(same_positions) {
  UvwFile file = UvwFile::CreateNew(kFilename);
  const std::array<double, 3> zero_uvw = {0.0, 0.0, 0.0};
  for(uint64_t i=0; i!=kNBaselines; ++i) {
    file.WriteUvw(i, kBaselines[i][0], kBaselines[i][1], zero_uvw.data());
  }
  for(uint64_t i=0; i!=kNBaselines; ++i) {
    std::array<double, 3> uvw = {1.0, 1.0, 1.0};
    file.ReadUvw(i, kBaselines[i][0], kBaselines[i][1], uvw.data());
      for(size_t z=0; z!=3; ++z)
        BOOST_CHECK_LT(std::fabs(uvw[z]), 1e-6);
  }
  file.Close();
  unlink(kFilename.c_str());
}

BOOST_AUTO_TEST_CASE(only_one_auto_correlation_baseline) {
  UvwFile file = UvwFile::CreateNew(kFilename);
  constexpr size_t kNRows = 10;
  const std::array<double, 3> zero_uvw = {0.0, 0.0, 0.0};
  for(uint64_t i=0; i!=kNRows; ++i) {
    file.WriteUvw(i, 0, 0, zero_uvw.data());
  }
  file.Close();

  file = UvwFile::OpenExisting(kFilename);
  BOOST_REQUIRE_EQUAL(file.NRows(), kNRows);
  for(size_t row=0; row!=kNRows; ++row) {
    std::array<double, 3> uvw = {-1, -1, -1};
    file.ReadUvw(row, 0, 0, uvw.data());
    BOOST_CHECK(uvw == zero_uvw);
  }

  file.Close();
  unlink(kFilename.c_str());
}

BOOST_AUTO_TEST_CASE(only_two_auto_correlation_baselines) {
  UvwFile file = UvwFile::CreateNew(kFilename);
  constexpr size_t kNRows = 10;
  const std::array<double, 3> zero_uvw = {0.0, 0.0, 0.0};
  for(uint64_t row=0; row!=kNRows; ++row) {
    const size_t antenna = row%2;
    file.WriteUvw(row, antenna, antenna, zero_uvw.data());
  }
  file.Close();

  file = UvwFile::OpenExisting(kFilename);
  BOOST_REQUIRE_EQUAL(file.NRows(), kNRows);
  for(size_t row=0; row!=kNRows; ++row) {
    const size_t antenna = row%2;
    std::array<double, 3> uvw = {-1, -1, -1};
    file.ReadUvw(row, antenna, antenna, uvw.data());
    BOOST_CHECK(uvw == zero_uvw);
  }

  file.Close();
  unlink(kFilename.c_str());
}

BOOST_AUTO_TEST_SUITE_END()

