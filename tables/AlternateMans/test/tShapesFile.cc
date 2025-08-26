#include <boost/test/unit_test.hpp>

#include <casacore/tables/AlternateMans/ShapesFileReader.h>
#include <casacore/tables/AlternateMans/ShapesFileWriter.h>

using casacore::IPosition;
using casacore::ShapesFileReader;
using casacore::ShapesFileWriter;

namespace casacore {

namespace {
constexpr const char* kFilename = "test-shapes-file.tmp";
const std::vector<IPosition> kTestList{{}, {5, 8, 2}, {1, 0, 0, 1, 1, 0, 1, 0}, {-1}, {}, {65537, -65537}};
} // namespace

BOOST_AUTO_TEST_SUITE(shapes_file)

BOOST_AUTO_TEST_CASE(read_and_write) {
  {
    ShapesFileWriter writer(kFilename);
    writer.Write(IPosition{3, 4});
    for(const IPosition& shape : kTestList)
      writer.Write(shape);
  }

  ShapesFileReader reader(kFilename);
  const IPosition first = reader.Read();
  BOOST_CHECK_EQUAL(first.size(), 2);
  BOOST_CHECK_EQUAL(first[0], 3);
  BOOST_CHECK_EQUAL(first[1], 4);

  for(const IPosition& reference : kTestList)
  {
    BOOST_CHECK(reader.Read() == reference);
  }
  // Reading past end of file should produce empty shapes
  BOOST_CHECK_EQUAL(reader.Read().size(), 0);
  BOOST_CHECK_EQUAL(reader.Read().size(), 0);
}

BOOST_AUTO_TEST_SUITE_END()

} // namespace casacore
