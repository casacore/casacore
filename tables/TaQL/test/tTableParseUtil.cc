#include <casacore/tables/TaQL/TableParseUtil.h>

#define BOOST_TEST_MODULE tables
#define BOOST_TEST_DYN_LINK

#include <boost/test/unit_test.hpp>

using casacore::String;
using casacore::Vector;
using casacore::TableParseUtil::splitName;

BOOST_AUTO_TEST_SUITE(table_parse_util)

BOOST_AUTO_TEST_CASE(split_name)
{
  String short_hand;
  String column_name;
  casacore::Vector<String> field_names;
  constexpr bool kCheckError = true;
  constexpr bool kAllowNoKey = false;
  bool result;

  result = splitName(short_hand, column_name, field_names, "Abc", kCheckError, true, kAllowNoKey);
  BOOST_CHECK(result);
  BOOST_CHECK_EQUAL(short_hand, "");
  BOOST_CHECK_EQUAL(column_name, "");
  BOOST_REQUIRE_EQUAL(field_names.size(), 1);
  BOOST_CHECK_EQUAL(field_names[0], "Abc");

  result = splitName(short_hand, column_name, field_names, "A::B", kCheckError, true, kAllowNoKey);
  BOOST_CHECK(result);
  BOOST_CHECK_EQUAL(short_hand, "");
  BOOST_CHECK_EQUAL(column_name, "A");
  BOOST_REQUIRE_EQUAL(field_names.size(), 1);
  BOOST_CHECK_EQUAL(field_names[0], "B");

  result = splitName(short_hand, column_name, field_names, "::A", kCheckError, true, kAllowNoKey);
  BOOST_CHECK(result);
  BOOST_CHECK_EQUAL(short_hand, "");
  BOOST_CHECK_EQUAL(column_name, "");
  BOOST_REQUIRE_EQUAL(field_names.size(), 1);
  BOOST_CHECK_EQUAL(field_names[0], "A");

  result = splitName(short_hand, column_name, field_names, "::A", kCheckError, false, kAllowNoKey);
  BOOST_CHECK(result);
  BOOST_CHECK_EQUAL(short_hand, "");
  BOOST_CHECK_EQUAL(column_name, "");
  BOOST_REQUIRE_EQUAL(field_names.size(), 1);
  BOOST_CHECK_EQUAL(field_names[0], "A");

  result = splitName(short_hand, column_name, field_names, "First::Second", kCheckError, true, kAllowNoKey);
  BOOST_CHECK(result);
  BOOST_CHECK_EQUAL(short_hand, "");
  BOOST_CHECK_EQUAL(column_name, "First");
  BOOST_REQUIRE_EQUAL(field_names.size(), 1);
  BOOST_CHECK_EQUAL(field_names[0], "Second");

  result = splitName(short_hand, column_name, field_names, "A::B", kCheckError, false, kAllowNoKey);
  BOOST_CHECK(result);
  BOOST_CHECK_EQUAL(short_hand, "");
  BOOST_CHECK_EQUAL(column_name, "A");
  BOOST_REQUIRE_EQUAL(field_names.size(), 1);
  BOOST_CHECK_EQUAL(field_names[0], "B");

  result = splitName(short_hand, column_name, field_names, "A::B.c", kCheckError, true, kAllowNoKey);
  BOOST_CHECK(result);
  BOOST_CHECK_EQUAL(short_hand, "");
  BOOST_CHECK_EQUAL(column_name, "A");
  BOOST_REQUIRE_EQUAL(field_names.size(), 2);
  BOOST_CHECK_EQUAL(field_names[0], "B");
  BOOST_CHECK_EQUAL(field_names[1], "c");

  result = splitName(short_hand, column_name, field_names, "B.c", kCheckError, true, kAllowNoKey);
  BOOST_CHECK(result);
  BOOST_CHECK_EQUAL(short_hand, "");
  BOOST_CHECK_EQUAL(column_name, "");
  BOOST_REQUIRE_EQUAL(field_names.size(), 2);
  BOOST_CHECK_EQUAL(field_names[0], "B");
  BOOST_CHECK_EQUAL(field_names[1], "c");

  result = splitName(short_hand, column_name, field_names, "A.b::C", kCheckError, true, kAllowNoKey);
  BOOST_CHECK(result);
  BOOST_CHECK_EQUAL(short_hand, "A");
  BOOST_CHECK_EQUAL(column_name, "b");
  BOOST_REQUIRE_EQUAL(field_names.size(), 1);
  BOOST_CHECK_EQUAL(field_names[0], "C");

  result = splitName(short_hand, column_name, field_names, "A.b::C.D.E", kCheckError, true, kAllowNoKey);
  BOOST_CHECK(result);
  BOOST_CHECK_EQUAL(short_hand, "A");
  BOOST_CHECK_EQUAL(column_name, "b");
  BOOST_REQUIRE_EQUAL(field_names.size(), 3);
  BOOST_CHECK_EQUAL(field_names[0], "C");
  BOOST_CHECK_EQUAL(field_names[1], "D");
  BOOST_CHECK_EQUAL(field_names[2], "E");

  result = splitName(short_hand, column_name, field_names, "::A.Bc", kCheckError, true, kAllowNoKey);
  BOOST_CHECK(result);
  BOOST_CHECK_EQUAL(short_hand, "");
  BOOST_CHECK_EQUAL(column_name, "");
  BOOST_REQUIRE_EQUAL(field_names.size(), 2);
  BOOST_CHECK_EQUAL(field_names[0], "A");
  BOOST_CHECK_EQUAL(field_names[1], "Bc");

  result = splitName(short_hand, column_name, field_names, "::A.Bc", kCheckError, false, kAllowNoKey);
  BOOST_CHECK(result);
  BOOST_CHECK_EQUAL(short_hand, "");
  BOOST_CHECK_EQUAL(column_name, "");
  BOOST_REQUIRE_EQUAL(field_names.size(), 2);
  BOOST_CHECK_EQUAL(field_names[0], "A");
  BOOST_CHECK_EQUAL(field_names[1], "Bc");

  result = splitName(short_hand, column_name, field_names, "Shorthand.Column::Field1.Field2.Field3", kCheckError, true, kAllowNoKey);
  BOOST_CHECK(result);
  BOOST_CHECK_EQUAL(short_hand, "Shorthand");
  BOOST_CHECK_EQUAL(column_name, "Column");
  BOOST_REQUIRE_EQUAL(field_names.size(), 3);
  BOOST_CHECK_EQUAL(field_names[0], "Field1");
  BOOST_CHECK_EQUAL(field_names[1], "Field2");
  BOOST_CHECK_EQUAL(field_names[2], "Field3");

  BOOST_CHECK_THROW(splitName(short_hand, column_name, field_names, "", kCheckError, true, kAllowNoKey), std::exception);
  BOOST_CHECK_THROW(splitName(short_hand, column_name, field_names, "", kCheckError, false, kAllowNoKey), std::exception);
  BOOST_CHECK_THROW(splitName(short_hand, column_name, field_names, "S.C::Field1..Field2", kCheckError, true, kAllowNoKey), std::exception);

  result = splitName(short_hand, column_name, field_names, "B.c", kCheckError, false, kAllowNoKey);
  BOOST_CHECK(!result);

}

BOOST_AUTO_TEST_SUITE_END()

