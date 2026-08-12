#include <casacore/casa/BasicSL/String.h>

#include <boost/test/unit_test.hpp>

BOOST_AUTO_TEST_SUITE(string_operations)

BOOST_AUTO_TEST_CASE(TrimInPlace)
{
  using casacore::TrimInPlace;

  std::string empty;
  TrimInPlace(empty);
  BOOST_CHECK_EQUAL(empty, "");

  std::string str1("-- hello --");
  TrimInPlace(str1, "-");
  BOOST_CHECK_EQUAL(str1, " hello ");

  TrimInPlace(str1);
  BOOST_CHECK_EQUAL(str1, "hello");

  std::string str2("-/- hel-/|lo -|-");
  TrimInPlace(str2, "-/| ");
  BOOST_CHECK_EQUAL(str2, "hel-/|lo");

  std::string str3("a\0hello\0a");
  TrimInPlace(str3, "a");
  BOOST_CHECK_EQUAL(str3, "\0hello\0");

  std::string str4("all gone");
  TrimInPlace(str4, "al gone");
  BOOST_CHECK_EQUAL(str4, "");

  std::string str5("none gone");
  TrimInPlace(str5, "./?");
  BOOST_CHECK_EQUAL(str5, "none gone");
}

BOOST_AUTO_TEST_CASE(LTrimInPlace) {
  using casacore::LTrimInPlace;

  std::string empty;
  LTrimInPlace(empty, '*');
  BOOST_CHECK_EQUAL(empty, "");

  std::string str1("-- hello --");
  LTrimInPlace(str1, '-');
  BOOST_CHECK_EQUAL(str1, " hello --");

  LTrimInPlace(str1, ' ');
  BOOST_CHECK_EQUAL(str1, "hello --");

  std::string str2("a\0hello\0a", 9);
  LTrimInPlace(str2, 'a');
  BOOST_CHECK_EQUAL(str2, std::string("\0hello\0a", 8));

  std::string str3("****");
  LTrimInPlace(str3, '*');
  BOOST_CHECK_EQUAL(str3, "");

  std::string str4("!");
  LTrimInPlace(str4, '?');
  BOOST_CHECK_EQUAL(str4, "!");
}

BOOST_AUTO_TEST_CASE(RTrimInPlace) {
  using casacore::RTrimInPlace;

  std::string empty;
  RTrimInPlace(empty, '*');
  BOOST_CHECK_EQUAL(empty, "");

  std::string str1("-- hello --");
  RTrimInPlace(str1, '-');
  BOOST_CHECK_EQUAL(str1, "-- hello ");

  RTrimInPlace(str1, ' ');
  BOOST_CHECK_EQUAL(str1, "-- hello");

  std::string str2("a\0hello\0a", 9);
  RTrimInPlace(str2, 'a');
  BOOST_CHECK_EQUAL(str2, std::string("a\0hello\0", 8));

  std::string str3("****");
  RTrimInPlace(str3, '*');
  BOOST_CHECK_EQUAL(str3, "");

  std::string str4("!");
  RTrimInPlace(str4, '?');
  BOOST_CHECK_EQUAL(str4, "!");
}

BOOST_AUTO_TEST_CASE(IndexStringWithChar) {
  using casacore::IndexString;
  const std::string kStr = "bla bla";
  BOOST_CHECK_EQUAL(IndexString(kStr, 'b'), 0);
  BOOST_CHECK_EQUAL(IndexString(kStr, 'l', 0), 1);
  BOOST_CHECK_EQUAL(IndexString(kStr, 'l', 1), 1);
  BOOST_CHECK_EQUAL(IndexString(kStr, 'b', 1), 4);
  // Apparently there's no way to use IndexString to get the last character,
  // only second to last. Casacore might depend on this behaviour so this is
  // kept as is.
  BOOST_CHECK_EQUAL(IndexString(kStr, 'a', -1), 2);
  BOOST_CHECK_EQUAL(IndexString(kStr, 'l', -1), 5);
  BOOST_CHECK_EQUAL(IndexString(kStr, 'b', -1), 4);
  BOOST_CHECK_EQUAL(IndexString(kStr, 'b', -3), 0);
  BOOST_CHECK_EQUAL(IndexString(kStr, 'b', -6), 0);

  BOOST_CHECK_EQUAL(IndexString(kStr, 'z', 0), std::string::npos);
  BOOST_CHECK_EQUAL(IndexString(kStr, 'a', 7), std::string::npos);
  BOOST_CHECK_EQUAL(IndexString(kStr, 'b', -7), std::string::npos);
}

BOOST_AUTO_TEST_CASE(IndexStringWithString) {
  using casacore::IndexString;
  const std::string kBlaBla = "bla bla";
  BOOST_CHECK_EQUAL(IndexString(kBlaBla, "b"), 0);
  BOOST_CHECK_EQUAL(IndexString(kBlaBla, "l", 0), 1);
  BOOST_CHECK_EQUAL(IndexString(kBlaBla, "l", 1), 1);
  BOOST_CHECK_EQUAL(IndexString(kBlaBla, "b", 1), 4);
  // See comment in IndexStringWithChar about indexing last character.
  BOOST_CHECK_EQUAL(IndexString(kBlaBla, "a", -1), 2);
  BOOST_CHECK_EQUAL(IndexString(kBlaBla, "l", -1), 5);
  BOOST_CHECK_EQUAL(IndexString(kBlaBla, "b", -1), 4);
  BOOST_CHECK_EQUAL(IndexString(kBlaBla, "b", -3), 0);
  BOOST_CHECK_EQUAL(IndexString(kBlaBla, "b", -6), 0);

  BOOST_CHECK_EQUAL(IndexString(kBlaBla, "z", 0), std::string::npos);
  BOOST_CHECK_EQUAL(IndexString(kBlaBla, "a", 7), std::string::npos);
  BOOST_CHECK_EQUAL(IndexString(kBlaBla, "b", -7), std::string::npos);

  const std::string kBlaBlaBli = "bla bla bli";
  BOOST_CHECK_EQUAL(IndexString(kBlaBlaBli, "bl"), 0);
  BOOST_CHECK_EQUAL(IndexString(kBlaBlaBli, "bl", 1), 4);
  BOOST_CHECK_EQUAL(IndexString(kBlaBlaBli, "bl", 5), 8);
  BOOST_CHECK_EQUAL(IndexString(kBlaBlaBli, "bla", 1), 4);
  BOOST_CHECK_EQUAL(IndexString(kBlaBlaBli, "bli", 0), 8);
  BOOST_CHECK_EQUAL(IndexString(kBlaBlaBli, "bli", 8), 8);
  BOOST_CHECK_EQUAL(IndexString(kBlaBlaBli, kBlaBlaBli, 0), 0);

  // As for individual chars, the old implementation is such that the
  // last match can't be found with -1. This is confusing but we keep
  // it as is.
  BOOST_CHECK_EQUAL(IndexString(kBlaBlaBli, "li", -1), std::string::npos);
  BOOST_CHECK_EQUAL(IndexString(kBlaBlaBli, "bli", -1), std::string::npos);
  BOOST_CHECK_EQUAL(IndexString(kBlaBlaBli, "bl", -1), 8);
  BOOST_CHECK_EQUAL(IndexString(kBlaBlaBli, "bla bla bl", -1), 0);
  BOOST_CHECK_EQUAL(IndexString(kBlaBlaBli, "bla", -1), 4);
  BOOST_CHECK_EQUAL(IndexString(kBlaBlaBli, "bla", -4), 4);
  BOOST_CHECK_EQUAL(IndexString(kBlaBlaBli, "bla", -5), 0);
  BOOST_CHECK_EQUAL(IndexString(kBlaBlaBli, "bla", -8), 0);

  BOOST_CHECK_EQUAL(IndexString(kBlaBlaBli, ""), 0);
  BOOST_CHECK_EQUAL(IndexString(kBlaBlaBli, "", 10), 10);
  BOOST_CHECK_EQUAL(IndexString(kBlaBlaBli, "", 11), 11);
  BOOST_CHECK_EQUAL(IndexString("", ""), 0);

  BOOST_CHECK_EQUAL(IndexString(kBlaBlaBli, "blu"), std::string::npos);
  BOOST_CHECK_EQUAL(IndexString(kBlaBlaBli, "bla", 5), std::string::npos);
  BOOST_CHECK_EQUAL(IndexString(kBlaBlaBli, "bli", 9), std::string::npos);
  BOOST_CHECK_EQUAL(IndexString(kBlaBlaBli, "bla", -9), std::string::npos);
  BOOST_CHECK_EQUAL(IndexString(kBlaBlaBli, kBlaBlaBli, 1), std::string::npos);
  BOOST_CHECK_EQUAL(IndexString(kBlaBlaBli, kBlaBlaBli, -1), std::string::npos);
  BOOST_CHECK_EQUAL(IndexString(kBlaBlaBli, "bla bla bli!", 0), std::string::npos);
  BOOST_CHECK_EQUAL(IndexString("", "?"), std::string::npos);
}

BOOST_AUTO_TEST_CASE(IndexStringReference) {
  using casacore::IndexString;
  // These were the original tests for casacore::String::index(), kept as reference
  const std::string kHello = "hello";
  BOOST_CHECK_EQUAL(IndexString(kHello, "lo"), 3);
  BOOST_CHECK_EQUAL(IndexString(kHello, "l", 2), 2);
  BOOST_CHECK_EQUAL(IndexString(kHello, "l", -1), 3);
}

BOOST_AUTO_TEST_CASE(StringToValue) {
  using casacore::StringToValue;
  BOOST_CHECK_EQUAL(StringToValue<unsigned>("0"), 0);
  BOOST_CHECK_EQUAL(StringToValue<unsigned>("3"), 3);
  BOOST_CHECK_EQUAL(StringToValue<int16_t>("-0"), 0);
  BOOST_CHECK_EQUAL(StringToValue<int16_t>("32767"), 32767);
  BOOST_CHECK_EQUAL(StringToValue<int16_t>("-32768"), -32768);
  BOOST_CHECK_EQUAL(StringToValue<bool>("0"), false);
  BOOST_CHECK_EQUAL(StringToValue<bool>("1"), true);
  BOOST_CHECK_EQUAL(StringToValue<int64_t>("0"), 0);
  BOOST_CHECK_EQUAL(StringToValue<int64_t>("-1"), -1);
  BOOST_CHECK_EQUAL(StringToValue<int64_t>("9223372036854775807"), 9223372036854775807ll);
  // C++ can't represent -9223372036854775808, because it interprets it as an unsigned value
  // and applies minus to it; hence the subtraction with one.
  BOOST_CHECK_EQUAL(StringToValue<int64_t>("-9223372036854775808"), -9223372036854775807ll - 1ll);

  BOOST_CHECK_THROW(StringToValue<int64_t>("Everything is awesome!"), std::runtime_error);
  BOOST_CHECK_THROW(StringToValue<unsigned short>("9223372036854775807"), std::runtime_error);
  BOOST_CHECK_THROW(StringToValue<unsigned>(""), std::runtime_error);
  BOOST_CHECK_THROW(StringToValue<unsigned>("0?"), std::runtime_error);
  BOOST_CHECK_THROW(StringToValue<short>("1e12"), std::runtime_error);
}

BOOST_AUTO_TEST_CASE(StringToInt) {
  using casacore::StringToInt;
  BOOST_CHECK_EQUAL(StringToInt("0"), 0);
  BOOST_CHECK_EQUAL(StringToInt("3"), 3);
  BOOST_CHECK_EQUAL(StringToInt("-0"), 0);
  BOOST_CHECK_EQUAL(StringToInt("-32768"), -32768);

  BOOST_CHECK_THROW(StringToInt(""), std::runtime_error);
  BOOST_CHECK_THROW(StringToInt("0?"), std::runtime_error);
  BOOST_CHECK_THROW(StringToInt("1e12"), std::runtime_error);
}

BOOST_AUTO_TEST_CASE(StringToFloat) {
  using casacore::StringToFloat;
  BOOST_CHECK_EQUAL(StringToFloat("0"), 0.0f);
  BOOST_CHECK_CLOSE_FRACTION(StringToFloat("3e3"), 3e3, 1e-6);
  BOOST_CHECK_EQUAL(StringToFloat("-0.0"), -0.0f);
  BOOST_CHECK_CLOSE_FRACTION(StringToFloat("-3.14159265e-4"), -3.14159265e-4f, 1e-6);

  BOOST_CHECK_THROW(StringToFloat(""), std::runtime_error);
  BOOST_CHECK_THROW(StringToFloat("0?"), std::runtime_error);
  BOOST_CHECK_THROW(StringToFloat("1e12e"), std::runtime_error);
}

BOOST_AUTO_TEST_CASE(StringToDouble) {
  using casacore::StringToDouble;
  BOOST_CHECK_EQUAL(StringToDouble("0"), 0.0);
  BOOST_CHECK_CLOSE_FRACTION(StringToDouble("3e3"), 3e3, 1e-8);
  BOOST_CHECK_EQUAL(StringToDouble("-0.0"), -0.0f);
  BOOST_CHECK_CLOSE_FRACTION(StringToDouble("-3.14159265e-4"), -3.14159265e-4, 1e-8);
  BOOST_CHECK_CLOSE_FRACTION(StringToDouble("1e308"), 1e308, 1e-8);

  BOOST_CHECK_THROW(StringToDouble(""), std::runtime_error);
  BOOST_CHECK_THROW(StringToDouble("0?"), std::runtime_error);
  BOOST_CHECK_THROW(StringToDouble("1e12e"), std::runtime_error);
}

BOOST_AUTO_TEST_CASE(SubStringCount) {
  using casacore::SubStringCount;
  BOOST_CHECK_EQUAL(SubStringCount("aaa", "a"), 3);
  BOOST_CHECK_EQUAL(SubStringCount("aaa", "b"), 0);
  BOOST_CHECK_EQUAL(SubStringCount("aaa", "aa"), 2);
  BOOST_CHECK_EQUAL(SubStringCount("In Lhee lees je boeken in de lheesbieb", "ee"), 3);
  BOOST_CHECK_EQUAL(SubStringCount("abab", "ab"), 2);
  BOOST_CHECK_EQUAL(SubStringCount("a", "ab"), 0);
  BOOST_CHECK_EQUAL(SubStringCount("", "ab"), 0);
}

BOOST_AUTO_TEST_CASE(GetStringViewUpToExcluding) {
  using casacore::GetStringUpToExcluding;
  constexpr const char* kStr = "Een twee drie";
  BOOST_CHECK_EQUAL(GetStringUpToExcluding(kStr, "drie"), "Een twee ");
  BOOST_CHECK_EQUAL(GetStringUpToExcluding(kStr, "twee"), "Een ");
  BOOST_CHECK_EQUAL(GetStringUpToExcluding(kStr, "Een"), "");
  BOOST_CHECK_EQUAL(GetStringUpToExcluding(kStr, "vier"), kStr);
  BOOST_CHECK_EQUAL(GetStringUpToExcluding(kStr, kStr), "");
  BOOST_CHECK_EQUAL(GetStringUpToExcluding("", "Een"), "");

  BOOST_CHECK_EQUAL(GetStringUpToExcluding(kStr, "Een", 1), kStr);
  BOOST_CHECK_EQUAL(GetStringUpToExcluding(kStr, "twee", 1), "Een ");
  BOOST_CHECK_EQUAL(GetStringUpToExcluding(kStr, "twee", 4), "Een ");
  BOOST_CHECK_EQUAL(GetStringUpToExcluding(kStr, "vier", 13), kStr);
}

BOOST_AUTO_TEST_CASE(GetStringUpToIncluding) {
  using casacore::GetStringUpToIncluding;
  constexpr const char* kStr = "Een twee drie";
  BOOST_CHECK_EQUAL(GetStringUpToIncluding(kStr, "drie"), "Een twee drie");
  BOOST_CHECK_EQUAL(GetStringUpToIncluding(kStr, "twee"), "Een twee");
  BOOST_CHECK_EQUAL(GetStringUpToIncluding(kStr, "Een"), "Een");
  BOOST_CHECK_EQUAL(GetStringUpToIncluding(kStr, "vier"), kStr);
  BOOST_CHECK_EQUAL(GetStringUpToIncluding(kStr, kStr), kStr);
  BOOST_CHECK_EQUAL(GetStringUpToIncluding("", "Een"), "");

  BOOST_CHECK_EQUAL(GetStringUpToIncluding(kStr, "Een", 1), kStr);
  BOOST_CHECK_EQUAL(GetStringUpToIncluding(kStr, "twee", 1), "Een twee");
  BOOST_CHECK_EQUAL(GetStringUpToIncluding(kStr, "twee", 4), "Een twee");
  BOOST_CHECK_EQUAL(GetStringUpToIncluding(kStr, "vier", 13), kStr);
}

BOOST_AUTO_TEST_CASE(GetStringFrom) {
  using casacore::GetStringFrom;
  constexpr const char* kStr = "Een twee drie";
  BOOST_CHECK_EQUAL(GetStringFrom(kStr, "drie"), "drie");
  BOOST_CHECK_EQUAL(GetStringFrom(kStr, "twee"), "twee drie");
  BOOST_CHECK_EQUAL(GetStringFrom(kStr, "Een"), "Een twee drie");
  BOOST_CHECK_EQUAL(GetStringFrom(kStr, "vier"), "");
  BOOST_CHECK_EQUAL(GetStringFrom(kStr, kStr), kStr);
  BOOST_CHECK_EQUAL(GetStringFrom("", "Een"), "");

  BOOST_CHECK_EQUAL(GetStringFrom(kStr, "Een", 1), "");
  BOOST_CHECK_EQUAL(GetStringFrom(kStr, "twee", 1), "twee drie");
  BOOST_CHECK_EQUAL(GetStringFrom(kStr, "twee", 4), "twee drie");
  BOOST_CHECK_EQUAL(GetStringFrom(kStr, "vier", 13), "");
}

BOOST_AUTO_TEST_CASE(GetStringAfter) {
  using casacore::GetStringAfter;
  constexpr const char* kStr = "Een twee drie";
  BOOST_CHECK_EQUAL(GetStringAfter(kStr, "drie"), "");
  BOOST_CHECK_EQUAL(GetStringAfter(kStr, "twee"), " drie");
  BOOST_CHECK_EQUAL(GetStringAfter(kStr, "Een"), " twee drie");
  BOOST_CHECK_EQUAL(GetStringAfter(kStr, "vier"), "");
  BOOST_CHECK_EQUAL(GetStringAfter(kStr, kStr), "");
  BOOST_CHECK_EQUAL(GetStringAfter("", "Een"), "");

  BOOST_CHECK_EQUAL(GetStringAfter(kStr, "Een", 1), "");
  BOOST_CHECK_EQUAL(GetStringAfter(kStr, "twee", 1), " drie");
  BOOST_CHECK_EQUAL(GetStringAfter(kStr, "twee", 4), " drie");
  BOOST_CHECK_EQUAL(GetStringAfter(kStr, "vier", 13), "");
}

BOOST_AUTO_TEST_CASE(ToUpperAndToLower) {
  using casacore::ToUpperCaseInPlace;
  using casacore::ToLowerCaseInPlace;
  const std::string kStart = "Hello? HELLO?!?";
  const std::string kLower = "hello? hello?!?";
  const std::string kUpper = "HELLO? HELLO?!?";
  std::string input = kStart;
  ToUpperCaseInPlace(input);
  BOOST_CHECK_EQUAL(input, kUpper);
  ToLowerCaseInPlace(input);
  BOOST_CHECK_EQUAL(input, kLower);
  ToLowerCaseInPlace(input);
  BOOST_CHECK_EQUAL(input, kLower);
  ToUpperCaseInPlace(input);
  BOOST_CHECK_EQUAL(input, kUpper);

  input = "";
  ToLowerCaseInPlace(input);
  BOOST_CHECK_EQUAL(input, "");
  ToUpperCaseInPlace(input);
  BOOST_CHECK_EQUAL(input, "");
}

BOOST_AUTO_TEST_CASE(CapitalizeStringInPlace) {
  using casacore::CapitalizeStringInPlace;

  std::string empty;
  CapitalizeStringInPlace(empty);
  BOOST_CHECK_EQUAL(empty, "");

  std::string sentence = "thIS is A senTENce, WiTh A cOMma aND 2 DIgItS. No 1! Ok 3THEN?";
  CapitalizeStringInPlace(sentence);
  const char* expected = "This Is A Sentence, With A Comma And 2 Digits. No 1! Ok 3then?";
  BOOST_CHECK_EQUAL(sentence, expected);
}

BOOST_AUTO_TEST_CASE(EraseStringFrom) {
  using casacore::EraseStringFrom;

  std::string input = "Remove the e from here and here";
  EraseStringFrom(input, "e");
  BOOST_CHECK_EQUAL(input, "Rmove the e from here and here");
  EraseStringFrom(input, " here");
  BOOST_CHECK_EQUAL(input, "Rmove the e from and here");
  EraseStringFrom(input, "!!");
  BOOST_CHECK_EQUAL(input, "Rmove the e from and here");
  EraseStringFrom(input, "");
  BOOST_CHECK_EQUAL(input, "Rmove the e from and here");
  input = "";
  EraseStringFrom(input, "a");
  BOOST_CHECK_EQUAL(input, "");
}

BOOST_AUTO_TEST_CASE(ReplaceAllInPlace) {
  using casacore::ReplaceAllInPlace;

  std::string input = "Remove the e from here and here";
  ReplaceAllInPlace(input, "e", "");
  BOOST_CHECK_EQUAL(input, "Rmov th  from hr and hr");
  ReplaceAllInPlace(input, "hr", "|-|");
  BOOST_CHECK_EQUAL(input, "Rmov th  from |-| and |-|");
  ReplaceAllInPlace(input, "|--|", "?");
  BOOST_CHECK_EQUAL(input, "Rmov th  from |-| and |-|");
  input = "";
  ReplaceAllInPlace(input, "a", "?");
  BOOST_CHECK_EQUAL(input, "");
}

BOOST_AUTO_TEST_CASE(StringContains) {
  using casacore::StringContains;

  BOOST_CHECK(StringContains("abc", "a"));
  BOOST_CHECK(StringContains("abc", "b"));
  BOOST_CHECK(StringContains("abc", "c"));
  BOOST_CHECK(StringContains("abc", "ab"));
  BOOST_CHECK(StringContains("abc", "bc"));
  BOOST_CHECK(StringContains("abc", "abc"));
  BOOST_CHECK(StringContains("abc", ""));
  BOOST_CHECK(StringContains("", ""));

  BOOST_CHECK(!StringContains("abc", "d"));
  BOOST_CHECK(!StringContains("abc", "ba"));
  BOOST_CHECK(!StringContains("abc", "abcd"));
  BOOST_CHECK(!StringContains("", "a"));
}

BOOST_AUTO_TEST_SUITE_END()
