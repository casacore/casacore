//# String.h: String class
//# Copyright (C) 2001,2002,2003
//# Associated Universities, Inc. Washington DC, USA.
//#
//# This library is free software; you can redistribute it and/or modify it
//# under the terms of the GNU Library General Public License as published by
//# the Free Software Foundation; either version 2 of the License, or (at your
//# option) any later version.
//#
//# This library is distributed in the hope that it will be useful, but WITHOUT
//# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
//# FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Library General Public
//# License for more details.
//#
//# You should have received a copy of the GNU Library General Public License
//# along with this library; if not, write to the Free Software Foundation,
//# Inc., 675 Massachusetts Ave, Cambridge, MA 02139, USA.
//#
//# Correspondence concerning AIPS++ should be addressed as follows:
//#        Internet email: casa-feedback@nrao.edu.
//#        Postal address: AIPS++ Project Office
//#                        National Radio Astronomy Observatory
//#                        520 Edgemont Road
//#                        Charlottesville, VA 22903-2475 USA

#ifndef CASACORE_STRING_H_
#define CASACORE_STRING_H_

// Uncomment this to issue warnings for all use of non-std::string functions
//#define CASACORE_DEPRECATE_STRING

#ifdef CASACORE_DEPRECATE_STRING
// Some of the (deprecated) code calls deprecated functions, so disable this
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wdeprecated-declarations"
#define DEPRECATED(X) [[deprecated(X)]]
#else
#define DEPRECATED(X)
#endif

#include <algorithm>
#include <cassert>
#include <cctype>
#include <sstream>
#include <string>
#include <string_view>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations
class String;
class Regex;

inline const std::string_view kWhiteSpaceCharacters = " \n\t\r\v\f";

// Return the position of the character in the string or npos if not found.
// Searches the first index of the character if the startpos >= 0, or the reverse index if
// startpos < 0. Note that a startpos of -1 indicates searching at the second to last character.
// This is in line with the old index() function, but rather confusing, so do not use for new code.
inline std::string::size_type IndexString(std::string_view str, char c, int startpos = 0) {
  if (startpos >= 0) {
    return str.find(c, startpos);
  } else {
    const int search_from = str.length() + startpos - 1;
    return search_from >= 0 ? str.rfind(c, search_from) : std::string::npos;
  }
}

// Return the position of the substring in the string or npos if not found.
// Searches the first index of the substring if the startpos >= 0, or the last index if
// startpos < 0. Note that a startpos of -1 indicates searching at the second to last character.
// This is in line with the old index() function, but rather confusing, so do not use for new code.
inline std::string::size_type IndexString(std::string_view str, std::string_view pattern, int startpos = 0) {
  if (startpos >= 0) {
    return str.find(pattern, startpos);
  } else {
    const int search_from = str.length() + startpos - pattern.length();
    return search_from >= 0 ? str.rfind(pattern, search_from) : std::string::npos;
  }
}

// Replacement of old member function "matches", which has somewhat confusing semantics: two empty strings do
// not match, and an empty pattern does also not match. Do not use this for new code and try to rewrite old code.
inline bool EqualStringsAndNotEmpty(std::string_view str1, std::string_view str2, int position = 0) {
  if(str1.empty() || str2.empty()) {
    return false;
  } else if(position < 0) {
    return std::string_view(str1.begin(), str1.begin() + std::min<size_t>(-position, str1.size())) == str2;
  } else {
    return std::string_view(str1.begin() + std::min<size_t>(position, str1.size()), str1.end()) == str2;
  }
}

// Convert a String to a value. All characters in the string must be used.
// It uses a shift from an ostringstream, so that operator must exist
// for the data type used.
// In case of an error, an exception is thrown if @p check is set.
// Otherwise it returns false and @p value contains the value read
// so far.
template<typename T>
inline bool StringToValue (const std::string& str, T& value, bool throw_on_error=true)
{
  std::istringstream is(str);
  is >> value;
  if (is.fail()  ||  !is.eof()) {
    if (throw_on_error) {
      if(is.fail()) {
        throw std::runtime_error ("String '" + str + "' failed to parse in StringToValue()");
      } else {
        std::string extra;
        std::getline(is, extra);
        throw std::runtime_error ("Extra characters after parsing string '" + str + "' in StringToValue(): '" + extra + "'");
      }
    }
    return false;
  }
  return true;
}

// Same as other StringToValue() overload, but returns the parsed value.
template<typename T>
inline T StringToValue(const std::string& str, bool throw_on_error = true)
{
  T value;
  StringToValue(str, value, throw_on_error);
  return value;
}

// Same as StringToValue<int>
inline int StringToInt(const std::string& str, bool throw_on_error = false) {
  return StringToValue<int>(str, throw_on_error);
}

// Same as StringToValue<float>
inline float StringToFloat(const std::string& str, bool throw_on_error = false) {
  return StringToValue<float>(str, throw_on_error);
}

// Same as StringToValue<double>
inline double StringToDouble(const std::string& str, bool throw_on_error = false) {
  return StringToValue<double>(str, throw_on_error);
}

template<typename T>
std::string ValueToString(T&& value)
{
  std::ostringstream os;
  os << std::forward<T>(value);
  return os.str();
}

// Replace all matches of the string @p str with the regex @pat by @repl.
int RegexReplaceAll(std::string& str, const Regex &pat, const std::string &repl);

// Checks if the regex @r matches with the string @str. If @p pos is provided,
// searching starts from that position.
bool RegexMatches(const std::string& str, const Regex &r, int pos = 0);

// Matches the regex and returns the matching part.
std::string RegexSubStr(const std::string& str, const Regex& r, size_t startpos = 0);

// Matches the regex and returns at what index in the string.
size_t RegexIndex(const std::string& str, const Regex& r, size_t startpos = 0);

// <summary> SubString help class to be used in at, before, ... </summary>
// <synopsis>
// The SubString class can only be used by the String class to be able to
// operate the Casacore defined replacement operators at, before, after,
// through, from. The class is used transparently in operations like:
// <srcblock>
//	string.at(2,3) = "five";
// </srcblock> 
// If the SubString starts at a position outside the length of the
// original string (like e.g. in after(1000000)), a zero length string is
// created (not an exception thrown like in standard string operations).
// </synopsis>

class SubString {
public:
  //# Friends
  friend class String;
  // Make a string
  DEPRECATED("class SubString will be removed")
  operator const std::string() const { return std::string(ref_p, pos_p, len_p); }
  // Default copy constructor.
  SubString (const SubString&) = default;
  // Assignment
  // <group>
  DEPRECATED("class SubString will be removed")
  SubString &operator=(const SubString &str);
  DEPRECATED("class SubString will be removed")
  SubString &operator=(const String &str);
  DEPRECATED("class SubString will be removed")
  SubString &operator=(const char *s);
  DEPRECATED("class SubString will be removed")
  SubString &operator=(const char c);
  // </group>
  // Get as (const) C array
  DEPRECATED("class SubString will be removed")
  const char *chars() const;
  // Obtain length
  DEPRECATED("class SubString will be removed")
  std::string::size_type length() const { return len_p; }

private:
  //# Constructors
  // Constructor (there are no public constructors)
  SubString(const std::string &str, std::string::size_type pos,
	    std::string::size_type len);
  //# Data
  // Referenced string
  const std::string &ref_p;
  // Start of sub-string
  std::string::size_type pos_p;
  // Length of sub-string
  std::string::size_type len_p;
};

// <summary> 
// String: the storage and methods of handling collections of characters.
// </summary>

// <use visibility=export>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="tString.cc" demos="">
// </reviewed>

// <prerequisite>
//   <li> Regex - the regular expressions class
//   <li> the std string class
// </prerequisite>
//
// <etymology>
// The String class name is a continuation of the "C" language custom of
// refering to collections of characters as "strings of characters".
// </etymology>
//
// <synopsis> 
// The String class is the Casacore implementation of a string class. It is
// from the standard library string class, and all operations
// and behaviour of strings as defined in the standard are available for
// a String. The only difference is the extension with additional functions
// in the Casacore String class as compared to the standard string class.
// 
// The String class may be instantiated in many ways:
// <ol>
// <li> A single character - <src>String myChar('C');</src>
// <li> A char* argument - <src>String myWord("Yowza");</src>
// <li> The first n chararcters of a pre-existing string - 
// <src>String myFoo("fooey", 3);</src>
// </ol> As well as the copy and default constructors and iterator based ones.
//
// A String may be concatinated with another object (String, or 
// char*) with either prepending or postpending.  A search for the position
// of a character within a String may return its position, a bool that it
// is contained within or a bool confirming your guess at the character's
// position is correct.  A check of the frequency of occurance of a string
// within a String will return the number of occurances.  
// 
// Strings may be extracted from Strings at, before, through, from and 
// after a starting position within the String.  Deletion of characters is
// possible after a given position within the String. Global substitution
// of characters within a String is provided, as well.  Splitting of Strings 
// into a carray of Strings is possible, based upon a given separator 
// character, with a return value of the number of elements split.  The joining
// together of the elements of an array of Strings into one String is possible.
// 
// Finally, transformations of case and conversions of type are provided. 
//
// The standard string class provides the following functionality:
// <ol>
// <li> Construction from (part of) String, (part of) char*,
//		(repeating) Char, iterator pair.
// <li> Assignment from String, char*, Char
// <li> Iterators: begin() and end(); rbegin() and rend() (Note: gcc reverse
//		iterators still weak)
// <li> Capacity: size, length, max_size, resize, capacity, reserve, clear,
//		empty
// <li> Special size: String::size_type, with indicator: String::npos
// <li> Element access: [pos] and at(pos) (both const and non-const)
// <li> Modifiers: += of String, char*, Char; append of (part of) String,
//		char*, char and iterator defined; assign() of (part of)
//		String, char* and (repeating) char and iterator;
//		insertion of same; replacing of same; erase of part of
//		String; a copy and a swap.
// <li> C-string: get char* with c_str() or data() and get the relevant
//		Allocator used (Note: not fully supported in gcc)
// <li> Operations: find, rfind, find_first_of, find_last_of, find_first_not_of,
//		find_last_not_of; substr (Note only readable substring);
//		compare with (part of) String, char*
// <li> Globals: Addition operators for String, char*, Char; all comparison
//		operators for String and char*; getline; input and output
//		stream operators
// <li> Typedef: All relevant typedefs for standard containers and iterator
// 		handling
// </ol>
// The Casacore additions are:
// <ol>
// <li> To standard: some char function arguments where appropriate; Regex
//		arguments in search like methods.
// <li> Substring additions: at, before, after, from, through functions taking
//		search String, char* as arguments can give (hidden) substrings
//		which can be assigned (as in <src> at(1,2) = ";"</src>)
// <li> Methods: prepend (in addition to standard append); del (as erase);
//		global substitution of String and patterns;
//		 freq (count of occurance); split/join of strings at separator
//		or pattern; upcase, downcase, reverse;
//		 common_suffix and _prefix; replicate; case insensitive
//		compare; creation from stream
// </ol>

// </synopsis> 
//
// <example>
// <srcblock>
// // Let's start with a simple string.
// String myString("the time");
// // add some more on the end...
// myString += " for all good men";
// // prepend some on the front...
// myString.prepend("Now is ");
// // do some concatination...
// String evenMore;
// evenMore += myString + " to come to";
// // do some three way concatination
// String allKeys, finishIt(" their country.");
// allKeys = evenMore + "the aid of" + finishIt;
// // find the spot where we put something earlier
// String::size_type position = allKeys.index(finishIt);
// // find if the word is in the String...
// bool query = myString.contains("good men");
// // ask if the position we think is true is correct...
// bool answer = allKeys.matches(finishIt, position);
// // How many spaces are in our phrase?
// int spacesCount = allKeys.freq(" ");
// </srcblock>
// </example>
//
// <motivation>
// The String class eases the handling of characters within the Casacore 
// environment.
// </motivation>
//
// <todo asof=2000/12/05">
//   <li> if old string disappeared; remove the alloc() call.
//   <li> add more tests (for string methods) when old String disappears
// </todo>

class String : public std::string {

 public:
  //# Constructors
  // Default constructor
  String() = default;
  // Construct from std::string
  // Construct from (part of) other string: acts as copy constructor
  // <thrown>
  // <li> out_of_range if pos > str.size()
  // </thrown>
  String(const std::string& str, size_type pos=0, size_type n=npos) :
    std::string(str, pos, n) {}
  // Construct from char* with given length
  // <thrown>
  // <li> length_error if n == npos
  // </thrown>
  String(const char* s, size_type n) : std::string(s, n) {}
  // Construct from char array
  String(const char* s) : std::string(s) {}
  // Construct from a single char (repeated n times)
  // <thrown>
  // <li> length_error if n == npos
  // </thrown>
  String(size_type n, char c) : std::string(n, c) {}
  // Construct from iterator
  template<class InputIterator>
    String(InputIterator begin, InputIterator end) : std::string(begin, end) {}
  // From single char (** Casacore addition).
  // <note role=warning> Note that there is no automatic Char-to-String
  // conversion available. This stops inadvertent conversions of
  // integer to string. </note>
  explicit String(char c) : std::string(1, c) {}
  // Construct from a SubString
  String(const SubString &str) : std::string(str.ref_p, str.pos_p, str.len_p) {}
  // Construct from a stream.
  DEPRECATED("Use os.str()")
  String(std::ostringstream &os);

  //# Destructor
  // Destructor
  ~String() = default;

  //# Operators
  // Assignments (they are all deep copies according to standard)
  // <group>
  String& operator=(const std::string& str) {
    return static_cast<String&>(std::string::operator=(str)); }
  String& operator=(const SubString &str) {
    return (*this = String(str)); }
  String& operator=(const char* s) {
    return static_cast<String&>(std::string::operator=(s)); }
  String& operator=(char c) {
    return static_cast<String&>(std::string::operator=(c)); }
  // </group>
  // ** Casacore addition: synonym for at(pos, len)
  DEPRECATED("Use substr()")
  SubString operator()(size_type pos, size_type len);

  // *** Casacore addition
  // <group>
  DEPRECATED("Use at()")
  const_reference elem(size_type pos) const {
    return std::string::at(pos); }

  DEPRECATED("Use front()")
  char firstchar() const { return at(static_cast<size_type>(0)); }
  DEPRECATED("Use back()")
  char lastchar() const { return at(length()-1); }

  // ** Casacore addition -- works as a capacity(n) -- Note Int
  DEPRECATED("Use capacity()")
  int allocation() const { return std::string::capacity(); }

  // ** Casacore addition -- works as a resize(n)
  DEPRECATED("Use resize()")
  void alloc(size_type n) { std::string::resize(n); }

  // ** Casacore addition
  DEPRECATED("Use append(1, c)")
  std::string& append(char c) {
    return std::string::append(1, c);
  }
  using std::string::append;

  // ** Casacore addition
  DEPRECATED("Use assign(1, c)")
  String& assign(char c)  {
    return static_cast<String&>(std::string::assign(1, c)); }

  // ** Casacore addition
  DEPRECATED("Use insert(pos, 1, c)")
  String& insert(size_type pos, char c) {
    return static_cast<String&>(std::string::insert(pos, 1, c)); }
  using std::string::insert;

  // ** Casacore addition
  DEPRECATED("Use other overload, e.g. String::replace(pos, n1, 1, c)")
  String& replace(size_type pos, size_type n1, char c) {
    return static_cast<String&>(std::string::replace(pos, n1, 1, c)); }
  // ** Casacore addition
  DEPRECATED("Use other overload, e.g. String::replace(i1, i2, 1, c)")
  String& replace(iterator i1, iterator i2, char c) {
    return static_cast<String&>(std::string::replace(i1, i2, 1, c)); }
  using std::string::replace;

  // ** Casacore synonym
  DEPRECATED("Use c_str()")
  const char *chars() const { return std::string::c_str(); }

  // Create a formatted string using the given printf format string.
  DEPRECATED("Use std::format if possible, or FormatString() if the format string can't be changed (e.g. runtime determined)")
  static String format (const char* picture, ...);

  // Convert a String to a value. All characters in the string must be used.
  // It uses a shift from an ostringstream, so that operator must exist
  // for the data type used.
  // <br>In case of an error, an exception is thrown if <src>chk</src> is set.
  // Otherwise it returns false and <src>value</src> contains the value read
  // so far.
  // <group>
  template<typename T>
  DEPRECATED("Use StringToValue()")
  inline bool fromString (T& value, bool throw_on_error=true) const
  {
    return StringToValue(*this, value, throw_on_error);
  }
  template<typename T>
  DEPRECATED("Use StringToValue()")
  inline T fromString() const
  {
    T value;
    fromString(value);
    return value;
  }
  // </group>

  // Convert a string to an Int, Float or Double.
  // <br>In case of an error, an exception is thrown if <src>chk</src> is set.
  // Otherwise the value read so far is returned (0 if nothing read).
  // <group>
  DEPRECATED("Use StringToInt()")
  static int toInt (const String& s, bool chk=false) {
    int v=0;
    StringToValue(s, v, chk);
    return v;
  }
  DEPRECATED("Use StringToFloat()")
  static float toFloat (const String& s, bool chk=false) {
    float v=0;
    StringToValue(s, v, chk);
    return v;
  }
  DEPRECATED("Use StringToDouble()")
  static double toDouble (const String& s, bool chk=false) {
    double v=0;
    StringToValue(s, v, chk);
    return v;
  }
  // </group>

  // Convert a value to a String.
  // It uses a shift into an ostringstream, so that operator must be
  // defined for the data type used.
  template<typename T>
  DEPRECATED("Use std::to_string or ValueToString()")
  static String toString(const T& value)
  {
    return ValueToString(value);
  }

  // Remove beginning and ending whitespace.
  DEPRECATED("Use free function TrimInPlace()")
  void trim();

  // Remove specified chars from beginning and end of string.
  DEPRECATED("Use free function TrimInPlace()")
  void trim(char c[], unsigned int n);

  // Remove specified character from beginning of string.
  // If the character is repeated more than once on the left, all instances
  // will be removed; e.g. ltrim(',') results in ",,xy" becoming "xy".
  DEPRECATED("Use free function LTrimInPlace()")
  void ltrim(char c);

  // Remove specified character from end of string.
  // If the character is repeated more than once on the right, all instances
  // will be removed; e.g. rtrim(',') results in "xy,," becoming "xy".
  DEPRECATED("Use free function RTrimInPlace()")
  void rtrim(char c);

  // Does the string start with the specified string?
  DEPRECATED("Use String.starts_with(beginString)")
  bool startsWith(const std::string& beginString) const
    { return starts_with(beginString); }

  using std::string::find;
  DEPRECATED("Use std::regex")
  size_type find(const Regex &r, size_type pos=0) const;

   // Containment. ** Casacore addition
  // <group name=contains>
  DEPRECATED("Use find(c) != npos")
  bool contains(char c) const {
    return (find(c) != npos); }
  DEPRECATED("Use find(str) != npos or StringContains()")
  bool contains(const std::string &str) const {
    return (find(str) != npos); }
  DEPRECATED("Use find(s) != npos or StringContains()")
  bool contains(const char *s) const {
    return (find(s) != npos); }
  DEPRECATED("Use std::regex_search(str, regex);")
  bool contains(const Regex &r) const;
  // </group>
  // Does the string starting at the given position contain the given substring?
  // If the position is negative, it is counted from the end.
  // ** Casacore addition
  // <group name=contains_pos>
  DEPRECATED("Use find(), rfind() or IndexString()")
  bool contains(char c, int pos) const {
    return IndexString(*this, c, pos) != npos; }
  DEPRECATED("Use find(), rfind(), IndexString() and/or StringContains()")
  bool contains(const std::string &str, int pos) const {
    return (IndexString(*this, str, pos) != npos); }
  DEPRECATED("Use find(), rfind() or IndexString() and/or StringContains()")
  bool contains(const char *s, int pos) const {
    return (IndexString(*this, s, pos) != npos); }
  DEPRECATED("Use std::regex_search(str.substr(pos), regex);")
  bool contains(const Regex &r, int pos) const {
    return (index(r, pos) != npos); }
  // </group>

  // Matches entire string from pos, or till pos if negative pos. ** Casacore addition
  // Returns false if either is empty and pos >= 0.
  // The original implementation has multiple bugs and awkward behaviour.
  // BUG: Returns false for two empty strings (except if pos < 0).
  // BUG: Returns false when pattern is empty, whereas normally an empty pattern matches everything.
  // BUG: When pos < 0, it does not behave as advertised; e.g. "xyxy" does not match with "x" when pos=-1 is specified.
  // <group name=matches>
  DEPRECATED("Use substr(pos) == str if possible, consider ends_with() / starts_with() for positive/negative pos or use EqualStringsAndNotEmpty() if direct replacement is necessary")
  bool matches(const std::string &str, int pos = 0) const;
  DEPRECATED("Use substr(pos) == str if possible, consider ends_with() / starts_with() for positive/negative pos or use EqualStringsAndNotEmpty() if direct replacement is necessary")
  bool matches(char c, int pos = 0) const {
    return EqualStringsAndNotEmpty(*this, std::string_view(&c, 1), pos); }
  DEPRECATED("Use substr(pos) == str if possible, consider ends_with() / starts_with() for positive/negative pos or use EqualStringsAndNotEmpty() if direct replacement is necessary")
  bool matches(const char *s, int pos = 0) const {
    return EqualStringsAndNotEmpty(*this, s, pos); }
  DEPRECATED("Use RegexMatches()")
  bool matches(const Regex &r, int pos = 0) const {
    return RegexMatches(*this, r, pos);
  }
  // </group>

  DEPRECATED("Use insert(0, str)")
  void prepend(const std::string &str);
  DEPRECATED("Use insert(0, str)")
  void prepend(const char *str);
  DEPRECATED("Use insert(0, c)")
  void prepend(char c);

  // Return the position of the target in the string or npos for failure.
  // Searches the first index if the startpos >= 0, or the last index if
  // startpos < 0.
  // ** Casacore addition
  DEPRECATED("Use find() or rfind(), or IndexString() if the search direction is runtime-dependent")
  size_type index(char c, int startpos = 0) const {
    return ((startpos >= 0) ? find(c, startpos) :
	    rfind(c, length() + startpos - 1)); }
  DEPRECATED("Use find() or rfind(), or IndexString() if the search direction is runtime-dependent")
  size_type index(const std::string &str, int startpos = 0) const {
    return ((startpos >= 0) ? find(str, startpos) :
	    rfind(str, length() + startpos - str.length())); }
  DEPRECATED("Use find() or rfind(), or IndexString() if the search direction is runtime-dependent")
  size_type index(const char *s, int startpos = 0) const {
    return ((startpos >= 0) ? find(s, startpos) :
	    rfind(s, length() + startpos - traits_type::length(s))); }
  DEPRECATED("Use RegexIndex()")
  size_type index(const Regex &r, int startpos = 0) const;

  //  Return the number of occurences of target in String. ** Casacore addition
  // <group name=freq>
  DEPRECATED("Use std::count()")
  int freq(char c) const;
  DEPRECATED("Use SubStringCount()")
  int freq(const std::string &str) const;
  DEPRECATED("Use SubStringCount()")
  int freq(const char *s) const;
  // </group>

  DEPRECATED("Use substr()")
  SubString at(size_type pos, size_type len);
  DEPRECATED("Use substr()")
  String at(size_type pos, size_type len) const {
    return String(*this, pos, len); }

  DEPRECATED("Use a combination of find and/or replace")
  SubString at(const std::string &str, int startpos = 0);
  DEPRECATED("Use GetSubViewFrom()")
  String at(const std::string &str, int startpos = 0) const;
  DEPRECATED("Use a combination of find and/or replace")
  SubString at(const char *s, int startpos = 0);
  DEPRECATED("Use GetSubViewFrom()")
  String at(const char *s, int startpos = 0) const;
  DEPRECATED("Use a combination of find and/or replace")
  SubString at(char c, int startpos = 0);
  DEPRECATED("Use GetSubViewFrom()")
  String at(char c, int startpos = 0) const;
  DEPRECATED("Use RegexSubStr")
  SubString at(const Regex &r, int startpos = 0);
  DEPRECATED("Use RegexSubStr")
  String at(const Regex &r, int startpos = 0) const;
  // Next ones for overloading reasons. 
  // <note role=tip> It is better to use the <src>substr()</src> method
  // in stead. </note>
  // <group>
  DEPRECATED("Use substr()")
  SubString at(int pos, int len) {
    return at(static_cast<size_type>(pos), static_cast<size_type>(len));
  };
  DEPRECATED("Use substr()")
  String at(int pos, int len) const {
    return at(static_cast<size_type>(pos), static_cast<size_type>(len));
  };
  DEPRECATED("Use substr()")
  SubString at(int pos, size_type len) {
    return at(static_cast<size_type>(pos), len);
  };
  DEPRECATED("Use substr()")
  String at(int pos, size_type len) const {
    return at(static_cast<size_type>(pos), len);
  };
  using std::string::at;
  // </group>

  // Start at startpos and extract the string "before" the argument's 
  // position, exclusive. ** Casacore addition
  // <group name=before>
  DEPRECATED("Use substr(0, pos) or create a view. Also consider if str.start_with() suffices.")
  SubString before(size_type pos);
  DEPRECATED("Use GetStringUpToExcluding()")
  SubString before(const std::string &str, size_type startpos = 0);
  DEPRECATED("Use GetStringUpToExcluding()")
  SubString before(const char *s, size_type startpos = 0);
  DEPRECATED("Use GetStringUpToExcluding()")
  SubString before(char c, size_type startpos = 0);
  DEPRECATED("Use std::regex")
  SubString before(const Regex &r, size_type startpos = 0);
  // Next one for overloading reasons
  DEPRECATED("Use substr(0, pos)")
  SubString before(int pos) {
    return before(static_cast<size_type>(pos)); };    
  // </group>

  // Start at startpos and extract the SubString "through" to the argument's 
  // position, inclusive. ** Casacore addition
  // <group name=through>
  DEPRECATED("Use substr(0, pos+1)")
  SubString through(size_type pos);
  DEPRECATED("Use GetStringUpToIncluding()")
  SubString through(const std::string &str, size_type startpos = 0);
  DEPRECATED("Use GetStringUpToIncluding()")
  SubString through(const char *s, size_type startpos = 0);
  DEPRECATED("Use GetStringUpToIncluding()")
  SubString through(char c, size_type startpos = 0);
  DEPRECATED("Use std::regex")
  SubString through(const Regex &r, size_type startpos = 0);
  // Next one for overloading reasons
  DEPRECATED("Use substr(0, pos+1)")
  SubString through(int pos) {
    return through(static_cast<size_type>(pos)); }
  // </group>

  // Start at startpos and extract the SubString "from" the argument's 
  // position, inclusive, to the String's end. ** Casacore addition
  // <group name=from>
  DEPRECATED("Use substr(pos)")
  SubString from(size_type pos);
  DEPRECATED("Use GetStringFrom()")
  SubString from(const std::string &str, size_type startpos = 0);
  DEPRECATED("Use GetStringFrom()")
  SubString from(const char *s, size_type startpos = 0);
  DEPRECATED("Use GetStringFrom()")
  SubString from(char c, size_type startpos = 0);
  DEPRECATED("Use std::regex")
  SubString from(const Regex &r, size_type startpos = 0);
  // Next one for overloading reasons
  DEPRECATED("Use substr(pos)")
  SubString from(int pos) {
    return from(static_cast<size_type>(pos));
  };
  // </group>

  // Start at startpos and extract the SubString "after" the argument's 
  // position, exclusive, to the String's end. ** Casacore addition
  // <group name=after>
  DEPRECATED("Use substr(pos + 1)")
  SubString after(size_type pos);
  DEPRECATED("Use GetStringAfter()")
  SubString after(const std::string &str, size_type startpos = 0);
  DEPRECATED("Use GetStringAfter()")
  SubString after(const char *s, size_type startpos = 0);
  DEPRECATED("Use GetStringAfter()")
  SubString after(char c, size_type startpos = 0);
  DEPRECATED("Use std::regex")
  SubString after(const Regex &r, size_type startpos = 0);
  // Next one for overloading reasons
  DEPRECATED("Use substr(pos + 1)")
  SubString after(int pos) {
    return after(static_cast<size_type>(pos));
  };
  // </group>

  // Maybe forget some. ** Casacore addition
  // <group>
  // internal transformation to reverse order of String.
  DEPRECATED("Use std::reverse(str.begin(), str.end())")
  void reverse();
  // internal transformation to capitalization of String.
  DEPRECATED("Use CapitalizeStringInPlace()")
  void capitalize();
  // internal transformation to uppercase of String
  DEPRECATED("Use ToUpperCaseInPlace()")
  void upcase();
  // internal transformation to lowercase of String
  DEPRECATED("Use ToLowerCaseInPlace()")
  void downcase();
  // </group>

  // Delete len chars starting at pos. ** Casacore addition
  DEPRECATED("Use erase()")
  void del(size_type pos, size_type len);

  // Delete the first occurrence of target after startpos. ** Casacore addition
  //<group name=del_after>
  DEPRECATED("Use EraseStringFrom()")
  void del(const std::string &str, size_type startpos = 0);
  DEPRECATED("Use EraseStringFrom()")
  void del(const char *s, size_type startpos = 0);
  DEPRECATED("Use EraseStringFrom()")
  void del(char c, size_type startpos = 0);
  DEPRECATED("Use std::regex")
  void del(const Regex &r, size_type startpos = 0);
  // Overload problem
  DEPRECATED("Use erase()")
  void del(int pos, int len) {
    del(static_cast<size_type>(pos), static_cast<size_type>(len)); }
  //</group> 

  // Global substitution: substitute all occurrences of pat with repl, and
  // return the number of replacements.
  // ** Casacore addition
  //<group name=gsub>
  DEPRECATED("Use ReplaceAllInPlace()")
  int gsub(const std::string &pat, const std::string &repl);
  DEPRECATED("Use ReplaceAllInPlace()")
  int gsub(const char *pat, const std::string &repl);
  DEPRECATED("Use ReplaceAllInPlace()")
  int gsub(const char *pat, const char *repl);
  DEPRECATED("Use RegexReplaceAll")
  int gsub(const Regex &pat, const std::string &repl) {
    return RegexReplaceAll(*this, pat, repl);
  }
  //</group>

private:
  // Helper functions for at, before etc
  // <group>
  SubString _substr(size_type first, size_type l) const {
    return SubString(*this, first, l); }
  // </group>

  // Helper function for fromString.
  void throwFromStringError() const;
};

// <summary>
// Global concatenation operators
// </summary>

// The global concatenation operators
// <group name=concatenator>
inline String operator+(const String &lhs, const String &rhs) {
  String str(lhs); str.append(rhs); return str; }
inline String operator+(const char *lhs, const String &rhs) {
  String str(lhs); str.append(rhs); return str; }
inline String operator+(char lhs, const String &rhs) {
  String str(lhs); str.append(rhs); return str; }
inline String operator+(const String &lhs, const char *rhs) {
  String str(lhs); str.append(rhs); return str; }
inline String operator+(const String &lhs, char rhs) {
  String str(lhs); str.append(rhs); return str; }
// </group>

// <summary>
// Global comparison operators
// </summary>

// The global comparison operators
// <group name=comparitor>
inline bool operator==(const String &x, const String &y) {
  return x.compare(y) == 0; }
inline bool operator!=(const String &x, const String &y) {
  return x.compare(y) != 0; }
inline bool operator>(const String &x, const String &y) {
  return x.compare(y) > 0; }
inline bool operator>=(const String &x, const String &y) {
  return x.compare(y) >= 0; }
inline bool operator<(const String &x, const String &y) {
  return x.compare(y) < 0; }
inline bool operator<=(const String &x, const String &y) {
  return x.compare(y) <= 0; }
inline bool operator==(const String &x, const char *t) {
  return x.compare(t) == 0; }
inline bool operator!=(const String &x, const char *t) {
  return x.compare(t) != 0; }
inline bool operator>(const String &x, const char *t) {
  return x.compare(t) > 0; }
inline bool operator>=(const String &x, const char *t) {
  return x.compare(t) >= 0; }
inline bool operator<(const String &x, const char *t) {
  return x.compare(t) < 0; }
inline bool operator<=(const String &x, const char *t) {
  return x.compare(t) <= 0; }
inline bool operator==(const String &x, const char t) {
  return x.compare(String(t)) == 0; }
inline bool operator!=(const String &x, const char t) {
  return x.compare(String(t)) != 0; }
inline bool operator>(const String &x, const char t) {
  return x.compare(String(t)) > 0; }
inline bool operator>=(const String &x, const char t) {
  return x.compare(String(t)) >= 0; }
inline bool operator<(const String &x, const char t) {
  return x.compare(String(t)) < 0; }
inline bool operator<=(const String &x, const char t) {
  return x.compare(String(t)) <= 0; }
// ** Casacore additions of global compares. Returns 0 if equal; lt or gt 0 if
// strings unequal or of unequal lengths.
// <group>
inline int compare(const std::string &x, const std::string &y) {
  return x.compare(y); }
inline int compare(const std::string &x, const char *y) {
  return x.compare(y); }
inline int compare(const std::string &x, const char y) {
  return x.compare(String(y)); }
// this version ignores case. ** Casacore addition. Result is 0 if equal
// strings of equal lengths; else lt or gt 0 to indicate differences.
int fcompare(const String& x, const String& y);
// </group>
// </group>

// <summary> Splitting </summary>
// Global function which splits the String into string array res at separator
// and returns the number of elements.  ** Casacore addition
// <group name=split>
int split(const std::string &str, std::string res[], int maxn,
	  const std::string &sep);
int split(const std::string &str, std::string res[], int maxn,
	  const char sep);
int split(const std::string &str, std::string res[], int maxn,
	  const Regex &sep);
//</group> 

// <summary> Some general functions </summary>
// Functions to find special patterns, join and replicate
// <group name=common>
String common_prefix(const std::string &x, const std::string &y,
		     int startpos = 0);
String common_suffix(const std::string &x, const std::string &y,
		     int startpos = -1);
String replicate(char c, String::size_type n);
String replicate(const std::string &str, String::size_type n);
String join(std::string src[], int n, const std::string &sep);
// </group>

// <summary> Casing and related functions </summary>
// Case conversion and rearrangement functions
// <group name=case>
// Global function which returns a transformation to reverse order of String.
String reverse(const std::string& str);
// Global function which returns a transformation to uppercase of String.
String upcase(const std::string& str);
// Global function which returns a transformation to lowercase of String.
String downcase(const std::string& str);
// Global function which returns a transformation to capitalization of
// String.
String capitalize(const std::string& str);
// Global function which removes leading and trailing whitespace.
String trim(const std::string& str);
// </group>

// <summary> IO </summary>
// <group name=io>
// Output
std::ostream &operator<<(std::ostream &s, const String &x);
// </group>

inline SubString::SubString(const std::string &str, std::string::size_type pos,
			    std::string::size_type len) :
  ref_p(str), pos_p((pos > str.length()) ? str.length() : pos),
  len_p((len == std::string::npos || pos_p+len > str.length()) ?
	str.length()-pos_p : len) {}

inline SubString String::operator()(size_type pos, size_type len) {
  return at(pos, len); }
inline  const char *SubString::chars() const {
  return String(*this).c_str(); }

inline std::ostream &operator<<(std::ostream &s, const String &x) {
  s << x.c_str(); return s; }

// Remove specified chars from beginning and end of string.
void TrimInPlace(std::string& str, std::string_view characters = " \t\n\r");

// Remove specified character from beginning of string.
// If the character is repeated more than once on the left, all instances
// will be removed; e.g. LTrimInPlace(str, ',') results in ",,xy" becoming "xy".
inline void LTrimInPlace(std::string& str, char character) {
  const std::size_t pos = str.find_first_not_of(character);
  str.erase(0, pos);
}

// Remove specified characters from beginning of string.
// If the characters are repeated more than once on the left, all instances
// will be removed; e.g. LTrimInPlace(str, "*-") results in "-**-xy" becoming "xy".
inline void LTrimInPlace(std::string& str, std::string_view characters) {
  const std::size_t pos = str.find_first_not_of(characters);
  str.erase(0, pos);
}

// Remove specified character from end of string.
// If the character is repeated more than once on the right, all instances
// will be removed; e.g. RTrimInPlace(str, ',') results in "xy,," becoming "xy".
inline void RTrimInPlace(std::string& str, char character) {
  const size_t pos = str.find_last_not_of(character);
  if(pos == std::string::npos)
    str.clear();
  else
    str.resize(pos + 1);
}

// Remove specified characters from end of string.
// If the characters are repeated more than once on the right, all instances
// will be removed; e.g. RTrimInPlace(str, "*-") results in "xy--**-" becoming "xy".
inline void RTrimInPlace(std::string& str, std::string_view characters) {
  const size_t pos = str.find_last_not_of(characters);
  if(pos == std::string::npos)
    str.clear();
  else
    str.resize(pos + 1);
}

// Like sprintf. Don't use for new code -- to be deprecated if possible.
std::string FormatString(const char* format_string, ...);

// Returns the number of times a given pattern occurs in a string.
// E.g. SubStringCount("ababaaba", "aba") returns 3.
inline std::size_t SubStringCount(std::string_view str, std::string_view pattern) {
  std::size_t p = 0;
  std::size_t count = 0;
  while (p < str.length()) {
    if ((p = str.find(pattern, p)) == std::string::npos) break;
    count++;
    p++;
  }
  return count;
}

// Gets a part of a string up to and excluding a specified pattern.
// An optional @p start_position can be specified to skip the beginning of the string.
inline std::string_view GetStringUpToExcluding(std::string_view input, std::string_view pattern, size_t start_position = 0) {
  assert(start_position <= input.size());
  const std::size_t end = std::min(input.size(), input.find(pattern, start_position));
  // input.subview() is only available from C++26, so construct manually:
  return std::string_view(input.begin(), input.begin() + end);
}

// Rather specific function -- this is a replacement for String:through() with string argument, but maybe String::through() is rarely
// used, in which case this function should be removed.
inline std::string_view GetStringUpToIncluding(std::string_view input, std::string_view pattern, size_t start_position = 0) {
  assert(start_position <= input.size());
  std::size_t end = input.find(pattern, start_position);
  if(end == std::string_view::npos) {
    end = input.size();
  } else {
    end += pattern.size();
  }
  // input.subview() is only available from C++26, so construct manually:
  return std::string_view(input.begin(), input.begin() + end);
}

// Get a string starting at a specific pattern that is found in the string. The @p start_position can be used
// to start the search at a later position.
inline std::string_view GetStringFrom(std::string_view input, std::string_view pattern, size_t start_position = 0) {
  assert(start_position <= input.size());
  std::size_t start = input.find(pattern, start_position);
  if(start == std::string_view::npos)
    start = input.size();
  // input.subview() is only available from C++26, so construct manually:
  return std::string_view(input.begin() + start, input.end());
}

// Rather specific function -- this is a replacement for String:after(), but maybe String::after() is rarely
// used, in which case this function should be removed.
inline std::string_view GetStringAfter(std::string_view input, std::string_view pattern, size_t start_position = 0) {
  assert(start_position <= input.size());
  std::size_t start = input.find(pattern, start_position);
  if(start == std::string_view::npos) {
    start = input.size();
  } else {
    start += pattern.size();
  }
  // input.subview() is only available from C++26, so construct manually:
  return std::string_view(input.begin() + start, input.end());
}

// Converts the specified string to upper case, in place.
inline void ToUpperCaseInPlace(std::string& str) {
  std::transform(str.begin(), str.end(), str.begin(),
                 [](unsigned char c) { return static_cast<char>(std::toupper(c)); });
}

// Converts the specified string to lower case, in place.
inline void ToLowerCaseInPlace(std::string& str) {
  std::transform(str.begin(), str.end(), str.begin(),
                 [](unsigned char c) { return static_cast<char>(std::tolower(c)); });
}

// Changes the casing such that every separate word starts with an uppercase
// character and continues lowercase.
inline void CapitalizeStringInPlace(std::string& str) {
  std::string::iterator p=str.begin();
  while(p != str.end()) {
    bool at_word;
    if (islower(*p)) {
      *p = toupper(*p);
      at_word = true;
    } else {
      at_word = isupper(*p) || isdigit(*p);
    }
    ++p;
    // at_word is now true if the previous *p is a character or digit
    if (at_word) {
      while (p != str.end()) {
        if (isupper(*p)) {
          *p = tolower(*p);
        }
        else if (!islower(*p) && !isdigit(*p)) break;
        ++p;
      }
      if(p != str.end()) ++p;
    }
  }
}

// Remove the end of a string, starting from a specified pattern.
// A start_position can be specified to skip the beginning during the search for the pattern
inline void EraseStringFrom(std::string& str, std::string_view pattern, size_t start_position = 0) {
  const std::size_t start = str.find(pattern, start_position);
  if(start != std::string::npos)
    str.erase(start, pattern.length());
}

// Replace every occurence of @c pattern in @c str by @c replacement.
inline size_t ReplaceAllInPlace(std::string& str, std::string_view pattern, std::string_view replacement) {
  std::size_t n_matches = 0;
  if (str.length() == 0 || pattern.length() == 0 ||
      str.length() < pattern.length()) return n_matches;
  std::size_t search_index = 0;
  while (str.length()-search_index >= pattern.length()) {
    const std::size_t pos = str.find(pattern, search_index);
    if (pos == std::string::npos) break;
    else {
      n_matches++;
      str.replace(pos, pattern.length(), replacement);
      search_index = pos + replacement.length();
    }
  }
  return n_matches;
}

// Determines if a given string contains another given string.
inline constexpr bool StringContains(std::string_view str, std::string_view pattern) {
  return str.find(pattern) != std::string_view::npos;
}

} //# NAMESPACE CASACORE - END


// Define the hash function for String, so unordered_set<String> can be used.
namespace std {
template<>
struct hash<casacore::String>
{
  std::size_t operator()(casacore::String const& k) const noexcept
    { return std::hash<std::string>()(k); }
};
} // namespace std

#ifdef CASACORE_DEPRECATE_STRING
#pragma GCC diagnostic pop
#endif

#undef DEPRECATED

#endif
