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

#ifndef CASA_STRING_H
#define CASA_STRING_H

#include <casacore/casa/aips.h>

#include <sstream>
#include <string>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations
class String;
class Regex;

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
  operator const std::string() const { return std::string(ref_p, pos_p, len_p); }
  // Default copy constructor.
  SubString (const SubString&) = default;
  // Assignment
  // <group>
  SubString &operator=(const SubString &str);
  SubString &operator=(const String &str);
  SubString &operator=(const Char *s);
  SubString &operator=(const Char c);
  // </group>
  // Get as (const) C array
  const Char *chars() const;
  // Obtain length
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
// <li> A Char* argument - <src>String myWord("Yowza");</src>
// <li> The first n chararcters of a pre-existing string - 
// <src>String myFoo("fooey", 3);</src>
// </ol> As well as the copy and default constructors and iterator based ones.
//
// A String may be concatinated with another object (String, or 
// char*) with either prepending or postpending.  A search for the position
// of a character within a String may return its position, a Bool that it
// is contained within or a Bool confirming your guess at the character's 
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
// <li> Construction from (part of) String, (part of) Char*,
//		(repeating) Char, iterator pair.
// <li> Assignment from String, Char*, Char
// <li> Iterators: begin() and end(); rbegin() and rend() (Note: gcc reverse
//		iterators still weak)
// <li> Capacity: size, length, max_size, resize, capacity, reserve, clear,
//		empty
// <li> Special size: String::size_type, with indicator: String::npos
// <li> Element access: [pos] and at(pos) (both const and non-const)
// <li> Modifiers: += of String, Char*, Char; append of (part of) String,
//		Char*, Char and iterator defined; assign() of (part of)
//		String, Char* and (repeating) Char and iterator;
//		insertion of same; replacing of same; erase of part of
//		String; a copy and a swap.
// <li> C-string: get Char* with c_str() or data() and get the relevant
//		Allocator used (Note: not fully supported in gcc)
// <li> Operations: find, rfind, find_first_of, find_last_of, find_first_not_of,
//		find_last_not_of; substr (Note only readable substring);
//		compare with (part of) String, Char*
// <li> Globals: Addition operators for String, Char*, Char; all comparison
//		operators for String and Char*; getline; input and output
//		stream operators
// <li> Typedef: All relevant typedefs for standard containers and iterator
// 		handling
// </ol>
// The Casacore additions are:
// <ol>
// <li> To standard: some Char function arguments where appropriate; Regex
//		arguments in search like methods.
// <li> Substring additions: at, before, after, from, through functions taking
//		search String, Char* as arguments can give (hidden) substrings
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
// Bool query = myString.contains("good men");
// // ask if the position we think is true is correct...
// Bool answer = allKeys.matches(finishIt, position);
// // How many spaces are in our phrase?
// Int spacesCount = allKeys.freq(" ");
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

  //# Basic container typedefs
  typedef std::string::traits_type 		traits_type;
  typedef std::string::value_type		value_type;
  typedef std::string::allocator_type	allocator_type;
  typedef std::string::size_type 		size_type;
  typedef std::string::difference_type	difference_type;

  typedef std::string::reference 		reference;
  typedef std::string::const_reference 	const_reference;
  typedef std::string::pointer		pointer;
  typedef std::string::const_pointer		const_pointer;

  typedef std::string::iterator iterator;
  typedef std::string::const_iterator const_iterator;
  typedef std::string::reverse_iterator reverse_iterator;
  typedef std::string::const_reverse_iterator const_reverse_iterator;
  //# Next cast necessary to stop warning in gcc
  static const size_type npos = static_cast<size_type>(-1);

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
  String(const Char* s, size_type n) : std::string(s, n) {}
  // Construct from char array
  String(const Char* s) : std::string(s) {}
  // Construct from a single char (repeated n times)
  // <thrown>
  // <li> length_error if n == npos
  // </thrown>
  String(size_type n, Char c) : std::string(n, c) {}
  // Construct from iterator
  template<class InputIterator>
    String(InputIterator begin, InputIterator end) : std::string(begin, end) {}
  // From single char (** Casacore addition).
  // <note role=warning> Note that there is no automatic Char-to-String
  // conversion available. This stops inadvertent conversions of
  // integer to string. </note>
  explicit String(Char c) : std::string(1, c) {}
  // Construct from a SubString
  String(const SubString &str) : std::string(str.ref_p, str.pos_p, str.len_p) {}
  // Construct from a stream.
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
  String& operator=(const Char* s) {
    return static_cast<String&>(std::string::operator=(s)); }
  String& operator=(Char c) {
    return static_cast<String&>(std::string::operator=(c)); }
  // </group>
  // ** Casacore addition: synonym for at(pos, len)
  SubString operator()(size_type pos, size_type len);
  // Concatenate
  // <group>
  String& operator+=(const std::string& str) {
    return static_cast<String&>(std::string::operator+=(str)); }
  String& operator+=(const Char* s) {
    return static_cast<String&>(std::string::operator+=(s)); }
  String& operator+=(Char c) {
    return static_cast<String&>(std::string::operator+=(c)); }
  // </group>

  // Indexing. The standard version is undefined if <src>pos > size()</src>, or 
  // <src>pos >= size()</src> for non-const version. 
  // <note role=warning> The const_reference version needs the at() version
  // for the gcc compiler: no const [] exists. </note>
  // <group>
  const_reference operator[](size_type pos) const {
    return std::string::at(pos); }
  reference operator[](size_type pos) {
    return std::string::operator[](pos); }
  // *** Casacore addition
  // <group>
  const_reference elem(size_type pos) const {
    return std::string::at(pos); }
  Char firstchar() const { return at(static_cast<size_type>(0)); }
  Char lastchar() const { return at(length()-1); }
  // </group>
  // </group>

  //# Member functions
  // Iterators
  // <group>
  iterator begin() { return std::string::begin(); }
  const_iterator begin() const { return std::string::begin(); }
  iterator end() { return std::string::end(); }
  const_iterator end() const { return std::string::end(); }
  reverse_iterator rbegin() { return std::string::rbegin(); }
  const_reverse_iterator rbegin() const { return std::string::rbegin(); }
  reverse_iterator rend() { return std::string::rend(); }
  const_reverse_iterator rend() const { return std::string::rend(); }
  // </group>

  // Capacity, size
  // <group>
  size_type size() const { return std::string::size(); }
  size_type length() const { return std::string::length(); }
  size_type max_size() const { return std::string::max_size(); }
  size_type capacity() const { return std::string::capacity(); }
  // ** Casacore addition -- works as a capacity(n) -- Note Int
  Int allocation() const { return std::string::capacity(); }
  // </group>

  // Resize by truncating or extending with copies of <src>c</src> (default 
  // Char())
  // <thrown>
  // <li> length_error if n > max_size()
  // <li> length_error if res_arg > max_size()
  // </thrown>
  // <group>
  // <note role=tip> The reserve length given is non-binding on the
  // implementation </note>
  String& resize(size_type n) {
    std::string::resize(n); return *this; }
  String& resize(size_type n, Char c) {
    std::string::resize(n, c); return *this; }
  String& reserve(size_type res_arg = 0) {
    std::string::reserve(res_arg); return *this; }
  // ** Casacore addition -- works as a resize(n)
  void alloc(size_type n) { std::string::resize(n); }
  // </group>

  // Clear the string
  // <note role=warning> clear() executed as erase() due to missing clear() in
  // gcc </note> 
  void clear() { std::string::erase(begin(), end()); }

  // Test for empty
  Bool empty() const { return std::string::empty(); }

  // Addressing
  // <thrown>
  // <li> out_of_range if pos >= size()
  // </thrown>
  // <group>
  const_reference at(size_type n) const { return std::string::at(n); }
  reference at(size_type n) { return std::string::at(n); }
  // </group>

  // Append
  // <thrown>
  // <li> out_of_range if pos > str.size()
  // <li> length_error if new size() >= npos
  // </thrown>
  // <note role=warning> The standard has a 
  // <src>void push_back(const Char) </src> which is completely undefined. It
  // probably is a remnant of the full list of container functions pop/push
  // back/front. </note>
  // <group>
  String& append(const std::string& str) {
    return static_cast<String&>(std::string::append(str)); }
  String& append(const std::string& str, size_type pos, size_type n) {
    return static_cast<String&>(std::string::append(str, pos, n)); }
  String& append(const Char* s, size_type n) {
    return static_cast<String&>(std::string::append(s, n)); }
  String& append(const Char* s) {
    return static_cast<String&>(std::string::append(s)); }
  String& append(size_type n, Char c) {
    return static_cast<String&>(std::string::append(n, c)); }
  template<class InputIterator>
    String& append(InputIterator first, InputIterator last) {
    return static_cast<String&>(std::string::append(first, last)); }
  // ** Casacore addition
  String& append(Char c) {
    return static_cast<String&>(std::string::append(1, c)); }
  // </group>

  // Assign
  // <thrown>
  // <li> out_of_range if pos > str.size()
  // </thrown>
  // <group>
  String& assign(const std::string& str) {
    return static_cast<String&>(std::string::assign(str)); }
  String& assign(const std::string& str, size_type pos, size_type n) {
    return static_cast<String&>(std::string::assign(str, pos, n)); }
  String& assign(const Char* s, size_type n) {
    return static_cast<String&>(std::string::assign(s, n)); }
  String& assign(const Char* s) {
    return static_cast<String&>(std::string::assign(s)); }
  String& assign(size_type n, Char c) {
    return static_cast<String&>(std::string::assign(n, c)); }
  template<class InputIterator>
    String& assign(InputIterator first, InputIterator last) {
    return static_cast<String&>(std::string::assign(first, last)); }
  // ** Casacore addition
  String& assign(Char c)  {
    return static_cast<String&>(std::string::assign(1, c)); }
  // </group>

  // Insert
  // <thrown>
  // <li> out_of_range if pos1 > str.size() or pos2 > str.size()
  // <li> length_error if new size() >= npos
  // </thrown>
  // <group>
  String& insert(size_type pos1, const std::string& str) {
    return static_cast<String&>(std::string::insert(pos1, str)); }
  String& insert(size_type pos1, const std::string& str,
		 size_type pos2, size_type n) {
    return static_cast<String&>(std::string::insert(pos1, str, pos2, n)); }
  String& insert(size_type pos, const Char* s, size_type n) {
    return static_cast<String&>(std::string::insert(pos, s, n)); }
  String& insert(size_type pos, const Char* s) {
    return static_cast<String&>(std::string::insert(pos, s)); }
  String& insert(size_type pos, size_type n, Char c) {
    return static_cast<String&>(std::string::insert(pos, n, c)); }
  // ** Casacore addition
  String& insert(size_type pos, Char c) {
    return static_cast<String&>(std::string::insert(pos, 1, c)); }

  iterator insert(iterator p, Char c) {
    return std::string::insert(p, c); }
  void insert(iterator p, size_type n, Char c) {
    std::string::insert(p, n, c); }
  template<class InputIterator>
    void insert(iterator p, InputIterator first, InputIterator last) {
    std::string::insert(p, first, last); }
  // ** Casacore additions
  // <group>
  String& insert(iterator p, const std::string& str) {
    return static_cast<String&>(std::string::insert(p-begin(), str)); }
  String& insert(iterator p, const Char* s, size_type n) {
    return static_cast<String&>(std::string::insert(p-begin(), s, n)); }
  String& insert(iterator p, const Char* s) {
    return static_cast<String&>(std::string::insert(p-begin(), s)); }
  // </group>
  // </group>

  // Compare. Returns 0 if strings equal and of equal size; else positive if
  // str larger or longer; else negative.
  // <note role=warning> The gcc compiler does not have the proper standard
  // compare functions. Hence they are locally implemented. </note>
  // <group>
  Int compare(const std::string& str) const {
    return std::string::compare(str); }
  Int compare(size_type pos1, size_type n1, const std::string& str) const {
    return String(*this, pos1, n1).compare(str); }
  Int compare(size_type pos1, size_type n1, const std::string& str,
	      size_type pos2, size_type n2) const {
    return String(*this, pos1, n1).compare(String(str, pos2, n2)); }
  Int compare(const Char* s) const {
    return std::string::compare(s); }
  Int compare(size_type pos1, size_type n1, const Char* s,
	      size_type n2=npos) const {
    return String(*this, pos1, n1).compare(String(s, n2)); }
  // </group>

  // Erase
  // <group>
  String& erase(size_type pos, size_type n = npos) {
    return static_cast<String&>(std::string::erase(pos, n)); }
  iterator erase(iterator position) {
    return std::string::erase(position); }
  iterator erase(iterator first, iterator last) {
    return std::string::erase(first, last); }
  // </group>

  // Replace
  // <thrown>
  // <li> out_of_range if pos1 > str.size() or pos2 > str.size()
  // <li> length_error if new size() > npos
  // </thrown>
  // <group>
  String& replace(size_type pos1, size_type n1, const std::string& str) {
    return static_cast<String&>(std::string::replace(pos1, n1, str)); }
  String& replace(size_type pos1, size_type n1, const std::string& str,
		  size_type pos2, size_type n2) {
    return static_cast<String&>(std::string::replace(pos1, n1, str, pos2, n2)); }
  String& replace(size_type pos, size_type n1, const Char* s, size_type n2) {
    return static_cast<String&>(std::string::replace(pos, n1, s, n2)); }
  String& replace(size_type pos, size_type n1, const Char* s) {
    return static_cast<String&>(std::string::replace(pos, n1, s)); }
  String& replace(size_type pos, size_type n1, size_type n2, Char c) {
    return static_cast<String&>(std::string::replace(pos, n1, n2, c)); }
  // ** Casacore addition
  String& replace(size_type pos, size_type n1, Char c) {
    return static_cast<String&>(std::string::replace(pos, n1, 1, c)); }
  String& replace(iterator i1, iterator i2, const std::string& str) {
    return static_cast<String&>(std::string::replace(i1, i2, str)); }
  String& replace(iterator i1, iterator i2, const Char* s, size_type n) {
    return static_cast<String&>(std::string::replace(i1, i2, s, n)); }
  String& replace(iterator i1, iterator i2, const Char* s) {
    return static_cast<String&>(std::string::replace(i1, i2, s)); }
  String& replace(iterator i1, iterator i2, size_type n, Char c) {
    return static_cast<String&>(std::string::replace(i1, i2, n, c)); }
  // ** Casacore addition
  String& replace(iterator i1, iterator i2, Char c) {
    return static_cast<String&>(std::string::replace(i1, i2, 1, c)); }
  template<class InputIterator>
    String& replace(iterator i1, iterator i2, InputIterator j1, 
		    InputIterator j2) {
    return static_cast<String&>(std::string::replace(i1, i2, j1, j2)); }
  // </group>

  // Copy
  // <thrown>
  // <li> out_of_range if pos > size()
  // </thrown>
  size_type copy(Char* s, size_type n, size_type pos = 0) const {
    return std::string::copy(s, n, pos); }

  // Swap
  void swap(std::string& s) { std::string::swap(s); }

  // Get char array
  // <group>
  // As a proper null terminated C-string
  const Char *c_str() const { return std::string::c_str(); }
  // As pointer to char array 
  const Char *data() const { return std::string::data(); }
  // ** Casacore synonym
  const Char *chars() const { return std::string::c_str(); }
  // </group>

  // Get allocator used
  // <note role=warning> gcc has no get_allocator() </note>
  allocator_type get_allocator() const { return std::string::allocator_type(); }

  // Get a sub string
  // <thrown>
  // <li> out_of_range if pos > size()
  // </thrown>
  String substr(size_type pos=0, size_type n=npos) const {
    return String(*this, pos, n); }

  // Create a formatted string using the given printf format string.
  static String format (const char* picture, ...);

  // Convert a String to a value. All characters in the string must be used.
  // It uses a shift from an ostringstream, so that operator must exist
  // for the data type used.
  // <br>In case of an error, an exception is thrown if <src>chk</src> is set.
  // Otherwise it returns False and <src>value</src> contains the value read
  // so far.
  // <group>
  template<typename T> inline Bool fromString (T& value, Bool chk=True) const
  {
    std::istringstream os(*this);
    os >> value;
    if (os.fail()  ||  !os.eof()) {
      if (chk) throwFromStringError();
      return False;
    }
    return True;
  }
  template<typename T> inline T fromString() const
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
  static Int toInt (const String& s, Bool chk=False);
  static Float toFloat (const String& s, Bool chk=False);
  static Double toDouble (const String& s, Bool chk=False);
  // </group>

  // Convert a value to a String.
  // It uses a shift into an ostringstream, so that operator must be
  // defined for the data type used.
  template<typename T>
  static String toString(const T& value)
  {
    std::ostringstream os;
    os << value;
    return os.str();
  }

  // Remove beginning and ending whitespace.
  void trim();

  // Remove specified chars from beginning and end of string.
  void trim(char c[], uInt n);

  // Remove specified character from beginning of string.
  // If the character is repeated more than once on the left, all instances
  // will be removed; e.g. ltrim(',') results in ",,xy" becoming "xy".
  void ltrim(char c);

  // Remove specified character from end of string.
  // If the character is repeated more than once on the right, all instances
  // will be removed; e.g. rtrim(',') results in "xy,," becoming "xy".
  void rtrim(char c);

  // Does the string start with the specified string?
  Bool startsWith(const std::string& beginString) const
    { return find(beginString) == 0; }

  // Search functions. Returns either npos (if not found); else position.
  // <note role=warning> The Regex ones are ** Casacore additions</note>
  // <group>
  size_type find(const std::string &str, size_type pos=0) const {
    return std::string::find(str, pos); }
  size_type find(const Char *s, size_type pos=0) const {
    return std::string::find(s, pos); }
  size_type find(const Char *s, size_type pos, size_type n) const {
    return std::string::find(s, pos, n); }
  size_type find(Char c, size_type pos=0) const {
    return std::string::find(c, pos); }
  size_type find(const Regex &r, size_type pos=0) const;
  size_type rfind(const std::string &str, size_type pos=npos) const {
    return std::string::rfind(str, pos); }
  size_type rfind(const Char *s, size_type pos=npos) const {
    return std::string::rfind(s, pos); }
  size_type rfind(const Char *s, size_type pos, size_type n) const {
    return std::string::rfind(s, pos, n); }
  size_type rfind(Char c, size_type pos=npos) const {
    return std::string::rfind(c, pos); }
  size_type find_first_of(const std::string &str, size_type pos=0) const {
    return std::string::find_first_of(str, pos); }
  size_type find_first_of(const Char *s, size_type pos=0) const {
    return std::string::find_first_of(s, pos); }
  size_type find_first_of(const Char *s, size_type pos, size_type n) const {
    return std::string::find_first_of(s, pos, n); }
  size_type find_first_of(Char c, size_type pos=0) const {
    return std::string::find_first_of(c, pos); }
  size_type find_last_of(const std::string &str, size_type pos=npos) const {
    return std::string::find_last_of(str, pos); }
  size_type find_last_of(const Char *s, size_type pos=npos) const {
    return std::string::find_last_of(s, pos); }
  size_type find_last_of(const Char *s, size_type pos, size_type n) const {
    return std::string::find_last_of(s, pos, n); }
  size_type find_last_of(Char c, size_type pos=npos) const {
    return std::string::find_last_of(c, pos); }
  size_type find_first_not_of(const std::string &str, size_type pos=0) const {
    return std::string::find_first_not_of(str, pos); }
  size_type find_first_not_of(const Char *s, size_type pos=0) const {
    return std::string::find_first_not_of(s, pos); }
  size_type find_first_not_of(const Char *s, size_type pos, size_type n) const {
    return std::string::find_first_not_of(s, pos, n); }
  size_type find_first_not_of(Char c, size_type pos=0) const {
    return std::string::find_first_not_of(c, pos); }
  size_type find_last_not_of(const std::string &str, size_type pos=npos) const {
    return std::string::find_last_not_of(str, pos); }
  size_type find_last_not_of(const Char *s, size_type pos=npos) const {
    return std::string::find_last_not_of(s, pos); }
  size_type find_last_not_of(const Char *s, size_type pos, size_type n) const {
    return std::string::find_last_not_of(s, pos, n); }
  size_type find_last_not_of(Char c, size_type pos=npos) const {
    return std::string::find_last_not_of(c, pos); }
  // </group>
  
  // Containment. ** Casacore addition
  // <group name=contains>
  Bool contains(Char c) const {
    return (find(c) != npos); }
  Bool contains(const std::string &str) const {
    return (find(str) != npos); }
  Bool contains(const Char *s) const {
    return (find(s) != npos); }
  Bool contains(const Regex &r) const;
  // </group>
  // Does the string starting at the given position contain the given substring?
  // If the position is negative, it is counted from the end.
  // ** Casacore addition
  // <group name=contains_pos>
  Bool contains(Char c, Int pos) const;
  Bool contains(const std::string &str, Int pos) const;
  Bool contains(const Char *s, Int pos) const;
  Bool contains(const Regex &r, Int pos) const;
  // </group>

  // Matches entire string from pos
  // (or till pos if negative pos). ** Casacore addition
  // <group name=matches>
  Bool matches(const std::string &str, Int pos = 0) const;
  Bool matches(Char c, Int pos = 0) const {
    return matches(String(c), pos); };
  Bool matches(const Char *s, Int pos = 0) const {
    return matches(String(s), pos); };
  Bool matches(const Regex &r, Int pos = 0) const;
  // </group>

  // Concatenate by prepending the argument onto String. ** Casacore addition
  // <group name=concatenation_method>
  void prepend(const std::string &str);
  void prepend(const Char *s);
  void prepend(Char c);
  // </group> 

  // Return the position of the target in the string or npos for failure.
  // ** Casacore addition
  // <group name=index>
  size_type index(Char c, Int startpos = 0) const {
    return ((startpos >= 0) ? find(c, startpos) :
	    rfind(c, length() + startpos - 1)); }
  size_type index(const std::string &str, Int startpos = 0) const {
    return ((startpos >= 0) ? find(str, startpos) :
	    rfind(str, length() + startpos - str.length())); }
  size_type index(const Char *s, Int startpos = 0) const {
    return ((startpos >= 0) ? find(s, startpos) :
	    rfind(s, length() + startpos - traits_type::length(s))); }
  size_type index(const Regex &r, Int startpos = 0) const;
  // </group>

  //  Return the number of occurences of target in String. ** Casacore addition
  // <group name=freq>
  Int freq(Char c) const; 
  Int freq(const std::string &str) const;
  Int freq(const Char *s) const;
  // </group>

  // Extract the string "at" the argument's position. ** Casacore addition
  // <group name=at>
  SubString at(size_type pos, size_type len);
  String at(size_type pos, size_type len) const {
    return String(*this, pos, len); }
  SubString at(const std::string &str, Int startpos = 0);
  String at(const std::string &str, Int startpos = 0) const;
  SubString at(const Char *s, Int startpos = 0);
  String at(const Char *s, Int startpos = 0) const;
  SubString at(Char c, Int startpos = 0);
  String at(Char c, Int startpos = 0) const;
  SubString at(const Regex &r, Int startpos = 0); 
  String at(const Regex &r, Int startpos = 0) const; 
  // Next ones for overloading reasons. 
  // <note role=tip> It is better to use the <src>substr()</src> method
  // in stead. </note>
  // <group>
  SubString at(Int pos, Int len) {
    return at(static_cast<size_type>(pos), static_cast<size_type>(len));
  };
  String at(Int pos, Int len) const {
    return at(static_cast<size_type>(pos), static_cast<size_type>(len));
  };
  SubString at(Int pos, size_type len) {
    return at(static_cast<size_type>(pos), len);
  };
  String at(Int pos, size_type len) const {
    return at(static_cast<size_type>(pos), len);
  };
  // </group>
  // </group>

  // Start at startpos and extract the string "before" the argument's 
  // position, exclusive. ** Casacore addition
  // <group name=before>
  SubString before(size_type pos) const;
  SubString before(const std::string &str, size_type startpos = 0) const;
  SubString before(const Char *s, size_type startpos = 0) const;
  SubString before(Char c, size_type startpos = 0) const;
  SubString before(const Regex &r, size_type startpos = 0) const;
  // Next one for overloading reasons
  SubString before(Int pos) const {
    return before(static_cast<size_type>(pos)); };    
  // </group>

  // Start at startpos and extract the SubString "through" to the argument's 
  // position, inclusive. ** Casacore addition
  // <group name=through>
  SubString through(size_type pos);
  SubString through(const std::string &str, size_type startpos = 0);
  SubString through(const Char *s, size_type startpos = 0);
  SubString through(Char c, size_type startpos = 0);
  SubString through(const Regex &r, size_type startpos = 0);
  // Next one for overloading reasons
  SubString through(Int pos) {
    return through(static_cast<size_type>(pos)); }
  // </group>

  // Start at startpos and extract the SubString "from" the argument's 
  // position, inclusive, to the String's end. ** Casacore addition
  // <group name=from>
  SubString from(size_type pos);
  SubString from(const std::string &str, size_type startpos = 0);
  SubString from(const Char *s, size_type startpos = 0);
  SubString from(Char c, size_type startpos = 0);
  SubString from(const Regex &r, size_type startpos = 0);
  // Next one for overloading reasons
  SubString from(Int pos) {
    return from(static_cast<size_type>(pos));
  };
  // </group>

  // Start at startpos and extract the SubString "after" the argument's 
  // position, exclusive, to the String's end. ** Casacore addition
  // <group name=after>
  SubString after(size_type pos);
  SubString after(const std::string &str, size_type startpos = 0);
  SubString after(const Char *s, size_type startpos = 0);
  SubString after(Char c, size_type startpos = 0);
  SubString after(const Regex &r, size_type startpos = 0);
  // Next one for overloading reasons
  SubString after(Int pos) {
    return after(static_cast<size_type>(pos));
  };
  // </group>

  // Maybe forget some. ** Casacore addition
  // <group>
  // internal transformation to reverse order of String.
  void reverse();
  // internal transformation to capitalization of String.
  void capitalize();
  // internal transformation to uppercase of String
  void upcase();
  // internal transformation to lowercase of String
  void downcase();
  // </group>

  // Delete len chars starting at pos. ** Casacore addition
  void del(size_type pos, size_type len);

  // Delete the first occurrence of target after startpos. ** Casacore addition
  //<group name=del_after>
  void del(const std::string &str, size_type startpos = 0);
  void del(const Char *s, size_type startpos = 0);
  void del(Char c, size_type startpos = 0);
  void del(const Regex &r, size_type startpos = 0);
  // Overload problem
  void del(Int pos, Int len) {
    del(static_cast<size_type>(pos), static_cast<size_type>(len)); }
  //</group> 

  // Global substitution: substitute all occurrences of pat with repl, and
  // return the number of replacements.
  // ** Casacore addition
  //<group name=gsub>
  Int gsub(const std::string &pat, const std::string &repl);
  Int gsub(const Char *pat, const std::string &repl);
  Int gsub(const Char *pat, const Char *repl);
  Int gsub(const Regex &pat, const std::string &repl);
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
inline String operator+(const Char *lhs, const String &rhs) {
  String str(lhs); str.append(rhs); return str; }
inline String operator+(Char lhs, const String &rhs) {
  String str(lhs); str.append(rhs); return str; }
inline String operator+(const String &lhs, const Char *rhs) {
  String str(lhs); str.append(rhs); return str; }
inline String operator+(const String &lhs, Char rhs) {
  String str(lhs); str.append(rhs); return str; }
// </group>

// <summary>
// Global comparison operators
// </summary>

// The global comparison operators
// <group name=comparitor>
inline Bool operator==(const String &x, const String &y) {
  return x.compare(y) == 0; }
inline Bool operator!=(const String &x, const String &y) {
  return x.compare(y) != 0; }
inline Bool operator>(const String &x, const String &y) {
  return x.compare(y) > 0; }
inline Bool operator>=(const String &x, const String &y) {
  return x.compare(y) >= 0; }
inline Bool operator<(const String &x, const String &y) {
  return x.compare(y) < 0; }
inline Bool operator<=(const String &x, const String &y) {
  return x.compare(y) <= 0; }
inline Bool operator==(const String &x, const Char *t) {
  return x.compare(t) == 0; }
inline Bool operator!=(const String &x, const Char *t) {
  return x.compare(t) != 0; }
inline Bool operator>(const String &x, const Char *t) {
  return x.compare(t) > 0; }
inline Bool operator>=(const String &x, const Char *t) {
  return x.compare(t) >= 0; }
inline Bool operator<(const String &x, const Char *t) {
  return x.compare(t) < 0; }
inline Bool operator<=(const String &x, const Char *t) {
  return x.compare(t) <= 0; }
inline Bool operator==(const String &x, const Char t) {
  return x.compare(String(t)) == 0; }
inline Bool operator!=(const String &x, const Char t) {
  return x.compare(String(t)) != 0; }
inline Bool operator>(const String &x, const Char t) {
  return x.compare(String(t)) > 0; }
inline Bool operator>=(const String &x, const Char t) {
  return x.compare(String(t)) >= 0; }
inline Bool operator<(const String &x, const Char t) {
  return x.compare(String(t)) < 0; }
inline Bool operator<=(const String &x, const Char t) {
  return x.compare(String(t)) <= 0; }
// ** Casacore additions of global compares. Returns 0 if equal; lt or gt 0 if
// strings unequal or of unequal lengths.
// <group>
inline Int compare(const std::string &x, const std::string &y) {
  return x.compare(y); }
inline Int compare(const std::string &x, const Char *y) {
  return x.compare(y); }
inline Int compare(const std::string &x, const Char y) {
  return x.compare(String(y)); }
// this version ignores case. ** Casacore addition. Result is 0 if equal
// strings of equal lengths; else lt or gt 0 to indicate differences.
Int fcompare(const String& x, const String& y);
// </group>
// </group>

// <summary> Splitting </summary>
// Global function which splits the String into string array res at separator
// and returns the number of elements.  ** Casacore addition
// <group name=split>
Int split(const std::string &str, std::string res[], Int maxn,
	  const std::string &sep);
Int split(const std::string &str, std::string res[], Int maxn,
	  const Char sep);
Int split(const std::string &str, std::string res[], Int maxn,
	  const Regex &sep);
//</group> 

// <summary> Some general functions </summary>
// Functions to find special patterns, join and replicate
// <group name=common>
String common_prefix(const std::string &x, const std::string &y,
		     Int startpos = 0);
String common_suffix(const std::string &x, const std::string &y,
		     Int startpos = -1);
String replicate(Char c, String::size_type n);
String replicate(const std::string &str, String::size_type n);
String join(std::string src[], Int n, const std::string &sep);
// </group>

// <summary> Casing and related functions </summary>
// Case conversion and rearrangement functions
// <group name=case>
// Global function which returns a transformation to reverse order of String.
String reverse(const std::string& str);
// Global function  which returns a transformation to uppercase of String.
String upcase(const std::string& str);
// Global function  which returns a transformation to lowercase of String.
String downcase(const std::string& str);
// Global function  which returns a transformation to capitalization of 
// String.
String capitalize(const std::string& str);
// Global function  which removes leading and trailing whitespace.
String trim(const std::string& str);
// </group>

// <summary> IO </summary>
// <group name=io>
// Output
std::ostream &operator<<(std::ostream &s, const String &x);
// </group>

//# Inlines
inline SubString::SubString(const std::string &str, std::string::size_type pos,
			    std::string::size_type len) :
  ref_p(str), pos_p((pos > str.length()) ? str.length() : pos),
  len_p((len == std::string::npos || pos_p+len > str.length()) ?
	str.length()-pos_p : len) {}

inline SubString String::operator()(size_type pos, size_type len) {
  return at(pos, len); }
inline  const Char *SubString::chars() const {
  return String(*this).c_str(); }

inline Bool String::contains(Char c, Int pos) const {
  return (index(c, pos) != npos); }
inline Bool String::contains(const std::string &str, Int pos) const {
  return (index(str, pos) != npos); }
inline Bool String::contains(const Char *s, Int pos) const {
  return (index(s, pos) != npos); }
inline Bool String::contains(const Regex &r, Int pos) const {
  return (index(r, pos) != npos); }

inline std::ostream &operator<<(std::ostream &s, const String &x) {
  s << x.c_str(); return s; }


} //# NAMESPACE CASACORE - END


// Define the hash function for String, so unordered_set<String> can be used.
namespace std {
template<>
struct hash<casacore::String>
{
  std::size_t operator()(casacore::String const& k) const noexcept
    { return std::hash<std::string>()(k); }
};

} // namespace casacore

#endif
