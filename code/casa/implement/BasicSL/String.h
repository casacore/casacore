//# String.h: String class
//# Copyright (C) 2001
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
//#        Internet email: aips2-request@nrao.edu.
//#        Postal address: AIPS++ Project Office
//#                        National Radio Astronomy Observatory
//#                        520 Edgemont Road
//#                        Charlottesville, VA 22903-2475 USA
//#
//# $Id$

#if !defined (AIPS_STRING_H)
#define AIPS_STRING_H

//# Includes
#include <aips/aips.h>

#ifndef USE_OLD_STRING		/* The new String class */
//# Includes
#include <string>

//# Forward Declarations
class String;
class Regex;
#include <aips/iosstrfwd.h>

// <summary> SubString help class to be used in at, before, ... </summary>
// <synopsis>
// The SubString class can only be used by the String class to be able to
// operate the aips++ defined replacement operators at, before, after, through,
// from. The class is used transparently in operations like:
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
  operator const string() const { return string(ref_p, pos_p, len_p); }
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
  string::size_type length() const { return len_p; }

private:
  //# Constructors
  // Constructor (there are no public constructors)
  SubString(const string &str, string::size_type pos,
	    string::size_type len);
  //# Data
  // Referenced string
  const string &ref_p;
  // Start of sub-string
  string::size_type pos_p;
  // Length of sub-string
  string::size_type len_p;
};

// <summary> 
// String: the storage and methods of handling collections of characters.
// </summary>

// <use visibility=export>

// <reviewed reviewer="" date="" tests="tString.cc" demos="">
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
// The String class is the aips++ implementation of a string class. It is
// closely based on the standard library string class, and all operations
// and behaviour of strings as defined in the standard are available for
// a String. The only difference is the extension with additional functions
// in the aips++ String class as compared to the standard string class.
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
// The aips++ addition are:
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
// </scrblock>
// </example>
//
// <motivation>
// The String class eases the handling of characters within the AIPS++ 
// environment.
// </motivation>
//
// <todo asof=2000/12/05">
//   <li> if old string disappeared; remove the alloc() call.
//   <li> add more tests (for string methods) when old String disappears
// </todo>

class String : public string {

 public:

  //# Basic container typedefs
  typedef string::traits_type 		traits_type;
  typedef string::value_type		value_type;
  typedef string::allocator_type	allocator_type;
  typedef string::size_type 		size_type;
  typedef string::difference_type	difference_type;

  typedef string::reference 		reference;
  typedef string::const_reference 	const_reference;
  typedef string::pointer		pointer;
  typedef string::const_pointer		const_pointer;

  typedef string::iterator iterator;
  typedef string::const_iterator const_iterator;
  typedef string::reverse_iterator reverse_iterator;
  typedef string::const_reverse_iterator const_reverse_iterator;
  //# Next cast necessary to stop warning in gcc
  static const size_type npos = static_cast<size_type>(-1);

  //# Constructors
  // Default constructor
  String() : string() {}
  // Construct from std string
  // Construct from (part of) other string: acts as copy constructor
  // <thrown>
  // <li> out_of_range if pos > str.size()
  // </thrown>
  String(const string& str, size_type pos=0, size_type n=npos) :
    string(str, pos, n) {}
  // Construct from char* with given length
  // <thrown>
  // <li> length_error if n == npos
  // </thrown>
  String(const Char* s, size_type n) : string(s, n) {}
  // Construct from char array
  String(const Char* s) : string(s) {}
  // Construct from a single char (repeated n times)
  // <thrown>
  // <li> length_error if n == npos
  // </thrown>
  String(size_type n, Char c) : string(n, c) {}
  // Construct from iterator
  template<class InputIterator>
    String(InputIterator begin, InputIterator end) : string(begin, end) {}
  // From single char (** aips++ addition).
  // <note role=warning> Note that there is no automatic Char-to-String
  // conversion available. This stops inadvertent conversions of
  // integer to string. </note>
  explicit String(Char c) : string(1, c) {}
  // Construct from a SubString
  String(const SubString &str) : string(str.ref_p, str.pos_p, str.len_p) {}
  // Construct from a stream.
  // <note role=tip> The ostrstream must be
  // dynamic, that is, created with its default constructor. After this call
  // <src>os</src> should not be used any more as its internal buffer has been
  // deleted. </note>
  // <note role=warning> This implementation will be superseded once the
  // stringstream (sstream include) is part of the standard system. </note>
  String(ostrstream &os);

  //# Destructor
  // Destructor
  ~String() {}

  //# Operators
  // Assignments (they are all deep copies according to standard)
  // <group>
  String& operator=(const string& str) {
    return static_cast<String&>(string::operator=(str)); }
  String& operator=(const SubString &str) {
    return (*this = String(str)); }
  String& operator=(const Char* s) {
    return static_cast<String&>(string::operator=(s)); }
  String& operator=(Char c) {
    return static_cast<String&>(string::operator=(c)); }
  // </group>
  // ** aips++ addition: synonym for at(pos, len)
  SubString operator()(size_type pos, size_type len);
  // Concatenate
  // <group>
  String& operator+=(const string& str) {
    return static_cast<String&>(string::operator+=(str)); }
  String& operator+=(const Char* s) {
    return static_cast<String&>(string::operator+=(s)); }
  String& operator+=(Char c) {
    return static_cast<String&>(string::operator+=(c)); }
  // </group>

  // Indexing. The standard version is undefined if <src>pos > size()</src>, or 
  // <src>pos >= size()</src> for non-const version. 
  // <note role=warning> The const_reference version needs the at() version
  // for the gcc compiler: no const [] exists. </note>
  // <group>
  const_reference operator[](size_type pos) const {
    return string::at(pos); }
  reference operator[](size_type pos) {
    return string::operator[](pos); }
  // *** aips++ addition
  // <group>
  const_reference elem(size_type pos) const {
    return string::at(pos); }
  Char firstchar() const { return at(static_cast<size_type>(0)); }
  Char lastchar() const { return at(length()-1); }
  // </group>
  // </group>

  // Convert to Char* (** aips++ addition)
  operator const Char*() const { return string::c_str(); } 

  //# Member functions
  // Iterators
  // <group>
  iterator begin() { return string::begin(); }
  const_iterator begin() const { return string::begin(); }
  iterator end() { return string::end(); }
  const_iterator end() const { return string::end(); }
  reverse_iterator rbegin() { return string::rbegin(); }
  const_reverse_iterator rbegin() const { return string::rbegin(); }
  reverse_iterator rend() { return string::rend(); }
  const_reverse_iterator rend() const { return string::rend(); }
  // </group>

  // Capacity, size
  // <group>
  size_type size() const { return string::size(); }
  size_type length() const { return string::length(); }
  size_type max_size() const { return string::max_size(); }
  size_type capacity() const { return string::capacity(); }
  // ** aips++ addition -- works as a capacity(n) -- Note Int
  Int allocation() const { return string::capacity(); } 
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
    string::resize(n); return *this; }
  String& resize(size_type n, Char c) {
    string::resize(n, c); return *this; }
  String& reserve(size_type res_arg = 0) {
    string::reserve(res_arg); return *this; }
  // ** aips++ addition -- works as a resize(n)
  void alloc(size_type n) { string::resize(n); }
  // </group>

  // Clear the string
  // <note role=warning> clear() executed as erase() due to missing clear() in
  // gcc </note> 
  void clear() { string::erase(begin(), end()); }

  // Test for empty
  Bool empty() const { return string::empty(); }

  // Addressing
  // <thrown>
  // <li> out_of_range if pos >= size()
  // </thrown>
  // <group>
  const_reference at(size_type n) const { return string::at(n); }
  reference at(size_type n) { return string::at(n); }
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
  String& append(const string& str) {
    return static_cast<String&>(string::append(str)); }
  String& append(const string& str, size_type pos, size_type n) {
    return static_cast<String&>(string::append(str, pos, n)); }
  String& append(const Char* s, size_type n) {
    return static_cast<String&>(string::append(s, n)); }
  String& append(const Char* s) {
    return static_cast<String&>(string::append(s)); }
  String& append(size_type n, Char c) {
    return static_cast<String&>(string::append(n, c)); }
  template<class InputIterator>
    String& append(InputIterator first, InputIterator last) {
    return static_cast<String&>(string::append(first, last)); }
  // ** aips++ addition
  String& append(Char c) {
    return static_cast<String&>(string::append(1, c)); }
  // </group>

  // Assign
  // <thrown>
  // <li> out_of_range if pos > str.size()
  // </thrown>
  // <group>
  String& assign(const string& str) {
    return static_cast<String&>(string::assign(str)); }
  String& assign(const string& str, size_type pos, size_type n) {
    return static_cast<String&>(string::assign(str, pos, n)); }
  String& assign(const Char* s, size_type n) {
    return static_cast<String&>(string::assign(s, n)); }
  String& assign(const Char* s) {
    return static_cast<String&>(string::assign(s)); }
  String& assign(size_type n, Char c) {
    return static_cast<String&>(string::assign(n, c)); }
  template<class InputIterator>
    String& assign(InputIterator first, InputIterator last) {
    return static_cast<String&>(string::assign(first, last)); }
  // ** aips++ addition
  String& assign(Char c)  {
    return static_cast<String&>(string::assign(1, c)); }
  // </group>

  // Insert
  // <thrown>
  // <li> out_of_range if pos1 > str.size() or pos2 > str.size()
  // <li> length_error if new size() >= npos
  // </thrown>
  // <group>
  String& insert(size_type pos1, const string& str) {
    return static_cast<String&>(string::insert(pos1, str)); }
  String& insert(size_type pos1, const string& str,
		 size_type pos2, size_type n) {
    return static_cast<String&>(string::insert(pos1, str, pos2, n)); }
  String& insert(size_type pos, const Char* s, size_type n) {
    return static_cast<String&>(string::insert(pos, s, n)); }
  String& insert(size_type pos, const Char* s) {
    return static_cast<String&>(string::insert(pos, s)); }
  String& insert(size_type pos, size_type n, Char c) {
    return static_cast<String&>(string::insert(pos, n, c)); }
  // ** aips++ addition
  String& insert(size_type pos, Char c) {
    return static_cast<String&>(string::insert(pos, 1, c)); }

  iterator insert(iterator p, Char c) {
    return string::insert(p, c); }
  void insert(iterator p, size_type n, Char c) {
    string::insert(p, n, c); }
  template<class InputIterator>
    void insert(iterator p, InputIterator first, InputIterator last) {
    string::insert(p, first, last); }
  // ** aips++ additions
  // <group>
  String& insert(iterator p, const string& str) {
    return static_cast<String&>(string::insert(p-begin(), str)); }
  String& insert(iterator p, const Char* s, size_type n) {
    return static_cast<String&>(string::insert(p-begin(), s, n)); }
  String& insert(iterator p, const Char* s) {
    return static_cast<String&>(string::insert(p-begin(), s)); }
  // </group>
  // </group>

  // Compare. Returns 0 if strings equal and of equal size; else positive if
  // str larger or longer; else negative.
  // <note role=warning> The gcc compiler does not have the proper standard
  // compare functions. Hence they are locally implemented. </note>
  // <group>
  Int compare(const string& str) const {	
    return string::compare(str); }
  Int compare(size_type pos1, size_type n1, const string& str) const {
    return String(*this, pos1, n1).compare(str); }
  Int compare(size_type pos1, size_type n1, const string& str,
	      size_type pos2, size_type n2) const {
    return String(*this, pos1, n1).compare(String(str, pos2, n2)); }
  Int compare(const Char* s) const {
    return string::compare(s); }
  Int compare(size_type pos1, size_type n1, const Char* s,
	      size_type n2=npos) const {
    return String(*this, pos1, n1).compare(String(s, n2)); }
  // </group>

  // Erase
  // <group>
  String& erase(size_type pos, size_type n = npos) {
    return static_cast<String&>(string::erase(pos, n)); }
  iterator erase(iterator position) {
    return string::erase(position); }
  iterator erase(iterator first, iterator last) {
    return string::erase(first, last); }
  // </group>

  // Replace
  // <thrown>
  // <li> out_of_range if pos1 > str.size() or pos2 > str.size()
  // <li> length_error if new size() > npos
  // </thrown>
  // <group>
  String& replace(size_type pos1, size_type n1, const string& str) {
    return static_cast<String&>(string::replace(pos1, n1, str)); }
  String& replace(size_type pos1, size_type n1, const string& str,
		  size_type pos2, size_type n2) {
    return static_cast<String&>(string::replace(pos1, n1, str, pos2, n2)); }
  String& replace(size_type pos, size_type n1, const Char* s, size_type n2) {
    return static_cast<String&>(string::replace(pos, n1, s, n2)); }
  String& replace(size_type pos, size_type n1, const Char* s) {
    return static_cast<String&>(string::replace(pos, n1, s)); }
  String& replace(size_type pos, size_type n1, size_type n2, Char c) {
    return static_cast<String&>(string::replace(pos, n1, n2, c)); }
  // ** aips++ addition
  String& replace(size_type pos, size_type n1, Char c) {
    return static_cast<String&>(string::replace(pos, n1, 1, c)); }
  String& replace(iterator i1, iterator i2, const string& str) {
    return static_cast<String&>(string::replace(i1, i2, str)); }
  String& replace(iterator i1, iterator i2, const Char* s, size_type n) {
    return static_cast<String&>(string::replace(i1, i2, s, n)); }
  String& replace(iterator i1, iterator i2, const Char* s) {
    return static_cast<String&>(string::replace(i1, i2, s)); }
  String& replace(iterator i1, iterator i2, size_type n, Char c) {
    return static_cast<String&>(string::replace(i1, i2, n, c)); }
  // ** aips++ addition
  String& replace(iterator i1, iterator i2, Char c) {
    return static_cast<String&>(string::replace(i1, i2, 1, c)); }
  template<class InputIterator>
    String& replace(iterator i1, iterator i2, InputIterator j1, 
		    InputIterator j2) {
    return static_cast<String&>(string::replace(i1, i2, j1, j2)); }
  // </group>

  // Copy
  // <thrown>
  // <li> out_of_range if pos > size()
  // </thrown>
  size_type copy(Char* s, size_type n, size_type pos = 0) const {
    return string::copy(s, n, pos); }

  // Swap
  void swap(string& s) { string::swap(s); }

  // Get char array
  // <group>
  // As a proper null terminated C-string
  const Char *c_str() const { return string::c_str(); }
  // As pointer to char array 
  const Char *data() const { return string::data(); }
  // ** aips++ synonym
  const Char *chars() const { return string::c_str(); }
  // </group>

  // Get allocator used
  // <note role=warning> gcc has no get_allocator() </note>
  allocator_type get_allocator() const { return string::allocator_type(); }

  // Get a sub string
  // <thrown>
  // <li> out_of_range if pos > size()
  // </thrown>
  String substr(size_type pos=0, size_type n=npos) const {
    return String(*this, pos, n); }

  // Search functions. Returns either npos (if not found); else position.
  // <note role=warning> The Regex ones are ** aips++ additions</note>
  // <group>
  size_type find(const string &str, size_type pos=0) const {
    return string::find(str, pos); }
  size_type find(const Char *s, size_type pos=0) const {
    return string::find(s, pos); }
  size_type find(const Char *s, size_type pos, size_type n) const {
    return string::find(s, pos, n); }
  size_type find(Char c, size_type pos=0) const {
    return string::find(c, pos); }
  size_type find(const Regex &r, size_type pos=0) const;
  size_type rfind(const string &str, size_type pos=0) const {
    return string::find(str, pos); }
  size_type rfind(const Char *s, size_type pos=0) const {
    return string::rfind(s, pos); }
  size_type rfind(const Char *s, size_type pos, size_type n) const {
    return string::rfind(s, pos, n); }
  size_type rfind(Char c, size_type pos=0) const {
    return string::rfind(c, pos); }
  size_type rfind(const Regex &r, size_type pos=0) const;
  size_type find_first_of(const string &str, size_type pos=0) const {
    return string::find_first_of(str, pos); }
  size_type find_first_of(const Char *s, size_type pos=0) const {
    return string::find_first_of(s, pos); }
  size_type find_first_of(const Char *s, size_type pos, size_type n) const {
    return string::find_first_of(s, pos, n); }
  size_type find_first_of(Char c, size_type pos=0) const {
    return string::find_first_of(c, pos); }
  size_type find_first_of(const Regex &r, size_type pos=0) const;
  size_type find_last_of(const string &str, size_type pos=0) const {
    return string::find_last_of(str, pos); }
  size_type find_last_of(const Char *s, size_type pos=0) const {
    return string::find_last_of(s, pos); }
  size_type find_last_of(const Char *s, size_type pos, size_type n) const {
    return string::find_last_of(s, pos, n); }
  size_type find_last_of(Char c, size_type pos=0) const {
    return string::find_last_of(c, pos); }
  size_type find_last_of(const Regex &r, size_type pos=0) const;
  size_type find_first_not_of(const string &str, size_type pos=0) const {
    return string::find_first_not_of(str, pos); }
  size_type find_first_not_of(const Char *s, size_type pos=0) const {
    return string::find_first_not_of(s, pos); }
  size_type find_first_not_of(const Char *s, size_type pos, size_type n) const {
    return string::find_first_not_of(s, pos, n); }
  size_type find_first_not_of(Char c, size_type pos=0) const {
    return string::find_first_not_of(c, pos); }
  size_type find_first_not_of(const Regex &r, size_type pos=0) const;
  size_type find_last_not_of(const string &str, size_type pos=0) const {
    return string::find_last_not_of(str, pos); }
  size_type find_last_not_of(const Char *s, size_type pos=0) const {
    return string::find_last_not_of(s, pos); }
  size_type find_last_not_of(const Char *s, size_type pos, size_type n) const {
    return string::find_last_not_of(s, pos, n); }
  size_type find_last_not_of(Char c, size_type pos=0) const {
    return string::find_last_not_of(c, pos); }
  size_type find_last_not_of(const Regex &r, size_type pos=0) const;
  // </group>
  
  // Containment. ** aips++ addition
  // <group name=contains>
  Bool contains(Char c) const {
    return (find(c) != npos); }
  Bool contains(const string &str) const {
    return (find(str) != npos); }
  Bool contains(const Char *s) const {
    return (find(s) != npos); }
  Bool contains(const Regex &r) const;
  // </group>
  // Containment after (or before if pos negative) pos. ** aips++ addition
  // <group name=contains_pos>
  Bool contains(Char c, Int pos) const;
  Bool contains(const string &str, Int pos) const;
  Bool contains(const Char *s, Int pos) const;
  Bool contains(const Regex &r, Int pos) const;
  // </group>

  // Matches entire string. ** aips++ addition
  // <group name=matches>
  Bool matches(const string &str, Int pos = 0) const;
  Bool matches(Char c, Int pos = 0) const {
    return matches(String(c), pos); };
  Bool matches(const Char *s, Int pos = 0) const {
    return matches(String(s), pos); };
  Bool matches(const Regex &r, Int pos = 0) const;
  // </group>

  // Concatenate by prepending the argument onto String. ** aips++ addition
  // <group name=concatenation_method>
  void prepend(const string &str); 
  void prepend(const Char *s);
  void prepend(Char c);
  // </group> 

  // Return the position of the target in the string or npos for failure.
  // ** aips++ addition
  // <group name=index>
  size_type index(Char c, Int startpos = 0) const {
    return ((startpos >= 0) ? find(c, startpos) :
	    rfind(c, length() + startpos - 1)); }
  size_type index(const string &str, Int startpos = 0) const { 
    return ((startpos >= 0) ? find(str, startpos) :
	    rfind(str, length() + startpos - str.length())); }
  size_type index(const Char *s, Int startpos = 0) const {
    return ((startpos >= 0) ? find(s, startpos) :
	    rfind(s, length() + startpos - traits_type::length(s))); }
  size_type index(const Regex &r, Int startpos = 0) const;
  // </group>

  //  Return the number of occurences of target in String. ** aips++ addition
  // <group name=freq>
  Int freq(Char c) const; 
  Int freq(const string &str) const;
  Int freq(const Char *s) const;
  // </group>

  // Extract the string "at" the argument's position. ** aips++ addition
  // <group name=at>
  SubString at(size_type pos, size_type len);
  String at(size_type pos, size_type len) const;
  SubString at(const string &str, Int startpos = 0);
  String at(const string &str, Int startpos = 0) const;
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
  // position, exclusive. ** aips++ addition
  // <group name=before>
  SubString before(size_type pos);
  SubString before(const string &str, Int startpos = 0);
  SubString before(const Char *s, Int startpos = 0);
  SubString before(Char c, Int startpos = 0);
  SubString before(const Regex &r, Int startpos = 0);
  // Next one for overloading reasons
  SubString before(Int pos) {
    return before(static_cast<size_type>(pos)); };    
  // </group>

  // Start at startpos and extract the SubString "through" to the argument's 
  // position, inclusive. ** aips++ addition
  // <group name=through>
  SubString through(size_type pos);
  SubString through(const string &str, Int startpos = 0);
  SubString through(const Char *s, Int startpos = 0);
  SubString through(Char c, Int startpos = 0);
  SubString through(const Regex &r, Int startpos = 0);
  // Next one for overloading reasons
  SubString through(Int pos) {
    return through(static_cast<size_type>(pos)); }
  // </group>

  // Start at startpos and extract the SubString "from" the argument's 
  // position, inclusive, to the String's end. ** aips++ addition
  // <group name=from>
  SubString from(size_type pos);
  SubString from(const string &str, Int startpos = 0);
  SubString from(const Char *s, Int startpos = 0);
  SubString from(Char c, Int startpos = 0);
  SubString from(const Regex &r, Int startpos = 0);
  // Next one for overloading reasons
  SubString from(Int pos) {
    return from(static_cast<size_type>(pos));
  };
  // </group>

  // Start at startpos and extract the SubString "after" the argument's 
  // position, exclusive, to the String's end. ** aips++ addition
  // <group name=after>
  SubString after(size_type pos);
  SubString after(const string &str, Int startpos = 0);
  SubString after(const Char *s, Int startpos = 0);
  SubString after(Char c, Int startpos = 0);
  SubString after(const Regex &r, Int startpos = 0);
  // Next one for overloading reasons
  SubString after(Int pos) {
    return after(static_cast<size_type>(pos));
  };
  // </group>

  // Maybe forget some. ** aips++ addition
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

  // Delete len chars starting at pos. ** aips++ addition
  void del(size_type pos, size_type len);

  // Delete the first occurrence of target after startpos. ** aips++ addition
  //<group name=del_after>
  void del(const string &str, size_type startpos = 0);
  void del(const Char *s, size_type startpos = 0);
  void del(Char c, size_type startpos = 0);
  void del(const Regex &r, size_type startpos = 0);
  // Overload problem
  void del(Int pos, Int len) {
    del(static_cast<size_type>(pos), static_cast<size_type>(len)); }
  //</group> 

  // Global substitution: substitute all occurrences of pat with repl, and
  // return the number of replacements.
  // ** aips++ addition
  //<group name=gsub>
  Int gsub(const string &pat, const string &repl);
  Int gsub(const Char *pat, const string &repl);
  Int gsub(const Char *pat, const Char *repl);
  Int gsub(const Regex &pat, const string &repl);
  //</group>

  // Convert a integer to a String. This is a convenience function. Use the
  // ostrstream class (or in the future the ostringstream class) for conversion
  // of floating point data types or more sophisticated formatting options.
  // <group>
  static String toString(Int value);
  static String toString(uInt value);
  // </group>

private:
  // Helper functions for at, before etc
  // <group>
  SubString _substr(size_type first, size_type l) {
    return SubString(*this, first, l); }
  // </group>
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
// ** aips++ additions of global compares. Returns 0 if equal; lt or gt 0 if
// strings unequal or of unequal lengths.
// <group>
inline Int compare(const string &x, const string &y) {
  return x.compare(y); }
inline Int compare(const string &x, const Char *y) {
  return x.compare(y); }
inline Int compare(const string &x, const Char y) {
  return x.compare(String(y)); }
// this version ignores case. ** aips++ addition. Result is 0 if equal
// strings of equal lengths; else lt or gt 0 to indicate differences.
Int fcompare(String x, String y);
// </group>
// </group>

// <summary> Splitting </summary>
// Global function which splits the String into string array res at separator
// and returns the number of elements.  ** aips++ addition
// <group name=split>
Int split(const string &str, string res[], Int maxn,
	  const string &sep);
Int split(const string &str, string res[], Int maxn,
	  const Char sep);
Int split(const string &str, string res[], Int maxn,
	  const Regex &sep);
//</group> 

// <summary> Some general functions </summary>
// Functions to find special patterns, join and replicate
// <group name=common>
String common_prefix(const string &x, const string &y, 
		     Int startpos = 0);
String common_suffix(const string &x, const string &y, 
		     Int startpos = -1);
String replicate(Char c, String::size_type n);
String replicate(const string &str, String::size_type n);
String join(string src[], Int n, const string &sep);
// </group>

// <summary> Casing and related functions </summary>
// Case conversion and rearrangement functions
// <group name=case>
// Global function which returns a transformation to reverse order of String.
String reverse(string str);
// Global function  which returns a transformation to uppercase of String.
String upcase(string str);
// Global function  which returns a transformation to lowercase of String.
String downcase(string str);
// Global function  which returns a transformation to capitalization of 
// String.
String capitalize(string str);
// </group>

// <summary> IO </summary>
// <group name=io>
// Output
ostream &operator<<(ostream &s, const String &x);
// </group>

//# Inlines
inline SubString::SubString(const string &str, string::size_type pos,
			    string::size_type len) :
  ref_p(str), pos_p((pos > str.length()) ? str.length() : pos),
  len_p((len == string::npos || pos_p+len > str.length()) ?
	str.length()-pos_p : len) {}

inline SubString String::operator()(size_type pos, size_type len) {
  return at(pos, len); }
inline  const Char *SubString::chars() const {
  return String(*this).c_str(); }

inline Bool String::contains(Char c, Int pos) const {
  return (index(c, pos) != npos); }
inline Bool String::contains(const string &str, Int pos) const {
  return (index(str, pos) != npos); }
inline Bool String::contains(const Char *s, Int pos) const {
  return (index(s, pos) != npos); }
inline Bool String::contains(const Regex &r, Int pos) const {
  return (index(r, pos) != npos); }

inline Bool String::matches(const string &str, Int pos) const {
  return ((pos < 0) ? index(str, pos) == 0 :
	  length() != 0 && str.length() != 0 &&
	  length() == pos+str.length() &&
	  static_cast<size_type>(pos) < length() &&
	  index(str, pos) == static_cast<size_type>(pos)) ; }
inline ostream &operator<<(ostream &s, const String &x) {
  s << x.c_str(); return s; }

#else /* Old String class */

//# String classes, adapted from gnu string classes.

//# This is the gnu string implementation,
//# modified by AIPS++ to use aips++ style exceptions, move some things out
//# of line, etc.


#include <aips/iostream.h>
#include <aips/strstream.h>
#include <aips/Utilities/Regex.h>

extern void stringThrowError(const char *);

// <summary> 
// StrRep: The internal String representation which handles most operations.
// </summary>

// <use visibility=local>

// <reviewed reviewer="Timothy P. P. Roberts, troberts@NRAO.edu" date="Mon 1995/03/27 15:57:01 GMT" tests="tString.cc" demos="">
// </reviewed>

// <prerequisite>
//   <li> none
// </prerequisite>
//
// <etymology>
// The StrRep struct name is a concatination of String Representation.  This
// is to indicate that StrReps handle most of the internal functionality of
// the String class.
// </etymology>
//
// <synopsis> 
// The user never uses StrRep directly - all processing is done at the String
// or SubString class level.
// </synopsis> 
//
// <motivation>
// By isolating the String functionality into a set of simple processes on a 
// small set of data elements the String class is broken into levels that may
// be easily coded, optimized and debugged.  Thus, StrReps may be changed 
// without changing the users interface.
// </motivation>
//
// <todo asof="Mon 1995/03/27 15:57:01 GMT">
//   <li> none noted.
// </todo>

struct StrRep
{

  // string length 
  unsigned int len;

  // allocated space
  unsigned int sz;

  // the string starts here 
  // (at least 1 char for trailing null)
  // allocated & expanded via non-public fcts
  char s[1];
};

//# primitive ops on StrReps -- nearly all String fns go through these.
StrRep *Salloc(StrRep*, const char*, int, int);
StrRep *Scopy(StrRep*, StrRep*);
StrRep *Sresize(StrRep*, int);
StrRep *Scat(StrRep*, const char*, int, const char*, int);
StrRep *Scat(StrRep*, const char*, int,const char*,int, const char*,int);
StrRep *Sprepend(StrRep*, const char*, int);
StrRep *Sreverse(StrRep*, StrRep*);
StrRep *Supcase(StrRep*, StrRep*);
StrRep *Sdowncase(StrRep*, StrRep*);
StrRep *Scapitalize(StrRep*, StrRep*);


//# These classes need to be defined in the order given

class String;
class SubString;

// <summary> 
// SubString: A reference to all or part of an existing String.
// </summary>

// <use visibility=export>

// <reviewed reviewer="Timothy P. P. Roberts, troberts@NRAO.edu" date="Mon 1995/03/27 15:57:01 GMT" tests="tString.cc" demos="">
// </reviewed>

// <prerequisite>
//   <li> String
//   <li> Regex - the regular expressions class
// </prerequisite>
//
// <etymology>
// The SubString class name is an indication of its role as a reference into
// a small part of a whole String.  
// </etymology>
//
// <synopsis> 
// There isn't a public SubString constructor - SubStrings are always 
// created by a String member function.  Since SubStrings are a reference to 
// part of an already existing String, changing the value of the SubString 
// will change the value of the parent String.  This was the intent behind 
// their creation.  SubStrings allow you to change the rapidly varying part
// of a String without needing to concatinate the String back together.
// <srcblock> 
//     String myOutput = "Error: class foo has thrown an exception.";
//     myOutput.at("foo") = "Oinker";
//     cout << myOutput << endl;
//     myOutput.at("Oinker") = "Swine";
//     cout << myOutput << endl;
// </srcblock> Would print the following:
//
// Error: class Oinker has thrown an exception.
// Error: class Swine has thrown an exception.
//
// The SubString class itself has many member functions which check if a set
// of characters is "contain"ed within a SubString or allow "match"ing of a 
// regular expression.  Length of the SubString, a check to see if the 
// SubString is empty and conversion to "const char *" are provided, as well.
// </synopsis> 
//
// <example>
// <srcblock>
// String MailingLabel("Dr. Richard Cranium, 1003 Lopezville Road, Socorro");
// MailingLabel += ", NM  87801";
// // we will extract some SubString "fields"
// // the part which holds "Richard Cranium" is attached
// SubString name = MailingLabel.at("Richard Cranium");
// // the part before ", Soc" and after skipping 20 characters of name
// SubString street = MailingLabel.before(", Soc", 20);
// // the through to ", NM" and after skipping 43 previous characters.
// SubString city = MailingLabel.through(", NM, 43);
// // the part from "NM" to the end
// SubString zip = MailingLabel.from("NM");
// // the possible extra part after the zip code (e.g. zip+4)
// SubString plusFour = MailingLabel.after("801");
// // Now we replace as needed - assume an Array of Strings is our data source
// for(int i = 0; i<5, i++){
//    name = creditCardData(i,0);
//    street = creditCardData(i,1);
//    city = creditCardData(i,2);
//    zip = creditCardData(i,3);
//    plusFour = creditCardData(i,4);
//    fprintf(Harass, "%s/n", MailingLabel.chars());
// }
// </srcblock>
// </example>
//
// <motivation>
// SubStrings were created to allow the user to access parts of a String 
// without changing the String's coded appearence.  This is part of the GNU
// C++ library and has been converted to use AIPS++ exception handling.
// </motivation>
//
// <todo asof="Mon 1995/03/27 15:57:01 GMT">
//   <li> none noted
// </todo>

class SubString 
{
public:

  friend class String;

  // destructor
  ~SubString();

  // assignment operator
  // <group>
  void operator = (const String &y);
  void operator = (const SubString &y);
  void operator = (const char *t);
  void operator = (char c);
  // </group>

  // ostream operator for output.
  friend ostream &operator<<(ostream &s, const SubString &x);

  // Return 1 if the target appears anywhere in SubString; else 0.
  //<group>
  int contains(char c) const;
  int contains(const String &y) const;
  int contains(const SubString &y) const;
  int contains(const char *t) const;
  int contains(const Regex &r) const;
  //</group>

  // Return 1 if the target matches entire SubString.
  int matches(const Regex &r) const;

  // length of the SubString
  unsigned int length() const;

  // return 1 if empty, 0 if filled.
  int empty() const;

  // convert to char* for use in "C" type character arguments
  const char *chars() const;

  // indexing ok
  int OK() const; 

protected:

  // The String I'm a substring of
  String &S;

  // starting position in S's rep
  unsigned int pos;

  // length of substring
  unsigned int len;

  void assign(StrRep *, const char *, int = -1);

  // <note> There are no public constructors for SubString.
  //        SubStrings are always created via String operations.</note>
  //<group> 
  SubString(String &x, unsigned int p, unsigned int l);
  SubString(const SubString &x);
  //</group>

};


// <summary> 
// String: the storage and methods of handling collections of characters.
// </summary>

// <use visibility=export>

// <reviewed reviewer="Timothy P. P. Roberts, troberts@NRAO.edu" date="Mon 1995/03/27 15:57:01 GMT" tests="tString.cc" demos="">
// </reviewed>

// <prerequisite>
//   <li> Regex - the regular expressions class
// </prerequisite>
//
// <etymology>
// The String class name is a continuation of the "C" language custom of
// refering to collections of characters as "strings of characters".
// </etymology>
//
// <synopsis> 
// The String class may be instantiated in many ways:
// <ol><li> A single character - <src>String myChar("C");</src>
// <li> A char* argument - <src>String myWord("Yowza");</src>
// <li> The first n chararcters of a pre-existing string - 
// <src>String myFoo("fooey", 3);</src>
// </ol> As well as the copy, default and SubString constructors.
//
// A String may be concatinated with another object (String, SubString, or 
// char*) with either prepending or postpending.  A search for the position
// of a character within a String may return its position, a Bool that it
// is contained within or a Bool confirming your guess at the charater's 
// position is correct.  A check of the frequency of occurance of a character
// within a String will return the number of occurances.  
// 
// SubStrings may be extracted from Strings at, before, through, from and 
// after a starting position within the String.  Deletion of characters is
// possible after a given position within the String. Global substitution
// of characters within a String is provided, as well.  Splitting of Strings 
// into a carray of Strings is possible, based upon a given separator 
// character, with a return value of the number of elements split.  The joining
// together of the elements of a carray of Strings into one String is possible.
// 
// Finally, transformations of case and conversions of type are provided. 
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
// int position = allKeys.index(finishIt);
// // find if the word is in the String...
// int query = myString.contains("good men");
// // ask if the position we think is true is correct...
// int answer = allKeys.matches(finishIt, position);
// // How many spaces are in our phrase?
// int spacesCount = allKeys.freq(" ");
// </scrblock>
// SubStrings are references to all or part of a String.  An obvious advantage
// of this would be attaching the small, rapidly changing part (i.e. a name) 
// within a lengthy String to a SubString.  Thus, the SubString may be changed 
// without affecting the larger String's handling.  
// <srcblock>
// String MailingLabel("Dr. Richard Cranium, 1003 Lopezville Road, Socorro");
// MailingLabel += ", NM  87801";
// // we will extract some SubString "fields"
// // the part which holds "Richard Cranium" is attached
// SubString name = MailingLabel.at("Richard Cranium");
// // the part before ", Soc" and after skipping 20 characters of name
// SubString street = MailingLabel.before(", Soc", 20);
// // the through to ", NM" and after skipping 43 previous characters.
// SubString city = MailingLabel.through(", NM, 43);
// // the part from "NM" to the end
// SubString zip = MailingLabel.from("NM");
// // the possible extra part after the zip code (e.g. zip+4)
// SubString plusFour = MailingLabel.after("801");
// // Now we replace as needed - assume an Array of Strings is our data source
// for(int i = 0; i<5, i++){
//    name = creditCardData(i,0);
//    street = creditCardData(i,1);
//    city = creditCardData(i,2);
//    zip = creditCardData(i,3);
//    plusFour = creditCardData(i,4);
//    fprintf(Harass, "%s/n", MailingLabel.chars());
// }
// </srcblock>
// </example>
//
// <motivation>
// The String class eases the handling of characters within the AIPS++ 
// environment.  The class was written as part of the GNU C++ library and 
// adapted to use the AIPS++ exception handling.
// </motivation>
//
// <todo asof="2001/01/15">
//   <li> proper implementation of standard finds with Regex
// </todo>

class String {
public:

  friend class SubString;

  // default constructors
  String();

  // copy constructor
  String(const String &x);

  // SubString constructor
  String(const SubString &x);

  // const char * constructor
  String(const char *t);

  // construct String of len number of characters from beginning of char *t
  String(const char *t, int len);

  // single char constructor
  String(char c);

  // Create a string from the characters in an ostrstream. The ostrstream must be
  // dynamic, that is created with its default constructor. After this call
  // <src>os</src> should not be used any more as its internal buffer has been
  // deleted.
  String(ostrstream &os);

  // destructor
  ~String();

  // Assignment operator
  // <group name=assign>
  void operator=(const String &y);
  void operator=(const char *y);
  void operator=(char  c);
  void operator=(const SubString &y);
  // </group>

  // This is a concatenation operator.
  //<group name=concate_ops>
  void operator+=(const String &y); 
  void operator+=(const SubString &y);
  void operator+=(const char *t);
  void operator+=(char c);
  //</group> 

  // This is a synonym for at (int pos, int len).
  SubString operator()(int pos, int len);

  // Stream operator
  // <group name=streams>
  friend ostream &operator<<(ostream &s, const String &x);
  friend ostream &operator<<(ostream &s, const SubString &x);
  friend istream &operator>>(istream &s, String &x);
  // </group>

  // Concatenate by prepending the argument onto String
  //<group name=concatenation_method>
  void prepend(const String &y); 
  void prepend(const SubString &y);
  void prepend(const char *t);
  void prepend(char c);
  //</group> 


  // Return the position of the target in the string or -1 for failure.
  //<group name=index>
  int index(char c, int startpos = 0) const; 
  int index(const String &y, int startpos = 0) const; 
  int index(const SubString &y, int startpos = 0) const; 
  int index(const char *t, int startpos = 0) const; 
  int index(const Regex &r, int startpos = 0) const; 
  //</group>

  // Return 1 if the target appears anyhere in the String; else 0.
  //<group name=contains>
  int contains(char c) const;
  int contains(const String &y) const;
  int contains(const SubString &y) const;
  int contains(const char *t) const;
  int contains(const Regex &r) const;
  //</group> 

  // Return 1 if the target appears anywhere after position pos 
  // (or before, if pos is negative) in String; else 0.
  //<group name=contains_pos>
  int contains(char c, int pos) const;
  int contains(const String &y, int pos) const;
  int contains(const SubString &y, int pos) const;
  int contains(const char *t, int pos) const;
  int contains(const Regex &r, int pos) const;
  //</group>

  // Return 1 if the target appears at position pos in String; else 0.
  //<group name=matches>
  int matches(char c, int pos = 0) const;
  int matches(const String &y, int pos = 0) const;
  int matches(const SubString &y, int pos = 0) const;
  int matches(const char *t, int pos = 0) const;
  int matches(const Regex &r, int pos = 0) const;
  //</group>

  //  Return the number of occurences of target in String.
  //<group name=freq>
  int freq(char c) const; 
  int freq(const String &y) const;
  int freq(const SubString &y) const;
  int freq(const char *t) const;
  //</group>

  // <note> You can't take a substring of a const String, since this leaves 
  // open the possiblility of indirectly modifying the String through the 
  // SubString </note>
  //<group name=SubString>
  // extract the SubString "at" the argument's position.
  // <group name=at>
  SubString at(int pos, int len);
  SubString at(const String &x, int startpos = 0); 
  SubString at(const SubString &x, int startpos = 0); 
  SubString at(const char *t, int startpos = 0);
  SubString at(char c, int startpos = 0);
  SubString at(const Regex &r, int startpos = 0); 
  //</group>

  // Start at startpos and extract the SubString "before" the argument's 
  // position, exclusive.
  // <group name=before>
  SubString before(int pos);
  SubString before(const String &x, int startpos = 0);
  SubString before(const SubString &x, int startpos = 0);
  SubString before(const char *t, int startpos = 0);
  SubString before(char c, int startpos = 0);
  SubString before(const Regex &r, int startpos = 0);
  //</group>

  // Start at startpos and extract the SubString "through" to the argument's 
  // position, inclusive.
  // <group name=through>
  SubString through(int pos);
  SubString through(const String &x, int startpos = 0);
  SubString through(const SubString &x, int startpos = 0);
  SubString through(const char *t, int startpos = 0);
  SubString through(char c, int startpos = 0);
  SubString through(const Regex &r, int startpos = 0);
  //</group>

  // Start at startpos and extract the SubString "from" the argument's 
  // position, inclusive, to the String's end.
  // <group name=from>
  SubString from(int pos);
  SubString from(const String &x, int startpos = 0);
  SubString from(const SubString &x, int startpos = 0);
  SubString from(const char *t, int startpos = 0);
  SubString from(char c, int startpos = 0);
  SubString from(const Regex &r, int startpos = 0);
  //</group>

  // Start at startpos and extract the SubString "after" the argument's 
  // position, exclusive, to the String's end.
  // <group name=after>
  SubString after(int pos);
  SubString after(const String &x, int startpos = 0);
  SubString after(const SubString &x, int startpos = 0);
  SubString after(const char *t, int startpos = 0);
  SubString after(char c, int startpos = 0);
  SubString after(const Regex &r, int startpos = 0);
  //</group>
  //</group> 

  // Delete len chars starting at pos.
  void del(int pos, int len);

  // Delete the first occurrence of target after startpos.
  //<group name=del_after>
  void del(const String &y, int startpos = 0);
  void del(const SubString &y, int startpos = 0);
  void del(const char *t, int startpos = 0);
  void del(char c, int startpos = 0);
  void del(const Regex &r, int startpos = 0);
  //</group> 

  // Global substitution: substitute all occurrences of pat with repl.
  //<group name=gsub>
  int gsub(const String &pat, const String &repl);
  int gsub(const SubString &pat, const String &repl);
  int gsub(const char *pat, const String &repl);
  int gsub(const char *pat, const char *repl);
  int gsub(const Regex &pat, const String &repl);
  //</group>

  // return a reference to the char element at position "i".
  // <group>
  char &operator[](int i);
  char operator[](int i) const;
  // </group>

  // return the char element at position "i".
  char elem(unsigned int i) const;

  // return the first char of the String
  char firstchar() const;

  // return the last char of the String
  char lastchar() const;

  // convert to const char * with bogus cast e.g. (const char*)myString.
  operator const char*() const;

  // return const char *
  const char *chars() const;

  // return the length of the String
  unsigned int length() const;

  // return 0 if the String is filled, 1 if empty
  int empty() const;

  // Status
  int OK() const;

  // Preallocate some space for String.
  void alloc(int newsize);

  // Report the current allocation (not length!).
  int allocation() const;

  // hash function from IV strings (pjt - march 1992).
  ///  unsigned long	hash() const;

  // internal transformation to reverse order of String.
  void reverse();

  // internal transformation to uppercase of String.
  void upcase();

  // internal transformation to lowercase of String.
  void downcase();

  // internal transformation to capitalization of String.
  void capitalize();

  // global function which returns a transformation to reverse order of String.
  friend String reverse(const String &x);

  // global function  which returns a transformation to uppercase of String.
  friend String upcase(const String &x);

  // global function  which returns a transformation to lowercase of String.
  friend String downcase(const String &x);

  // global function  which returns a transformation to capitalization of 
  // String.
  friend String capitalize(const String &x);

  // Global function which splits the String into carray res at separators
  // and returns the number of elements.
  //<group name=split>
  friend int split(const String &x, String res[], int maxn, const String &sep);
  friend int split(const String &x, String res[], int maxn, const Regex & sep);
  //</group> 

  friend String common_prefix(const String &x, const String &y, 
			      int startpos = 0);
  friend String common_suffix(const String &x, const String &y, 
			      int startpos = -1);
  friend String replicate(char c, int n);
  friend String replicate(const String &y, int n);
  friend String join(String src[], int n, const String &sep);


  // IO
  friend int readline(istream &s, String &x, char terminator = '\n',
		      int discard_terminator = 1);

  // Convert a integer to a String. This is a convenience function. Use the
  // oststream class (or in the future the ostringstream class) for conversion
  // of floating point data types or more sophisticated formatting options.
  // <group>
  static String toString(Int value);
  static String toString(uInt value);
  // </group>

protected:

  // Strings are pointers to their representations
  StrRep *rep;

  // This is a helper function.
  //<group name=helperfunc>
  int search(int, int, const char*, int = -1) const;
  int search(int, int, char) const;
  int match(int, int, int, const char*, int = -1) const;
  int _gsub(const char*, int, const char *,int);
  int _gsub(const Regex&, const char*, int);
  SubString _substr(unsigned int, unsigned int);
  //</group>

};


// This is here for backward compatibility.
typedef String StrTmp;

//<summary>
// Global Functions which compare Strings
//</summary>

// The global function "compare" returns 0 if the arguments are equal,
// <0 is x is less and >0 is x is greater.
// <group name=compare>
int compare(const String &x, const String &y);
int compare(const String &x, const SubString &y);
int compare(const String &x, const char *y);
int compare(const SubString &x, const String &y);
int compare(const SubString &x, const SubString &y);
int compare(const SubString &x, const char *y);

// this version ignores case
int fcompare(const String &x, const String &y);
//</group>

extern StrRep _nilStrRep;
extern String _nilString;

//<summary>
// Global Functions which concatenate Strings with the +operator.
//</summary>

// The global "+" operator is a method of concatination
// <group name=operat>
String operator+(const String &x, const String &y);
String operator+(const String &x, const SubString &y);
String operator+(const String &x, const char *y);
String operator+(const String &x, char y);
String operator+(const SubString &x, const String &y);
String operator+(const SubString &x, const SubString &y);
String operator+(const SubString &x, const char *y);
String operator+(const SubString &x, char y);
String operator+(const char *x, const String &y);
String operator+(const char *x, const SubString &y);
// </group>

//<summary>
// Global Functions which compare Strings with comparison operators.
//</summary>

// The global "less than, greater than, equal to and not equal to" operators
// return 1 if fulfilled, 0 otherwise.
// <group name=comparitor>

inline int operator==(const String &x, const String &y) 
{
  return compare(x, y) == 0; 
}

inline int operator!=(const String &x, const String &y)
{
  return compare(x, y) != 0; 
}

inline int operator>(const String &x, const String &y)
{
  return compare(x, y) > 0; 
}

inline int operator>=(const String &x, const String &y)
{
  return compare(x, y) >= 0; 
}

inline int operator<(const String &x, const String &y)
{
  return compare(x, y) < 0; 
}

inline int operator<=(const String &x, const String &y)
{
  return compare(x, y) <= 0; 
}

inline int operator==(const String &x, const SubString &y) 
{
  return compare(x, y) == 0; 
}

inline int operator!=(const String &x, const SubString &y)
{
  return compare(x, y) != 0; 
}

inline int operator>(const String &x, const SubString &y) 
{
  return compare(x, y) > 0; 
}

inline int operator>=(const String &x, const SubString &y)
{
  return compare(x, y) >= 0; 
}

inline int operator<(const String &x, const SubString &y) 
{
  return compare(x, y) < 0; 
}

inline int operator<=(const String &x, const SubString &y)
{
  return compare(x, y) <= 0; 
}

inline int operator==(const String &x, const char *t) 
{
  return compare(x, t) == 0; 
}

inline int operator!=(const String &x, const char *t) 
{
  return compare(x, t) != 0; 
}

inline int operator>(const String &x, const char *t) 
{
  return compare(x, t) > 0; 
}

inline int operator>=(const String &x, const char *t) 
{
  return compare(x, t) >= 0; 
}

inline int operator<(const String &x, const char *t) 
{
  return compare(x, t) < 0; 
}

inline int operator<=(const String &x, const char *t) 
{
  return compare(x, t) <= 0; 
}

inline int operator==(const SubString &x, const String &y) 
{
  return compare(y, x) == 0; 
}

inline int operator!=(const SubString &x, const String &y)
{
  return compare(y, x) != 0;
}

inline int operator>(const SubString &x, const String &y) 
{
  return compare(y, x) < 0;
}

inline int operator>=(const SubString &x, const String &y) 
{
  return compare(y, x) <= 0;
}

inline int operator<(const SubString &x, const String &y) 
{
  return compare(y, x) > 0;
}

inline int operator<=(const SubString &x, const String &y) 
{
  return compare(y, x) >= 0;
}

inline int operator==(const SubString &x, const SubString &y) 
{
  return compare(x, y) == 0; 
}

inline int operator!=(const SubString &x, const SubString &y)
{
  return compare(x, y) != 0;
}

inline int operator>(const SubString &x, const SubString &y) 
{
  return compare(x, y) > 0;
}

inline int operator>=(const SubString &x, const SubString &y)
{
  return compare(x, y) >= 0;
}

inline int operator<(const SubString &x, const SubString &y) 
{
  return compare(x, y) < 0;
}

inline int operator<=(const SubString &x, const SubString &y)
{
  return compare(x, y) <= 0;
}

inline int operator==(const SubString &x, const char *t) 
{
  return compare(x, t) == 0; 
}

inline int operator!=(const SubString &x, const char *t) 
{
  return compare(x, t) != 0;
}

inline int operator>(const SubString &x, const char *t) 
{
  return compare(x, t) > 0; 
}

inline int operator>=(const SubString &x, const char *t) 
{
  return compare(x, t) >= 0; 
}

inline int operator<(const SubString &x, const char *t) 
{
  return compare(x, t) < 0; 
}

inline int operator<=(const SubString &x, const char *t) 
{
  return compare(x, t) <= 0; 
}
//</group>


//#-----------------begin inlining------------------------------------

//#
//# status reports, needed before defining other things
//#

inline unsigned int String::length() const { return rep->len; }
inline int String::empty() const { return rep->len == 0; }
inline const char *String::chars() const { return &(rep->s[0]); }
inline int String::allocation() const { return rep->sz; }
inline void String::alloc(int newsize) { rep = Sresize(rep, newsize); }

inline unsigned int SubString::length() const { return len; }
inline int SubString::empty() const { return len == 0; }
inline const char *SubString::chars() const { return &(S.rep->s[pos]); }


//#
//# constructors
//#

inline String::String(const String &x) 
  : rep(Scopy(0, x.rep)) {}
inline String::String(const char *t) 
  : rep(Salloc(0, t, -1, -1)) {}
inline String::String(const char *t, int tlen)
  : rep(Salloc(0, t, tlen, tlen)) {}
inline String::String(const SubString &y)
  : rep(Salloc(0, y.chars(), y.length(), y.length())) {}
inline String::String(char c) 
  : rep(Salloc(0, &c, 1, 1)) {}

inline SubString::SubString(const SubString &x)
  :S(x.S), pos(x.pos), len(x.len) {}
inline SubString::SubString(String &x, unsigned int first, unsigned int l)
  :S(x), pos(first), len(first+l > x.length()  ?  x.length()-first : l) {}

inline SubString::~SubString() {}

//#
//# assignment
//#

inline void String::operator = (const String &y)
{ 
  rep = Scopy(rep, y.rep);
}

inline void String::operator=(const char *t)
{
  rep = Salloc(rep, t, -1, -1); 
}

inline void String::operator=(const SubString &y)
{
  rep = Salloc(rep, y.chars(), y.length(), y.length());
}

inline void String::operator=(char c)
{
  rep = Salloc(rep, &c, 1, 1); 
}


inline void SubString::operator = (const char *ys)
{
  assign(0, ys);
}

inline void SubString::operator = (char ch)
{
  assign(0, &ch, 1);
}

inline void SubString::operator = (const String &y)
{
  assign(y.rep, y.chars(), y.length());
}

inline void SubString::operator = (const SubString &y)
{
  assign(y.S.rep, y.chars(), y.length());
}

//#
//# operator versions of cats
//#

inline void String::operator +=(const String &y) {
  rep = Scat(rep, chars(), length(), y.chars(), y.length());
}

inline void String::operator +=(const SubString &y) {
  rep = Scat(rep, chars(), length(), y.chars(), y.length());
}

inline void String::operator += (const char *y) {
  rep = Scat(rep, chars(), length(), y, -1);
}

inline void String:: operator +=(char y) {
  rep = Scat(rep, chars(), length(), &y, 1);
}

//#
//# element extraction
//#

inline char &String::operator [] (int i) 
{ 
  if (((uInt)i) >= length()) {
    stringThrowError("invalid index");
  }
  return rep->s[i];
}

inline char String::operator [] (int i) const
{ 
  if (((uInt)i) >= length()) {
    stringThrowError("invalid index");
  }
  return rep->s[i];
}

inline char String::elem (unsigned int i) const
{ 
  if (i >= length()) {
    stringThrowError("invalid index");
  }
  return rep->s[i];
}

inline char String::firstchar() const
{ 
  return elem(0);
}

inline char String::lastchar() const
{ 
  return elem(length() - 1);
}


//#
//# searching
//#

inline int String::index(char c, int startpos) const
{
  return search(startpos, length(), c);
}

inline int String::index(const char *t, int startpos) const
{ 
  return search(startpos, length(), t);
}

inline int String::index(const String &y, int startpos) const
{ 
  return search(startpos, length(), y.chars(), y.length());
}

inline int String::index(const SubString &y, int startpos) const
{ 
  return search(startpos, length(), y.chars(), y.length());
}

inline int String::index(const Regex &r, int startpos) const
{
  int unused;  return r.search(chars(), length(), unused, startpos);
}



inline int String::contains(char c) const
{
  return search(0, length(), c) >= 0;
}

inline int String::contains(const char *t) const
{ 
  return search(0, length(), t) >= 0;
}

inline int String::contains(const String &y) const
{ 
  return search(0, length(), y.chars(), y.length()) >= 0;
}

inline int String::contains(const SubString &y) const
{ 
  return search(0, length(), y.chars(), y.length()) >= 0;
}

inline int String::contains(char c, int p) const
{
  return match(p, length(), 0, &c, 1) >= 0;
}

inline int String::contains(const char *t, int p) const
{
  return match(p, length(), 0, t) >= 0;
}

inline int String::contains(const String &y, int p) const
{
  return match(p, length(), 0, y.chars(), y.length()) >= 0;
}

inline int String::contains(const SubString &y, int p) const
{
  return match(p, length(), 0, y.chars(), y.length()) >= 0;
}

inline int String::contains(const Regex &r) const
{
  int unused;  return r.search(chars(), length(), unused, 0) >= 0;
}

inline int String::contains(const Regex &r, int p) const
{
  return r.match(chars(), length(), p) >= 0;
}


inline int String::matches(const SubString &y, int p) const
{
  return match(p, length(), 1, y.chars(), y.length()) >= 0;
}

inline int String::matches(const String &y, int p) const
{
  return match(p, length(), 1, y.chars(), y.length()) >= 0;
}

inline int String::matches(const char *t, int p) const
{
  return match(p, length(), 1, t) >= 0;
}

inline int String::matches(char c, int p) const
{
  return match(p, length(), 1, &c, 1) >= 0;
}

inline int String::matches(const Regex &r, int p) const
{
  int l = (p < 0)? -p : length() - p;
  return r.match(chars(), length(), p) == l;
}


inline int SubString::contains(const char *t) const
{ 
  return S.search(pos, pos+len, t) >= 0;
}

inline int SubString::contains(const String &y) const
{ 
  return S.search(pos, pos+len, y.chars(), y.length()) >= 0;
}

inline int SubString::contains(const SubString &y) const
{ 
  return S.search(pos, pos+len, y.chars(), y.length()) >= 0;
}

inline int SubString::contains(char c) const
{
  return S.search(pos, pos+len, c) >= 0;
}

inline int SubString::contains(const Regex &r) const
{
  int unused;  return r.search(chars(), len, unused, 0) >= 0;
}

inline int SubString::matches(const Regex &r) const
{
  return r.match(chars(), len, 0) == int(len);
}


inline int String::gsub(const String &pat, const String &r)
{
  return _gsub(pat.chars(), pat.length(), r.chars(), r.length());
}

inline int String::gsub(const SubString &pat, const String &r)
{
  return _gsub(pat.chars(), pat.length(), r.chars(), r.length());
}

inline int String::gsub(const Regex &pat, const String &r)
{
  return _gsub(pat, r.chars(), r.length());
}

inline int String::gsub(const char *pat, const String &r)
{
  return _gsub(pat, -1, r.chars(), r.length());
}

inline int String::gsub(const char *pat, const char *r)
{
  return _gsub(pat, -1, r, -1);
}



inline ostream &operator<<(ostream &s, const String &x)
{
   s << x.chars(); return s;
}



//
// This is a helper needed by at, before, etc.
//

inline SubString String::_substr(unsigned int first, unsigned int l)
{
  if (first >= length() )
    return SubString(_nilString, 0, 0) ;
  else 
    return SubString(*this, first, l);
}

#endif		/* sgi */

#endif
