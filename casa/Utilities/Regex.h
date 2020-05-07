//# Regex.h: Regular expression class
//# Copyright (C) 1993,1994,1995,1996,1997,1999,2000,2001,2003
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

#ifndef CASA_REGEX_H
#define CASA_REGEX_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/casa/iosfwd.h>
#include <regex>
#include <casacore/casa/BasicSL/String.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward declarations.
struct re_pattern_buffer;
struct re_registers;

// <summary>
// Regular expression class (based on std::regex)
// </summary>

// <use visibility=export>

// <reviewed reviewer="Friso Olnon" date="1995/03/20" tests="tRegex" demos="">
// </reviewed>

// <synopsis> 
// This class provides regular expression functionality, such as
// matching and searching in strings, comparison of expressions, and
// input/output. It is built on the standard C++ regular expression class
// using the ECMAScript syntax. It is almost the same as the regular expression
// syntax used until March 2019 which used GNU's cregex.cc.
// ECMAScript offers more functionality (such as non-greedy matching),
// but there is a slight difference how brackets are used. In the old
// regex they did not need to be escaped, while they have to for ECMAScript.
// Furthermore, in the old Regex up to 9 backreferences could be given, so
// \15 meant the first backreference followed by a 5. In ECMAScript it means
// the 15th and parentheses are needed to get the old meaning.
// These differences are solved in the Regex constructor which adds escape
// characters as needed. Thus existing code using Regex does not need to be changed.
//
// Apart from proper regular expressions, it also supports glob patterns
// (UNIX file name patterns) by means of a conversion to a proper regex string.
// Also ordinary strings and SQL-style patterns can be converted to a proper
// regex string.
// <p>
// See http://www.cplusplus.com/reference/regex/ECMAScript for the syntax.
// <dl>
//  <dt> ^
//  <dd> matches the beginning of a line.
//  <dt> $
//  <dd> matches the end of a line.
//  <dt> .
//  <dd> matches any character
//  <dt> *
//  <dd> zero or more times the previous subexpression.
//  <dt> +
//  <dd> one or more times the previous subexpression.
//  <dt> ?
//  <dd> zero or one time the previous subexpression.
//  <dt> {n,m}
//  <dd> interval operator to specify how many times a subexpression
//       can match. See man page of egrep or regexp for more detail.
//  <dt> []
//  <dd> matches any character inside the brackets; e.g. <src>[abc]</src>.
//       A hyphen can be used for a character range; e.g. <src>[a-z]</src>.
//       <br>
//       A ^ right after the opening bracket indicates "not";
//       e.g. <src>[^abc]</src> means any character but a, b, and c.
//       If ^ is not the first character, it is a literal caret.
//       If - is the last character, it is a literal hyphen.
//       If ] is the first character, it is a literal closing bracket.
//       <br>
//       Special character classes are
//       [:alpha:], [:upper:], [:lower:],  [:digit:], [:alnum:], [:xdigit:],
//       [:space:], [:print:], [:punct:], [:graph:], and [:cntrl:].
//       The brackets are part of the name; e.g.
//       <src>[^[:upper:]]</src> is equal to <src>[^A-Z]</src>.
//       Note that [:upper:] is more portable, because A-Z fails
//       for the EBCDIC character set.
//  <dt> ( )
//  <dd> grouping to change the normal operator precedence.
//  <dt> |
//  <dd> or operator. Matches left side or right side.
//  <dt> \\1 till \\9. Backreference to a subexpression. Matches part of string
//       equal to string part that matched the subexpression.
// </dl>
// Special characters have to be escaped with a backslash to use them
// literally. Only inside the square brackets, escaping should not be done.
// See the man page of egrep or regexp for more information about
// regular expressions.
// <p>
// Several global Regex objects are predefined for common functionality.
// <dl>
//  <dt> RXwhite
//  <dd> one or more whitespace characters
//  <dt> RXint
//  <dd> integer number (also negative)
//  <dt> RXdouble
//  <dd> double number (with e or E as exponent)
//  <dt> RXalpha
//  <dd> one or more alphabetic characters (lowercase and/or uppercase)
//  <dt> RXlowercase
//  <dd> lowercase alphabetic
//  <dt> RXuppercase
//  <dd> uppercase alphabetic
//  <dt> RXalphanum
//  <dd> one or more alphabetic/numeric characters (lowercase and/or uppercase)
//  <dt> RXidentifier
//  <dd> identifier name (first alphabetic or underscore, then zero or
//       more alphanumeric and/or underscores
// </dl>
// The static member function <src>fromPattern</src> converts a shell-like
// pattern to a String which can be used to create a Regex from it.
// A pattern has the following special characters:
// <dl>
//  <dt> *
//  <dd> Zero or more arbitrary characters.
//  <dt> ?
//  <dd> One arbitrary character
//  <dt> []
//  <dd> The same as [] in a regular expression (see above).
//       In addition to ^ a ! can be used to indicate "not".
//  <dt> {,}
//  <dd> A brace expression which is like brace expansion in some shells.
//       It is similar to the | construct in a regular expression.
//       <br>
//       E.g. <src>{abc,defg}</src> means <src>abc</src> or <src>defg</src>.
//       Brace expressions can be nested and can contain other
//       special characters.
//       <br>
//       E.g. St{Man*.{h,cc},Col?*.{h,cc,l,y}}
//       <br>A literal comma or brace in a brace expression can be given by
//       escaping it with a backslash.
// </dl>
// The static member function <src>fromSQLPattern</src> converts an SQL-like
// pattern to a String which can be used to create a Regex from it.
// A pattern has the following special characters:
// <dl>
//  <dt> %
//  <dd> Zero or more arbitrary characters.
//  <dt> _
//  <dd> One arbitrary character
// </dl>
// The static member function <src>fromString</src> converts a normal
// string to a regular expression. This function escapes characters in
// the string which are special in a regular expression. In this way a
// normal string can be passed to a function taking a regular expression.
//
// The static member function <src>makeCaseInsensitive</src> returns a
// new regular expression string containing the case-insensitive version of
// the given expression string.
// </synopsis> 

// <example>
// <srcblock>
// Regex RXwhite("[ \n\t\r\v\f]+", 1);
//        (blank, newline, tab, return, vertical tab, formfeed)
// Regex RXint("-?[0-9]+", 1);
// Regex RXdouble("-?(([0-9]+\\.[0-9]*)|([0-9]+)|(\\.[0-9]+))([eE][+-]?[0-9]+)?", 1, 200);
// Regex RXalpha("[A-Za-z]+", 1);
// Regex RXlowercase("[a-z]+", 1);
// Regex RXuppercase("[A-Z]+", 1);
// Regex RXalphanum("[0-9A-Za-z]+", 1);
// Regex RXidentifier("[A-Za-z_][A-Za-z0-9_]*", 1);
// </srcblock>
// In RXdouble the . is escaped via a backslash to get it literally.
// The second backslash is needed to escape the backslash in C++.
// <srcblock>
// Regex rx1 (Regex::fromPattern ("St*.{h,cc}");
//            results in regexp "St.*\.((h)|(cc))"
// Regex rx2 (Regex::fromString ("tRegex.cc");
//            results in regexp "tRegex\.cc"
// </srcblock>
// </example>

//# <todo asof="2001/07/15">
//# </todo>


class Regex: std::regex
{
public:
  // Default constructor uses a zero-length regular expression.
  Regex();
    
  // Construct a regular expression from the string.
  // If toECMAScript=True, function toEcma is called to convert the old cregex
  // syntax to the new ECMAScript syntax.
  // If fast=True, Matching efficiency is preferred over efficiency constructing
  // the regex object.
  explicit Regex(const String& exp, Bool fast=False, Bool toECMAScript=True);

  // Construct a new regex (using the default Regex constructor arguments).
  void operator=(const String& str);

  // Convert the possibly old-style regex to the Ecma regex which means
  // that unescaped [ and ] inside a bracket expression will be escaped and
  // that a numeric character after a backreference is enclosed in brackets
  // (otherwise the backreference uses multiple characters).
  static String toEcma(const String& rx);

  // Convert a shell-like pattern to a regular expression string.
  // This is useful for people who are more familiar with patterns
  // than with regular expressions.
  static String fromPattern(const String& pattern);

  // Convert an SQL-like pattern to a regular expression string.
  // This is useful TaQL which mimics SQL.
  static String fromSQLPattern(const String& pattern);

  // Convert a normal string to a regular expression string.
  // This consists of escaping the special characters.
  // This is useful when one wants to provide a normal string
  // (which may contain special characters) to a function working
  // on regular expressions.
  static String fromString(const String& str);

  // Create a case-insensitive regular expression string from the given
  // regular expression string.
  // It does it by inserting the lowercase and uppercase version of
  // characters in the input string into the output string.
  static String makeCaseInsensitive (const String& str);

  // Get the regular expression string.
  const String& regexp() const
    { return itsStr; }
    
  // Test if the regular expression matches (part of) string <src>s</src>.
  // The return value gives the length of the matching string part,
  // or String::npos if there is no match or an error.
  // The string has <src>len</src> characters and the test starts at
  // position <src>pos</src>. The string may contain null characters.
  // Negative p is allowed to match at end.
  //
  // <note role=tip>
  // Use the appropriate <linkto class=String>String</linkto> functions
  // to test if a string matches a regular expression. 
  // <src>Regex::match</src> is pretty low-level.
  // </note>
  String::size_type match(const Char* s,
                          String::size_type len,
                          String::size_type pos=0) const;

  // Test if the regular expression occurs in string <src>s</src>.
  // The return value gives the position of the first substring
  // matching the regular expression. The length of that substring
  // is returned in <src>matchlen</src>.
  // The string has <src>len</src> characters and the test starts at
  // position <src>pos</src>. The string may contain null characters.
  // If the pos given is less than 0, the search starts -pos from the end.
  // <note role=tip>
  // Use the appropriate <linkto class=String>String</linkto> functions
  // to test if a regular expression occurs in a string.
  // <src>Regex::search</src> is pretty low-level.
  // </note>
  // <group>
  String::size_type search(const Char* s,
                           String::size_type len,
                           Int& matchlen,
                           Int pos=0) const;
  String::size_type find(const Char* s, String::size_type len,
                         Int& matchlen,
                         String::size_type pos=0) const;
  // </group>

  // Search backwards.
  String::size_type searchBack(const Char* s, String::size_type len,
                               Int& matchlen,
                               uInt pos) const;

  // Write the regex string.
  friend ostream& operator<<(ostream& ios, const Regex& exp);
    
protected:
  String itsStr;                 // the reg. exp. string
};


// some built in regular expressions

extern const Regex RXwhite;          //# = "[ \n\t\r\v\f]+"
extern const Regex RXint;            //# = "-?[0-9]+"
extern const Regex RXdouble;         //# = "-?(([0-9]+\\.[0-9]*)|
                                     //#    ([0-9]+)|(\\.[0-9]+))
                                     //#    ([eE][+-]?[0-9]+)?"
extern const Regex RXalpha;          //# = "[A-Za-z]+"
extern const Regex RXlowercase;      //# = "[a-z]+"
extern const Regex RXuppercase;      //# = "[A-Z]+"
extern const Regex RXalphanum;       //# = "[0-9A-Za-z]+"
extern const Regex RXidentifier;     //# = "[A-Za-z_][A-Za-z0-9_]*"


} //# NAMESPACE CASACORE - END

#endif
