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
#include <casacore/casa/BasicSL/RegexBase.h>
#include <casacore/casa/iosfwd.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward declarations.
struct re_pattern_buffer;
struct re_registers;

// <summary>
// Regular expression class
// </summary>

// <use visibility=export>

// <reviewed reviewer="Friso Olnon" date="1995/03/20" tests="tRegex" demos="">
// </reviewed>

// <synopsis> 
// This class provides regular expression functionality, such as
// matching and searching in strings, comparison of expressions, and
// input/output. It is built on the regular expression functions in the
// GNU library (see files cregex.h and cregex.cc).
// <br> Apart from proper regular expressions, it also supports glob patterns
// (UNIX file name patterns) by means of a conversion to a proper regex.
// Also ordinary strings can be converted to a proper regex.
// <p>
// cregex.cc supports many syntaxes. Regex supports
// only one syntax, the extended regular expression with { and not \\{
// as a special character. The special characters are:
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

// <todo asof="2001/07/15">
//   <li> Let sgi ifdef go
//   <li> Decide on documentation of GNU stuff (cregex.h, cregex.cc)
// </todo>


class Regex : public RegexBase {
public:
    // Default constructor uses a zero-length regular expression.
    // <thrown>
    //  <li> invalid_argument
    // </thrown>
    Regex();
    
    // Construct a regular expression.
    // Optionally a fast map can be created, a buffer size can be given
    // and a translation table (of 256 chars) can be applied.
    // The translation table can, for instance, be used to map
    // lowercase characters to uppercase.
    // See cregex.cc (the extended regular expression matching and search
    // library) for detailed information.
    // <thrown>
    //  <li> invalid_argument
    // </thrown>
    Regex(const String &exp, Bool fast = False, Int sz = 40, 
	  const Char *translation = 0);

    // Copy constructor (copy semantics).
    // <thrown>
    //  <li> invalid_argument
    // </thrown>
    Regex(const Regex &that);
    
    virtual ~Regex();
    
    // Assignment (copy semantics).
    // <thrown>
    //  <li> invalid_argument
    // </thrown>
    // <group>
    Regex &operator=(const Regex &that);
    Regex &operator=(const String &strng);
    // </group>

    // Convert a shell-like pattern to a regular expression.
    // This is useful for people who are more familiar with patterns
    // than with regular expressions.
    static String fromPattern(const String &pattern);

    // Convert an SQL-like pattern to a regular expression.
    // This is useful TaQL which mimics SQL.
    static String fromSQLPattern(const String &pattern);

    // Convert a normal string to a regular expression.
    // This consists of escaping the special characters.
    // This is useful when one wants to provide a normal string
    // (which may contain special characters) to a function working
    // on regular expressions.
    static String fromString(const String &strng);

    // Create a case-insensitive reular expression string from the given
    // regular expression string.
    // It does it by inserting the lowercase and uppercase version of
    // characters in the input string into the output string.
    static String makeCaseInsensitive (const String &strng);

    // Get the regular expression string.
    const String &regexp() const
      { return str; }
    
    // Get the translation table (can be a zero pointer).
    const Char *transtable() const
      { return trans; }
    
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
    virtual String::size_type match(const Char *s,
				    String::size_type len,
				    String::size_type pos=0) const;
    
    // Test if the regular expression occurs in string <src>s</src>.
    // The return value gives the position of the first substring
    // matching the regular expression. The length of that substring
    // is returned in <src>matchlen</src>.
    // The string has <src>len</src> characters and the test starts at
    // position <src>pos</src>. The string may contain null characters.
    // The search will do a reverse search if the pos given is less than 0.
    // <note role=tip>
    // Use the appropriate <linkto class=String>String</linkto> functions
    // to test if a regular expression occurs in a string.
    // <src>Regex::search</src> is pretty low-level.
    // </note>
    // <group>
    virtual String::size_type search(const Char *s, String::size_type len,
				     Int &matchlen,
				     Int pos=0) const;
    virtual String::size_type find(const Char *s, String::size_type len,
				   Int &matchlen,
				   String::size_type pos=0) const;
    // </group>

    // Return some internal <src>cregex</src> info.
    Int match_info(Int& start, Int& length, Int nth = 0) const;

    // Does it contain a valid Regex?
    Bool OK() const;
    
    // Write as ASCII.
    friend ostream &operator<<(ostream &ios, const Regex &exp);
    
protected:
    String             str;                 // the reg. exp. string
    Int                fastval;             // fast flag
    Int                bufsz;               // buffer size given
    Char*              trans;               // possible translation table
    re_pattern_buffer* buf;                 // compiled reg.exp.
    re_registers*      reg;                 // internal reg.exp. stuff
    
    // Compile the regular expression
    // <thrown>
    //  <li> invalid_argument
    // </thrown>
    void create(const String&, Int, Int, const Char*);
    
    // Deallocate the stuff allocated by <src>create</src>.
    void dealloc();
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
