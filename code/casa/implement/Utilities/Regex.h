//# Regex.h: Regular expression class
//# Copyright (C) 1993,1994,1995,1996,1997,1999,2000
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

#if !defined(AIPS_REGEX_H)
#define AIPS_REGEX_H

#if defined(__GNUG__)
#pragma interface
#endif

#if defined(SHORT_NAMES) || defined(VMS)
#define re_compile_pattern	recmppat
#define re_pattern_buffer	repatbuf
#define re_registers		reregs
#endif

// Forward declarations.
#if defined(AIPS_STDLIB)
#include <iosfwd>
#else
class ostream;
#endif
class  String;
class  AipsIO;
struct re_pattern_buffer;       // defined elsewhere
struct re_registers;

#include <aips/RTTI/Typeinfo.h>

// Initialize the String type() rtti functions.
rtti_dcl_init(Regex);

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
// <br>
// cregex.cc supports many syntaxes. Regex supports
// only one syntax, the extended regular expression with { and not \{
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
// The static member function <src>fromString</src> converts a normal
// string to a regular expression. This function escapes characters in
// the string which are special in a regular expression. In this way a
// normal string can be passed to a function taking a regular expression.
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

//# <todo asof="1995/03/17">
//#   <li> Decide on documentation of GNU stuff (cregex.h, cregex.cc)
//# </todo>


class Regex
{
public:
    // Default constructor uses a zero-length regular expression.
    // <thrown>
    //  <li> RegexExpressnError
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
    //  <li> RegexExpressnError
    // </thrown>
    Regex(const String&, 
	  int fast = 0, 
	  int bufsize = 40, 
	  const char* transtable = 0);
    
    // Copy constructor (copy semantics).
    // <thrown>
    //  <li> RegexExpressnError
    // </thrown>
    Regex(const Regex&);
    
    ~Regex();
    
    // Assignment (copy semantics).
    // <thrown>
    //  <li> RegexExpressnError
    // </thrown>
    // <group>
    Regex& operator = (const Regex&);
    Regex& operator = (const String&);
    // </group>

    // Convert a shell-like pattern to a regular expression.
    // This is useful for people who are more familiar with patterns
    // than with regular expressions.
    static String fromPattern (const String& string);

    // Convert a normal string to a regular expression.
    // This consists of escaping the special characters.
    // This is useful when one wants to provide a normal string
    // (which may contain special characters) to a function working
    // on regular expressions.
    static String fromString (const String& string);

    // Get the regular expression string.
    const String& regexp() const;
    
    // Get the translation table (can be a zero pointer).
    const char* transtable() const;
    
    // Test if the regular expression matches (part of) string <src>s</src>.
    // The return value gives the length of the matching string part,
    // -1 if there is no match, or -2 in case of an internal error.
    // The string has <src>len</src> characters and the test starts at
    // position <src>pos</src>. The string may contain null characters.
    //
    // <note role=tip>
    // Use the appropriate <linkto class=String>String</linkto> functions
    // to test if a string matches a regular expression. 
    // <src>Regex::match</src> is pretty low-level.
    // </note>
    int match(const char* s, int len, int pos = 0) const;
    
    // Test if the regular expression occurs in string <src>s</src>.
    // The return value gives the position of the first substring
    // matching the regular expression. The length of that substring
    // is returned in <src>matchlen</src>.
    // The string has <src>len</src> characters and the test starts at
    // position <src>pos</src>. The string may contain null characters.
    //
    // <note role=tip>
    // Use the appropriate <linkto class=String>String</linkto> functions
    // to test if a regular expression occurs in a string.
    // <src>Regex::search</src> is pretty low-level.
    // </note>
    int search(const char* s, int len, int& matchlen, int startpos = 0) const;
    
    // Return some internal <src>cregex</src> info.
    int match_info(int& start, int& length, int nth = 0) const;
    
    // Representation invariant.
    // <thrown>
    //  <li> RegexMemAllocError
    // </thrown>
    int OK() const;
    
    // Write as ASCII.
    friend ostream& operator<< (ostream&, const Regex&);
    
    // Write into AipsIO.
    friend AipsIO& operator<< (AipsIO&, const Regex&);
    
    // Read from AipsIO.
    // <thrown>
    //  <li> RegexExpressnError
    // </thrown>
    friend AipsIO& operator>> (AipsIO&, Regex&);
    
protected:
    String*            str;                 // the reg. exp.
    int                fastval;             // fast flag
    int                bufsz;               // buffer size given
    char*              trans;               // possible translation table
    re_pattern_buffer* buf;                 // compiled reg.exp.
    re_registers*      reg;                 //# internal reg.exp. stuff
    
    // Compile the regular expression
    // <thrown>
    //  <li> RegexExpressnError
    // </thrown>
    void create (const String&, int, int, const char*);
    
    // Deallocate the stuff allocated by <src>create</src>.
    void dealloc ();
};


// some built in regular expressions

extern const Regex RXwhite;          // = "[ \n\t\r\v\f]+"
extern const Regex RXint;            // = "-?[0-9]+"
extern const Regex RXdouble;         // = "-?(([0-9]+\\.[0-9]*)|
                                     //    ([0-9]+)|(\\.[0-9]+))
                                     //    ([eE][+-]?[0-9]+)?"
extern const Regex RXalpha;          // = "[A-Za-z]+"
extern const Regex RXlowercase;      // = "[a-z]+"
extern const Regex RXuppercase;      // = "[A-Z]+"
extern const Regex RXalphanum;       // = "[0-9A-Za-z]+"
extern const Regex RXidentifier;     // = "[A-Za-z_][A-Za-z0-9_]*"

#endif
