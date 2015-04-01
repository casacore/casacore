//# RegexBase.h: Abstract interface class to regular expressions for String
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
//#
//# $Id$

#ifndef CASA_REGEXBASE_H
#define CASA_REGEXBASE_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/casa/BasicSL/String.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward declarations

// <summary>
// Abstract interface class to regular expressions for String
// </summary>

// <use visibility=local>

// <reviewed reviewer="Friso Olnon" date="1995/03/20" tests="tRegex" demos="">
// </reviewed>

// <prerequisite>
//   <li> Regular expression syntax
//   <li> <linkto class=String>String</linkto> class
// </prerequisite>
//
//
// <synopsis>
// This class provides a standard abstract interface to regular expression
// classes. This interface class is used in the String class, to enable
// the use of different actual regular expression classes.<br>
// Actual regular expression classes should define the following
// implementation (in addition, of course, to the standard constructors and
// assignments):
// <dl>
//  <dt> String::size_type find(const Char *s, String::size_type len,
//		 Int &matchlen, String::size_type pos=0) const;
//  <dd> Test if the regular expression occurs in string <src>s</src>.
// The return value gives the position of the first substring
// matching the regular expression (or String::npos if no match).
// The length of that substring is returned in <src>matchlen</src>.
// The string has <src>len</src> characters and the test starts at
// position <src>pos</src>. The string may contain null characters.
// </dl>
// The base class provides also default implementations of a few other methods
// used in the String classes' Casacore extensions. These implementations
// can, of course, be overwritten with more efficient specialised ones if
// necessary:
// <dl>
//  <dt>  String::size_type match(const Char *s,
//		 String::size_type len, String::size_type pos=0) const;
//  <dd> Test if the regular expression matches string <src>s</src>.
// The return value gives the length of the matching string part,
// or String::npos if there is no match, or in case of an internal error.
// The string has <src>len</src> characters and the test starts at
// position <src>pos</src>. The string may contain null characters.
// The default implementation checks if the regular expression is found
// at position <src>pos</src> and with length (<src>len-pos</src>.
//  <dt> String::size_type rfind(const Char *s, String::size_type len,
//               Int &matchlen, String::size_type pos=npos) const;
//  <dd> Test if the regular expression occurs in string <src>s</src>,
//	 searching reversed.
// The return value gives the position of the first substring
// matching the regular expression (or String::npos if no match).
// The length of that substring is returned in <src>matchlen</src>.
// The string has <src>len</src> characters and the test starts at
// position <src>pos</src> (or at end of string).
// The string may contain null characters. The default implementation
// starts checking for the regular expression at <src>pos</src> (or at
// the end of the string if that is less), and loops until it is
// found. Looping is by decrementing the search position until begin of
// string.
//  <dt> String::size_type search(const Char *s, String::size_type len,
//               Int &matchlen, Int pos=0) const;
//  <dd> Test if the regular expression occurs in string <src>s</src>.
// The return value gives the position of the first substring
// matching the regular expression (or String::npos if no match).
// The length of that substring is returned in <src>matchlen</src>.
// The string has <src>len</src> characters and the test starts at
// position <src>pos</src>. The string may contain null characters.
// Following the special rule for Casacore string methods extensions:
// a negative position will indicate a reverse find. The default implementation
// checks for the sign of <src>pos</src> and calls either <src>find</src>
// or <src>rfind</src>.
// </dl>
//
// It is advisable to provide (static) methods to create strings from
// patterns and v.v., including file search patterns. See
// <linkto class=Regex>Regex</linkto> for an example.
// </synopsis>
//
// <example>
//  See examples in appropriate regular expression implementation
// (e.g. <linkto class=Regex>Regex</linkto>)
// </example>
//
// <motivation>
//  To allow for different regular expression classes in String matches
// </motivation>
//
// <todo asof="2001/05/22">
//   <li> nothing I know of
// </todo>

class RegexBase {
 public:
  //# Constructors
  // Destructor
  virtual ~RegexBase();
  //# Member functions
  // Search string <src>s</src> of length <src>len</src>, starting at position
  // <src>pos</src>. Returned is the address of the first character of
  // the substring found (or <src>String::npos</src> if not found). The
  // matched length is returned in <src>matchlen</src>
  virtual String::size_type find(const Char *s, String::size_type len,
				 Int &matchlen,
				 String::size_type pos=0) const=0;
  // Match the string <src>s</src> of length <src>len</src> starting at
  // position <src>pos</src>. Return the first matched character pointer, or
  // <src>String::npos</src> if no match.
  virtual String::size_type match(const Char *s,
				  String::size_type len,
				  String::size_type pos=0) const;
  // Do an rfind() on the string <src>s</src> of length <src>len</src>
  // starting at position <src>pos</src>. Return the position matched, or
  // <src>String::npos</src>
  virtual String::size_type rfind(const Char *s, String::size_type len,
				  Int &matchlen,
				  String::size_type pos=String::npos) const;
  // Search string <src>s</src> of length <src>len</src>, starting at position
  // <src>pos</src>. Returned is the address of the first character of
  // the substring found (or <src>String::npos</src> if not found). The
  // matched length is returned in <src>matchlen</src>. If <src>pos<0</src>
  // do a reverse find.
  virtual String::size_type search(const Char *s, String::size_type len,
				   Int &matchlen,
				   Int pos=0) const;
 private:

};


} //# NAMESPACE CASACORE - END

#endif


