//# Regex.cc: Regular expression class
//# Copyright (C) 1993,1994,1995,1996,1997,2000,2001,2002,2003,2019
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

// Regex class implementation

#include <casacore/casa/Utilities/Regex.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/stdexcept.h>
#include <casacore/casa/iostream.h>
#include <casacore/casa/vector.h>
#include <stdlib.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

Regex::Regex()
{}

Regex::Regex(const String& str, Bool fast, Bool toECMAScript)
  : itsStr (str)
{
  // Make the possible exception thrown by regex a bit more clear.
  try {
    std::regex::flag_type flags = std::regex::ECMAScript;
    if (fast) {
      flags |= std::regex::optimize;
    }
    if (toECMAScript) {
      std::regex::operator= (std::regex(toEcma(str), flags));
    } else {
      std::regex::operator= (std::regex(str, flags));
    }
  } catch (const std::exception& x) {
    throw AipsError ("Error in regex " + str + ": " + x.what());
  }
}

void Regex::operator=(const String& str)
{
  std::regex::operator= (Regex(str));
  itsStr = str;
}

String::size_type Regex::match(const Char* s,
			       String::size_type len,
			       String::size_type pos) const
{
  Int ps = static_cast<Int>(pos);
  if (ps < 0) {
    ps += len;
  }
  if (ps < 0) return String::npos;
  // A zero-length string can match .*
  // Therefore try fullMatch for such a case.
  if (ps == static_cast<Int>(len)  &&  fullMatch (s+ps, 0)) {
    return 0;
  }
  if (ps >= static_cast<Int>(len)) return String::npos;
  Int matchlen;
  String::size_type res = search(s, len, matchlen, ps);
  if (res != String::npos) {
    if (static_cast<Int>(res) == ps) {
      res = matchlen;
    } else {
      res = String::npos;     // no match from start on
    }
  }
  return res;
}

Bool Regex::fullMatch(const Char* s, String::size_type len) const
{
  return std::regex_match(s, s+len, *this);
}
                               
String::size_type Regex::search(const Char* s, String::size_type len,
                                Int& matchlen,
                                Int pos) const
{
  // Searching from the end means trying to match from the end on.
  if (pos < 0) {
    return searchBack (s, len, matchlen, -pos);
  }
  if (pos >= static_cast<Int>(len)) return String::npos;
  std::cmatch result;
  if (std::regex_search(s+pos, s+len, result, *this)) {
    matchlen = result.length(0);
    return pos + result.position(0);     // start position of match
  }
  matchlen = 0;
  return String::npos;                   // no match
}

String::size_type Regex::searchBack(const Char* s, String::size_type len,
                                    Int& matchlen,
                                    uInt pos) const
{
  if (pos >= len) {
    return String::npos;
  }
  for (Int p = len-pos; p>=0; --p) {
    String::size_type ml = match(s, len, p);
    if (ml != String::npos) {
      matchlen = ml;
      return p;
    }
  }
  matchlen = 0;
  return String::npos;             // no match
}

String::size_type Regex::find(const Char* s, String::size_type len,
                              Int& matchlen,
                              String::size_type pos) const {
  Int xpos = pos;
  if (xpos<0) return String::npos;
  return search(s, len, matchlen, xpos);
}

ostream& operator<<(ostream& ios, const Regex& exp) {
  return ios << exp.itsStr;
}

String Regex::toEcma(const String& rx)
{
  Int inbrcount = -1;
  Bool inBracket = False;
  Bool charClass = False;
  Bool escaped = False;
  uInt pattLeng = rx.length();
  String result;
  result.reserve (rx.size());
  for (uInt i=0; i<pattLeng; i++) {
    Char c = rx[i];
    if (escaped) {
      escaped = False;
      if (c >= '1'  &&  c <= '9') {
        // This is a backreference.
        // Put brackets around the next character if numeric as well.
        // Note backreferences cannot be used in a bracket expression.
        if (i+1 < pattLeng  &&  rx[i+1] >= '0'  &&  rx[i+1] <= '9') {
          result.push_back (c);
          result.push_back ('[');
          result.push_back (rx[i+1]);
          c = ']';
          i++;
        }
      }
    } else if (c == '\\') {
      escaped = True;
      inbrcount = 1;      // in case escaped inside bracket expression
    } else if (!inBracket) {
      if (c == '[') {
        // Opening bracket puts us in a bracket expression.
        inBracket = True;
        charClass = False;
        inbrcount = -1;       // to know if ] is normal or end of br.expr.
      } else if (c == ']') {
        // Outside a bracket expression ] has to be escaped as well.
        result.push_back ('\\');
      }
    } else if (c == ']') {
      if (inbrcount > 0) {
        // A closing bracket puts us back in the normal state.
        // But a closing bracket immediately after the start of a bracket
        // expression is a literal ] and not the end of the expression.
        // It has to be escaped in Ecma.
        inBracket = False;
      } else {
        result.push_back ('\\');
        inbrcount = 0;
      }
    } else if (c != '^') {
      // A starting ^ is a not and does not count yet.
      // I.e., a ] given hereafter is not the end of bracket expression yet.
      // Some character is found.
      inbrcount = 0;
      // An opening bracket followed by a colon is the start of a
      // Posix character class.
      // Go to next char, so closing bracket in [[:] is end of
      // bracket and not end of character class.
      // Otherwise [ has to be escaped.
      if (c == '[') {
        if (i+1<pattLeng  &&  rx[i+1] == ':') {
          result.push_back (c);
          c = rx[++i];
          charClass = True;
        } else {
          result.push_back ('\\');
        }
      } else if (charClass  &&
                 c == ':'  &&  i+1<pattLeng  && rx[i+1] == ']') {
        // End of Posix character class.
        result.push_back (c);
        c = rx[++i];
        charClass = False;
      }
    }
    inbrcount++;
    // Write the character.
    result.push_back (c);
  }
  return result;
}

String Regex::fromPattern(const String& pattern)
{
    enum CState{stream, bracketopen, escapechar};
    uInt bracecount = 0;
    Int inbrcount = -1;
    Bool skipChar = False;
    Bool charClass = False;
    vector<Int> emptySubStr;
    uInt pattLeng = pattern.length();
    String result;
    result.reserve (3*pattLeng);
    CState state = stream;
    for (uInt i=0; i<pattLeng; i++) {
	Char c = pattern[i];
	switch(state) {

	case stream :
	    switch (c) {
	    case '^':
	    case '$':
	    case '(':
	    case ')':
	    case '.':
	    case '+':
	    case '|':
		// Special chars have to be escaped.
                result.push_back ('\\');
		break;
	    case '{':
		// Opening brace gets (
		c = '(';
		bracecount++;
                emptySubStr.push_back(0);    // no empty substrings yet
		break;
	    case ',':
		// Comma after opening brace gets |.
		// Otherwise it's still a comma.
                // Handle an empty substring a bit differently because
                // a regex cannot handle an empty |. Instead a ? gets inserted
                // after the closing brace.
		if (bracecount) {
		    c = '|';
                    if (pattern[i-1] == '{'  ||  pattern[i-1] == ',') {
                        emptySubStr[bracecount-1] += 1;
                        skipChar = True;
                    }
		}
		break;
	    case '}':
		// Closing brace after opening brace gets )
		// Otherwise it's still an opening brace to be escaped.
		if (bracecount) {
		    bracecount--;
		    c = ')';
                    // If an empty substring was used, the block is optional.
                    if (emptySubStr.back() > 0) {
                        result.push_back (c);
                        c = '?';
                    }
                    emptySubStr.pop_back();
		} else {
                    result.push_back ('\\');
                }
		break;
	    case '[':
		// Opening bracket puts us in a special state.
		state = bracketopen;
                charClass = False;
		inbrcount = -1;
		break;
	    case '*':
		// * gets .*
                result.push_back ('.');
		break;
	    case '?':
		// ? gets .
		c = '.';
		break;
	    case '\\':
		// Backslash puts us in a special state.
		state = escapechar;
		break;
	    // leave all other chars unchanged
	    }
	    break;

	case bracketopen:
            if (c == ']'  &&  inbrcount > 0) {
                // A closing bracket immediately after the start of a bracket
                // expression is a literal ] and not the end of the expression.
                // Otherwise a closing bracket puts us back in the normal state.
		state = stream;
	    } else if ((c == '!'  ||  c == '^')  &&  inbrcount < 0) {
		// A starting ! or ^ is a not and does not count yet.
		c = '^';
            } else {
                // Some character is found.
                if (inbrcount < 0) {
                    inbrcount = 0;
                }
                // An opening bracket followed by a colon is the start of a
                // Posix character class.
                // Go to next char, so closing bracket in [[:] is end of
                // bracket and not end of character class.
                if (c == '['  &&  i+1<pattLeng  &&  pattern[i+1] == ':') {
                    result.push_back (c);
                    c = pattern[++i];
                    charClass = True;
                } else if (charClass  &&
                           c == ':'  &&  i+1<pattLeng  && pattern[i+1] == ']') {
                    // End of Posix character class.
                    result.push_back (c);
                    c = pattern[++i];
                    charClass = False;
                }
            }
            inbrcount++;
	    break;

	case escapechar:
	    // An escaped comma can be turned into a normal comma, thus
	    // does not need the backslash.
	    if (c != ',') {
                result.push_back ('\\');
	    }
	    state = stream;
	    break;
	}
	// Wait with storing an escape character.
	if (!skipChar  &&  state != escapechar) {
          result.push_back (c);
	}
        skipChar = False;
    }
    // Store a trailing backslash.
    if (state == escapechar) {
        result.push_back ('\\');
    }
    return result;
}

String Regex::fromSQLPattern(const String& pattern)
{
    // In SQL a % is 0 or more characters and _ is a single character.
    // AFAIK there are no special escape characters.
    // So simply replace them by * and %.
    // Escape all special regex characters.
    uInt strLeng = pattern.length();
    String result;
    result.reserve(2*strLeng);
    for (uInt i=0; i<strLeng; i++) {
	Char c = pattern[i];
	switch (c) {
	case '%':
            result.push_back ('.');
            result.push_back ('*');
	    break;
	case '_':
            result.push_back ('.');
	    break;
	// Escape special characters.
	case '^':
	case '$':
	case '[':
	case ']':
	case '*':
	case '+':
	case '?':
	case '.':
	case '|':
	case '{':
	case '}':
	case '(':
	case ')':
	case '\\':
            result.push_back ('\\');
            CASACORE_FALLTHROUGH;
	default:
            result.push_back (c);
	}
    }
    return result;
}

String Regex::fromString (const String& str)
{
    uInt strLeng = str.length();
    String result;
    result.reserve(2*strLeng);
    for (uInt i=0; i<strLeng; i++) {
	Char c = str[i];
	// Escape special characters.
	switch (c) {
	case '^':
	case '$':
	case '[':
	case ']':
	case '*':
	case '+':
	case '?':
	case '.':
	case '|':
	case '{':
	case '}':
	case '(':
	case ')':
	case '\\':
            result.push_back ('\\');
	}
	// fall through
	result.push_back (c);
    }
    return result;
}

String Regex::makeCaseInsensitive (const String& str)
{
  uInt strLeng = str.length();
  String result;
  result.reserve(4*strLeng);
  Bool inBracket = False;
  Bool openBracket = False;
  Bool escaped = False;
  Bool charClass = False;
  for (uInt i=0; i<strLeng; i++) {
    Char c = str[i];
    if (escaped) {
      result.push_back (c);
      escaped = False;
    } else if (c == '\\'  &&  !inBracket) {
      // Note that a \ inside a set is part of the set and not an escape.
      result.push_back (c);
      escaped = True;
    } else if (c == '['  &&  !inBracket) {
      result.push_back (c);
      inBracket = True;
      openBracket = True;
      charClass = False;
    } else if (c == ']'  &&  !openBracket) {
      // Note that a ] right after a [ is part of the set and not a closing ].
      result.push_back (c);
      inBracket = False;
    } else if (c == '^'  ||  c == '!') {
      // Note that a ^ or ! can be right after the [ and keeps the openBracket.
      result.push_back (c);
    } else {
      // Character classes like [:alpha:] should not be changed.
      if (inBracket) {
        if (c=='['  &&  i+1<strLeng  &&  str[i+1] == ':') {
          result.push_back (c);
          c = str[++i];
          charClass = True;
        } else if (charClass && c==':' && i+1<strLeng  &&  str[i+1] == ']') {
          result.push_back (c);
          c = str[++i];
          charClass = False;
        }
      }
      openBracket = False;
      // An alphabetic character needs both cases.
      int c1 = c;
      int c2 = -1;
      if (!charClass) {
        if (islower(c1)) {
          c2 = toupper(c1);
        } else if (isupper(c1)) {
          c2 = tolower(c1);
        }
      }
      if (c2 >= 0) {
        if (inBracket) {
          // If a range, the entire range must be copied if both ends are alpha.
          if (i+2 < strLeng  &&  str[i+1] == '-'  &&  isalpha(str[i+2])) {
            i += 2;
            int ec = str[i];
            result.push_back (c1);
            result.push_back ('-');
            result.push_back (ec);
            if (islower(ec)) {
              result.push_back (c2);
              result.push_back ('-');
              result.push_back (toupper(ec));
            } else if (isupper(ec)) {
              result.push_back (c2);
              result.push_back ('-');
              result.push_back (tolower(ec));
            }
          } else {
            // Add to the bracketed characters.
            result.push_back (c1);
            result.push_back (c2);
          }
        } else {
          // It was a single character; put in brackets now.
          result.push_back ('[');
          result.push_back (c1);
          result.push_back (c2);
          result.push_back (']');
        }
      } else {
        // Copy any other char.
        result.push_back (c);
      }
    }
  }
  return result;
}


// some built-in Regular expressions

const Regex RXwhite("[ \n\t\r\v\f]+");
const Regex RXint("[-+]?[0-9]+");
const Regex RXdouble("[-+]?(([0-9]+\\.[0-9]*)|([0-9]+)|(\\.[0-9]+))([eE][+-]?[0-9]+)?");
const Regex RXalpha("[A-Za-z]+");
const Regex RXlowercase("[a-z]+");
const Regex RXuppercase("[A-Z]+");
const Regex RXalphanum("[0-9A-Za-z]+");
const Regex RXidentifier("[A-Za-z_][A-Za-z0-9_]*");

} //# NAMESPACE CASACORE - END

