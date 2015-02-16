//# Regex.cc: Regular expression class
//# Copyright (C) 1993,1994,1995,1996,1997,2000,2001,2002,2003
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

// Regex class implementation

#include <casacore/casa/Utilities/cregex.h>

#include <casacore/casa/Utilities/Regex.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/casa/stdexcept.h>
#include <casacore/casa/iostream.h>
#include <casacore/casa/vector.h>
#include <cstring>                  //# for memcpy with gcc-4.3
#include <stdlib.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

Regex::Regex() {
  create("",0,0,0);
}

void Regex::create(const String& exp, Int fast, Int bufsize, 
		    const Char* transtable) {
  str     = exp;
  fastval = fast;
  bufsz   = bufsize;
  trans   = 0;
  if (transtable) {
    trans = new Char[256];
    memcpy(trans, transtable, 256);
  }
  Int tlen = exp.length();
  buf = new re_pattern_buffer;
  reg = new re_registers;
  if (fast) buf->fastmap = new Char[256];
  else buf->fastmap = 0;
  buf->translate = trans;
  if (tlen > bufsize)
    bufsize = tlen;
  buf->allocated = bufsize;
  buf->buffer = (Char *) malloc(buf->allocated);
  Int orig = a2_re_set_syntax(RE_NO_BK_PARENS+     // use () for grouping
			    RE_NO_BK_VBAR+         // use | for OR
			    RE_INTERVALS+          // intervals are possible
			    RE_NO_BK_CURLY_BRACES+ // use {} for interval
			    RE_CHAR_CLASSES+       // [:upper:], etc. possible
			    RE_NO_EMPTY_BK_REF+    // backreferences possible
			    RE_NO_EMPTY_RANGES+    // e.g. [z-a] is empty set
			    RE_CONTEXTUAL_INVALID_OPS);
  const char* msg = a2_re_compile_pattern((Char*)(exp.chars()), tlen, buf);
  a2_re_set_syntax(orig);
  if (msg != 0) {
    throw(invalid_argument("Regex: invalid regular expression " + exp +
			   " given (" + String(msg) + ')'));
  } else if (fast) a2_re_compile_fastmap(buf);
}

void Regex::dealloc() {
  if ( buf != 0 ) {
    free(buf->buffer);
    delete [] buf->fastmap;
    delete buf;
    buf= 0;
  }
  delete reg;
  reg=0;
  delete [] trans;
  trans=0;
}

Int Regex::match_info(Int& start, Int& length, Int nth) const {
  if ((unsigned)(nth) >= RE_NREGS) return 0;
  else {
    start = reg->start[nth];
    length = reg->end[nth] - start;
    return start >= 0 && length >= 0;
  }
}

Bool Regex::OK() const {
  Bool v = buf != 0;             // have a regex buf
  v &= buf->buffer != 0;         // with a pat
  return v;
}

ostream &operator<<(ostream &ios, const Regex &exp) {
  return ios << exp.str;
}

String::size_type Regex::find(const Char *s, String::size_type len,
				Int &matchlen,
				String::size_type pos) const {
  Int xpos = pos;
  if (xpos<0) return String::npos;
  return search(s, len, matchlen, xpos);
}

String::size_type Regex::search(const Char *s, String::size_type len,
                                Int &matchlen,
                                Int pos) const {
  Int matchpos, xpos, range;
  if (pos >= 0) {
    xpos = pos;
    range = len - pos;
  } else {
    xpos = len + pos;
    range = -xpos;
  }
  matchpos = a2_re_search_2(buf, 0, 0, (Char*)s, len, xpos, range, reg, len);
  if (matchpos >= 0) matchlen = reg->end[0] - reg->start[0];
  else {
    matchlen = 0;
    return String::npos;
  }
  return matchpos;
}

String::size_type Regex::match(const Char *s,
			       String::size_type len,
			       String::size_type pos) const {
  Int res;
  Int ps = static_cast<Int>(pos);
  if (ps < 0) {
    ps += len;
    if (ps > static_cast<Int>(len)) return String::npos;
    res = a2_re_match_2(buf, 0, 0, (Char*)s, ps, 0, reg, ps);
  } else if (ps > static_cast<Int>(len)) return String::npos;
  else res = a2_re_match_2(buf, 0, 0, (Char*)s, len, ps, reg, len);
  if (res < 0) return String::npos;
  return static_cast<String::size_type>(res);
}

Regex::Regex(const String &exp, Bool fast, Int sz,
	      const Char *translation) {
  create(exp, fast, sz, translation);
}

Regex::~Regex() {
  dealloc();
}

Regex::Regex(const Regex &that) : RegexBase() {
  create(that.str, that.fastval, that.bufsz, that.trans);
}

Regex &Regex::operator=(const Regex &that) {
  dealloc();
  create(that.str, that.fastval, that.bufsz, that.trans);
  return *this;
}

Regex &Regex::operator=(const String &strng) {
  dealloc();
  create(strng, 0, 40, 0);
  return *this;
}

String Regex::fromPattern(const String &pattern)
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

String Regex::fromSQLPattern(const String &pattern)
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
	default:
            result.push_back (c);
	}
    }
    return result;
}

String Regex::fromString (const String& strng)
{
    uInt strLeng = strng.length();
    String result;
    result.reserve(2*strLeng);
    for (uInt i=0; i<strLeng; i++) {
	Char c = strng[i];
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
	result.push_back (c);
    }
    return result;
}

String Regex::makeCaseInsensitive (const String &strng)
{
  uInt strLeng = strng.length();
  String result;
  result.reserve(4*strLeng);
  Bool inBracket = False;
  Bool openBracket = False;
  Bool escaped = False;
  Bool charClass = False;
  for (uInt i=0; i<strLeng; i++) {
    Char c = strng[i];
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
        if (c=='['  &&  i+1<strLeng  &&  strng[i+1] == ':') {
          result.push_back (c);
          c = strng[++i];
          charClass = True;
        } else if (charClass && c==':' && i+1<strLeng  &&  strng[i+1] == ']') {
          result.push_back (c);
          c = strng[++i];
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
          if (i+2 < strLeng  &&  strng[i+1] == '-'  &&  isalpha(strng[i+2])) {
            i += 2;
            int ec = strng[i];
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

const Regex RXwhite("[ \n\t\r\v\f]+", 1);
const Regex RXint("-?[0-9]+", 1);
const Regex RXdouble("-?(([0-9]+\\.[0-9]*)|([0-9]+)|(\\.[0-9]+))([eE][+-]?[0-9]+)?", 1, 200);
const Regex RXalpha("[A-Za-z]+", 1);
const Regex RXlowercase("[a-z]+", 1);
const Regex RXuppercase("[A-Z]+", 1);
const Regex RXalphanum("[0-9A-Za-z]+", 1);
const Regex RXidentifier("[A-Za-z_][A-Za-z0-9_]*", 1);

} //# NAMESPACE CASACORE - END

