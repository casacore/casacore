//# Regex.cc: Regular expression class
//# Copyright (C) 1993,1994,1995,1996,1997,2000,2001
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

// the following includes are commented out but left in just in case
//#include <std.h>
//#include <ctype.h>
//#include <values.h>
//#include <new.h>
//#include <builtin.h>
#include <aips/Utilities/RegexError.h>

// extern "C" {
#include <aips/Utilities/cregex.h>
// }

#include <aips/Utilities/Regex.h>
#include <aips/Utilities/String.h>
#include <aips/IO/AipsIO.h>
#include <iostream.h>
#include <stdlib.h>


Regex::Regex ()
    { create ("",0,0,0); }

void Regex::create (const String& exp, int fast, int bufsize, 
		    const char* transtable)
{
  str     = new String(exp);
  fastval = fast;
  bufsz   = bufsize;
  trans   = 0;
  if (transtable) {
    trans = new char[256];
    memcpy (trans, transtable, 256);
  }
  int tlen = exp.length();
  buf = new re_pattern_buffer;
  reg = new re_registers;
  if (fast)
    buf->fastmap = new char[256];
  else
    buf->fastmap = 0;
  buf->translate = trans;
  if (tlen > bufsize)
    bufsize = tlen;
  buf->allocated = bufsize;
  buf->buffer = (char *) malloc (buf->allocated);
  int orig = a2_re_set_syntax (RE_NO_BK_PARENS+       // use () for grouping
			    RE_NO_BK_VBAR+         // use | for OR
			    RE_INTERVALS+          // intervals are possible
			    RE_NO_BK_CURLY_BRACES+ // use {} for interval
			    RE_CHAR_CLASSES+       // [:upper:], etc. possible
			    RE_NO_BK_REFS+         // backreferences possible
			    RE_NO_EMPTY_RANGES+    // e.g. [z-a] is empty set
			    RE_CONTEXTUAL_INVALID_OPS);
  char* msg = a2_re_compile_pattern((char*)(exp.chars()), tlen, buf);
  a2_re_set_syntax (orig);
  if (msg != 0) {
    throw(RegexExpressnError( msg));
  } else if (fast)
    a2_re_compile_fastmap(buf);
}

void Regex::dealloc()
{
  free (buf->buffer);
  delete [] buf->fastmap;
  delete buf;
  delete reg;
  delete str;
  delete [] trans;
}



int Regex::match_info(int& start, int& length, int nth) const
{
  if ((unsigned)(nth) >= RE_NREGS)
    return 0;
  else
  {
    start = reg->start[nth];
    length = reg->end[nth] - start;
    return start >= 0 && length >= 0;
  }
}

ostream& operator<< (ostream& ios, const Regex& exp)
    { return ios << *exp.str; }

AipsIO& operator<< (AipsIO& ios, const Regex& exp)
{
    ios.putstart ("Regex",1);
    ios << *exp.str;
    ios << exp.fastval;
    ios << exp.bufsz;
    if (exp.trans) {
	ios << True;
	ios.put (256,exp.trans);
    }else{
	ios << False;
    }
    ios.putend();
    return ios;
}

AipsIO& operator>> (AipsIO& ios, Regex& exp)
{
    exp.dealloc();                 // first deallocate current stuff
    String s;
    int    fast,bufsize;
    Bool   tr;
    uInt   trsize;
    char*  trtab = 0;
    ios.getstart ("Regex");
    ios >> s;
    ios >> fast;
    ios >> bufsize;
    ios >> tr;
    if (tr) {
	ios.getnew (trsize, trtab);
    }
    ios.getend();
    exp.create (s, fast, bufsize, trtab);
    delete [] trtab;
    return ios;
}



int Regex::search(const char* s, int len, int& matchlen, int startpos) const
{
  int matchpos, pos, range;
  if (startpos >= 0)
  {
    pos = startpos;
    range = len - startpos;
  }
  else
  {
    pos = len + startpos;
    range = -pos;
  }
  matchpos = a2_re_search_2(buf, 0, 0, (char*)s, len, pos, range, reg, len);
  if (matchpos >= 0)
    matchlen = reg->end[0] - reg->start[0];
  else
    matchlen = 0;
  return matchpos;
}

int Regex::match(const char*s, int len, int p) const
{
  if (p < 0)
  {
    p += len;
    if (p > len)
      return -1;
    return a2_re_match_2(buf, 0, 0, (char*)s, p, 0, reg, p);
  }
  else if (p > len)
    return -1;
  else
    return a2_re_match_2(buf, 0, 0, (char*)s, len, p, reg, len);
}

int Regex::OK() const
{
// can't verify much, since we've lost the original string
  int v = buf != 0;             // have a regex buf
  v &= buf->buffer != 0;        // with a pat
  if (!v) throw(RegexMemAllocError("invariant failure"));
  return v;
}


Regex::Regex (const String& exp, int fast, int sz, const char* translation)
{ create (exp,fast,sz,translation); }

Regex::~Regex ()
{ dealloc(); }

Regex::Regex (const Regex& that)
{ create (*that.str, that.fastval, that.bufsz, that.trans); }

Regex& Regex::operator= (const Regex& that)
{
    dealloc();
    create (*that.str, that.fastval, that.bufsz, that.trans);
    return *this;
}

Regex& Regex::operator= (const String& strng)
{
    dealloc();
    create (strng, 0, 40, 0);
    return *this;
}

const String& Regex::regexp() const
{ return *str; }

const char* Regex::transtable() const
{ return trans; }


String Regex::fromPattern (const String& pattern)
{
    enum CState{stream, bracketopen, escapechar};
    Int len = 0;
    Int bracecount = 0;
    Int inbrcount = 0;
    Int pattLeng = pattern.length();
    String result;
    result.alloc (3 * pattLeng);
    CState state = stream;
    for (Int i=0; i<pattLeng; i++) {
	char c = pattern[i];
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
		result[len++] = '\\';
		break;
	    case '{':
		// Opening brace gets ((
		result[len++] = '(';
		c = '(';
		bracecount++;
		break;
	    case ',':
		// Comma after opening brace gets )|(
		// Otherwise it's still a comma.
		if (bracecount) {
		    result[len++] = ')';
		    result[len++] = '|';
		    c = '(';
		}
		break;
	    case '}':
		// Closing brace after opening brace gets ))
		// Otherwise it's still an opening brace.
		if (bracecount) {
		    bracecount--;
		    result[len++] = ')';
		    c = ')';
		}
		break;
	    case '[':
		// Opening bracket puts us in a special state.
		state = bracketopen;
		inbrcount = 0;
		break;
	    case '*':
		// * gets .*
		result[len++] = '.';
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
	    // A closing bracket immediately after the start of a bracket
	    // expression is a literal ] and not the end of the expression.
	    // Otherwise a closing bracket puts us back in the normal state.
	    if (c == ']'  &&  inbrcount) {
		state = stream;
	    } else if (c == '!'  &&  !inbrcount) {
		// A starting ! is a not.
		c = '^';
	    }
	    inbrcount++;
	    break;
	    
	case escapechar:
	    // An escaped comma can be turned into a normal comma, thus
	    // does not need the backslash.
	    if (c != ',') {
		result[len++] = '\\';
	    }
	    state = stream;
	    break;
	}
	// Wait with storing an escape character.
	if (state != escapechar) {
	    result[len++] = c;
	}
    }
    // Store a trailing backslash.
    if (state == escapechar) {
	result[len++] = '\\';
    }
    return String (result.chars(), len);
}

String Regex::fromString (const String& string)
{
    Int strLeng = string.length();
    String result;
    result.alloc (2 * strLeng);
    Int len = 0;
    for (Int i=0; i<strLeng; i++) {
	char c = string[i];
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
	    result[len++] = '\\';
	}
	result[len++] = c;
    }
    return String (result.chars(), len);
}


/*
 some built-in Regular expressions
*/

const Regex RXwhite("[ \n\t\r\v\f]+", 1);
const Regex RXint("-?[0-9]+", 1);
const Regex RXdouble("-?(([0-9]+\\.[0-9]*)|([0-9]+)|(\\.[0-9]+))([eE][+-]?[0-9]+)?", 1, 200);
const Regex RXalpha("[A-Za-z]+", 1);
const Regex RXlowercase("[a-z]+", 1);
const Regex RXuppercase("[A-Z]+", 1);
const Regex RXalphanum("[0-9A-Za-z]+", 1);
const Regex RXidentifier("[A-Za-z_][A-Za-z0-9_]*", 1);
