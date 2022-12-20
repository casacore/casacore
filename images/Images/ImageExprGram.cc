//# ImageExprGram.cc: Grammar for image expressions
//# Copyright (C) 1998,1999,2001,2003
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

// ImageExprGram; grammar for image command lines

// This file includes the output files of bison and flex for
// parsing command lines operating on lattices.

#include <casacore/lattices/LEL/LatticeExprNode.h>
#include <casacore/casa/Arrays/Slice.h>
#include <casacore/casa/Containers/Block.h>
#include <casacore/images/Images/ImageExprGram.h>
#include <casacore/images/Images/ImageExprParse.h>  // needed for bison actions
#include <casacore/casa/Exceptions/Error.h>

//# stdlib.h is needed for bison 1.28 and needs to be included here
//# (before the flex/bison files).
//# Bison defines WHERE, which is also defined in LogOrigin.h (which
//# is included in auto-template mode).
//# So undefine WHERE first.
#undef WHERE
#include <casacore/casa/stdlib.h>

//# Let clang and gcc ignore some warnings in bison/flex code.
//# Undef YY_NULL because flex can redefine it slightly differently (giving a warning).
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wpragmas"
#pragma GCC diagnostic ignored "-Wdeprecated-register"
#pragma GCC diagnostic ignored "-Wsign-compare"
#include "ImageExprGram.ycc"                  // bison output
#ifdef YY_NULL
# undef YY_NULL
#endif
#include "ImageExprGram.lcc"                  // flex output
#pragma GCC diagnostic pop




// Define the yywrap function for flex.
int ImageExprGramwrap()
{
    return 1;
}

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Declare a file global pointer to a char* for the input string.
static const char*  strpImageExprGram = 0;
static Int          posImageExprGram = 0;

  
// Define a class to delete the yy_buffer in case of an exception.
class ImageExprGramState {
public:
  ImageExprGramState (YY_BUFFER_STATE state)
    : itsState (state)
  {}
  ~ImageExprGramState()
    { clear(); }
  void clear()
    { if (itsState) {
        ImageExprGram_delete_buffer(itsState);
        itsState=0;
      }
    }
  YY_BUFFER_STATE state() const
    { return itsState; }
private:
  ImageExprGramState (const ImageExprGramState&);
  ImageExprGramState& operator= (const ImageExprGramState&);
  YY_BUFFER_STATE itsState;      //# this is a pointer to yy_buffer_state
};

  
//# Parse the command.
int imageExprGramParseCommand (const String& command)
{
    // Save current state for re-entrancy.
    int sav_yy_start = yy_start;
    const char* savStrpImageExprGram = strpImageExprGram;
    Int savPosImageExprGram= posImageExprGram;
    YY_BUFFER_STATE sav_state = YY_CURRENT_BUFFER;
    // Create a new state buffer for new expression.
    ImageExprGramState next (ImageExprGram_create_buffer (ImageExprGramin, YY_BUF_SIZE));
    ImageExprGram_switch_to_buffer (next.state());
    yy_start = 1;
    strpImageExprGram = command.chars();     // get pointer to command string
    posImageExprGram  = 0;                   // initialize string position
    int sts = ImageExprGramparse();          // parse command string
    // The current state has to be deleted before switching back to previous.
    next.clear();
    // Restore previous state.
    yy_start = sav_yy_start;
    strpImageExprGram = savStrpImageExprGram;
    posImageExprGram= savPosImageExprGram;
    ImageExprGram_switch_to_buffer (sav_state);
    return sts;
}

//# Give the string position.
Int& imageExprGramPosition()
{
    return posImageExprGram;
}

//# Get the next input characters for flex.
int imageExprGramInput (char* buf, int max_size)
{
    int nr=0;
    while (*strpImageExprGram != 0) {
	if (nr >= max_size) {
	    break;                         // get max. max_size char.
	}
	buf[nr++] = *strpImageExprGram++;
    }
    return nr;
}

void ImageExprGramerror (const char*)
{
    throw (AipsError ("Image Expression: Parse error at or near '" +
		      String(ImageExprGramtext) + "'"));
}

String imageExprGramRemoveEscapes (const String& in)
{
    String out;
    int leng = in.length();
    for (int i=0; i<leng; i++) {
	if (in[i] == '\\') {
	    i++;
	}
	out += in[i];
    }
    return out;
}

String imageExprGramRemoveQuotes (const String& in)
{
    //# A string is formed as "..."'...''...' etc.
    //# All ... parts will be extracted and concatenated into an output string.
    String out;
    String str = in;
    int leng = str.length();
    int pos = 0;
    while (pos < leng) {
	//# Find next occurrence of leading ' or ""
	int inx = str.index (str[pos], pos+1);
	if (inx < 0) {
	    throw (AipsError ("ImageExprParse - Ill-formed quoted string: " +
			      str));
	}
	out += str.at (pos+1, inx-pos-1);             // add substring
	pos = inx+1;
    }
    return out;
}

} //# NAMESPACE CASACORE - END

