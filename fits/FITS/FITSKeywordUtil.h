//# FITSKeywordUtil.h: Class of static functions to help with FITS Keywords.
//# Copyright (C) 2002
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


#ifndef FITS_FITSKEYWORDUTIL_H
#define FITS_FITSKEYWORDUTIL_H

#include <casacore/casa/aips.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

class ConstFitsKeywordList;
class FitsKeywordList;
class RecordInterface;
class IPosition;
class String;
template<class T> class Vector;

// <summary>
// A class with static functions to help deal with FITS Keywords.
// </summary>

// <use visibility=export>

// <reviewed reviewer="Eric Sessoms" date="2002/08/19" tests="tFITSKeywordUtil.cc">
// </reviewed>

// <prerequisite>
//   <li> General knowledge of FITS, and particularly FITS keywords, is
//        assumed.
//   <li> Presumably you are using this class in conjunction
//        with the "native" 
//        <linkto class=FitsKeywordList>FitsKeywordList</linkto>
//   <li> You also need to understand the
//        <linkto class=RecordInterface>RecordInterface</linkto>
//        class.
// </prerequisite>
//
// <etymology>
// This is a collection of static utility functions for use with FITS
// keywords.
// </etymology>
//
// <synopsis>
// This class provides functions to conveniently interconvert between Casacore
// types and a FitsKeywordList which is needed by the native FITS classes.
// It is more convenient to maintain the list within Casacore
// as a Record, so we only need methods to turn a FitsKeywordList into a 
// Record, and vice versa.
//
// Note that it is not necessary to construct a FITSKeywordUtil object
// since you can use its static functions directly.
// </synopsis>
//
// <example>
// This example shows how you put values from a Record into a
// FItsKeywordList.
// <srcblock>
// Record rec;
// rec.define("hello", 6.5);
// rec.define("world", True);
// Vector<Int> naxis(5); 
// naxis(0) = 128; 
// naxis(1) = 64; 
// naxis(2) = 32;
// naxis(3) = 16;
// naxis(4) = 8;
// rec.define("naxis", naxis);
// // fields can have comments
// rec.setComment("hello","A comment for HELLO");
// // Add a comment to the rec
// FITSKeywordUtil::addComment(rec,"My comment goes here");
// // Create an empty FitsKeywordList, containing only "SIMPLE=T"
// FitsKeywordList kwl = FITSKeywordUtil::makeKeywordList();
// // and add the fields in rec to this list
// FITSKeywordUtil::addKeywords(kwl, rec);
// </srcblock>
// </example>
//
// <example>
// This example shows how you extract fits keywords into a Record.
// <srcblock>
// Record rec;
// FitsKeywordList kwl;
// ConstFitsKeywordList kwlRO;
// Vector<String> ignore(1);
// ignore(1)= "simple"; // ignore the SIMPLE keyword
// FITSKeywordUtil::getKeywords(rec, kwlRO, ignore);
// </srcblock>
// </example>
//
// <motivation>
// The FitsKeywordList class can be somewhat tedious to use, as it deals with,
// e.g., char* pointers rather than Strings. This class makes it easy to
// interconvert between FITS keywords and Casacore types.
// </motivation>
//
// <todo asof="2000/06/21">
//   <li> Get/set history as a vector of strings as well.
//   <li> This could be a namespace rather than a class.
// </todo>

class FITSKeywordUtil
{
public:
    // Make an initial FitsKeywordList for either a FITS primary header
	 // or a FITS extension header (image or table). A primary header
	 // requires "SIMPLE = T", an extension header "XTENSION = IMAGE "
	 // or "XTENSION = BINTABLE " for image or table, respectively.
    // This is required of any FITS keyword list. This is provided as
	 // a convenience so that you do not have to know anything about the class
    // <linkto class=FitsKeywordList>FitsKeywordList</linkto>.
    static FitsKeywordList makeKeywordList(Bool primHead=True, Bool binImage=True);

    // Add the fields from in to the out FitsKeywordList as keywords.
    // Upcases field names, turns arrays into indexed keywords, tries to interleave
    // e.g. crval, crpix, etc. 
    // COMMENT* are standalone comments, and HISTORY* are history cards.
    // (COMMENT and HISTORY may be of any capitalization). Note however that
    // you will generally add History keywords with the class
    // <linkto class=FITSHistoryUtil>FITSHistoryUtil</linkto>.
    // Returns False in the following instances:
    // <ul>
    // <li> The value of a string field is longer than 68 characters.  The value is truncated.
    // <li> An illegal type for a FITS keyword (e.g. Complex).  The field is ignored.
    // <li> An array field has more than 2 dimensions. The field is stored as a vector.
    // <li> An array field name is too long to hold the name and the index characters.  The name is truncated.
    // <li> Too many rows or columns for a 2D array (first 999 in each are used).
    // <li> Too many elements in a 1D array (first 999 are used).
    // <li> A field is neither a scalar or an array (e.g. a record).  The field is ignored.
    // </ul>
    static Bool addKeywords(FitsKeywordList &out, const RecordInterface &in);

    // Extract keywords from in and define them in out.  
    // Output field names are downcased.  Keywords matching
    // the names in ignore (which are treated as regular expressions) are
    // not created in out.  This test happens after the field names
    // have been downcased.
    // All indexed keywords will be ignored if the root name is in the ignore
    // vector (e.g. NAXIS implies NAXIS4 and other indexed NAXIS keywords
    // are ignored).
    // By default history keywords are ignored, since they
    // should be handled in class 
    // <linkto class=FITSHistoryUtil>FITSHistoryUtil</linkto>.
    // This always returns True.
    static Bool getKeywords(RecordInterface &out, ConstFitsKeywordList &in, 
			    const Vector<String> &ignore, 
			    Bool ignoreHistory=True);

    
    // Remove some keywords from a record. This can be useful
    // if, e.g., you first need to construct a coordinate system from the
    // header, but you later want to remove CROTA etc.
    // The strings in the ignore vector are treated as regular expressions.
    static void removeKeywords(RecordInterface &out, 
			       const Vector<String> &ignore);

    // Convert a TDIMnnn keyword value into an IPosition.  This returns
    // False if the tdim string has an invalid format.
    static Bool fromTDIM(IPosition& shape, const String& tdim);

    // Convert an IPosition to a String appropriate for use as the
    // value of a TDIMnnn keyword.  This returns False if the
    // converted string has more than 71 characters
    // (making it impossible to be used as a string keyword value).
    static Bool toTDIM(String& tdim, const IPosition& shape);

    // Add a comment/history to the supplied record. It will automatically
    // figure out a unique name and add it to the end. If the comment contains
    // embedded newlines this function will break the string across multiple
    // FITS comment entries. At present it will not however make sure that the
    // strings are short enough (i.e. <= 72 characters per line).
    // 
    // Note that while you can add history anywhere into header, in the actual
    // keyword list they will always appear after the END keyword.

    // Note however that you will generally manipulate History keywords with
    // the class <linkto class=FITSHistoryUtil>FITSHistoryUtil</linkto>.
    // <group>
    static void addComment(RecordInterface &header, const String &comment);
    static void addHistory(RecordInterface &header, const String &history);
    // </group>
};


} //# NAMESPACE CASACORE - END

#endif
