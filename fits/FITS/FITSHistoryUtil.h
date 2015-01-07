//# FITSHistoryUtil.h: Class of static functions to help with FITS History cards.
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


#ifndef FITS_FITSHISTORYUTIL_H
#define FITS_FITSHISTORYUTIL_H

#include <casacore/casa/aips.h>
#include <vector>



namespace casacore { //# NAMESPACE CASACORE - BEGIN

class ConstFitsKeywordList;
class FitsKeywordList;
class String;
template<class T> class Vector;
class LoggerHolder;

// <summary>
// A class with static functions to help deal with FITS History cards.
// </summary>

// <use visibility=export>

// <reviewed reviewer="Eric Sessoms" date="2002/08/19" tests="tFITSHistoryUtil.cc">
// </reviewed>

// <prerequisite>
//   <li> General knowledge of FITS, and particularly FITS keywords, is
//        assumed.
//   <li> Presumably you are using this class in conjunction
//        with the "native" 
//        <linkto class=FitsKeywordList>FitsKeywordList</linkto>
// </prerequisite>
//
// <etymology>
// This is a collection of static utility functions for use with FITS
// HISTORY keywords.
// </etymology>
//
// <synopsis>
// Manipulate HISTORY information. FITS HISTORY cards are interconverted with
// String as follows:
// <ul>
// <li> 'HISTORY ' and trailing blanks are removed from each card.
// <li> Continuation cards are CARDS that have '>' in the first line.
// <li> A string is made by concatenating the leading card and all continuation
//    cards.
// </ul>
// For example:
// <srcblock>
// HISTORY Every good
// HISTORY > boy deserves
// HISTORY >fudge.
// </srcblock>
// Becomes the C++ String: "Every good boy deservesfudge." Note the lack of
// a space between deserves and fudge.
//
// History cards are broken into groups. A group is delimited by
// <srcblock>
// HISTORY AIPS++ START TYPE
// HISTORY AIPS++ END [TYPE]
// </srcblock>
// Where type might be, e.g., LOGTABLE. HISTORY cards not enclosed between
// START/END pairs are implicitly of type "" (i.e. the empty string).
// The TYPE is optional on the END statement. It is essentially a comment.
//
// At present, START/END pairs cannot be nested, although this would be an
// obvious extension.
// </synopsis>
//
// <motivation>
// The FitsKeywordList class can be somewhat tedious to use, as it deals with,
// e.g., char* pointers rather than Strings. This class makes it easy to
// interconvert between the HISTORY keywords and a Vector of related history
// information.
// </motivation>
//

class FITSHistoryUtil
{
public:
    // Get the strings in the next keyword group. Returns the number of
    // strings found (0 when no history remains).  If necessary, strings will be
    // resized larger. in must be set to the first card before the first call to
    // getHistoryGroup, and should not be reset until all history is extracted
    // (otherwise the same history will be extracted more than once). This method
    // can be used as follows:
    // <srcBlock>
    // uInt n;
    // Vector<String> group;
    // String type;
    // ConstFITSKeywordList keys(...);
    // ...
    // keys.first();
    // while ((n = FITSHistoryUtil::getHistoryGroup(group, type, keys)) != 0) {
    //     ... process this history group
    // }
    // </srcBlock>
    // strings will have no embedded newlines. strings is not resized if it is more 
    // than large enough to hold the number of history cards in the group (i.e. there
    // may be values at the end of strings which are not part of the requested group.
    static uInt getHistoryGroup(Vector<String> &strings, String &groupType,
				ConstFitsKeywordList &in);

    // Add history strings of the specified groupType to an existing FitsKeywordList.
    // This function will split long strings across HISTORY cards and set
    // up the group START/END keywords if necessary. nstrings must be specified
    // because strings might have come from something like getHistoryGroup, i.e.
    // it might have garbage entries at the end. The strings may have embedded
    // newlines, but they must have no other non-printable characters.
    static void addHistoryGroup(FitsKeywordList &out,
				const std::vector<String> &strings,
				uInt nstrings, const String &groupType);

    // Some functions to help convert between log tables and FITS HISTORY cards.
    // It is intended that these functions will only be used by the functions in
    // classes like ImageFITSConverter.
    // 
    // Table rows are in Casacore format if they have a valid time and priority,
    // otherwise they are in the standard FITS HISTORY format. The history lines
    // are processed by contiguous groups where all lines in that group are
    // either in Casacore or HISTORY format. Note that history.nelements() might
    // be greater than nstrings for efficiency (i.e.  the history vector will
    // not be shrunk unnecessarily).
    //
    // Note that these functions are in a separate .cc file so that if they
    // are not used the table function is not linked in if other functions in
    // this class are used.
    //
    // The strings are assumed to be from or going to the get/addHistoryGroup
    // functions, i.e. strings that span multiple lines are joined, 
    // AIPS++ START/END cards are stripped, etc.
    //
    // The Casacore format is: the first line DATE PRIORITY [SRCCODE='xxx']
    // [OBJID='xxx'] and the second lins is the message.  These entries are in
    // an AIPS++ START LOGTABLE history sequence.
    // <group>
    static void fromHISTORY(LoggerHolder& logSink, 
			    const Vector<String>& history, 
			    uInt nstrings, Bool aipsppFormat);

    // toHistory signals that it is done by setting nstrings to 0.
    // The returned value is firstLine + n_lines_read, i.e. use
    // it as firstLine in your next call.
    static uInt toHISTORY(std::vector<String>& history, Bool& aipsppFormat,
			  uInt& nstrings, uInt firstLine, 
			  const LoggerHolder& logSink);
    // </group>

};


} //# NAMESPACE CASACORE - END

#endif
