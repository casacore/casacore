//# FitsKeywordUtil: this defines FitsKeywordUtil
//# Copyright (C) 2002,2003
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

#include <casacore/fits/FITS/FITSKeywordUtil.h>

#include <casacore/casa/Arrays/Array.h>
#include <casacore/casa/Arrays/ArrayUtil.h>
#include <casacore/casa/Arrays/IPosition.h>
#include <casacore/casa/Arrays/Matrix.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Containers/RecordInterface.h>
#include <casacore/fits/FITS/fits.h>
#include <casacore/casa/Logging/LogIO.h>
#include <casacore/casa/Logging/LogOrigin.h>
#include <casacore/casa/BasicMath/Random.h>
#include <casacore/casa/OS/Time.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Utilities/Regex.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/casa/IO/ArrayIO.h>

#include <casacore/casa/sstream.h>
#include <casacore/casa/iomanip.h>
#include <ctype.h>
#include <casacore/casa/stdlib.h>
#include <map>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

// Do a reverse lookup since the FITS classes need it.
static bool findReservedName(FITS::ReservedName &name, const String &basename)
{
    const uint32_t n = FITS::ResWord.no();

    for (uint32_t i=0; i<n; i++) {
	if (basename == FITS::ResWord[i].aname()) {
	    name = FITS::ResWord[i].name();
	    return true;
	}
    }
    return false;
}

static void splitKW1D(String &name, int32_t &num, String &fullName)
{
    name = "";

    int32_t where = fullName.length(); // Where to split the number and base name
    while (--where >= 0 && isdigit(fullName[where])) {
	; // Nothing
    }
    where++;
    // where now points to the start of the numerical part - its
    // also the length of the number of characters before that
    name = fullName(0, where);
    String snum = fullName(where, (fullName.length()-where));
    num = atol(snum.chars());
}

static bool splitKW2D(String &name, int32_t &nrow, int32_t &ncol, String &fullName)
{
    name = "";

    if(fullName.contains("_")){ // assume new matrix syntax  ii_jj or i_j
      uint32_t where = 0;// Where the frst number starts
      while (where++ < fullName.length() && !isdigit(fullName[where])) {
	; // Nothing
      }
      name = fullName(0, where);
      // found first non-digit
      String::size_type where2 = fullName.find('_');
      if (where2 == String::npos || where2 == fullName.length()-1){
	return false;
      }
      String snum1 = fullName(where, where2-where);
      int32_t nc = 2; // don't use the last digit if there are three
      if(fullName.length()-where2<2){ 
	nc = 1;
      }
      String snum2 = fullName(where2+1, nc);
      nrow = atol(snum1.chars());
      ncol = atol(snum2.chars());
    }
    else { // old matrix syntax
      // We assume that 1/2 the characters belong to each of the two
      // numbers.
      int32_t where = fullName.length();// Where to split the number and base name
      while (--where >= 0 && isdigit(fullName[where])) {
	; // Nothing
      }
      where++;
      // where now points to the start of the numerical part - its
      // also the length of the number of characters before that
      name = fullName(0, where);
      int32_t numlen = fullName.length() - where;
      if (numlen != 6) {
	// 2D arrays must be xxxyyy and so there must be 6 digits
	return false;
      }
      String snum1 = fullName(where, 3);
      String snum2 = fullName(where+3, 3);
      nrow = atol(snum1.chars());
      ncol = atol(snum2.chars());
    }

    return true;
}

FitsKeywordList FITSKeywordUtil::makeKeywordList(bool primHead, bool binImage)
{
    FitsKeywordList retval;
    if (primHead)
   	 retval.mk(FITS::SIMPLE, true, "Standard FITS");
    else
   	 if (binImage)
   		 retval.mk(FITS::XTENSION,"IMAGE   ","IMAGE extension");
   	 else
   		 retval.mk(FITS::XTENSION,"BINTABLE   ","TABLE extension");
    return retval;
}

bool FITSKeywordUtil::addKeywords(FitsKeywordList &out, 
				  const RecordInterface &in)
{
    LogIO os(LogOrigin("FITSKeywordUtil", "addKeywords", WHERE));

    bool ok = true;

    const uint32_t n = in.nfields();
    static Regex commentName("^COMMENT");
    static Regex historyName("^HISTORY");

    uint32_t i = 0;
    while (i < n) {
	DataType type = in.type(i);
	if (isScalar(type)) {
	    String name = upcase(in.name(i));
	    if (name.length() > 8) {
		// silently truncate COMMENT* and HISTORY* - the addCommand and addHistory
		// functions keep them unique in the RecordInterface by adding a random trailing
		// number
		if (!(name.contains(commentName) || name.contains(historyName))) {
		    // this is just a warning, everything is still ok
		    os << LogIO::WARN
		       << "Name is too long for keyword " << name
		       << " - truncated to first 8 characters." << LogIO::POST;
		}
		name = name.before(8);
	    }
	    // comments are automatically truncated by the FITS classes
	    String comment = in.comment(i);
	    switch(type) {
	    case TpBool:
		{
		    bool val;
		    in.get(i, val);
		    out.mk(name.chars(), val, comment.chars());
		}
		break;
	    case TpInt:
		{
		    int32_t val;
		    in.get(i, val);
		    out.mk(name.chars(), val, comment.chars());
		}
	    break;
	    case TpShort:
		{
		    int16_t val;
		    in.get(i, val);
		    out.mk(name.chars(), val, comment.chars());
		}
	    break;
	    case TpUInt:
		{
		    uint32_t val;
		    in.get(i, val);
		    out.mk(name.chars(), int(val), comment.chars());
		}
	    break;
	    case TpFloat:
		{
		    float val;
		    in.get(i, val);
		    out.mk(name.chars(), val, comment.chars());
		}
	    break;
	    case TpDouble:
		{
		    double val;
		    in.get(i, val);
		    out.mk(name.chars(), val, comment.chars());
		}
	    break;
	    case TpString:
		{
		    String val;
		    in.get(i, val);
		    if (name.contains(commentName)) {
			if (val == "") {
			    out.spaces();
			} else {
			    out.comment(val.chars());
			}
		    } else if (name.contains(historyName)) {
			out.history(val.chars());
		    } else {
			if (val.length() > 68) {
			    os << LogIO::SEVERE 
			       << "Value of keyword " << name.chars() 
			       << " will be truncated at 68 characters." << LogIO::POST;
			    ok = false;
			    val = val.before(68);
			}
			out.mk(name.chars(), val.chars(), comment.chars());
		    }
		}
	    break;
	    default:
		os << LogIO::SEVERE << 
		    "Illegal FITS type " << int32_t(in.type(i)) << " for field '" <<
		    name << "': ignoring this field." << LogIO::POST;
		// Note that we carry on anyway
		ok = false;
	    }
	} else if (isArray(type)) {
	    // Find out how many like-shaped array columns there are in a row
	    // and interleave them, i.e. so we have crval1 crpix1 cdelt1,
	    // crval2 crpix2 cdelt2, ..
	    int32_t start = i;
	    const int32_t length = in.shape(i).product();
	    uint32_t ndim = in.shape(i).nelements();

	    // SPECIAL: NAXIS is both an array AND a scalar!
	    if (upcase(in.name(i)) == "NAXIS") {
		out.mk("NAXIS", int(length));
	    }

	    // To do: special treatment of the PV array here which should not be interleaved

	    if (ndim > 2) {
		os << LogIO::SEVERE << ndim << " dimensional array found. "
		    " Flattening to 1 dimension" << LogIO::POST;
		ok = false;
		ndim = 1;
	    }
	    uint32_t end = i+1;
	    while (end < n) {
		// If it's not an array
		if (!isArray(in.type(end))) {
		    break;
		}
		// or it's size has changed
		if (in.shape(end).product() != length) {
		    break;
		}
		// or its dimensionality has changed the run has ended.
		if (in.shape(end).nelements() != ndim) {
		    break;
		}
		end++;
	    }

	    // Advance i to the right place so we don't see these fields
	    // more than once.
	    i = end - 1;

	    // name length check.  Keyword names must be <= 8 characters.
	    // also do number of elements check 
	    // Note that we emit a SEVERE error but go on any way.
	    for (uint32_t j=start; j<end; j++) {
		if (ndim == 2) {
		    // need ii_jja for this one, only 2 characters left for name
		    if (in.name(j).length() > 2) {
			os << LogIO::SEVERE 
			   << "Name is too long for array field " << in.name(j) 
			   << " - name will be truncated to first 2 characters." << LogIO::POST;
			ok = false;
		    }
		    // at most, 99 elements per dimension
		    if (in.shape(i)(0) > 99 || in.shape(i)(1) > 99) {
			os << LogIO::SEVERE 
			   << "Too many rows or columns for array field " << in.name(j) 
			   << " - the first 99 rows and the first 99 columns will be used." << LogIO::POST;
			ok = false;		    }
		} else {
		    // at most 999 elements
		    if (length > 999) {
			os << LogIO::SEVERE 
			   << "Too many elements for field " << in.name(j) 
			   << " - the first 999 elements will be used." << LogIO::POST;
			ok = false;
		    }
		    String slen = String::toString(length);
		    if (in.name(j).length() + slen.length() > 8) {
			os << LogIO::SEVERE 
			   << "Name is too long for array field " << in.name(j) 
			   << " - name will be truncated to first "
			   << (8-uint32_t(slen.length())) << " characters." << LogIO::POST;
			ok = false;
		    }
		}
	    }
		    
	    // This is inefficient because we are getting the arrays many
	    // times. We could optimize this if this is ever a problem.
 	    for (int32_t k=0; k<length; k++) {
 		for (uint32_t j=start; j<end; j++) {
 		    DataType type = in.type(j);
 		    String name = upcase(in.name(j));
		    String num;
 		    if (ndim == 2) {
			if (name.length() > 2) name = name.before(2);
 			// Form i_j name
			int32_t nrow = in.shape(i)(0);
 			int32_t ii = k % nrow + 1;
 			int32_t jj = k / nrow + 1;
			ostringstream ostr;
			if(nrow>9){ // i.e. the indices have more than one digit
			  ostr << setfill('0') << setw(2) << ii
			       << "_"
			       << setfill('0') << setw(2) << jj;
			}
			else{
			  ostr << setw(1) << ii << "_" << setw(1) << jj;
			}
			name += String(ostr);
 		    } else {
			ostringstream ostr;
			ostr << k + 1;
			num = String(ostr);
		    }
		    if (name.length() > (8-num.length())) name = name.before(8-num.length());
 		    switch(type) {
 		    case TpArrayBool:
 			{
 			    Array<bool> val;
 			    in.get(j, val);
 			    bool deleteIt;
 			    bool *storage = val.getStorage(deleteIt);
			    if (ndim == 2) {
				out.mk(name.chars(), storage[k]);
			    } else {
				FITS::ReservedName fname;
				if (findReservedName(fname, name)) {
				    out.mk(int(k+1), fname, storage[k]);
				} else {
				    out.mk((name + num).chars(), storage[k]);
				}
			    }
 			    val.putStorage(storage, deleteIt);
 			}
 		    break;
 		    case TpArrayInt:
 			{
 			    Array<int32_t> val;
 			    in.get(j, val);
 			    bool deleteIt;
 			    int32_t *storage = val.getStorage(deleteIt);
			    if (ndim == 2) {
				out.mk(name.chars(), storage[k]);
			    } else {
				FITS::ReservedName fname;
				if (findReservedName(fname, name)) {
				    out.mk(int(k+1), fname, storage[k]);
				} else {
				    out.mk((name + num).chars(), storage[k]);
				}
			    }
 			    val.putStorage(storage, deleteIt);
 			}
 		    break;
 		    case TpArrayFloat:
 			{
 			    Array<float> val;
 			    in.get(j, val);
 			    bool deleteIt;
 			    float *storage = val.getStorage(deleteIt);
			    if (ndim == 2) {
				out.mk(name.chars(), storage[k]);
			    } else {
				FITS::ReservedName fname;
				if (findReservedName(fname, name)) {
				    out.mk(int(k+1), fname, storage[k]);
				} else {
				    out.mk((name + num).chars(), storage[k]);
				}
			    }
 			    val.putStorage(storage, deleteIt);
 			}
 		    break;
 		    case TpArrayDouble:
 			{
 			    Array<double> val;
 			    in.get(j, val);
 			    bool deleteIt;
 			    double *storage = val.getStorage(deleteIt);
			    if (ndim == 2) {
				out.mk(name.chars(), storage[k]);
			    } else {
				FITS::ReservedName fname;
				if (findReservedName(fname, name)) {
				    out.mk(int(k+1), fname, storage[k]);
				} else {
				    out.mk((name + num).chars(), storage[k]);
				}
			    }
 			    val.putStorage(storage, deleteIt);
 			}
 		    break;
 		    case TpArrayString:
 			{
 			    Array<String> val;
 			    in.get(j, val);
 			    bool deleteIt;
 			    String *storage = val.getStorage(deleteIt);
			    String thisVal = storage[k];
			    if (thisVal.length() > 68) {
				os << LogIO::SEVERE 
				   << "Value of keyword " << name.chars() 
				   << " will be truncated at 68 characters." << LogIO::POST;
				ok = false;
				thisVal = thisVal.before(68);
			    }
			    if (ndim == 2) {
				out.mk(name.chars(), thisVal.chars());
			    } else {
				FITS::ReservedName fname;
				if (findReservedName(fname, name)) {
				    out.mk(int(k+1), fname, thisVal.chars());
				} else {
				    out.mk((name + num).chars(), thisVal.chars());
				}
			    }
 			    val.putStorage(storage, deleteIt);
 			}
 		    break;
 		    default:
			os << LogIO::SEVERE << 
			    "Illegal FITS type " << int32_t(type) << " for field '" <<
			    name << "': ignoring this field." << LogIO::POST;
			// Note that we carry on anyway
			ok = false;
 		    }
 		}
 	    }
	} else {
	    os << LogIO::SEVERE << 
		"Illegal FITS type " << int32_t(in.type(i)) << " for field '" <<
		in.name(i) << ". 'Must be scalar or array." << LogIO::POST;
	    // Note that we carry on anyway
	    ok = false;
	}
	i++;
    }

    return ok;
}

bool FITSKeywordUtil::getKeywords(RecordInterface &out, 
				  ConstFitsKeywordList &in, 
				  const Vector<String> &ignore,
				  bool ignoreHistory)
{
    bool ok = true;

    LogIO os(LogOrigin("FITSKeywordUtil", "getKeywords", WHERE));

    // Reset to the beginning of the KW list
    in.first();

    const Regex kw1D("^[a-z0-9]*[a-z][1-9]+"); // We can have X3F, but not XF3
    // This fails with more than 99 axes.
    const Regex kw2D("^[a-z0-9]*[a-z]0[0-9][0-9]0[0-9][0-9]");
    const Regex kw2Dmodern("^[a-z][a-z]?[0-9][0-9]?[_][0-9][0-9]?");
    const Regex kw2Dstandard("^[[a-z][a-z]?[0-9]?[_][0-9]?");
    const Regex crota("crota");
    const Regex trailing(" *$");
    const Regex cd("^cd[0-9]+[_][0-9]+");
    const String empty;

    // The complication in this function springs from the fact that we want to
    // combine all the indexed and matrix (i.e. PCiiijjj or PCii_jja) keywords 
    // together into array fields in the output record.
    //
    // First we take a pass through the keywords noting the array keywords and
    // their minimum and maximum indexes. Unfortunately we have to ignore the
    // FITS keyword functions isindexed() etc. because they do not know about
    // all indexed keywords, e.g. user-defined or "new"
    // indexed keywords.
    //
    // In addition, CROTA is a special case.  It is only found on the latitude
    // axis of direction coordinates.  If we find a CROTA, we make a full length
    // vector of it in the output header record.  This is because the FITS
    // header cracking routines expect all the vector fields to be length naxis
    // The CROTA vector will be 0, except for the actual axis that had a CROTA

    // CD is another special case.  It is left as scalar keywords to be interpreted
    // by the CoordinateSystem methods according to rules coded there.

    
    std::map<String, int32_t> min1D, max1D, min2Drow, min2Dcol, max2Drow, max2Dcol;

    // this may be a bug in fits that it isn't in.curr()
    const FitsKeyword *key = in.next();
//
    bool foundCROTA = false;
    String baseCROTA;

    int32_t naxis = -1;
    int32_t maxis = -1;

    while(key) {
	String name = downcase(key->name());
	if (name == "naxis" && !key->isindexed()) {
	    if (key->type()==FITS::LONG) {
		naxis = key->asInt();
	    }
        }
	if (name == "maxis" && !key->isindexed()) {
	    if (key->type()==FITS::LONG) {
		maxis = key->asInt();
	    }
        }
//
        if (name.contains(crota)) {
	    foundCROTA = true;
	    baseCROTA = name;
        }
//


	if ((name.contains(kw2Dstandard) || name.contains(kw2D) || name.contains(kw2Dmodern)) 
            && !name.contains(cd)) {
	    int32_t nrow, ncol;
	    String base;
	    if (!splitKW2D(base, nrow, ncol, name)) {
		os << LogIO::SEVERE << "Illegal matrix keyword " << name << 
		    LogIO::EXCEPTION;
	    }
            if (min2Drow.find(base) == min2Drow.end()) min2Drow[base] = 99999;
            if (min2Dcol.find(base) == min2Dcol.end()) min2Dcol[base] = 99999;
	    if (nrow < min2Drow[base]) {min2Drow[base] = nrow;}
	    if (nrow > max2Drow[base]) {max2Drow[base] = nrow;}
	    if (ncol < min2Dcol[base]) {min2Dcol[base] = ncol;}
	    if (ncol > max2Dcol[base]) {max2Dcol[base] = ncol;}
        } 
	else if ((key->isindexed() || name.contains(kw1D)) && !name.contains(cd)) {
	    String base;
	    int32_t num;
	    if (key->isindexed()) {
		base = name;
		num = key->index();
	    } else {
		splitKW1D(base, num, name);
	    }
            if (min1D.find(base) == min1D.end()) min1D[base] = 99999;
	    if (num < min1D[base]) {min1D[base] = num;}
	    if (num > max1D[base]) {max1D[base] = num;}
	}  
	key = in.next();
    }

    if (foundCROTA) {
	if (naxis==-1) {
	    os << LogIO::SEVERE << "Failed to decode naxis keyword" << LogIO::EXCEPTION;
	}
	if (maxis==-1) {
	  min1D[baseCROTA] = 1;
	  max1D[baseCROTA] = naxis;
	}
	else{ // apparently a FITS-IDI file
	  min1D[baseCROTA] = 1;
	  max1D[baseCROTA] = maxis;
	}

    }

    // OK, now step through actually writing all the keywords

    in.first();
    key = in.next(); // I think it's a bug in FITS that this isn't in.curr()
    while(key) {
	String fullName = downcase(key->name());

	// Naxis and Maxis are special - it is both a scalar keywords and an
	// indexed keyword. If we have both ignore the scalar version.
	if ((fullName == "naxis" || fullName == "maxis") && !key->isindexed()) {
	    key = in.next();
	    continue;
	}

	// OK, it's a keyword we have to process
	if ((fullName.contains(kw2Dstandard) || fullName.contains(kw2D) ||
	     fullName.contains(kw2Dmodern)) && !fullName.contains(cd)) {
	    int32_t thisRow, thisCol;
	    String base;
	    splitKW2D(base, thisRow, thisCol, fullName);
	    thisRow -= min2Drow[base];
	    thisCol -= min2Dcol[base];
	    int32_t fnum = out.fieldNumber(base);
	    int32_t nrow = max2Drow[base]-min2Drow[base]+1;
	    int32_t ncol = max2Dcol[base]-min2Dcol[base]+1;
	    switch (key->type()) {
	    case FITS::LOGICAL:
		{
		    if (fnum >= 0 && out.type(fnum) != TpArrayBool) {
			os << LogIO::SEVERE << "Ignoring field '" << fullName <<
			    "' because its type does not match already created" <<
			    " field " << base << ". Continuing." << LogIO::POST;
			break;
		    }
		    Matrix<bool> mat;
		    if (! out.isDefined(base)) {
			mat.resize(nrow,ncol);
			mat = false;
		    } else {
			out.get(base, mat);
		    }
		    mat(thisRow,thisCol) = key->asBool();
		    out.define(base, mat);
		}
		break;
	    case FITS::STRING : 
		{
		    if (fnum >= 0 && out.type(fnum) != TpArrayString) {
			os << LogIO::SEVERE << "Ignoring field '" << fullName <<
			    "' because its type does not match already created" <<
			    " field " << base << ". Continuing." << LogIO::POST;
			break;
		    }
		    Matrix<String> mat;
		    if (! out.isDefined(base)) {
			mat.resize(nrow,ncol);
			mat = "";
		    } else {
			out.get(base, mat);
		    }
		    // I think its a bug that the FITS classes leave keywords
		    // with trailing blank spaces, but they do.  Trailing blanks
		    // are not significant in FITS keywords.
		    // at any rate, we need to remove them.
		    String tmp = key->asString();
		    tmp.gsub(trailing, empty);
		    mat(thisRow,thisCol) = tmp;
		    out.define(base, mat);
		}
		break;
	    case FITS::FLOAT :  // Convert to DOUBLE!!
		{
		    if (fnum >= 0 && out.type(fnum) != TpArrayDouble) {
			os << LogIO::SEVERE << "Ignoring field '" << fullName <<
			    "' because its type does not match already created" <<
			    " field " << base << ". Continuing." << LogIO::POST;
			break;
		    }
		    Matrix<double> mat;
		    if (! out.isDefined(base)) {
			mat.resize(nrow,ncol);
			mat = 0.0;
		    } else {
			out.get(base, mat);
		    }
		    mat(thisRow,thisCol) = key->asFloat();
		    out.define(base, mat);
		}
		break;
	    case FITS::DOUBLE : 
		{
		    if (fnum >= 0 && out.type(fnum) != TpArrayDouble) {
			os << LogIO::SEVERE << "Ignoring field '" << fullName <<
			    "' because its type does not match already created" <<
			    " field " << base << ". Continuing." << LogIO::POST;
			break;
		    }
		    Matrix<double> mat;
		    if (! out.isDefined(base)) {
			mat.resize(nrow,ncol);
			mat = 0.0;
		    } else {
			out.get(base, mat);
		    }
		    mat(thisRow,thisCol) = key->asDouble();
		    out.define(base, mat);
		}
		break;
	    case FITS::LONG : 
		{
		    if (fnum >= 0 && out.type(fnum) != TpArrayInt) {
			os << LogIO::SEVERE << "Ignoring field '" << fullName <<
			    "' because its type does not match already created" <<
			    " field " << base << ". Continuing." << LogIO::POST;
			break;
		    }
		    Matrix<int32_t> mat;
		    if (! out.isDefined(base)) {
			mat.resize(nrow,ncol);
			mat = 0;
		    } else {
			out.get(base, mat);
		    }
		    mat(thisRow,thisCol) = key->asInt();
		    out.define(base, mat);
		}
		break;
	    case FITS::COMPLEX : 
		{
		    if (fnum >= 0 && out.type(fnum) != TpArrayComplex) {
			os << LogIO::SEVERE << "Ignoring field '" << fullName <<
			    "' because its type does not match already created" <<
			    " field " << base << ". Continuing." << LogIO::POST;
			break;
		    }
		    Matrix<Complex> mat;
		    if (! out.isDefined(base)) {
			mat.resize(nrow,ncol);
			mat = Complex(0,0);
		    } else {
			out.get(base, mat);
		    }
		    mat(thisRow,thisCol) = key->asComplex();
		    out.define(base, mat);
		}
		break;
	    case FITS::DCOMPLEX : 
		{
		    if (fnum >= 0 && out.type(fnum) != TpArrayDComplex) {
			os << LogIO::SEVERE << "Ignoring field '" << fullName <<
			    "' because its type does not match already created" <<
			    " field " << base << ". Continuing." << LogIO::POST;
			break;
		    }
		    Matrix<DComplex> mat;
		    if (! out.isDefined(base)) {
			mat.resize(nrow,ncol);
			mat = DComplex(0,0);
		    } else {
			out.get(base, mat);
		    }
		    mat(thisRow,thisCol) = key->asDComplex();
		    out.define(base, mat);
		}
		break;
	    default:
		os << LogIO::SEVERE << "Unknown type for keyword '" 
		   << fullName << "'. Continuing." << LogIO::POST;
	    }
	} else if (key->isindexed() || (fullName.contains(kw1D) && !fullName.contains(cd))){
            String base;
	    int32_t num;
	    if (key->isindexed()) {
		base = fullName;
		num = key->index();
	    } else {
		splitKW1D(base, num, fullName);
	    }
	    
	    int32_t offset = num - min1D[base];
	    int32_t nelm = 0;
	    int32_t fnum = out.fieldNumber(base);
	    switch (key->type()) {
	    case FITS::LOGICAL:
		if (fnum >= 0 && out.type(fnum) != TpArrayBool) {
		    os << LogIO::WARN << "Ignoring field '" << fullName <<
			"' because its type does not match already created" <<
			" field " << base << ". Continuing." << LogIO::POST;
		    break;
		}
		if (! out.isDefined(base)) {
		    Vector<bool> vec(max1D[base] - min1D[base] + 1);
		    vec = false;
		    vec(offset) = key->asBool();
 		    out.define(base, vec);
		} else {
		    Vector<bool> vec;
		    out.get(base, vec);
		    nelm = vec.size();
		    if(offset<nelm){
		      vec(offset) = key->asBool();
		      out.define(base, vec);
		    }
		    else{
		      os << LogIO::WARN << "Ignoring field '" << fullName << 
			"' because the maximum permitted number " << nelm <<
			" is already reached. Continuing." << LogIO::POST;
		    }
		}
		break;
	    case FITS::STRING : 
		{
		    if (fnum >= 0 && out.type(fnum) != TpArrayString) {
			os << LogIO::WARN << "Ignoring field '" << fullName <<
			    "' because its type does not match already created" <<
			    " field " << base << ". Continuing." << LogIO::POST;
			break;
		    }
		    String tmp = key->asString();
		    // I think its a bug that that the FITS classes leave keywords
		    // with trailing blank spaces, but they do.  Trailing blanks
		    // are not significant in FITS keywords.
		    // at any rate, we need to remove them
		    tmp.gsub(trailing, empty);
		    if (! out.isDefined(base)) {
			Vector<String> vec(max1D[base] - min1D[base] + 1);
			vec = "";
			vec(offset) = tmp;
			out.define(base, vec);
		    } else {
			Vector<String> vec;
			out.get(base, vec);
			nelm = vec.size();
			if(offset<nelm){
			  vec(offset) = tmp;
			  out.define(base, vec);
			}
			else{
			  os << LogIO::WARN << "Ignoring field '" << fullName << 
			    "' because the maximum permitted number " << nelm << 
			    " is already reached. Continuing." << LogIO::POST;
			}
		    }
		}
		break;
	    case FITS::FLOAT :  // Convert to DOUBLE!!
		if (fnum >= 0 && out.type(fnum) != TpArrayDouble) {
		    os << LogIO::WARN << "Ignoring field '" << fullName <<
			"' because its type does not match already created" <<
			" field " << base << ". Continuing." << LogIO::POST;
		    break;
		}
		if (! out.isDefined(base)) {
		    Vector<double> vec(max1D[base] - min1D[base] + 1);
		    vec = 0.0;
		    vec(offset) = key->asFloat();
		    out.define(base, vec);
		} else {
		    Vector<double> vec;
		    out.get(base, vec);
		    nelm = vec.size();
		    if(offset<nelm){
		      vec(offset) = key->asFloat();
		      out.define(base, vec);
		    }
		    else{
		      os << LogIO::WARN << "Ignoring field '" << fullName << 
			"' because the maximum permitted number " << nelm <<
			" is already reached. Continuing." << LogIO::POST;
		    }
		}
		break;
	    case FITS::DOUBLE : 
		if (fnum >= 0 && out.type(fnum) != TpArrayDouble) {
		    os << LogIO::WARN << "Ignoring field '" << fullName <<
			"' because its type does not match already created" <<
			" field " << base << ". Continuing." << LogIO::POST;
		    break;
		}
		if (! out.isDefined(base)) {
		    Vector<double> vec(max1D[base] - min1D[base] + 1);
		    vec = 0.0;
		    vec(offset) = key->asDouble();
		    out.define(base, vec);
		} else {
		    Vector<double> vec;
		    out.get(base, vec);
		    nelm = vec.size();
		    if(offset<nelm){
		      vec(offset) = key->asDouble();
		      out.define(base, vec);
		    }
		    else{
		      os << LogIO::WARN << "Ignoring field '" << fullName << 
			"' because the maximum permitted number " << nelm <<
			" is already reached. Continuing." << LogIO::POST;
		    }
		}
		break;
	    case FITS::LONG : 
		if (fnum >= 0 && out.type(fnum) != TpArrayInt) {
		    os << LogIO::WARN << "Ignoring field '" << fullName <<
			"' because its type does not match already created" <<
			" field " << base << ". Continuing." << LogIO::POST;
		    break;
		}
		if (! out.isDefined(base)) {
		    Vector<int32_t> vec(max1D[base] - min1D[base] + 1);
		    vec = 0;
		    vec(offset) = key->asInt();
		    out.define(base, vec);
		} else {
		    Vector<int32_t> vec;
		    out.get(base, vec);
		    nelm = vec.size();
		    if(offset<nelm){
		      vec(offset) = key->asInt();
		      out.define(base, vec);
		    }
		    else{
		      os << LogIO::WARN << "Ignoring field '" << fullName << 
			"' because the maximum permitted number " << nelm <<
			" is already reached. Continuing." << LogIO::POST;
		    }
		}
		break;
	    case FITS::COMPLEX : 
		if (fnum >= 0 && out.type(fnum) != TpArrayComplex) {
		    os << LogIO::WARN << "Ignoring field '" << fullName <<
			"' because its type does not match already created" <<
			" field " << base << ". Continuing." << LogIO::POST;
		    break;
		}
		if (! out.isDefined(base)) {
		    Vector<Complex> vec(max1D[base] - min1D[base] + 1);
		    vec = Complex(0,0);
		    vec(offset) = key->asComplex();
		    out.define(base, vec);
		} else {
		    Vector<Complex> vec;
		    out.get(base, vec);
		    nelm = vec.size();
		    if(offset<nelm){
		      vec(offset) = key->asComplex();
		      out.define(base, vec);
		    }
		    else{
		      os << LogIO::WARN << "Ignoring field '" << fullName << 
			"' because the maximum permitted number " << nelm <<
			" is already reached. Continuing." << LogIO::POST;
		    }
		}
		break;
	    case FITS::DCOMPLEX : 
		if (fnum >= 0 && out.type(fnum) != TpArrayDComplex) {
		    os << LogIO::WARN << "Ignoring field '" << fullName <<
			"' because its type does not match already created" <<
			" field " << base << ". Continuing." << LogIO::POST;
		    break;
		}
		if (! out.isDefined(base)) {
		    Vector<DComplex> vec(max1D[base] - min1D[base] + 1);
		    vec = DComplex(0,0);
		    vec(offset) = key->asDComplex();
		    out.define(base, vec);
		} else {
		    Vector<DComplex> vec;
		    out.get(base, vec);
		    nelm = vec.size();
		    if(offset<nelm){
		      vec(offset) = key->asDComplex();
		      out.define(base, vec);
		    }
		    else{
		      os << LogIO::WARN << "Ignoring field '" << fullName << 
			"' because the maximum permitted number " << nelm <<
			" is already reached. Continuing." << LogIO::POST;
		    }
		}
		break;
	    default:
		os << LogIO::SEVERE << "Unknown type for keyword '" 
		   << fullName << "'. Continuing." << LogIO::POST;
	    }
	} else {
	    // It's a scalar
	    if (out.isDefined(fullName) && fullName != "naxis") {
		os << LogIO::WARN << "Scalar keyword " << fullName << 
		    " will overwrite array field of same name. Continuing." << 
		    LogIO::POST;
	    }

	    switch (key->type()) {
	    case FITS::LOGICAL:
		out.define(fullName,key->asBool());
		break;
	    case FITS::STRING :
		{
		    String tmp = key->asString();
		    // I think its a bug that that the FITS classes leave keywords
		    // with trailing blank spaces, but they do.  Trailing blanks
		    // are not significant in FITS keywords.
		    // at any rate, we need to remove them
		    tmp.gsub(trailing, empty);
		    out.define(fullName, tmp);
		}
		break;
	    case FITS::FLOAT : 
		out.define(fullName,key->asFloat());
		break;
	    case FITS::DOUBLE : 
		out.define(fullName,key->asDouble());
		break;
	    case FITS::LONG : 
		out.define(fullName,key->asInt());
		break;
	    case FITS::COMPLEX : 
		out.define(fullName,key->asComplex());
		break;
	    case FITS::DCOMPLEX : 
		out.define(fullName,key->asDComplex());
		break;
	    case FITS::NOVALUE :
		// Skip all other than history
		if (!ignoreHistory && key->kw().name() == FITS::HISTORY) {
		    addHistory(out, key->comm());
		}
		break;
	    default:
		os << LogIO::SEVERE << "Unknown type for keyword '" << 
		    fullName << "'. Continuing." << LogIO::POST;
	    }
        }

	if (out.isDefined(fullName) && key->comm() != 0 && key->commlen()>0) {
	    out.setComment(fullName, key->comm());
	}

	key = in.next();
    }


    removeKeywords(out, ignore);
    return ok;
}

void FITSKeywordUtil::removeKeywords(RecordInterface &out,
				     const Vector<String> &ignore)
{
    LogIO os(LogOrigin("FITSKeywordUtil", "removeKeywords", WHERE));

    const int32_t nregex = ignore.nelements();
    Regex *regexlist = new Regex[nregex];
    AlwaysAssert(regexlist, AipsError);
    int32_t i;
    for (i=0; i < nregex; i++) {
        regexlist[i] = Regex(ignore(i));
    }

    const int32_t nfields = out.nfields();
    // Go backwards because removing a field causes the previous fields to
    // be renumbered.
    String nametmp;
    for (i=nfields - 1; i>= 0; i--) {
	nametmp = out.name(i);
	for (int32_t j=0; j<nregex; j++) {
	    if (nametmp.contains(regexlist[j])) {
		out.removeField(i);
		break;
	    }
	}
    }

    delete [] regexlist;
}

bool FITSKeywordUtil::fromTDIM(IPosition& shape, const String& tdim)
{
    bool ok = true;
    // verify that it has the right form
    // whitespace ( anything ) whitespace
    if (tdim.matches(Regex("[:space:]*[(].*[)][:space:]*"))) {
	// count commas to get number of elements
	String fields(tdim);
	fields = fields.after('(');
	fields = fields.before(')');
	int32_t nelem = fields.freq(',') + 1;
	String * carrst = new String [nelem];
       	if (split(fields, carrst, nelem, ',') != nelem) {
	    ok = false;
	} else {
	    shape.resize(nelem);
	    for (int32_t i=0;i<nelem;i++) {
		shape(i) = atoi(carrst[i].chars());
	    }
	}
	delete [] carrst;
    } else {
	ok = false;
    }
    return ok;
}

bool FITSKeywordUtil::toTDIM(String& tdim, const IPosition& shape)
{
    bool ok = true;

    ostringstream ostr;
    ostr << "(";
    if (shape.nelements()>0) ostr << shape(0);
    for (uint32_t i=1;i<shape.nelements();i++) {
	ostr << "," << shape(i);
    }
    ostr << ")";
    tdim = String(ostr);
    if (tdim.length() > 71) {
	ok = false;
    }
    return ok;
}

static void addText(RecordInterface &header, const String &comment,
		    const char *leader)
{
    static MLCG random;
    static bool init = false;
    if (!init) {
	Time now;
	init = true;
	random.seed1(long(now.modifiedJulianDay()*86400.0));
    }

    Vector<String> lines = stringToVector(comment, '\n');
    // Use a random number to prevent a CUBIC behaviour:
    //   (N cards * N passes through the following loop * N for isDefined)
    String keyname;
    for (uint32_t i=0; i<lines.nelements(); i++) {
        int32_t offset = static_cast<int32_t>(random.asuInt());
	do {
	    ostringstream os;
	    os << offset;
	    keyname = leader + String(os);
	    offset++;
	} while (header.isDefined(keyname));
	header.define(keyname, lines(i));
    }
}

void FITSKeywordUtil::addComment(RecordInterface &header, const String &comment)
{
    addText(header, comment, "comment");
}

void FITSKeywordUtil::addHistory(RecordInterface &header, const String &comment)
{
    addText(header, comment, "history");
}

} //# NAMESPACE CASACORE - END

