//# Unit.cc: defines the Unit class
//# Copyright (C) 1994-1999,2001,2004,2008
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

//# Includes

#include <casa/Exceptions/Error.h>
#include <casa/Quanta/Unit.h>
#include <casa/Utilities/Regex.h>
#include <casa/OS/malloc.h>
#include <string.h>

namespace casa { //# NAMESPACE CASA - BEGIN

Unit::Unit() 
: uName(), uVal() {}

Unit::Unit(const Unit &other) 
: uName(), uVal() {
    uName = other.uName;
    uVal = other.uVal;
}

Unit::Unit(const String &other) 
: uName(other), uVal() {
    check();
}

Unit::Unit(const char *other) 
: uName(other), uVal() {
    check();
}

Unit::Unit(const  char *other, Int len) 
: uName(other, len), uVal() {
    check();
}

Unit::Unit(char other) 
: uName(other), uVal() {
    check();
}

Unit::~Unit() {}


Unit &Unit::operator=(const Unit &other) {
    if (this != &other) {
	uName = other.uName;
	uVal = other.uVal;
    }
    return *this;
}

Bool Unit::operator==(const Unit &other) const {
    return (uVal == other.uVal);
}

Bool Unit::operator!=(const Unit &other) const {
    return (uVal != other.uVal);
}

Bool Unit::empty() const{
    return (uName.empty());
}

const UnitVal &Unit::getValue() const {
    return uVal;
}

const String &Unit::getName() const {
    return uName;
}

void Unit::setValue(const UnitVal &in) {
    uVal = in;
}

void Unit::setName(const String &in) {
    uName = in;
}

//#  --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
//#  The following functions are designed to replace (in two passes)
//#  the following regular expression operations...
//#  --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
//#  static Regex f1("\\*\\*"); static String sf1("");
//#  static Regex f2("\\^"); static String sf2("");
//#  static Regex f3("\\*"); static String sf3(".");
//#  static Regex f4(" */ *"); static String sf4("/");
//#  static Regex f5("//"); static String sf5(".");
//#  static Regex f6("\\.*/\\.*"); static String sf6("/");
//#  static Regex sp(" +"); static String ssp(".");
//#  static Regex pd("\\.+"); static String spd(".");
//#  static Regex bp("^\\.+"); static String sbp("");
//#  static Regex ep("\\.+$"); static String ebp("");
//#  ...
//#  uName.gsub(f1, sf1);
//#  uName.gsub(f2, sf2);
//#  uName.gsub(f3, sf3);
//#  uName.gsub(f4, sf4);
//#  uName.gsub(f5, sf5);
//#  uName.gsub(f6, sf6);
//#  uName.gsub(sp, ssp);
//#  uName.gsub(pd, spd);
//#  uName.gsub(bp, sbp);
//#  uName.gsub(ep, ebp);
//#  --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---

static inline void pass_one( const char *source, char *dest ) {
    while( *source ) { 
	switch ( *source ) {
	case '^': ++source;
	          continue;                       /** f2 **/
	case '*':
	    switch ( *++source ) {
	    case '*': ++source;
	              continue;                   /** f1 **/
	    default: *dest++ = '.';               /** f3 **/
	             continue;
	    }
	case ' ': {
	    bool found_slash = false;
	    while ( *source == ' ' || *source == '/' ) { if ( *source++ == '/' ) found_slash = true; }
	    *dest++ = found_slash ? '/' : '.';    /** f4 **/
	                                          /** sp **/
	    continue;
	}
	case '/':
	    switch( *++source ) {
	    case '/':
	        ++source;
		*dest++ = '.';
		continue;                         /** f5 **/
	    default:
		*dest++ = '/';
		continue;
	    }
	default: *dest++ = *source++;
	}
    }
    *dest = '\0';
}

static inline void pass_two( char *source, char *dest ) {
    while( *source == '.' ) ++source;		/** bp **/
    for ( char *end = source + strlen(source) - 1; end != source; --end ) {
	if ( *end == '.' )
	    *end = '\0';			/** ep **/
	else
	    break;
    }
    while ( *source ) {				/** f6 **/
	switch( *source ) {
	case '.': {
	    bool go = true;
	    bool found_slash = false;
	    while ( *source && go ) {
		switch ( *source ) {
	        case '/':
		    found_slash = true;
		case '.':
		    ++source;
		    continue;
		default: go = false;
		}
	    }
	    *dest++ = (found_slash ? '/' : '.');
	}
	default:
	    *dest++ = *source++;
	}
    }
    *dest = '\0';
}
	    


void Unit::check() {

  if (!UnitVal::check(uName, uVal)) {
    throw (AipsError("Unit::check Illegal unit string '" +
		     uName + "'"));
  };

  char *b1 = strdup(uName.c_str());
  char *b2 = (char*) malloc((uName.size()+1)*sizeof(char));
  pass_one(b1,b2);
  pass_two(b2,b1);
  uName = b1;
  free(b1);
  free(b2);
}

} //# NAMESPACE CASA - END

