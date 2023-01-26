//# MVTime.cc: Class to handle date/time type conversions and I/O
//# Copyright (C) 1996,1997,1998,1999,2000,2001,2002,2003,2004,2008
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

//# Includes
#include <casacore/casa/Quanta/MVTime.h>
#include <casacore/casa/Quanta/MVAngle.h>
#include <casacore/casa/Quanta/MVEpoch.h>
#include <casacore/casa/Utilities/MUString.h>
#include <casacore/casa/sstream.h>
#include <casacore/casa/iomanip.h>
#include <casacore/casa/OS/Time.h>
#include <casacore/casa/BasicMath/Math.h>
#include <casacore/casa/BasicSL/Constants.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/casa/Utilities/Assert.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

// MVTime class
//# Static members
MVTime::Format MVTime::defaultFormat = MVTime::Format();
MVTime::Format MVTime::interimFormat = MVTime::Format();
bool MVTime::interimSet = false;

//# Constructors
MVTime::MVTime() : 
val(0){}

MVTime::MVTime(double d) : 
val(d){}

MVTime::MVTime(const Time &other) : 
val(other.modifiedJulianDay()){}

MVTime::MVTime(const MVEpoch &other) : 
val(other.get()){}

MVTime::MVTime(int32_t yy, int32_t mm, double dd, double d) {
    if (mm < 3) {
	yy--;
	mm += 12;
    }
    dd += d;
    int32_t b = 0;
    if (yy>1582 || (yy==1582 && (mm>10 || (mm==10 && dd >= 15)))) { 
	b = ifloor(yy/100.);
	b = 2 - b + (int32_t)(b/4);
    }
    val = ifloor(365.25*yy) + ifloor(30.6001*(mm+1)) + dd - 679006.0 +b;
}

MVTime::MVTime(const MVTime &other) :
val(other.val) {}

MVTime::MVTime(const Quantity &other) {
    val = other.getBaseValue();
    if (other.check(UnitVal::ANGLE)) {
	val /= C::circle;
    } else {
	other.assure(UnitVal::TIME);
	val /= C::day;
    }
}

MVTime &MVTime::operator=(const MVTime &other) {
    if (this != &other) {
	val = other.val;
    }
    return *this;
}

// Destructor
MVTime::~MVTime() {}

// Operators
MVTime::operator double() const {
    return val;
}

// Member functions
double MVTime::day() const {
    return val;
}

double MVTime::hour() const {
    return val*24.;
}

double MVTime::minute() const {
    return val*24.*60.;
}

double MVTime::second() const {
    return val*24.*3600.;
}

Quantity MVTime::get() const {
    return Quantity(val,"d");
}

Quantity MVTime::get(const Unit &inunit) const {
    if (inunit.getValue() == UnitVal::TIME) {
	return Quantity(val, "d").get(inunit);
    }
    return Quantity(val*C::circle,"rad").get(inunit);
}

Time MVTime::getTime() const {
    return Time(val+2400000.5);
}

const String &MVTime::dayName(uint32_t which) {
  static const String weekDay[7] = {
    "Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"};
  AlwaysAssert(which > 0 && which < 8, AipsError);
  return weekDay[which-1];
}

const String &MVTime::dayName() const {
  return (dayName(weekday()));
}

const String &MVTime::monthName(uint32_t which) {
  static const String mon[12] = {
    "Jan", "Feb", "Mar", "Apr", "May", "Jun",
    "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"};
  return mon[which-1];
}

const String &MVTime::monthName() const {
  return (monthName(month()));
}

uint32_t MVTime::weekday() const {
  return ((ifloor(val+2.)%7 + 7)%7 + 1);
}

uint32_t MVTime::month() const {
  int32_t c,e,a;
  ymd(c,e,a);
  return e;
}

uint32_t MVTime::monthday() const {
  int32_t c,e,a;
  ymd(c,e,a);
  return a;
}

int32_t MVTime::year() const {
  int32_t c,e,a;
  ymd(c,e,a);
  return c;
}

int32_t MVTime::ymd() const {
  int32_t c,e,a;
  ymd(c,e,a);
  if (c < 0) {
    return -(abs(c)*10000 + e*100 + a);
  }
  return (c*10000 + e*100 + a);
}

uint32_t MVTime::yearday() const {
  int32_t c,e,a;
  ymd(c,e,a);
  if (c%4 == 0 && (c%100 != 0 || c%400 == 0)) {
    c = (e+9)/12;
  } else {
    c = 2 * ((e+9)/12);
  }
  return ((275*e)/9 - c + a - 30);
}

uint32_t MVTime::yearweek() const {
  int32_t yd(yearday()-4);
  uint32_t yw((yd+7)/7);
  yd %= 7;
  // Check for other week
  if (yd >= 0) {
    if (yd >= (int32_t)weekday()) return yw+1;
  } else if (yd+7 >= (int32_t)weekday()) return yw+1;
  return yw;
}

void MVTime::ymd(int32_t &yyyy, int32_t &mm, int32_t &dd) const {
	int32_t z = ifloor(val + 2400001.0);
	dd = z;
	if (z >= 2299161) {
	    long al = ifloor(((double) z - 1867216.25)/36524.25);
	    dd = z + 1 + al - (int32_t)(al/4);
	}
	dd += 1524;
        // tmp introduced to circumvent optimization problem with gcc2.7.2.1
        // on the DecAlpha
        int32_t tmp = ifloor((dd - 122.1)/365.25);
        yyyy = tmp;
        int32_t d = ifloor(365.25 * tmp);
	mm = tmp = ifloor((dd - d)/30.6001);
	dd -= d + ifloor(30.6001 * tmp); // day
	if (mm < 14) {			// month
	    mm--;
	} else {
	    mm -= 13;
	}
	yyyy -= 4715;			// year
	if (mm > 2) yyyy--;
}


MVTime::Format MVTime::setFormat(MVTime::formatTypes intyp, 
			  uint32_t inprec) {
    Format tmp = MVTime::defaultFormat;
    MVTime::defaultFormat.typ = intyp;
    MVTime::defaultFormat.prec = inprec;
    MVTime::interimSet = false;
    return tmp;
}

MVTime::Format MVTime::setFormat(uint32_t intyp, uint32_t inprec) {
    return setFormat((MVTime::formatTypes) intyp, inprec);
}

MVTime::Format MVTime::setFormat(uint32_t inprec) {
    return  setFormat(MVTime::TIME, inprec);
}

MVTime::Format MVTime::setFormat(const MVTime::Format &form) {
    Format tmp = MVTime::defaultFormat;
    MVTime::defaultFormat = form;
    MVTime::interimSet = false;
    return tmp;
}

MVTime::Format MVTime::getFormat() {
    return MVTime::defaultFormat;
}

MVTime::formatTypes MVTime::giveMe(const String &in) {
  const int32_t N_name = 32;
  static const String tab[N_name] = {
    "ANGLE",
    "TIME",
    "CLEAN",
    "NO_D",
    "NO_DM",
    "YMD",
    "DMY",
    "MJD",
    "DAY",
    "NO_TIME",
    "DIG2",
    "FITS",
    "LOCAL",
    "USE_SPACE",
    "ALPHA",
    "USE_Z",
    "ISO",
    "BOOST",
    "NO_H",
    "NO_HM",
    "ANGLE_CLEAN",
    "ANGLE_NO_D",
    "ANGLE_NO_DM",
    "ANGLE_CLEAN_NO_D",
    "ANGLE_CLEAN_NO_DM",
    "TIME_CLEAN",
    "TIME_NO_H",
    "TIME_NO_HM",
    "TIME_CLEAN_NO_H",
    "TIME_CLEAN_NO_HM",
    "YMD_ONLY",
    "MOD_MASK"
  };
  static const MVTime::formatTypes nam[N_name] = {
    MVTime::ANGLE,
    MVTime::TIME,
    MVTime::CLEAN,
    MVTime::NO_D,
    MVTime::NO_DM,
    MVTime::YMD,
    MVTime::DMY,
    MVTime::MJD,
    MVTime::DAY,
    MVTime::NO_TIME,
    MVTime::DIG2,
    MVTime::FITS,
    MVTime::LOCAL,
    MVTime::USE_SPACE,
    MVTime::ALPHA,
    MVTime::USE_Z,
    MVTime::ISO,
    MVTime::BOOST,
    MVTime::NO_H,
    MVTime::NO_HM,
    MVTime::ANGLE_CLEAN,
    MVTime::ANGLE_NO_D,
    MVTime::ANGLE_NO_DM,
    MVTime::ANGLE_CLEAN_NO_D,
    MVTime::ANGLE_CLEAN_NO_DM,
    MVTime::TIME_CLEAN,
    MVTime::TIME_NO_H,
    MVTime::TIME_NO_HM,
    MVTime::TIME_CLEAN_NO_H,
    MVTime::TIME_CLEAN_NO_HM,
    MVTime::YMD_ONLY,
    MVTime::MOD_MASK
  };
  int32_t t = MUString::minimaxNC(in, N_name, tab);
  return (t<N_name ? nam[t] : (MVTime::formatTypes) 0);
}

String MVTime::string() const {
    if (MVTime::interimSet) {
	MVTime::interimSet = false;
	return string(MVTime::interimFormat);
    }
    return string(MVTime::defaultFormat);
}
   
String MVTime::string(uint32_t inprec) const {
    return string(MVTime::Format(inprec));
}

String MVTime::string(MVTime::formatTypes intyp, 
		       uint32_t inprec) const {
    return string(MVTime::Format(intyp, inprec));
}

String MVTime::string(uint32_t intyp, uint32_t inprec) const {
    return string(MVTime::Format(intyp, inprec));
}

String MVTime::string(const MVTime::Format &form) const {
    ostringstream oss;
    print (oss, form);
    return oss;
}

double MVTime::timeZone() {
  return MVAngle::timeZone();
}  
void MVTime::print(ostream &oss,
		    const MVTime::Format &form) const {
    uint32_t inprec = form.prec;
    uint32_t intyp = form.typ;
    uint32_t i1 = intyp & ~MVTime::MOD_MASK;
    // Next is to try to solve the problem with the Intel's indecision
    // arithmetic
    double loctmp(val);
    if ((intyp & MVTime::LOCAL) == MVTime::LOCAL) {
      loctmp += MVTime::timeZone();
    }
    int32_t locday = ifloor(loctmp);
    MVTime loc = double(locday);
    loctmp = (loctmp - loc.val)*C::circle;
    MVAngle atmp(loctmp);
    atmp(0.0);

    if ((intyp & MVTime::DAY) == MVTime::DAY) {
      oss << loc.dayName();
      if (i1 == MVTime::YMD || i1 == MVTime::DMY ||
	  i1 == MVTime::MJD ||
	  (intyp & MVTime::NO_TIME) != MVTime::NO_TIME) {
        if (intyp & MVTime::USE_SPACE) {
          oss << ' ';
        } else {
          oss << '-';
        }
      }
    }
    if (i1 == MVTime::YMD || i1 == MVTime::DMY || i1 == MVTime::FITS) {
      int32_t c,e,a;
      loc.ymd(c,e,a);			// y,m,d
      char sfill = oss.fill();
      if (i1 == MVTime::YMD) {
	oss << setfill('0') << setw(4) << c << "/" << 
	  setw(2) << e << "/" << 
	  setw(2) << a;
      } else if (i1 == MVTime::DMY) {
	oss << setfill('0') << setw(2) << a << "-" <<
	  setw(3) << monthName(e) << "-" <<
	  setw(4) << c;
      } else {				// FITS
	oss << setfill('0') << setw(4) << c << "-" << 
	  setw(2) << e << "-" << 
	  setw(2) << a;
      }
      if ((intyp & MVTime::NO_TIME) != MVTime::NO_TIME) {
        if (intyp & MVTime::USE_SPACE) {
          oss << ' ';
	} else if (i1 == MVTime::FITS) {
	  oss << "T";
	} else {
	  oss << "/";
	}
      }
      oss.fill(sfill);
    }
    if (i1 == MVTime::MJD) {
      int32_t c = ifloor(loc);
      oss << c;
      if ((intyp & MVTime::NO_TIME) != MVTime::NO_TIME) {
	oss << "/";
      }
    }
    if ((intyp & MVTime::NO_TIME) != MVTime::NO_TIME) {
	MVAngle::Format ftmp((MVAngle::formatTypes) intyp, inprec);
	atmp.print(oss, ftmp);
    }
    if ((intyp & MVTime::USE_Z) == MVTime::USE_Z) {
        oss << 'Z';
    }
}

bool MVTime::read(Quantity &res, MUString &in, bool chk) {
  return read (res, in, chk, false);
}
bool MVTime::read(Quantity &res, MUString &in, bool chk, bool throwExcp) {
  static const String mon[12] = {
    "January", "February", "March", "April", "May", "June",
    "July", "August", "September", "October", "November", "December"};
  res = Quantity(0.0, "d");
  int32_t tp = 0;
  in.skipBlank();
  in.push();			// Save position
  double s = in.getSign();
  if (in.tSkipStringNC("today") || in.tSkipStringNC("now") ||
      in.testChar('/')) {
    if (in.tSkipChar('/') || in.tSkipChar('-') || in.tSkipChar(' ')) {
      if (MVAngle::read(res, in, chk)) {
	res = Quantity(res.get("deg").getValue()/360., "d");
	res += Quantity(double((int32_t) Time().modifiedJulianDay()),
			"d");
      } else {
	return MVAngle::handleReadError (in, throwExcp);
      }
    } else {
      res = Quantity(Time().modifiedJulianDay(), "d");
    }
  } else {
    int32_t r = in.getuInt();
    int32_t mm = 0;
    double dd = 0;
    if (in.testChar('-') || in.testAlpha()) {
      if (in.testChar('-')) in.skipChar();
      if (in.testAlpha()) {
        String amon(in.getAlpha());
        if (amon.length() < 3) {
          return MVAngle::handleReadError (in, throwExcp);
        } else {
          mm = MUString::minimaxNC(amon, 12, mon);
          if (mm < 12) {
            mm++;
          } else {
            return MVAngle::handleReadError (in, throwExcp);
          }
        }
      } else {
        if (in.testChar('-')) {
          mm = 1;
        } else {
          mm = in.getuInt();
        }
        if (!in.testChar('-')) {
          return MVAngle::handleReadError (in, throwExcp);
        }
      }
      in.skipChar('-');
      int32_t dd2 = in.getuInt();
      if (r > 1000) {		// New FITS format
        dd = dd2;
      } else {
        if (dd2 < 50) {
          dd2 += 2000;
        } else if (dd2 < 100) {
          dd2 += 1900;
        }
        dd = r;			// Swap day/year
        r = dd2;
      }    
    } else if (in.testChar('/')) {
      if (in.freqChar('/') > 1) {
        in.skipChar();
        if (in.testChar('/')) {
          in.skipChar();
          mm = 1;
        } else {
          mm = in.getuInt();
          if (!in.tSkipChar('/')) {
            return MVAngle::handleReadError (in, throwExcp);
          }
        }
        dd = in.getDouble();
      } else {
        tp = 1;
      }
    } else {
      return MVAngle::handleReadError (in, throwExcp);
    }
    if (in.tSkipChar('/') || in.tSkipChar('-') || in.tSkipChar(' ')) {
      if (MVAngle::read(res, in, chk)) {
        res = Quantity(res.get("deg").getValue()/360., "d");
      } else {
        return MVAngle::handleReadError (in, throwExcp);
      }
    } else if (in.tSkipChar('T')) {	// new FITS/ISO
      if (MVAngle::read(res, in, false)) {
        res = Quantity(res.get("deg").getValue()/360., "d");
        // Allow possible time zone as in ISO-8601
        if (in.testChar('+') || in.testChar('-')) {
          double s = in.getSign();
          double r = in.getuInt();
          if (in.tSkipChar(':')) {
            r += double(in.getuInt())/60.0;
          }
          res -= Quantity(s*r/24.0,"d");	// Time zone
        } else {
          in.tSkipChar('Z');	// accept FITS/ISO UTC
        }
      } else {
        return MVAngle::handleReadError (in, throwExcp);
      }
    }
    if (tp == 0) {
      res += MVTime(r, mm, dd).get();
    } else {
      res += r;
    }
  }
  res *= s;			// Sign
  if (chk) {
    in.skipBlank();
    if (!in.eos()) {
      return MVAngle::handleReadError (in, throwExcp);  // not fully consumed
    }
  }
  in.unpush();
  return true;
}

bool MVTime::read(Quantity &res, const String &in, bool chk) {
  return read (res, in, chk, false);
}
bool MVTime::read(Quantity &res, const String &in, bool chk, bool throwExcp) {
  MUString tmp(in);		// Pointed non-const String
  if (!MVTime::read(res, tmp, chk, throwExcp)) {
    double r = tmp.getDouble();
    UnitVal u; String us;
    if (!MVAngle::unitString(u,us,tmp)) {
      return MVAngle::handleReadError (tmp, throwExcp);
    }
    if (u == UnitVal::NODIM) {
      res = Quantity(r,"d");
    } else if (u == UnitVal::TIME) {
      res = Quantity(r,us);
    } else if (u == UnitVal::ANGLE) {
      res = Quantity(Quantity(r/C::_2pi,us).getBaseValue(), "d");
    } else {
      return MVAngle::handleReadError (tmp, throwExcp);
    }
  }
  return true;
}

ostream &operator<<(ostream &os, const MVTime &meas) {
    if (MVTime::interimSet) {
	MVTime::interimSet = false;
	meas.print(os, MVTime::interimFormat);
    } else {
	meas.print(os, MVTime::defaultFormat);
    }
    return os;
}

istream &operator>>(istream &is, MVTime &meas) {
  String str;
  is >> str;
  if (ios::failbit & is.rdstate()) return is;
  Quantity t;
  if (MVTime::read(t, str)) {
    meas = MVTime(t).get();
  } else {
    is.clear(ios::failbit | is.rdstate());
  }
  return is;
}

ostream &operator<<(ostream &os, const MVTime::Format &form) {
    MVTime::interimFormat = form;
    MVTime::interimSet = true;
    return os;
}

} //# NAMESPACE CASACORE - END

