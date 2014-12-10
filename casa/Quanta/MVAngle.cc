//# MVAngle.cc: Class to handle angle type conversions and I/O
//# Copyright (C) 1996,1997,1998,1999,2000,2001,2002,2003
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
#include <casacore/casa/Quanta/MVAngle.h>
#include <casacore/casa/sstream.h>
#include <casacore/casa/iomanip.h>
#include <casacore/casa/BasicMath/Math.h>
#include <casacore/casa/BasicSL/Constants.h>
#include <casacore/casa/Quanta/QMath.h>
#include <casacore/casa/Utilities/MUString.h>
#include <casacore/casa/System/AppInfo.h>
//
#include <casacore/casa/Logging/LogIO.h>
namespace casacore { //# NAMESPACE CASACORE - BEGIN

// MVAngle class
//# Static members
MVAngle::Format MVAngle::defaultFormat = MVAngle::Format();
MVAngle::Format MVAngle::interimFormat = MVAngle::Format();
Bool MVAngle::interimSet = False;

//# Constructors
MVAngle::MVAngle() : 
val(0){}

MVAngle::MVAngle(Double d) : 
val(d){}

MVAngle::MVAngle(const MVAngle &other) :
val(other.val) {}

MVAngle::MVAngle(const Quantity &other) {
    static const Double factor = C::circle/C::day;
    val = other.getBaseValue();
    if (other.check(UnitVal::ANGLE)) {
    } else {
	other.assure(UnitVal::TIME);
	val *= factor;
    }
}

MVAngle &MVAngle::operator=(const MVAngle &other) {
    if (this != &other) {
	val = other.val;
    }
    return *this;
}

// Destructor
MVAngle::~MVAngle() {}

// Operators
MVAngle::operator Double() const {
    return val;
}

const MVAngle &MVAngle::operator()() {
    return (operator()(-0.5));
}

const MVAngle &MVAngle::operator()(Double norm) {
    Double t = val/C::_2pi - norm;
    if (t < 0 || t >=1) {
      // Next statement necessary for Linux gnu; val -= expr; gives incorrect
      // result of order 2e-11
      Double df = std::floor(t)*C::_2pi;
      val -= df;  /// val - = std::floor(t)*C::_2pi;
    }
    return *this;
}

const MVAngle &MVAngle::operator()(const MVAngle &norm) {
    return (operator()(norm.circle() - 0.5));
}

// Member functions
MVAngle MVAngle::coAngle() const {
  MVAngle t = C::pi_2 - val;
  return (t());
}

Double MVAngle::radian() const {
    return val;
}

Double MVAngle::degree() const {
    return val/C::degree;
}

Double MVAngle::circle() const {
    return val/C::circle;
}

Quantity MVAngle::get() const {
    return Quantity(val,"rad");
}

Quantity MVAngle::get(const Unit &inunit) const {
    if (inunit.getValue() == UnitVal::TIME) {
	return Quantity(circle(), "d").get(inunit);
    }
    return Quantity(val,"rad").get(inunit);
}

MVAngle::Format MVAngle::setFormat(MVAngle::formatTypes intyp, 
			  uInt inprec) {
    Format tmp = MVAngle::defaultFormat;
    MVAngle::defaultFormat.typ = intyp;
    MVAngle::defaultFormat.prec = inprec;
    MVAngle::interimSet = False;
    return tmp;
}

MVAngle::Format MVAngle::setFormat(uInt intyp, uInt inprec) {
    return setFormat((MVAngle::formatTypes)intyp, inprec);
}

MVAngle::Format MVAngle::setFormat(uInt inprec) {
    return setFormat(MVAngle::ANGLE, inprec);
}

MVAngle::Format MVAngle::setFormat(const MVAngle::Format &form) {
    Format tmp = MVAngle::defaultFormat;
    MVAngle::defaultFormat = form;
    MVAngle::interimSet = False;
    return tmp;
}

MVAngle::Format MVAngle::getFormat() {
    return MVAngle::defaultFormat;
}

MVAngle::formatTypes MVAngle::giveMe(const String &in) {
  const Int N_name = 6;
  static const String tab[N_name] = {
    "ANGLE",
    "TIME",
    "CLEAN",
    "NO_D",
    "NO_DM",
    "DIG2"
  };
  static MVAngle::formatTypes nam[N_name] = {
    MVAngle::ANGLE,
    MVAngle::TIME,
    MVAngle::CLEAN,
    MVAngle::NO_D,
    MVAngle::NO_DM,
    MVAngle::DIG2
  };
  Int t = MUString::minimaxNC(in, N_name, tab);
  return (t<N_name ? nam[t] : (MVAngle::formatTypes) 0);
}

String MVAngle::string() const {
    if (MVAngle::interimSet) {
	MVAngle::interimSet = False;
	return string(MVAngle::interimFormat);
    }
    return string(MVAngle::defaultFormat);
}
   
String MVAngle::string(uInt inprec) const {
    return string(MVAngle::Format(inprec));
}

String MVAngle::string(MVAngle::formatTypes intyp, 
		       uInt inprec) const {
    return string(MVAngle::Format(intyp, inprec));
}

String MVAngle::string(uInt intyp, uInt inprec) const {
    return string(MVAngle::Format(intyp, inprec));
}

String MVAngle::string(const MVAngle::Format &form) const {
    ostringstream oss;
    print (oss, form);
    return oss;
}

Double MVAngle::timeZone() {
  return AppInfo::timeZone();
}

void MVAngle::print(ostream &oss,
		    const MVAngle::Format &form) const {
  print(oss, form, False);
}

void MVAngle::print(ostream &oss,
		    const MVAngle::Format &form, Bool loc) const {
    uInt inprec = form.prec;
    uInt intyp = form.typ;
    uInt i1 = intyp & ~MVAngle::MOD_MASK;
    Double t, t1;
    Char sep1, sep2='m';
    if (i1 == MVAngle::ANGLE) {
      t = val/C::degree;
      if ((intyp & MVAngle::ALPHA) == MVAngle::ALPHA) {
        sep1 = 'd';
        } else {
        sep1 = '.';
        sep2 = '.';
      }
    } else {
      if (loc) {
	t = MVAngle::timeZone() * 24.0;
      } else {
	t = val/C::circle;
	t = (t - std::floor(t)) * 24.;
	if (((intyp & MVAngle::DIG2) == MVAngle::DIG2) && t > 12) {
	  t -= 24.0;
	}
      }
      if ((intyp & MVAngle::ALPHA) == MVAngle::ALPHA) {
        sep1 = 'h';
      } else {
        sep1 = ':';
        sep2 = ':';
      }
    }
    if (inprec == 0) inprec = oss.precision();
    Char sfill = oss.fill();
    t1 = 1.0;
    if (inprec > 2) t1 /= 60.;
    if (inprec > 4) t1 /= 60.;
    // The next (Double)s necessary for wrong choice of pow
    if (inprec > 6) t1 /= std::pow(Double(10), Double(inprec-6));
    if (i1 == MVAngle::ANGLE || ((intyp & MVAngle::DIG2) == MVAngle::DIG2)) {
	if (t < 0) {
	  oss << '-';
	} else if ((intyp & MVAngle::CLEAN) != MVAngle::CLEAN ||
		   ((intyp & MVAngle::DIG2) == MVAngle::DIG2)) {
	  oss << '+';
	} else {
	  oss << ' ';
	}
    }
    // The next 0.1 necessary for some rounding errors
    t = std::abs((std::floor(std::abs(t)/t1+0.5)+0.1)*t1);
    Int h = ifloor(t);
    if ((intyp & MVAngle::NO_D) != MVAngle::NO_D) {
	if (i1 == MVAngle::ANGLE) {
	  if ((intyp & MVAngle::DIG2) != MVAngle::DIG2) {
	    if (h > 999) {
	      oss << "***";
	    } else if ((intyp & MVAngle::CLEAN) != MVAngle::CLEAN) {
	      oss << setfill('0') << setw(3) << h;
	    } else {
	      oss << setfill(' ') << setw(3) << h;
	    }
	  } else {
	    if (h > 99) {
	      oss << "**";
	    } else if ((intyp & MVAngle::CLEAN) != MVAngle::CLEAN) {
	      oss << setfill('0') << setw(2) << h;
	    } else {
	      oss << setfill(' ') << setw(2) << h;
	    }
	  }
	} else {
	  if (h > 99) {
	    oss << "**";
	  } else {
	    oss << setfill('0') << setw(2) << h;
	  }
	}
	if ((inprec > 2) || ((intyp & MVAngle::CLEAN) != MVAngle::CLEAN)) {
	    oss << sep1;
	}
    } else if ((intyp & MVAngle::CLEAN) != MVAngle::CLEAN) {
	oss << sep1;
    }
    if (inprec > 2) {
	t = std::fmod(t,1.0) *60.;
	h = ifloor(t);
	if ((intyp & MVAngle::NO_DM) != MVAngle::NO_DM) {
	    oss << setfill('0') << setw(2) << h;
	    if ((inprec > 4) || ((intyp & MVAngle::CLEAN) != MVAngle::CLEAN)) {
		oss << sep2;
	    }
	} else if ((intyp & MVAngle::CLEAN) != MVAngle::CLEAN) {
	    oss << sep2;
	}
    } else if ((intyp & MVAngle::CLEAN) != MVAngle::CLEAN) {
	oss << sep2;
    }
    if (inprec > 4 && inprec < 7) {
	t = std::fmod(t,1.0) *60.;
	h = ifloor(t);
	oss << setfill('0') << setw(2) << h;
    }
    if (inprec > 6) {
      t = std::abs((std::fmod(t, 1.0) - 6.0*t1)*60.);
      // The following was necessary since abs(0) becomes -0 (Solaris at least)
      if (t <= 0.0) t = 0;
      Int oprec = oss.precision();
      ios::fmtflags oldb = oss.setf(ios::fixed,ios::floatfield);
      oss << setfill('0') << setprecision(inprec-6) << setw(inprec-3) << t <<
	setprecision(oprec);
      oss.setf(oldb,ios::floatfield);
    }
    if ((intyp & MVAngle::FITS) == MVAngle::FITS) {
      if ((intyp & MVAngle::LOCAL) == MVAngle::LOCAL) {
	MVAngle my = MVAngle::timeZone() * C::circle;
	my.print(oss, MVAngle::Format(MVAngle::TIME_CLEAN | MVAngle::DIG2, 4),
		 True);
      }
    }
    oss.fill(sfill);
}

const MVAngle &MVAngle::binorm(Double norm) {
    Double t = val/C::pi - norm;
    if (t < 0 || t >=1) {
      Double df = std::floor(t)*C::pi;
      val -= df;
    }
    return *this;
}

Bool MVAngle::unitString(UnitVal &uv, String &us, MUString &in) {
  in.skipBlank();
  us = in.get();
  return UnitVal::check(in.get(),uv);
}

Bool MVAngle::read(Quantity &res, MUString &in, Bool chk) {
  LogIO os(LogOrigin("MVAngle", "read()", WHERE));
  res = Quantity(0.0, "rad");
  in.skipBlank();
  in.push();			// Save position
  Double s = in.getSign();
  Double r = in.getuInt();
  Int tp = 0;
  if (in.testChar('.')) {
    in.skipChar();
    Double r1 = in.getuInt();
    if (in.testChar('.')) {
      in.skipChar();
      r += r1/60.0 + in.getDouble()/3600.0;
      r *= s;
      tp = 4;
    }
  }else if (in.tSkipOneCharNC('d')) {
    tp = 1;
  } else if (in.tSkipOneCharNC('h')) {
    tp = 2;
  } else if (in.tSkipOneChar(':')) {
    tp = 3;
  }
  switch (tp) {

  case 1:
  case 2:
  case 3: {
    if (in.testCharNC('m') || in.testCharNC(':')) {
      tp = 0;
    } else {
      Char tc = 'm';
      if (tp == 3) tc = ':';
      in.push();
      Double r1 = in.getuInt();
      // Test if : or m is given.
      // If not, something like 11:23 was given which cannot be followed by
      // a dot (otherwise decimal minutes would be given).
      // Also a slash and alpha are not possible after 11:23.
      // However, Z is accepted because it is the UTC time zone designator.
      // The reason is that Quantum::read first tries to read a string as an
      // MVAngle, thereafter as MVTime, and finally as value+unit.
      // So MVAngle::read has to be very strict in what it accepts to avoid
      // that it does not accept something that is meant to be something else.
      if (in.tSkipOneCharNC(tc)) {
	r += r1/60.0 + in.getDouble()/3600.;
	if (tp != 3) in.tSkipOneCharNC('s');
      } else if (tp == 3 && (in.testChar('Z') ||
                             (!in.testChar('.') &&
                              !in.testChar('/') && !in.testAlpha()))) {
	r += r1/60.0;
      } else if ( !(tp == 1 && r1 == 0 && !in.testChar('.') &&
                    !in.testChar('/'))) {
        // Accept something like 10d (meaning 10deg).
        // Normally the d in 10d would be a unit (day), but in this way
        // it is accepted as 10deg (which is what people wanted).
        // Note: 10.d is 10 days.
	tp = 0;
      }
      in.unpush();
      r *= s;
    }
  }
  break;

  default:
    break;
  }

  if (chk) {

    in.skipBlank();
    if (!in.eos()) tp = 0;	     // incorrect
  }
  switch (tp) {

  case 1:
  case 4:
    res = Quantity(r,"deg");
    break;

  case 2:
  case 3:
    res = Quantity(Quantity(r/240.,"h").getBaseValue(), "deg");
    break;

  default:
    in.pop(); return False;
    break;

  }
  in.unpush();
  return True;

}

Bool MVAngle::read(Quantity &res, const String &in, Bool chk) {
  MUString tmp(in);		// Pointed non-const String
  if (!MVAngle::read(res, tmp, chk)) {
    Double r = tmp.getDouble();
    UnitVal u; String us;
    if (!MVAngle::unitString(u,us,tmp)) return False;
    if (u == UnitVal::NODIM) {
      res = Quantity(r,"rad");
    } else if (u == UnitVal::ANGLE) {
      res = Quantity(r,us);
    } else if (u == UnitVal::TIME) {
      res = Quantity(Quantity(r/240.,us).getBaseValue(), "deg");
    } else {
      return False;
    }
  }
  return True;
}

ostream &operator<<(ostream &os, const MVAngle &meas) {
    if (MVAngle::interimSet) {
	MVAngle::interimSet = False;
	meas.print(os, MVAngle::interimFormat);
    } else {
	meas.print(os, MVAngle::defaultFormat);
    }
    return os;
}

istream &operator>>(istream &is, MVAngle &meas) {
  String str;
  is >> str;
  if (ios::failbit & is.rdstate()) return is;
  Quantity t;
  if (MVAngle::read(t, str)) {
    meas = MVAngle(t)();
  } else {
    is.clear(ios::failbit | is.rdstate());
  }
  return is;
}

ostream &operator<<(ostream &os, const MVAngle::Format &form) {
    MVAngle::interimFormat = form;
    MVAngle::interimSet = True;
    return os;
}

} //# NAMESPACE CASACORE - END

