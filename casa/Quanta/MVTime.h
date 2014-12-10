//# MVTime.h: Class to handle date/time type conversions and I/O
//# Copyright (C) 1996,1997,1998,1999,2000,2001
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

#ifndef CASA_MVTIME_H
#define CASA_MVTIME_H


//# Includes
#include <casacore/casa/aips.h>
#include <casacore/casa/Quanta/Quantum.h>
#include <casacore/casa/iosfwd.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations
class String;
class MVEpoch;
class Time;

//# Constants (SUN compiler does not accept non-simple default arguments)

// <summary>
// Class to handle date/time type conversions and I/O
// </summary>

// <use visibility=export>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="tMeasure" demos="">
// </reviewed>

// <prerequisite>
//   <li> <linkto class=Quantum>Quantum</linkto>
//   <li> <linkto class=MVAngle>MVAngle</linkto>
//   <li> <a href="http://mcps.k12.md.us/departments/year2000/Technology/ISO_std.html">
//		ISO8601 standard</a> on dates and time.
// </prerequisite>
//
// <etymology>
// From Measure, Value and Time
// </etymology>
//
// <synopsis>
// An MVTime is a simple Double for date/time conversions and I/O.
// Its internal value is in MJD. For high precision the 
// <linkto class=MVEpoch>MVEpoch</linkto> class should be used.<br>
// It can be constructed from a Double (in which case MJD are assumed),
// or from a Quantity (<src>Quantum<Double></src>). Quantities must be in
// either angle or time units, or from a
// <linkto class=MVEpoch>MVEpoch</linkto><br>
// The <linkto class=Time>OS/Time class</linkto> can be used as both input
// and output. An <src>MVTime(Time)</src> constructor exists, as well
// as a <src>Time getTime()</src>.<br>
// Construction from year, month, day is also supported.
// <note role=caution> Dates before 16 Oct 1582 are considered to be Julian, 
// rather than Gregorian</note>
// It has an automatic conversion to Double, so all standard mathematical
// operations can operate on it.<br>
// The class has a number of special functions to obtain data:
// <ul>
//   <li> <src>Double day()</src> will return value in days
//   <li> <src>Double hour()</src> will return value in hours
//   <li> <src>Double minute()</src> will return value in minutes
//   <li> <src>Double second()</src> will return value in seconds
//   <li> <src>Quantity get()</src> will return days
//   <li> <src>Quantity get(Unit)</src> will return in specified units 
//		(angle(in which case it will be between -pi and +pi) or time)
//   <li> <src>uInt weekday()</src> will return day of week (1=Mon, 7=Sun)
//   <li> <src>uInt month()</src> will return month (1=Jan)
//   <li> <src>Int year()</src> will return year
//   <li> <src>uInt monthday()</src> will return day of the month
//   <li> <src>uInt yearday()</src> will return day of year (Jan01 = 1)
//   <li> <src>uInt yearweek()</src> will return week of year
//		(week containing Jan04 = 1, week start on Monday).
//		The week before the first week will be called 0, contrary
//		to standard practice (week 53/52 of previous year).
//   <li> <src>Int ymd()</src> will return yyyymmdd as a single number
//   <li> <src>const String &dayName()</src> will return name of day
//		(Sun, Mon, Tue, Wed, Thu, Fri, Sat)
//   <li> <src>const String &monthName()</src> will retrun name of Month
//		(Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec)
// </ul>
// Output formatting is done with the <src><<</src> statement, with the
// following rules:
// <ul>
//   <li> standard output is done in the following format:
//	<src>hh:mm:ss.tt</src>. The number of
//	digits presented will be based on the precision attached to the
//	current stream
//   <li> output can be formatted by using either the <src>setFormat()</src>
//	method for global angle format setting, or the output of
//	<src>MVTime::Format()</src> data for a once off change (see later).
//	Formats have a first argument which
//	determines the type (default, if not given, MVTime::TIME, other
//	possibility MVTime::ANGLE (as +ddd.mm.ss.tt..),
//	the second the number of digits wanted (default stream precision),
//	with a value:
//	<ul>
//	  <li> <3 : hh:: only
//	  <li> <5 : hh:mm:
//	  <li> <7 : hh:mm:ss
//	  <li> >6 : with precision-6 t's added
//	</ul> 
//	comparable for angle. <note role=tip> The added colons are 
//	to enable input
//	checking of the format. Look at the 'clean' types to bypass them.
//	</note>
//	The <src>MVTime::YMD</src> format implies TIME, and will
//	precede the time with 'yyyy/mm/dd/' (or use 
//	<src>MVTime::YMD_ONLY</src> to include <src>NO_TIME</src>
//	modifier).<br>
//	The <src>MVTime::DMY</src> format implies TIME, and will
//	precede the time with 'dd-Mon-yyyy/'.<br>
//	The <src>MVTime::FITS</src> format implies TIME, and will
//	precede the time with 'ccyy-mm-ddT'.
//      The <src>BOOST</src> format implies DMY and USE_SPACE (space instead
//      of slash between date and time).
//	<br>
//	The output format can be modified with modifiers (specify as
//	MVTime::TIME | MVTime::MOD (or + MVTime::MOD)). 
//	<note role=caution> For overloading/casting 
//	problems with some compilers, the
//	use of modifiers necessitates either the presence of a precision
//	(i.e. <src>(A|B, prec)</src>), or an explicit cast:
//	<src>((MVTime::formatTypes)(A|B))</src>, or make use of
//	the provided <src>TIME[_CLEAN][_NO_H[M]]</src> and
//	<src>ANGLE[_CLEAN][_NO_D[M]]</src>.
//      </note>
//
//	The modifiers can be:
//	<ul>
//	 <li> <src>MVTime::CLEAN</src> to suppress leading or trailing
//		periods (or colons for TIME). Note that he result can not be
//		read automatically.
//	 <li> <src>MVTime::NO_H</src> (or <src>NO_D</src>) to suppress
//		the output of hours (or degrees): useful for offsets
//	 <li> <src>MVTime::NO_HM</src> (or <src>NO_DM</src>), to
//		suppress the degrees and minutes.
//       <li> <src>MVTime::DAY</src> will precede the output with
//		'Day-' (e.g. Wed-). Space delimiter is used for USE_SPACE.
//	 <li> <src>MVTime::NO_TIME</src> will suppress printing of time.
//	</ul>
//	Output in formats like <src>20'</src> can be done via the standard
//	Quantum output (e.g. <src> stream << time.get("'") </src>).
//   <li> Available formats:
//	<ul>
//	  <li> MVTime::ANGLE in +ddd.mm.ss.ttt format
//	  <li> MVTime::TIME in hh:mm:ss.ttt format
//	  <li> MVTime::[ANGLE|TIME]_CLEAN format without superfluous periods
//	  <li> MVTime::[ANGLE|TIME][_CLEAN]_NO_[D|H][M] in format with
//		leading zero fields left empty.
//	  <li> MVTime::CLEAN modifier for suppressing superfluous periods
//        <li> MVTime::USE_SPACE to use a space instead of a slash
//             as delimiter between date and time.
//	  <li> MVTime::NO_[D|H][M] modifier to suppress first field(s)
//	  <li> MVTime::DIG2 modifier to get +dd.mm.ss.ttt in angle or
//		time format(i.e. in range -90 - +90 or -12 - +12)
//	  <li> MVTime::LOCAL modifier to produce local time (as derived from
//		aipsrc time.tzoffset). In FITS mode the time zone will
//		be appended (as <src><sign>hh:mm</src>).
//		<note role=caution>The adding of the timezone is not part
//		of the FITS standard, but of the underlying ISO standard. It can
//		be used to export local times in standard format.</note>
//	</ul>
// </ul>
// The default formatting can be overwritten by a 
// <src> MVTime::setFormat(); </src> statement; which returns an 
// MVTime::Format
// structure, that can be used in a subsequent one to reset to previous.
// The format set holds for all MVTime output on all streams.<br>
// Temporary formats (i.e. for one MVTime output only), can be set by
// outputting a format (i.e. <src> stream << MVTime::Format() << ... </src>).
// <note role=caution> A setFormat() will also 
// reset any lingering temporary format.
// A setFormat(getFormat()) will reset without changing. Problems could
// arise in parallel processors. </note>
// Input can be read if the values are in any of the above (non-clean) output
// formats. <br>
// For other formatting practice, the output can be written to a String with
// the string() member functions.<br>
// Note that using a temporary format is inherently thread-unsafe because
// the format is kept in a static variable. Another thread may overwrite
// the format just set. The only thread-safe way to format an MVTime is using
// a <src>print</src> or <src>string</src> that accepts a Format object.
//
// Strings and input can be converted to an MVTime (or Quantity) by
// <src>Bool read(Quantity &out, const String &in)</src> and
// <src> istream >> MVTime &</src>. In the latter case the actual
// reading is done by the String read, which reads between white-spaces.<br>
// The following input formats (note no blanks allowed) are supported
// (+stands for an optional + or -; v for an unsigned integer; dv for a
// floating number. [] indicate optional values. Separating codes are
// case insensitive), numbers(like yyyy) can be of any length.
// The separator between date and time part can be a slash (as shown below),
// a hyphen, or one or more spaces.
// <ul>
//   <li> today		    -- (UT) time now
//   <li> today/[time]	    -- time on today (0:0:0 if omitted)
//   <li> yyyy/mm/dd[/time] -- date + time. An omitted date (leading /)
//			    will be today + time; an omitted month will
//			indicate use of day number in year (1 == 1/1)
//   <li> dd[-]MMM[-]yyyy[/time]   -- date +time If yyyy <100: around 2000.
//				MMM can be at least first three characters
//				of month name; or a month number (1 == Jan).
//				Omitted month indicates day is day number.
//   <li> ccyy-mm-dd[Ttime[Z|+-hh[:mm]]]  -- new FITS format the 'T' as time 
//				separator. Time should be UTC.
//				The 'Z' separator (for UTC) is part of an
//				earlier FITS proposal, and will be recognised
//				for backward compatibility.
//				A signed hh or hh:mm can be present to
//				indicate time zone. This value will be
//				subtracted to give UTC. To recognise this
//				format, the year should be greater than 1000. 
//				<note role=caution> The time-zone information
//				is not part of the FITS standard, but of the
//				underlying ISO standard.</note>
// </ul>
// The time can be expressed as described in 
// <linkto class=MVAngle>MVAngle</linkto>
// Examples of valid strings:
// <srcblock>
//	ToDay		   note case independence
//	1996/11/20	   20 November 1996 0h UT
//	1996/11/20/5:20    20 November 1996 at 5h20m
//	20Nov96-5h20m	   same (again no case dependence)
//	1996-11-20T5:20    same (FITS format, case dependent)
// </srcblock>
// </synopsis>
//
// <example>
// See synopsis
// </example>
//
// <motivation>
// To be able to format date/time-like values in user-required ways.
// </motivation>
//
// <todo asof="1996/11/15">
//   <li> Nothing I know of
// </todo>

class MVTime {

    public:

//# Enumerations
// Format types
    enum formatTypes {
	ANGLE,
	TIME,
	CLEAN 			= 4,
	NO_D 			= 8,
	NO_DM 			= NO_D+16,
	YMD			= TIME+32,
	DMY			= TIME+64,
	DAY			= 128,
	NO_TIME			= 256,
	MJD			= TIME+512,
	DIG2			= 1024,
	FITS			= TIME+2048,
	LOCAL			= 4096,
        USE_SPACE               = 8192,
        ALPHA                   = 16384,
        BOOST                   = DMY + USE_SPACE,
	NO_H 			= NO_D,
	NO_HM 			= NO_DM,
	ANGLE_CLEAN 		= ANGLE + CLEAN,
	ANGLE_NO_D 		= ANGLE + NO_D,
	ANGLE_NO_DM 		= ANGLE + NO_DM,
	ANGLE_CLEAN_NO_D	= ANGLE + CLEAN + NO_D,
	ANGLE_CLEAN_NO_DM	= ANGLE + CLEAN + NO_DM,
	TIME_CLEAN 		= TIME + CLEAN,
	TIME_NO_H 		= TIME + NO_H,
	TIME_NO_HM 		= TIME + NO_HM,
	TIME_CLEAN_NO_H		= TIME + CLEAN + NO_H,
	TIME_CLEAN_NO_HM	= TIME + CLEAN + NO_HM,
	YMD_ONLY		= YMD + NO_TIME,
	MOD_MASK		= CLEAN + NO_DM + DAY + NO_TIME + DIG2 +
                                  LOCAL + USE_SPACE + ALPHA
    };

//# Local structure
// Format structure
    class Format {
	public:
	friend class MVTime;
	Format(MVTime::formatTypes intyp = MVTime::TIME,
	       uInt inprec = 0) :
	typ(intyp), prec(inprec) {;};
	Format(uInt inprec) :
	typ(MVTime::TIME), prec(inprec) {;};
// Construct from type and precision (present due to overlaoding problems)
	Format(uInt intyp, uInt inprec) :
	typ((MVTime::formatTypes)intyp), prec(inprec) {;};
	Format(const Format &other) :
	typ(other.typ), prec(other.prec) {;};
	private:
	MVTime::formatTypes typ;
	uInt prec;
    };

//# Friends
// Output a date/time
    friend ostream &operator<<(ostream &os, const MVTime &meas);
// Input a date/time
    friend istream &operator>>(istream &is, MVTime &meas);
// Set a temporary format
    friend ostream &operator<<(ostream &os, const MVTime::Format &form);

//# Constructors
// Default constructor: generate a zero value
    MVTime();
// Copy constructor
    MVTime(const MVTime &other);
// Copy assignment
    MVTime &operator=(const MVTime &other);
// Constructor from Double (in MJD)
    MVTime(Double d);
// Constructor from Quantum : value can be an angle or time
// <thrown>
//   <li> AipsError if not a time or angle
// </thrown>
    MVTime(const Quantity &other);
// Constructor from Time
    MVTime(const Time &other);
// Constructor from MVEpoch;
    MVTime(const MVEpoch &other);
// Constructor from yy, mm, dd, dd (all dd with fractions allowed)
    MVTime(Int yy, Int mm, Double dd, Double d=0.0);

//# Destructor
    ~MVTime();

//# Operators
// Conversion operator
    operator Double() const;

//# General member functions
  // Make res time Quantity from string. The String version will accept
  // a time/angle Quantity as well. The chk checks for eos
  // <group>
  static Bool read(Quantity &res, const String &in, Bool chk=True);
  static Bool read(Quantity &res, MUString &in, Bool chk=True);
  // </group>
// Get value of date/time (MJD) in given units
// <group>
    Double day() const;
    Double hour() const;
    Double minute() const;
    Double second() const;
    Quantity get() const;
    Quantity get(const Unit &inunit) const;
    Time getTime() const;
// </group>
// Get indicated part of the time/date
// <group>
  const String &dayName() const;
  static const String &dayName(uInt which);
  const String &monthName() const;
  static const String &monthName(uInt which);
  // Mon = 1; Sun = 7;
  uInt weekday() const;
  // Jan =1
  uInt month() const;
  uInt monthday() const;
  Int year() const;
  Int ymd() const;
  uInt yearday() const;
  uInt yearweek() const;
// </group>
// Output data.
// <note role=warning>
// The first function below is thread-unsafe because it uses the result of
// the setFormat function which changes a static class member.
// The other functions are thread-safe because the format is directly given.
// </note>
// <group>
    String string() const;
    String string(MVTime::formatTypes intyp, uInt inprec = 0) const;
    String string(uInt intyp, uInt inprec) const;
    String string(uInt inprec) const;
    String string(const MVTime::Format &form) const;
    void print(ostream &oss, const MVTime::Format &form) const;
// </group>
// Set default format
// <note role=warning>
// It is thread-unsafe to print using the setFormat functions because they
// change a static class member. The only thred-safe way to print a time is
// to use the print function above. 
// </note>
// <group>
    static Format setFormat(MVTime::formatTypes intyp, 
			    uInt inprec = 0);
    static Format setFormat(uInt intyp, uInt inprec);
    static Format setFormat(uInt inprec = 0);
    static Format setFormat(const Format &form);
// </group>
  // Get default format
  static Format getFormat();
  // Get code belonging to string. 0 if not known
  static MVTime::formatTypes  giveMe(const String &in);
  // Get time zone offset (in days)
static Double timeZone();

    private:
//# Data
// Value
    Double val;
// Default format
    static MVTime::Format defaultFormat;
// Temporary format
// <group>
    static MVTime::Format interimFormat;
    static Bool interimSet;
// </group>

//# Member functions
  // Get the y,m,d values
  void ymd(Int &yyyy, Int &mm, Int &dd) const;
};

// Global functions.
// Output 
// <group>
ostream &operator<<(ostream &os, const MVTime &meas);
ostream &operator>>(ostream &is, MVTime &meas);
// Set a temporary format (thread-unsafe).
ostream &operator<<(ostream &os, const MVTime::Format &form);
// </group>

// is equal operator, uses operator Double which returns days
inline Bool operator==(const MVTime &lh, const MVTime &rh) 
{ return (lh.operator Double() == rh.operator Double());}



} //# NAMESPACE CASACORE - END

#endif
