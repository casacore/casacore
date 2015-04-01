//# Time.cc: Class to handle dates and times
//# Copyright (C) 1993,1994,1995,1996,1999
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

#include <casacore/casa/OS/Time.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/BasicSL/Constants.h>
#include <casacore/casa/OS/HostInfo.h>

#if defined(AIPS_SOLARIS) || defined(_AIX) || defined(AIPS_IRIX)
#include <sys/time.h>
#elif defined(AIPS_OSF)
#include <sys/timers.h>
#include <time.h>
#elif defined(AIPS_LINUX)
#include <time.h>
#include <sys/timeb.h>
#else
#include <sys/timeb.h>
#endif

#if defined(AIPS_SOLARIS) || defined(AIPS_IRIX)
extern time_t altzone;	// Not declared in all <time.h> files.
#endif


namespace casacore { //# NAMESPACE CASACORE - BEGIN

inline double daysFrom1970()
{
    return HostInfo::secondsFrom1970() / C::day;
}

Time::Time() {

  now();
}

Time::Time(double jdn) {

  mJulianDayfrac= jdn - (int)jdn;
  if( mJulianDayfrac >= 0.5) {
    mJulianDayfrac= mJulianDayfrac - 0.5;
    mJulianDay= (int)jdn - 2400000;
  }
  else {
    mJulianDayfrac= mJulianDayfrac + 0.5;
    mJulianDay= (int)jdn - 2400001;
  }
}

Time::Time(uInt year, uInt month, uInt day, uInt hour, uInt min, double sec) {

  setDate (year, month, day, hour, min, sec);
}

Time::Time( const Time& time ) {
// Copy constructor

  mJulianDay= time.mJulianDay;
  mJulianDayfrac= time.mJulianDayfrac;
}

double Time::julianDay() const {
// return Julian day
    return (double)mJulianDay + mJulianDayfrac + 2400000.5;
}

double Time::modifiedJulianDay() const {
// return modify Julian day

    return (double)mJulianDay + mJulianDayfrac;
}

Time &Time::operator=(const Time &time) {

  mJulianDay= time.mJulianDay;
  mJulianDayfrac= time.mJulianDayfrac;
  return *this;
}

double Time::operator-(const Time& begin) {

  double d,s;

  s=mJulianDayfrac - begin.mJulianDayfrac;
  if( s < 0) {
    s +=1;
    d = (double)mJulianDay - (double)begin.mJulianDay - 1.0;
  }
  else
    d = (double)mJulianDay - (double)begin.mJulianDay;

  s=(d+s)*C::day;

  return s;
}

Time Time::operator+(const double plus) {

  Time result;
  double mjd;

  mjd = mJulianDayfrac + (plus/C::day - (int)(plus/C::day));
  if(mjd >= 1.0) {
    result.mJulianDay= mJulianDay + (int)(plus/C::day) + 1;
    result.mJulianDayfrac= mjd - 1.0;
  }
  else {
    result.mJulianDay= mJulianDay + (int)(plus/C::day);
    result.mJulianDayfrac= mjd;
  }

  return result;
}

Bool Time::operator==(const Time &other) const {

  if( mJulianDay == other.mJulianDay ) {
    return mJulianDayfrac == other.mJulianDayfrac;
  }
  return False;
}

Bool Time::operator!=(const Time &other) const {

  if( mJulianDay == other.mJulianDay ) {
    return mJulianDayfrac != other.mJulianDayfrac;
  }
  return True;
}

Bool Time::operator>(const Time &other) const {

  if( mJulianDay > other.mJulianDay ) {
    return True;
  } else if (mJulianDay == other.mJulianDay ) {
    return mJulianDayfrac > other.mJulianDayfrac;
  }
  return False;
}

Bool Time::operator<(const Time &other) const {

  if( mJulianDay < other.mJulianDay ) {
    return True;
  } else if(mJulianDay == other.mJulianDay ) {
    return mJulianDayfrac < other.mJulianDayfrac;
  }
  return False;
}

String Time::toString(const Bool iso) const
{
  // if iso is True, then use ISO 8601 format
  // otherwise,
  // Produce the string of the form
  // Tue Mar 22 16:40:24 1994
  // with GMT time

  ostringstream out;

  int i,j,jd,l,n,day,month,year,dayweek,sec;
  double jdf,hour,min;

  //  Julian day
  jdf=julianDay()+0.5;
  jd=(int)jdf;
  // Julian day fraction
  jdf=jdf-(int)jdf;

  l=jd+68569;
  n=(4*l)/146097;
  l=l-(146097*n+3)/4;
  i=(4000*(l+1))/1461001;
  l=l-(1461*i)/4+31;
  j=(80*l)/2447;
  day=l-(2447*j)/80;
  l=j/11;
  month=j+2-12*l;
  year=100*(n-49)+i+l;
  dayweek=jd-7*((jd+1)/7)+2;

  hour= jdf*24.0;
  min= (hour-(int)hour)*60.0;
  // seconds are rounded to nearest second
  sec= (int)((min-(int)min)*60.0 + 0.5);
  if (sec >= 60) {
      min += 1.0;
      sec -= 60;
  }
  if (min >= 60.0) {
      hour = hour + 1.0;
      min -= 60.0;
  }
  // I don't think it should be necessary to go past hour
  // since the day, dayweek, etc. calculation is all
  // separately

  if(iso) {
      out << year  << '-';
      if (month < 10) out << '0';
      out << month << '-';
      if (day  <  10) out << '0';
      out << day   << ' ';   // NOTE: ISO8601 requires a 'T' char, not space
      if (hour <  10) out << '0';
      out << (int) hour << ':';
      if (min  <  10) out << '0';
      out << (int) min  <<':';
      if (sec  <  10) out << '0';
      out << sec;
  } else {
      switch(dayweek) {
      case 1: out<<"Sun ";
        break;
      case 2: out<<"Mon ";
        break;
      case 3: out<<"Tue ";
        break;
      case 4: out<<"Wed ";
        break;
      case 5: out<<"Thu ";
        break;
      case 6: out<<"Fri ";
        break;
      case 7: out<<"Sat ";
        break;
      }

      switch(month) {
      case 1: out<<"Jan ";
        break;
      case 2: out<<"Feb ";
        break;
      case 3: out<<"Mar ";
        break;
      case 4: out<<"Apr ";
        break;
      case 5: out<<"May ";
        break;
      case 6: out<<"Jun ";
        break;
      case 7: out<<"Jul ";
        break;
      case 8: out<<"Aug ";
        break;
      case 9: out<<"Sep ";
        break;
      case 10: out<<"Oct ";
        break;
      case 11: out<<"Nov ";
        break;
      case 12: out<<"Dec ";
        break;
      }

      out<<day;
      out<<" ";
      if(hour<10) {
        out<<"0";
      }
      out<<(int)hour;
      out<<":";
      if(min<10){
        out<<"0";
      }
      out<<(int)min;
      out<<":";
      if(sec<10){
        out<<"0";
      }
      out<<sec;
      out<<" ";
      out<<year;
  }

  return out;
}

istream& operator>>(istream& in, Time& other) {
  // The operator>> read in date with the next format
  // month/day/year,hour:min:sec
  // Is importan this format

  uInt year=0, month=0, day=0, hour=0, min=0, sec=0;
  char ch=0;

  in>>ch;
  in.putback(ch);
  in>> month >>ch;
  if(ch == '/') {
    in>> day >>ch;
  }
  if(ch == '/') {
    in>> year>>ch;
  }
  if(ch == ',') {
    in>> hour >>ch;
  }
  if(ch == ':') {
    in>> min >>ch;
  }
  if(ch == ':') {
    in>> sec;
  }

  other.setDate (year, month, day, hour, min, sec);
  return in;
}

void Time::now() {
   // Return modify Julian day number and update datas
  double d = daysFrom1970();

  // 40587 modify Julian day number = 00:00:00 January 1, 1970, GMT.
  mJulianDay= 40587 + (int)d;

  // tmv.tv_usec  Microseconds after second.
  mJulianDayfrac= d - (int)d;
}

void Time::setDate(uInt year, uInt month, uInt day, uInt hour, uInt min, double sec) {

// Converting between Julian calendar date and Julian date number
// Valid for all values of year>=-4712 ( for all dates with Julian 
// Day >= 0).

  DebugAssert(sec < 60, AipsError);
  DebugAssert(min < 60, AipsError);
  DebugAssert(hour < 24, AipsError);

  DebugAssert(month <= 12, AipsError);
  if(month==1 || month==3 || month==5 || month==7 || month==8 || month==10 || month==12) {
    DebugAssert(day <= 31, AipsError);
  } else if(month==4 || month==6 || month==9 || month==11) {
    DebugAssert(day <= 30, AipsError);
  } else {
   // Check february days for leap years
    if (isLeapYear(year)) {
      DebugAssert(day <= 29, AipsError);
    } else {
      DebugAssert(day <= 28, AipsError);
    }
  }

  double jd;  // the fraction of the day
  uInt md;    // Modify Julian day number
  int y=year,m=month,d=day;

  md =(1461*(y+4800+(m-14)/12))/4+(367*(m-2-12*((m-14)/12)))/12-(3*((y+4900+(m-14)/12)/100))/4+d-32075;

  jd= ((double)hour/24) + ((double)min/1440) + ((double)sec/C::day);
  // with this formula (calculated md)
  // date --> Julian day number - 0.5
  // then Julian day -1 (jd-1) and fraction +0.5 (md+0.5)

  mJulianDayfrac = jd;
  mJulianDay= md - 2400001;
}

double Time::age() {
   // time in seconds between some Time object and now
  double sn = HostInfo::secondsFrom1970();
  if(mJulianDay >= 40587) {
    double jd;
    jd=(double)mJulianDay - 40587.0;
    double se = sn - ((jd + mJulianDayfrac)*C::day);
    return se;
  } else {
    sn = 3506716800.0 + sn;
    double se = sn - (((double)mJulianDay + mJulianDayfrac)*C::day);
    return se;
  }
}

uInt Time::seconds() {
  // return integral seconds after the minute [0,59]
  return (uInt)(dseconds());
}

double Time::dseconds() {
  // return seconds after the minute [0,59]
  double hour,min;

  hour= mJulianDayfrac*24.0;
  min= (hour-(int)hour)*60.0;

  return ((min-(int)min)*60.0);
}

uInt Time::minutes() {
  // return minutes after the hour [0,59]
  double hour;

  hour= mJulianDayfrac*24.0;
  return (uInt)((hour-(int)hour)*60.0);
}

uInt Time::hours() {
  // return hours after the day [0,23]

  return (uInt)(mJulianDayfrac*24.0);
}

uInt Time::dayOfMonth() {
   // Return day of the month [1,31] with local time

  uInt jd,j,l,n,i;

//  Julian day
  jd= mJulianDay + 2400001;

  l=jd+68569;
  n=(4*l)/146097;
  l=l-(146097*n+3)/4;
  i=(4000*(l+1))/1461001;
  l=l-(1461*i)/4+31;
  j=(80*l)/2447;

  return l-(2447*j)/80;
}

uInt Time::month() {
  // Return month of the year [1,12] with local time

  uInt jd,j,l,n,i;

//  Julian day
  jd= mJulianDay + 2400001;

  l=jd+68569;
  n=(4*l)/146097;
  l=l-(146097*n+3)/4;
  i=(4000*(l+1))/1461001;
  l=l-(1461*i)/4+31;
  j=(80*l)/2447;
  l=j/11;

  return j+2-12*l;
}

uInt Time::year() {
  // Return year

  uInt jd,j,l,n,i;

//  Julian day
  jd= mJulianDay + 2400001;

  l=jd+68569;
  n=(4*l)/146097;
  l=l-(146097*n+3)/4;
  i=(4000*(l+1))/1461001;
  l=l-(1461*i)/4+31;
  j=(80*l)/2447;
  l=j/11;

  return 100*(n-49)+i+l;
}

uInt Time::dayOfWeek() {
  // Return day of the week for the Julian day number.
  // Where day runs from 1 though 7, with 1 being Sunday

  uInt jd;
//  Julian day
  jd= mJulianDay + 2400001;

  return jd-7*((jd+1)/7)+2;
}

uInt Time::dayOfYear() {
  // Return day of the year for the Julian day number.
  // Where day runs from 1 though 366.

  int i,j,jd,l,n,day,month,year;
 
  jd= mJulianDay + 2400001;
  l=jd+68569;
  n=(4*l)/146097;
  l=l-(146097*n+3)/4;
  i=(4000*(l+1))/1461001;
  l=l-(1461*i)/4+31;
  j=(80*l)/2447;
  day=l-(2447*j)/80;
  l=j/11;
  month=j+2-12*l;
  year=100*(n-49)+i+l;

  switch(month) {
  case 1: return day;
  case 2: return day+=31;
  case 3: day+=59;   // jan + feb = 31 + 28 = 59
    break;
  case 4: day+=90;   // jan + feb + march = 31+28+31= 90
    break;
  case 5: day+=120;  // 31+28+31+30 = 120
    break;
  case 6: day+=151;  // 31+28+31+30+31 = 151
    break;
  case 7: day+=181;  // 31+28+31+30+31+30 = 181
    break;
  case 8: day+=212;  // 31+28+31+30+31+30+31 = 212
    break;
  case 9: day+=243;  // 31+28+31+30+31+30+31+31 = 243
    break;
  case 10: day+=273; // 31+28+31+30+31+30+31+31+30 = 273
    break;
  case 11: day+=304; // 31+28+31+30+31+30+31+31+30+31 = 304
    break;
  case 12: day+=334; // 31+28+31+30+31+30+31+31+30+31+30 = 334
    break;
  }

  if (isLeapYear(year)) {
    day++;
  }

  return day;
}

uInt Time::howManyDaysInMonth(uInt month, uInt year) {
  // Return how many days are in a month
  // Note: for february, always return 28

  DebugAssert(month <= 12, AipsError);
  if(month==1 || month==3 || month==5 || month==7 || month==8 || month==10 || month==12) {
    return 31;
  } else if (month==4 || month==6 || month==9 || month==11) {
    return 30;
  }
  if (isLeapYear(year)) {
    return 29;
  } else {
    return 28;
  }
}


uInt Time::howManyDaysInMonth() {
  // Return how many days are in a months
  Time time;
  return howManyDaysInMonth (time.month(), time.year());
}

Bool Time::isLeapYear() {

  return isLeapYear (Time().year());
}

Bool Time::isLeapYear(uInt lyear) {

  if( lyear%100 == 0) {
    return lyear%400 == 0;
  }
  return lyear%4 == 0;
}

// Used internally here to determine if Daylight Savings Time (Summer
// Time) is currently active.  1 is True, 0 False.
static Int isDST () {
  time_t tim = time (NULL);
  struct tm *tm_info = localtime (&tim);
  return tm_info->tm_isdst;
}

// Returns the difference, in seconds, between UTC and local time.
// Negative values are west of GMT, positive are east.
#if defined(AIPS_SOLARIS) || defined(AIPS_IRIX)
Int Time::timeZoneSeconds () {
  return isDST () ? -altzone : -timezone;
}
#elif defined(AIPS_OSF) || defined(AIPS_DARWIN) || defined(AIPS_BSD)
Int Time::timeZoneSeconds () {
  time_t tim = time (NULL);
  struct tm *tm_info = localtime (&tim);
  return tm_info->tm_gmtoff;
}
#else
Int Time::timeZoneSeconds () {
  // This will not be accurate unless the DST correction is +1 hour.
  // HP/UX and AIX do not have an altzone variable--at least not that I
  // can find--and this is also generic enough that it should work for
  // most other "reasonable" UNIX-like OS's.  Note: Linux *had* an
  // altzone varialbe before the release of libc6 (glibc), but it's gone
  // now.
  Int dst = isDST ();
  return Int (-timezone + (C::hour * dst));
}
#endif

Double Time::timeZoneDays () {
// Same as timeZoneSeconds(), but returns fractional days rather than
// seconds.
  return (double)timeZoneSeconds () / C::day; // Turn seconds into days.
}

String Time::timeZoneName () {
// Returns a string, e.g. "EST" or "MDT", describing the current local
// time zone.
  return isDST () ? tzname[1] : tzname[0];
}

} //# NAMESPACE CASACORE - END

