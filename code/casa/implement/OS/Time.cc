//# Time.cc: Class to handle dates and times
//# Copyright (C) 1993,1994,1995,1996
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

#include <aips/OS/Time.h>
#include <aips/Exceptions/Error.h>
#include <aips/Utilities/Assert.h>

#if defined(AIPS_SOLARIS) || defined(_AIX) || defined(AIPS_IRIX)
#include <sys/time.h>
#elif defined(AIPS_OSF)
#include <sys/timers.h>
#else
#include <sys/timeb.h>
#endif

#if defined(AIPS_SOLARIS) && !defined(__CLCC__)
extern "C" { int gettimeofday(struct timeval *tp, void*); };
#endif
#if defined(AIPS_OSF)
extern "C" { int getclock(int clock_type, struct timespec* tp); };
#endif

#if defined(AIPS_SOLARIS) && defined(__CLCC__)
static double secondsFrom1970()
{
    struct timeval  tp;
    AlwaysAssert(gettimeofday(&tp) >= 0, AipsError);
    double total = tp.tv_sec;
    total += tp.tv_usec * 0.000001;
    return total;
}
#elif defined(AIPS_SOLARIS) || defined(_AIX) || defined(AIPS_IRIX)
static double secondsFrom1970()
{
    struct timeval  tp;
    struct timezone tz;
    tz.tz_minuteswest = 0;
    AlwaysAssert(gettimeofday(&tp, &tz) >= 0, AipsError);
    double total = tp.tv_sec;
    total += tp.tv_usec * 0.000001;
    return total;
}
#elif defined(AIPS_OSF)
static double secondsFrom1970()
{
  struct timespec tp;
  AlwaysAssert(getclock(TIMEOFDAY,&tp) == 0, AipsError);
  double total = tp.tv_sec;
  total += tp.tv_nsec * 1.e-9;
  return total;
}
#else
static double secondsFrom1970()
{
    struct timeb ftm;
    AlwaysAssert(ftime(&ftm) >= 0, AipsError);
    double total = ftm.time;
    total += ftm.millitm*0.001;
    return total;
}
#endif

inline double daysFrom1970()
{
    return secondsFrom1970() / 86400.0;
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

// Converting between Julian calendar date and Julian date number
// Valid for all values of year>=-4712 ( for all dates with Julian 
// Day >= 0).

  if(sec >= 60)
    DebugAssert(sec >= 60, AipsError);
  if(min >= 60)
    DebugAssert(min >= 60, AipsError);
  if(hour >= 24)
    DebugAssert(hour >= 24, AipsError);

  if(month > 12)
    DebugAssert(month > 12, AipsError);
  if(month==1 || month==3 || month==5 || month==7 || month==8 || month==10 || month==12) {
    if(day > 31)
      DebugAssert(day > 31, AipsError);
  }
  else if(month==4 || month==6 || month==9 || month==11) {
    if(day > 30)
      DebugAssert(day > 30, AipsError);
  }
  else {
    // Check february days for leap years
    if( year%100 == 0)
      if( year%400 == 0) {
        if(day > 29)
        DebugAssert(day > 29, AipsError);
      }
      else {
        if(day > 28)
        DebugAssert(day > 28, AipsError);
      }
    else
      if( year%4 == 0) {
        if(day > 29)
          DebugAssert(day > 29, AipsError);
      }
      else {
        if(day > 28)
          DebugAssert(day > 28, AipsError);
      }
  }

  double jd;  // the fraction of the day
  uInt md;    // Modify Julian day number
  int y=year,m=month,d=day;

  md =(1461*(y+4800+(m-14)/12))/4+(367*(m-2-12*((m-14)/12)))/12-(3*((y+4900+(m-14)/12)/100))/4+d-32075;

  jd= ((double)hour/24) + ((double)min/1440) + ((double)sec/86400);
  // with this formula (calculated md)
  // date --> Julian day number - 0.5
  // then Julian day -1 (jd-1) and fraction +0.5 (md+0.5)

  mJulianDayfrac = jd;
  mJulianDay= md - 2400001;
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

  s=(d+s)*86400.0;

  return s;
}

Time Time::operator+(const double plus) {

  Time result;
  double mjd;

  mjd = mJulianDayfrac + (plus/86400.0 - (int)(plus/86400.0));
  if(mjd >= 1.0) {
    result.mJulianDay= mJulianDay + (int)(plus/86400.0) + 1;
    result.mJulianDayfrac= mjd - 1.0;
  }
  else {
    result.mJulianDay= mJulianDay + (int)(plus/86400.0);
    result.mJulianDayfrac= mjd;
  }

  return result;
}

Bool Time::operator==(const Time &other) const {

  if( mJulianDay == other.mJulianDay ) {
    if( mJulianDayfrac == other.mJulianDayfrac)
      return True;
    else
      return False;
  }
  else
    return False;
}

Bool Time::operator!=(const Time &other) const {

  if( mJulianDay != other.mJulianDay )
    return True;
  else {
    if( mJulianDayfrac != other.mJulianDayfrac)
      return True;
    else
      return False;
  }
}

Bool Time::operator>(const Time &other) const {

  if( mJulianDay > other.mJulianDay )
    return True;
  else if(mJulianDay == other.mJulianDay ) {
    if( mJulianDayfrac > other.mJulianDayfrac)
      return True;
    else
      return False;
  }
  else
    return False;
}

Bool Time::operator<(const Time &other) const {

  if( mJulianDay < other.mJulianDay )
    return True;
  else if(mJulianDay == other.mJulianDay ) {
    if( mJulianDayfrac < other.mJulianDayfrac)
      return True;
    else
      return False;
  }
  else
    return False;
}

ostream& operator<<(ostream& out, const Time& other) {
  // The operator<< write out Time
  // Produce the string of the form
  // Tue Mar 22 16:40:24 1994
  // with GMT time

  int i,j,jd,l,n,day,month,year,dayweek,sec;
  double jdf,hour,min;

  //  Julian day
  jdf=other.julianDay()+0.5;
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
  // deparately

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
  if(hour<10){
    out<<"0";
    out<<(int)hour;
  }
  else
    out<<(int)hour;
  out<<":";
  if(min<10){
    out<<"0";
    out<<(int)min;
  }
  else
    out<<(int)min;
  out<<":";
  if(sec<10){
    out<<"0";
    out<<sec;
  }
  else
    out<<sec;
  out<<" ";
  out<<year;

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
  if(ch == '/')
    in>> day >>ch;
  if(ch == '/')
    in>> year>>ch;
  if(ch == ',')
    in>> hour >>ch;
  if(ch == ':')
    in>> min >>ch;
  if(ch == ':')
    in>> sec;

// Converting between Julian calendar date and Julian date number
// Valid for all values of year>=-4712 ( for all dates with Julian Day >=
// 0).
  if(sec >= 60)
    DebugAssert(sec >= 60, AipsError);
  if(min >= 60)
    DebugAssert(min >= 60, AipsError);
  if(hour >= 24)
    DebugAssert(hour >= 24, AipsError);

  if(month > 12)
    DebugAssert(month > 12, AipsError);
  if(month==1 || month==3 || month==5 || month==7 || month==8 || month==10 || month==12) {
    if(day > 31)
      DebugAssert(day > 31, AipsError);
  }
  else if(month==4 || month==6 || month==9 || month==11) {
    if(day > 30)
      DebugAssert(day > 30, AipsError);
  }
  else {
    // Check february days for leap years
    if( year%100 == 0)
      if( year%400 == 0) {
        if(day > 29)
        DebugAssert(day > 29, AipsError);
      }
      else {
        if(day > 28)
        DebugAssert(day > 28, AipsError);
      }
    else
      if( year%4 == 0) {
        if(day > 29)
          DebugAssert(day > 29, AipsError);
      }
      else {
        if(day > 28)
          DebugAssert(day > 28, AipsError);
      }
  }

  double jd;  // the fraction of the day
  uInt md;    // Modify Julian day number
  int y=year,m=month,d=day;

  md =(1461*(y+4800+(m-14)/12))/4+(367*(m-2-12*((m-14)/12)))/12-(3*((y+4900+(m-14)/12)/100))/4+d-32075;

  jd= ((double)hour/24) + ((double)min/1440) + ((double)sec/86400);
  // with this formula (calculated md)
  // date --> Julian day number - 0.5
  // then Julian day -1 (jd-1) and fraction +0.5 (md+0.5)

  other.mJulianDayfrac = jd;
  other.mJulianDay = md - 2400001 ;

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

  if(sec >= 60)
    DebugAssert(sec >= 60, AipsError);
  if(min >= 60)
    DebugAssert(min >= 60, AipsError);
  if(hour >= 24)
    DebugAssert(hour >= 24, AipsError);

  if(month > 12)
    DebugAssert(month > 12, AipsError);
  if(month==1 || month==3 || month==5 || month==7 || month==8 || month==10 || month==12) {
    if(day > 31)
      DebugAssert(day > 31, AipsError);
  }
  else if(month==4 || month==6 || month==9 || month==11) {
    if(day > 30)
      DebugAssert(day > 30, AipsError);
  }
  else {
   // Check february days for leap years
    if( year%100 == 0)
      if( year%400 == 0) {
        if(day > 29)
        DebugAssert(day > 29, AipsError);
      }
      else {
        if(day > 28)
        DebugAssert(day > 28, AipsError);
      }
    else
      if( year%4 == 0) {
        if(day > 29)
          DebugAssert(day > 29, AipsError);
      }
      else {
        if(day > 28)
          DebugAssert(day > 28, AipsError);
      }
  }

  double jd;  // the fraction of the day
  uInt md;    // Modify Julian day number
  int y=year,m=month,d=day;

  md =(1461*(y+4800+(m-14)/12))/4+(367*(m-2-12*((m-14)/12)))/12-(3*((y+4900+(m-14)/12)/100))/4+d-32075;

  jd= ((double)hour/24) + ((double)min/1440) + ((double)sec/86400);
  // with this formula (calculated md)
  // date --> Julian day number - 0.5
  // then Julian day -1 (jd-1) and fraction +0.5 (md+0.5)

  mJulianDayfrac = jd;
  mJulianDay= md - 2400001;
}

double Time::age() {
   // time in seconds between some Time object and now
  double sn = secondsFrom1970();
  if(mJulianDay >= 40587) {
    double jd;
    jd=(double)mJulianDay - 40587.0;
    double se = sn - ((jd + mJulianDayfrac)*86400.0);
    return se;
  }
  else {
    sn = 3506716800.0 + sn;
    double se = sn - (((double)mJulianDay + mJulianDayfrac)*86400.0);
    return se;
  }
}

uInt Time::seconds() {
  // return seconds after the minute [0,59]
  double hour,min;

  hour= mJulianDayfrac*24.0;
  min= (hour-(int)hour)*60.0;

  return (uInt)((min-(int)min)*60.0);
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

  if( year%100 == 0)
    if( year%400 == 0)
      day++;
  else
    if( year%4 == 0)
      day++;

  return day;
}

uInt Time::leapSeconds() {
  // Return leap seconds
  uInt leapsec=10;

  if(Time().mJulianDay>=41499) leapsec++;    // 1 July      1972
  if(Time().mJulianDay>=41683) leapsec++;    // 1 January   1973
  if(Time().mJulianDay>=42048) leapsec++;    // 1 January   1974
  if(Time().mJulianDay>=42413) leapsec++;    // 1 January   1975
  if(Time().mJulianDay>=42778) leapsec++;    // 1 January   1976
  if(Time().mJulianDay>=43144) leapsec++;    // 1 January   1977
  if(Time().mJulianDay>=43509) leapsec++;    // 1 January   1978
  if(Time().mJulianDay>=43874) leapsec++;    // 1 January   1979
  if(Time().mJulianDay>=44239) leapsec++;    // 1 January   1980
  if(Time().mJulianDay>=44786) leapsec++;    // 1 July      1981
  if(Time().mJulianDay>=45151) leapsec++;    // 1 July      1982
  if(Time().mJulianDay>=45516) leapsec++;    // 1 July      1983
  if(Time().mJulianDay>=46247) leapsec++;    // 1 July      1985
  if(Time().mJulianDay>=47161) leapsec++;    // 1 January   1988
  if(Time().mJulianDay>=47892) leapsec++;    // 1 January   1990
  if(Time().mJulianDay>=48257) leapsec++;    // 1 January   1991
  if(Time().mJulianDay>=48804) leapsec++;    // 1 July      1992
  if(Time().mJulianDay>=49169) leapsec++;    // 1 July      1993

  return leapsec;
}

uInt Time::howManyDaysInMonth(uInt month, uInt year) {
  // Return how many days are in a month
  // Note: for february, always return 28

  if(month > 12)
    DebugAssert(month > 12, AipsError);
  if(month==1 || month==3 || month==5 || month==7 || month==8 || month==10 || month==12)
    return 31;
  else if(month==4 || month==6 || month==9 || month==11)
    return 30;
  else
    if(Time().isLeapYear(year))
      return 29;
    else
      return 28;
}

uInt Time::howManyDaysInMonth(uInt month) {
  // Return how many days are in a month
  // Note: for february, always return 28

  if(month > 12)
    DebugAssert(month > 12, AipsError);
  if(month==1 || month==3 || month==5 || month==7 || month==8 || month==10 || month==12)
    return 31;
  else if(month==4 || month==6 || month==9 || month==11)
    return 30;
  else
    if(Time().isLeapYear())
      return 29;
    else
      return 28;
}

uInt Time::howManyDaysInMonth() {
  // Return how many days are in a months
  // Note: for february, always return 28

  uInt months=Time().month();
  if(months > 12)
    DebugAssert(months > 12, AipsError);
  if(months==1 || months==3 || months==5 || months==7 || months==8 || months==10 || months==12)
    return 31;
  else if(months==4 || months==6 || months==9 || months==11)
    return 30;
  else
    if(Time().isLeapYear())
      return 29;
    else
      return 28;
}

Bool Time::isLeapYear() {

  uInt lyear=Time().year();
  if( lyear%100 == 0)
    if( lyear%400 == 0)
      return True;
    else
      return False;
  else
    if( lyear%4 == 0)
      return True;
    else
      return False;
}

Bool Time::isLeapYear(uInt lyear) {

  if( lyear%100 == 0)
    if( lyear%400 == 0)
      return True;
    else
      return False;
  else
    if( lyear%4 == 0)
      return True;
    else
      return False;
}
