//# Time.h: enquiry functions for calendar and clock time, with some operations
//# Copyright (C) 1994,1995,1999,2000,2001
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

#ifndef CASA_TIME_H
#define CASA_TIME_H

#include <casacore/casa/aips.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/casa/iosfwd.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

// <Summary> date and time enquiry functions, with some operations.
// </summary>
//
// <use visibility=export>
//
// <reviewed reviewer="Paul Shannon" date="1995/03/01" tests="tTime" demos="">
// This class might be better named a Date object, especially given that
// more accurate Time classes are going to be required.
// </reviewed>

// <prerequisite>
//   <li> you should understand the difference between "Julian" and 
//        "modified Julian" date
// </prerequisite>

// <synopsis>
//  This class provides convenient date objects for the programmer.
//  Once constructed, they may be compared, read and written, and
//  queried for a wide variety of re-expressions.  In a typical (?) use
//  you might create a Time object, and then query it to find out
//  the current month, day of the week, and whether it is a leap
//  year.  You can also find out the number of seconds which have elapsed
//  since a specific Time.
//
// <note role=caution> This class should not be used for very high precision
//                     work. The time from epoch (1970.0) in seconds is
//                     interconverted between computer "double" values, and
//                     some loss of accuracy might result.
// </note>
// </synopsis>

// <example>
// <srcblock>
//  Time startTime;
//  Time moonLanding (1969,7,14);
//  cout << "date and time of moon landing: " << moonLanding << endl;
//  cout << "day of week: " << moonLanding.dayOfWeek () << endl;
//  cout << "day of year: " << moonLanding.dayOfYear () << endl;
//  cout << "seconds since moon landing: " << moonLanding.age () << endl;
//  cout << "weeks since moon landing: " <<
//     moonLanding.age () / (60 * 60 * 24 * 7) << endl;
//  cout << "seconds elapsed since start: " << startTime.age () << endl;
// </srcblock>
// </example>
//

// <todo asof="1995/03/23">
//   <li> member function 'age' might be renamed 'elapsedTime'
//   <li> A reference to the source of each algorithm should be provided.
// </todo>
class Time {

 public:
     // the default constructor returns an object with the present date and time
   Time ();
     // Construct time with Julian day number
   Time (double jdn);
     // Construct Time with Gregorian calendar
     // <ul>
     //   <li> seconds after the minute [0,59.999] (include milliseconds)
     //   <li> minutes after the hour [0,59]
     //   <li> hours after midnight [0,23]
     //   <li> day of the month [1,31]
     //   <li> month of the year [1,12]
     //  <li>  year. Beware, because '94' refers to the early Christian era, not
     // the 20th century.
     // </ul>
   Time (uInt year, uInt month, uInt day, uInt hour=0, uInt min=0, 
         double sec=0.0);

     // Copy constructor
   Time (const Time& time);

     // return the Julian day
   double julianDay () const;
     // return the modified Julian day
   double modifiedJulianDay () const;

     // initialise the julian day data with Time class
   Time& operator = (const Time& time);

   double operator - (const Time& begin);
   Time operator + (const double plus);

   Bool operator == (const Time& other) const;
   Bool operator != (const Time& other) const;
   Bool operator > (const Time& other) const;
   Bool operator < (const Time& other) const;

    // if iso is True, then use ISO 8601 format
    // otherwise, produce the string of the form
    // Tue Mar 22 16:40:24 1994
    // with GMT time
  String toString(const Bool iso=False) const;

    // returns a String in ISO 8601 format YYYY-MM-DDTHH:MM:SS in GMT
    // note: for dates beyond year 9999, use more digits for year
  const String ISODate() const
    { return toString(True); }

    // write the current time, GMT, in format
    //        Tue Mar 22 16:40:24 1994
  friend ostream& operator<<(ostream& out, const Time& other)
  {
    out << other.toString(False);
    return out;
  }

     // read in date, which must be in the following format
     //     month/day/year,hour:min:sec
     // where month,day,year,hour,min and sec are uInt.
   friend istream& operator >> (istream&, Time&);

     // reset date to the present instant
   void now ();
   void setDate  (uInt year, uInt month, uInt day, uInt hour=0, uInt min=0, 
                double sec=0.0);

     // number of seconds which have elapsed since Time object was created
     // or reset
   double age ();

   // Return the seconds, minutes or hour part of the time.
   // <group>
   uInt seconds ();
   double dseconds ();
   uInt minutes ();
   uInt hours ();
   // </group>

   uInt dayOfMonth ();
   uInt month ();

   uInt year ();

   uInt dayOfWeek ();


   uInt dayOfYear ();

   static uInt howManyDaysInMonth ();

   static uInt howManyDaysInMonth (uInt month,uInt year);

   static Bool isLeapYear ();

   static Bool isLeapYear (uInt year);

     // Returns the difference, in seconds, between UTC and local time.
     // Negative values are west of GMT, positive are east.
   static Int timeZoneSeconds ();
     // Same as timeZoneSeconds(), but returns fractional days rather
     // than seconds.
   static Double timeZoneDays ();
     // Returns a string, e.g. "EST" or "MDT", describing the current
     // local time zone.
   static String timeZoneName ();

 protected:

     // Modified Julian day number
     // 40587 modified Julian day number = 00:00:00 January 1, 1970, GMT.
   uInt mJulianDay;
     // the fraction of the day
   double mJulianDayfrac;

};


} //# NAMESPACE CASACORE - END

#endif
//# roel's original comments -- these may be useful in creating demo
//# programs when we get some time...

//# The function now () updated datas with the present time.
//
//# When create a object. The mJulianDay and  mJulianDayfrac datas are
//# initialise with actual modified Julian day. (<now() function).
//# The default constructor is at preset time.
//#
//# i.e. 40587 modified Julian day number = 00:00:00 January 1, 1970, GMT.
//# and 2440587.5 Julian day number = 00:00:00 January 1, 1970, GMT,
//# then modified Julian day number = Julian day number - 2400000.5
//#
//# Important :We are consindered GMT time for all functions.
//# We are considered only dates after 2400000 Julian day = 12:00:00
//# November 15, 1858, GMT.
//#
//# When execute the now() function the actual mJulianDay and 
//# mJulianDayFrac datas are replace for the new modified Julian day
//# and the fraction of the day.
//#
//# The function is invoked looks as follows
//#
//# <code>
//#
//# Time t;   // The default constructor is at present time (now())
//#
//# t.now();
//#
//# </code>
//#
//# When execute the setDate() function the actual mJulianDay and 
//# mJulianDayFrac datas are replace for the new date
//#
//# The function is invoked looks as follows
//#
//# <code>
//#
//# Time t;   // The default constructor is at present time (now())
//#
//# t.setDate(1915,2,21);
//#
//# </code>
//#
//# The function age() return the time in seconds between
//# some Time object and now.
//#
//# The function is invoked looks as follows
//#
//# <code>
//#
//# Time t;   // The default constructor is at present time (now())
//#
//# cout<<"time since x "<< t.age() <<"\n";
//#
//# </code>
//#
//# The function julianDay() return the julian day number.
//#
//# The function is invoked looks as follows
//#
//# <code>
//#
//# Time t;
//#
//# cout<<"Julian day number "<<t.julianDay()<<"\n";
//#
//# </code>
//#
//# The function modifiedJulianDay() return the modified julian day number.
//#
//# The function is invoked looks as follows
//#
//# <code>
//#
//# Time t;
//#
//# cout<<"Modified Julian day number "<<t.modifiedJulianDay()<<"\n";
//#
//# </code>
//#
//# The function dayOfMonth() return day of the month [1,31]
//# Note: This function doesn't modified the actual datas (mJulianDay and  '
//# mJulianDayFrac).
//#
//# The function is invoked looks as follows
//#
//# <code>
//#
//# Time t;
//#
//#
//# cout<<"day of month "<< t.dayOfMonth() <<"\n";
//#
//# </code>
//#
//# The function month() return month of the year [1,12]
//# Note: This function doesn't modified the actual datas (mJulianDay and   '
//# mJulianDayFrac).
//#
//# The function is invoked looks as follows
//#
//# <code>
//#
//# Time t;
//#
//#
//# cout<<"month "<< t.month() <<"\n";
//#
//# </code>
//#
//# The function year() return the year.
//# Note: This function doesn't modified the actual datas (mJulianDay and   '
//# mJulianDayFrac).
//#
//# The function is invoked looks as follows
//#
//# <code>
//#
//# Time t;
//#
//#
//# cout<<"Year "<< t.year() <<"\n";
//#
//# </code>
//#
//# The function dayOfWeek() return days since sunday [1,7].
//# Note: This function doesn't modified the actual datas (mJulianDay and   '
//# mJulianDayFrac).
//#
//# The function is invoked looks as follows
//#
//# <code>
//#
//# Time t;
//#
//#
//# cout<<"day of week "<< t.dayOfWeek() <<"\n";
//#
//# </code>
//#
//# The function dayOfYear() return day  of the year [1,366]
//# Note: This function doesn't modified the actual datas (mJulianDay and   '
//# mJulianDayFrac).
//#
//# The function is invoked looks as follows
//#
//# <code>
//#
//# Time t;
//#
//#
//# cout<<"day of year "<< t.dayOfYear() <<"\n";
//#
//# </code>
//#
//# The function leapSeconds() return leap seconds.
//# We have the next datas
//#
//# -Note: leapSeconds() removed 1997.10.07 by Jeff Uphoff, after
//# -recommendation by Wim Brouw.
//#
//#  leapsec=10;
//#
//#  if(modified Julian Day>=41499) leapsec++;    // 1 July      1972
//#  if(modified Julian Day>=41683) leapsec++;    // 1 January   1973
//#  if(modified Julian Day>=42048) leapsec++;    // 1 January   1974
//#  if(modified Julian Day>=42413) leapsec++;    // 1 January   1975
//#  if(modified Julian Day>=42778) leapsec++;    // 1 January   1976
//#  if(modified Julian Day>=43144) leapsec++;    // 1 January   1977
//#  if(modified Julian Day>=43509) leapsec++;    // 1 January   1978
//#  if(modified Julian Day>=43874) leapsec++;    // 1 January   1979
//#  if(modified Julian Day>=44239) leapsec++;    // 1 January   1980
//#  if(modified Julian Day>=44786) leapsec++;    // 1 July      1981
//#  if(modified Julian Day>=45151) leapsec++;    // 1 July      1982
//#  if(modified Julian Day>=45516) leapsec++;    // 1 July      1983
//#  if(modified Julian Day>=46247) leapsec++;    // 1 July      1985
//#  if(modified Julian Day>=47161) leapsec++;    // 1 January   1988
//#  if(modified Julian Day>=47892) leapsec++;    // 1 January   1990
//#  if(modified Julian Day>=48257) leapsec++;    // 1 January   1991
//#  if(modified Julian Day>=48804) leapsec++;    // 1 July      1992
//#  if(modified Julian Day>=49169) leapsec++;    // 1 July      1993
//#
//# The function is invoked looks as follows
//#
//# <code>
//#
//# Time t;
//#
//# cout<<"Leap seconds "<< t.leapSeconds() <<"\n";
//#
//# </code>
//#
//# The function howManyDaysInMonth() return how many days are in a month.
//#
//# The function is invoked looks as follows
//#
//# <code>
//#
//# Time t;
//# uInt month=1,month2=2,year=1992;
//#
//# cout<<"how many days are in this month "<< howManyDaysInMonth() <<"\n"
//# cout<<"how many days are in January "<< howManyDaysInMonth(month) <<"\n";
//# cout<<"how many days are in february of 1992 "<< 
//#        howManyDaysInMonth(month,year) <<"\n"; // 1992 is a leap year
//#
//# </code>
//#
//# The function isLeapYear() return bool value. True if is a leap year
//# and False in other case.
//#
//# The function is invoked looks as follows
//#
//# <code>
//#
//# Time t;
//#
//# uInt year=1992;
//#
//# if(isLeapYear(year))
//#   cout<<"Is a leap year";
//#
//# if(isLeapYear())
//#   cout<<"This year is a leap year";
//#
//# </code>
