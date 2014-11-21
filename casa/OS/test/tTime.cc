//# tTime.cc: a test program for the Time class
//# Copyright (C) 1993,1994,1995,1999,2001
//# Associated Universities, Inc. Washington DC, USA.
//#
//# This program is free software; you can redistribute it and/or modify
//# it under the terms of the GNU General Public License as published by
//# the Free Software Foundation; either version 2 of the License, or
//# (at your option) any later version.
//#
//# This program is distributed in the hope that it will be useful,
//# but WITHOUT ANY WARRANTY; without even the implied warranty of
//# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//# GNU General Public License for more details.
//#
//# You should have received a copy of the GNU General Public License
//# along with this program; if not, write to the Free Software
//# Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
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
#include <casacore/casa/iostream.h>
#include <casacore/casa/stdlib.h>
#include <casacore/casa/math.h>

#include <casacore/casa/namespace.h>
int main() {

  Time t;
  cout<<"now "<<t.dayOfMonth()<<" "<<t.month()<<" "<<t.year()<<"\n";
  cout<<"Julian day "<<t.julianDay() - 2400000 <<"\n";
  cout<<"modify Julian day "<<t.modifiedJulianDay()<<"\n";
  cout<<"day of week "<< t.dayOfWeek()<<"\n";
  cout<<"day of year "<< t.dayOfYear()<<"\n";
  cout<<"Age "<< t.age()<<"\n";
  //  cout<<"leap seconds "<< t.leapSeconds() <<"\n";
  cout<< t <<"\n";
  Time t1(t),t2,t3(2440587.0),t4(1970,1,1),t5,t6(2449480.5);
  t2=t1;
  t5=2440588.6;

  cout<<"t1 Age "<< t1.age()<<"\n";
  cout<< t1 <<"\n";
  cout<< t2 <<"\n";
  cout<<"modify Julian day "<<t3.modifiedJulianDay()<<"\n";
  cout<<"t3 Age "<< t3.age() <<"\n";
  cout<< t3 <<"\n";
  cout<<"modify Julian day "<<t4.modifiedJulianDay()<<"\n";
  cout<<"t4-t3 "<<t4-t3<<"\n";
  cout<<"t4 Age "<< t4.age() <<"\n";
  cout<< t4 <<"\n";
  cout<<"t5 Age "<< t5.age() <<"\n";
  cout<<"modify Julian day "<<t5.modifiedJulianDay()<<"\n";
  cout<< t5 <<"\n";
  Time t7;
  t7=t5+86401.234;
  cout<<t7<<"\n";

  cout<<"t6 Age "<< t6.age() <<"\n";
  cout<<"modify Julian day "<<t6.modifiedJulianDay()<<"\n";
  cout<< t6 <<"\n";

//  Time t8;
//  cout<<"date \n";
//  cin>>t8;
//  cout<<"modify Julian day "<<t8.modifiedJulianDay()<<"\n";
//  cout<< t8 <<"\n";

  Time t9;
  cout<< t9 <<"\n";
  cout<<t9.dayOfMonth()<<" "<<t9.month()<<" "<<t9.year()<<" "<<t9.hours()<<" "<<t9.minutes()<<" "<<t9.seconds()<<"\n";
  uInt m=2,y=1992;
  cout<<"how many days this month "<<t9.howManyDaysInMonth()<<"\n";
  cout<<"how many days february of 1992 "<<t9.howManyDaysInMonth(m,y)<<"\n";

  if(t9.isLeapYear(y))
    cout<<"1992 leap year \n";
  else
    cout<<"1992 no is leap year\n";
  if(t9.isLeapYear())
    cout<<"leap year \n";
  else
    cout<<"no is leap year\n";

  if(t>t3)
    cout<<"t greater t3 \n";
  else
    cout<<"less or equal\n";
  if(t5<t6)
    cout<<"t5 less t6\n";
  else
    cout<<"greater or equal\n";
  if(t1==t2)
    cout<<"t1 equal t2\n";
  else
   cout<<"Not equal\n";
  if(t9!=t4)
    cout<<"t9 Not equal t4\n";
  else
    cout<<"equal\n";

  if(t>t2)
    cout<<"t greater t2\n";
  else
    cout<<"less or equal\n";
  if(t9>t)
    cout<<"t9 greater t\n";
  else
    cout<<"less or equal\n";

  t9.setDate(1972,7,1);
  cout<< t9 <<"\n";
  Time currentTime;
  cout << "currentTime: " << currentTime << endl;
  // Ummm...I thought the moon landing was on July 20th. <grin> --Jeff U.
  Time moonLanding (1969,7,14);
  cout << "moonLanding: " << moonLanding << endl;
  cout << "elapsed time as double: " << (currentTime - moonLanding) << endl;
  cout << "current time jd:  " << currentTime.julianDay () << endl;
  cout << "current time mjd: " << currentTime.modifiedJulianDay () << endl;
  cout << "moon landing jd:  " << moonLanding.julianDay () << endl;
  cout << "moon landing mjd: " << moonLanding.modifiedJulianDay () << endl;
  cout << "since moon landing ---------" << endl;
  cout << "age ():  " << currentTime.age () << endl;
  cout << "hours:   " << currentTime.hours () << endl;
  cout << "minutes: " << currentTime.minutes () << endl;
  cout << "seconds:" << currentTime.seconds () << endl;
  {
    Time startTime;
  Time moonLanding (1969,7,14);
  cout << "date and time of moon landing: " << moonLanding << endl;
  cout << "day of week: " << moonLanding.dayOfWeek () << endl;
  cout << "day of year: " << moonLanding.dayOfYear () << endl;
  cout << "seconds since moon landing: " << moonLanding.age () << endl;
  cout << "weeks since moon landing: " <<
    moonLanding.age () / (60 * 60 * 24 * 7) << endl;
    cout << "seconds elapsed since start: " << startTime.age () << endl;
  }

  Int seconds = t.timeZoneSeconds ();
  cout << "time zone: " << abs (seconds) << " seconds ";
  cout << (seconds < 0 ? "west" : "east");
  cout << " of UTC" << endl;

  Double days = t.timeZoneDays ();
  cout << "time zone: " << fabs (days) << " days ";
  cout << (days < 0 ? "west" : "east");
  cout << " of UTC" << endl;

  cout << "time zone name: " << t.timeZoneName () << endl;
  cout<<"\n end test\n";
}
