//# Timer.h:  measure the time it takes to execute parts of a program
//# Copyright (C) 1993,1994,1995,1996,1997,1999,2000,2001
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

#ifndef CASA_TIMER_H
#define CASA_TIMER_H


#include <casacore/casa/aips.h>
#include <sys/types.h>

//# Forward declarations
#include <casacore/casa/iosfwd.h>

#if defined(DOS) || defined(MSDOS)
#include <sys/timeb.h>
extern "C" {
#include <time.h>
}

#elif defined(AIPS_SOLARIS) || defined(AIPS_IRIX) || defined(AIPS_OSF) || defined(__hpux__) || defined(AIPS_LINUX) || defined(AIPS_DARWIN) || defined(AIPS_BSD)
  #if defined(AIPS_CRAY_PGI)
    #include <sys/time.h>
    #include <sys/resource.h>
    #include <unistd.h>
    extern "C" int getrusage(int, struct rusage*);
  #else
    #include <sys/times.h>
    #include <unistd.h>
  #endif

#else
#include <sys/timeb.h>
#include <sys/time.h>
extern "C" int getrusage(int, struct rusage*);
extern "C" int ftime(struct timeb*);
#include <sys/resource.h>
#endif

namespace casacore { //# NAMESPACE CASACORE - BEGIN

// Class declaration.
class String;

// <summary> measure the time it takes to execute parts of a program</summary>

// <use visibility=export>

// <reviewed reviewer="Paul Shannon" date="1995/03/01/ tests="tTimer" demos="">
// </reviewed>

// <synopsis>
// The Timer class  provides an interface to system timing.  It
// allows a C++ program to record the time between a reference
// point  (mark) and  now.  This  class  uses the system time(2)
// interface to provide time resolution at either millisecond or
// microsecond granularity,  depending  upon  operating  system
// support and features. Since the time duration  is  stored  in
// a  32-bit word,  the  maximum  time  period  before rollover 
// occurs is about 71 minutes.
//
// Due to operating system dependencies, the  accuracy  of  all
// member  function results may not be as documented. For example
// some operating  systems  do  not  support  timers  with
// microsecond  resolution. In those cases, the values returned
// are provided to the nearest millisecond  or  other  unit  of
// time  as  appropriate. See the Timer header file for system-
// specific notes.
//
// <note role=tip> This Timer class is based on the TI COOL library 
//        Timer class
// </note>
// </synopsis>

// <example>
// Here's how to create a timer, start it (the 'mark' member function)
// and display a breakdown.  Recall that 
// <srcblock>        realtime = user time  + system time
// </srcblock>
// <srcblock>
// 
//  Timer timer;   // the mark is set at construction time
//  timer.mark();  // if you want to restart the clock
//   ...do some calculation...
//   cout << "user:   " << timer.user () << endl; 
//   cout << "system: " << timer.system () << endl;
//   cout << "real:   " << timer.real () << endl; 
//
// </srcblock>
//  </example>

// <todo asof="1995/03/01">
//   <li> it might be useful to have a stop () member function:  for
//        really precise timing, the successive calls to user, system
//        and real all happen at measurably different times
//   <li> provide an enquiry function that reports the resolution of
//        the timer
//   <li> add 'start' member function, a synonym for 'mark' but more
//        comprehensible
//        
// </todo>


class Timer {
public:
  //
  // Construct a timer and set the mark ("mark()").
  //
  Timer() {mark();}

  //
  // Set the timer mark -- i.e., start the clock ticking
  //
  void mark();

  //
  // Get the user time (in seconds) since last "mark()".
  //
  double user() const;

  //
  // Get the system time (in seconds) since last "mark()".
  //
  double system() const;

  //
  // Get the user+system time (in seconds) since last "mark()".
  //
  double all() const;

  //
  // Get the real time (in seconds) since last "mark()".
  //
  double real() const;

  // Show real, user, system time (in seconds) on cout or a user supplied
  // stream.
  // <group>
  void show() const;
  void show(ostream &os) const;
  // </group>

  // Show real, user, system time (in seconds) on cout or a user supplied
  // stream preceeded by the string parameter.
  // <group>
  void show(const String&) const;
  void show(ostream &os, const String&prefix) const;
  // </group>

  //
  // Get the user time (in microseconds) since last "mark()".
  //
  double user_usec() const;

  //
  // Get the system time (in microseconds) since last "mark()".
  //
  double system_usec() const;

  //
  // Get the user+system time (in microseconds) since last "mark()".
  //
  double all_usec() const;

private:
#if defined(DOS) || defined(MSDOS)
    clock_t usage0;
    timeb   real0;          //# elapsed real time at last mark
#elif defined(AIPS_SOLARIS) || defined(AIPS_IRIX) || defined(AIPS_OSF) || defined(__hpux__) || defined(AIPS_LINUX) || defined(AIPS_DARWIN) || defined(AIPS_BSD)
  #if defined(AIPS_CRAY_PGI)
    //struct timeval usage0;
    rusage usage0;          //# rusage structure at last mark
    struct timeval real0;   //# elapsed real time at last mark
  #else
    tms     usage0;         //# tms structure at last mark
    clock_t real0;          //# elapsed real time at last mark
  #endif
#else
    rusage  usage0;         //# rusage structure at last mark
    timeb   real0;          //# elapsed real time at last mark
#endif
};


} //# NAMESPACE CASACORE - END

#endif
