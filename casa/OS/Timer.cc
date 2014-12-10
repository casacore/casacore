//# Timer.cc: Timing facility
//# Copyright (C) 1993,1994,1995,1996,1999,2001
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

//  User time:
//    time cpu spends in user mode on behalf of the program.
//  System time:
//    time cpu spends in system mode on behalf of the program.
//  Real time:
//    what you get from a stop watch timer.

#include <casacore/casa/OS/Timer.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/casa/iostream.h>
#include <casacore/casa/iomanip.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

  void Timer::mark()
  {
#if defined (DOS) || defined (MSDOS)
    usage0 = clock();
    ftime(&real0);
#elif defined (AIPS_SOLARIS) || defined(AIPS_IRIX) || defined(AIPS_OSF) || defined(__hpux__) || defined(AIPS_LINUX) || defined(AIPS_DARWIN) || defined(AIPS_BSD)
# ifdef AIPS_CRAY_PGI
    struct timezone tz;
    getrusage(0, &usage0);
    timerclear(&real0);
    gettimeofday(&real0, &tz);
# else
    real0 = times (&usage0);
# endif
#else
    getrusage(0, &usage0);
    ftime(&real0);
#endif
  }

  double Timer::real() const
  {
#if defined (DOS) || defined (MSDOS)
    double s, ms;
    timeb  real1;        // current elapsed real time
    int err;
    long ls;

    err = ftime(&real1);
    ls = real1.time - real0.time;
    s = ls;
    ls = real1.millitm - real0.millitm;
    ms = ls;
    ms = ms * 0.001 + s;
    return (ms);
 
#elif defined (AIPS_SOLARIS) || defined(AIPS_IRIX) || defined(AIPS_OSF) || defined(__hpux__) || defined(AIPS_LINUX) || defined(AIPS_DARWIN) || defined(AIPS_BSD)
# ifdef AIPS_CRAY_PGI
    struct timeval now;
    struct timezone tz;
    timerclear(&now);
    gettimeofday(&now, &tz);
    return ((double) (now.tv_sec - real0.tv_sec));
# else
    clock_t real1;       // current time
    tms usage1;          // current tms structure
    real1 = times (&usage1);
    return ((double) (real1 - real0)) / ((double) sysconf(_SC_CLK_TCK));
# endif
#else
    double s, ms;
    timeb  real1;        // current elapsed real time
    int err;
    long ls;

    err = ftime(&real1);
    ls = real1.time - real0.time;
    s = ls;
    ls = real1.millitm - real0.millitm;
    ms = ls;
    ms = ms * 0.001 + s;
    return (ms);

#endif
  }

  double Timer::user() const
  {
#if defined (DOS) || defined (MSDOS)
    register clock_t  usage1;
    if ((usage1 = clock()) != (clock_t) -1) {
      return (usage1 - usage0);
    }
    // error: Processor time not available
    return (0.0);
 
#elif defined (AIPS_SOLARIS) || defined(AIPS_IRIX) || defined(AIPS_OSF) || defined(__hpux__) || defined(AIPS_LINUX) || defined(AIPS_DARWIN) || defined(AIPS_BSD)
# ifdef AIPS_CRAY_PGI
    double dsec, dusec;
    rusage usage1;       // current rusage structure
    getrusage(0, &usage1);
    dsec  = (double)usage1.ru_utime.tv_sec  - (double)usage0.ru_utime.tv_sec;
    dusec = (double)usage1.ru_utime.tv_usec - (double)usage0.ru_utime.tv_usec;
    return(dsec + dusec * 0.000001);
# else
    tms usage1;          // current tms structure
    times (&usage1);
    return ((double) (usage1.tms_utime - usage0.tms_utime))
      / ((double) sysconf(_SC_CLK_TCK));
# endif
#else
    double dsec, dusec;
    rusage usage1;       // current rusage structure
    getrusage(0, &usage1);
    dsec  = (double)usage1.ru_utime.tv_sec  - (double)usage0.ru_utime.tv_sec;
    dusec = (double)usage1.ru_utime.tv_usec - (double)usage0.ru_utime.tv_usec;
    return(dsec + dusec * 0.000001);

#endif
  }

  double Timer::system() const
  {
#if defined (DOS) || defined (MSDOS)
    return(0L);

#elif defined (AIPS_SOLARIS) || defined(AIPS_IRIX) || defined(AIPS_OSF) || defined(__hpux__) || defined(AIPS_LINUX) || defined(AIPS_DARWIN) || defined(AIPS_BSD)
# ifdef AIPS_CRAY_PGI
    double dsec, dusec;
    rusage usage1;       // current rusage structure
    getrusage(0, &usage1);
    dsec  = usage1.ru_stime.tv_sec  - usage0.ru_stime.tv_sec;
    dusec = usage1.ru_stime.tv_usec - usage0.ru_stime.tv_usec;
    return(dsec + dusec*0.000001);
# else
    tms usage1;          // current tms structure
    times (&usage1);
    return ((double) (usage1.tms_stime - usage0.tms_stime))
      / ((double) sysconf(_SC_CLK_TCK));
# endif
#else
    register double dsec, dusec;
    rusage usage1;       // current rusage structure
    getrusage(0, &usage1);
    dsec  = usage1.ru_stime.tv_sec  - usage0.ru_stime.tv_sec;
    dusec = usage1.ru_stime.tv_usec - usage0.ru_stime.tv_usec;
    return(dsec + dusec*0.000001);

#endif
  }

  double Timer::all() const
  {
#if defined (DOS) || defined (MSDOS)
    register clock_t  usage1;
     if ((usage1 = clock()) != (clock_t) -1) {
      return (usage1 - usage0);
    }
    // error: Processor time not available
    return (0.0);
 
#elif defined (AIPS_SOLARIS) || defined(AIPS_IRIX) || defined(AIPS_OSF) || defined(__hpux__) || defined(AIPS_LINUX) || defined(AIPS_DARWIN) || defined(AIPS_BSD)
# ifdef AIPS_CRAY_PGI
    double dsec, dusec;
    rusage usage1;       // current rusage structure
    getrusage(0, &usage1);
    dsec  = (usage1.ru_utime.tv_sec  - usage0.ru_utime.tv_sec)
      + (usage1.ru_stime.tv_sec  - usage0.ru_stime.tv_sec);
    dusec = (usage1.ru_utime.tv_usec - usage0.ru_utime.tv_usec)
      + (usage1.ru_stime.tv_usec - usage0.ru_stime.tv_usec);
    return(dsec + dusec*0.000001);
# else
    tms usage1;          // current tms structure
    times (&usage1);
    return ((double) (  (usage1.tms_utime - usage0.tms_utime)
                        + (usage1.tms_stime - usage0.tms_stime)  ))
      / ((double) sysconf(_SC_CLK_TCK));
# endif
#else
    register double dsec, dusec;
    rusage usage1;       // current rusage structure
    getrusage(0, &usage1);
    dsec  = (usage1.ru_utime.tv_sec  - usage0.ru_utime.tv_sec)
      + (usage1.ru_stime.tv_sec  - usage0.ru_stime.tv_sec);
    dusec = (usage1.ru_utime.tv_usec - usage0.ru_utime.tv_usec)
      + (usage1.ru_stime.tv_usec - usage0.ru_stime.tv_usec);
    return(dsec + dusec*0.000001);

#endif
  }

  double Timer::user_usec() const
  {
#if defined (DOS) || defined (MSDOS)
    register clock_t  usage1;
    if ((usage1 = clock()) != (clock_t) -1) {
      return (usage1 - usage0);
    }
    // error: Processor time not available
    return (0.0);
 
#elif defined (AIPS_SOLARIS) || defined(AIPS_IRIX) || defined(AIPS_OSF) || defined(__hpux__) || defined(AIPS_LINUX) || defined(AIPS_DARWIN) || defined(AIPS_BSD)
# ifdef AIPS_CRAY_PGI
    double dsec, dusec;
    rusage usage1;       // current rusage structure
    getrusage(0, &usage1);
    dsec  = usage1.ru_utime.tv_sec  - usage0.ru_utime.tv_sec;
    dusec = usage1.ru_utime.tv_usec - usage0.ru_utime.tv_usec;
    return(dsec*1000000.0 + dusec);
# else
    tms usage1;          // current tms structure
    times (&usage1);
    return 1000000.0 *
      ((double) (usage1.tms_utime - usage0.tms_utime))
      / ((double) sysconf(_SC_CLK_TCK));
# endif
#else
    register double dsec, dusec;
    rusage usage1;       // current rusage structure
    getrusage(0, &usage1);
    dsec  = usage1.ru_utime.tv_sec  - usage0.ru_utime.tv_sec;
    dusec = usage1.ru_utime.tv_usec - usage0.ru_utime.tv_usec;
    return(dsec*1000000.0 + dusec);

#endif
  }

  double Timer::system_usec() const
  {
#if defined (DOS) || defined (MSDOS)
    return(0.0);

#elif defined (AIPS_SOLARIS) || defined(AIPS_IRIX) || defined(AIPS_OSF) || defined(__hpux__) || defined(AIPS_LINUX) || defined(AIPS_DARWIN) || defined(AIPS_BSD)
# ifdef AIPS_CRAY_PGI
    double dsec, dusec;
    rusage usage1;       // current rusage structure
    getrusage(0, &usage1);
    dsec  = usage1.ru_stime.tv_sec  - usage0.ru_stime.tv_sec;
    dusec = usage1.ru_stime.tv_usec - usage0.ru_stime.tv_usec;
    return(dsec*1000000.0 + dusec);
# else
    tms usage1;          // current tms structure
    times (&usage1);
    return 1000000.0 *
      ((double) (usage1.tms_stime - usage0.tms_stime))
      / ((double) sysconf(_SC_CLK_TCK));
# endif
#else
    register double dsec, dusec;
    rusage usage1;       // current rusage structure
    getrusage(0, &usage1);
    dsec  = usage1.ru_stime.tv_sec  - usage0.ru_stime.tv_sec;
    dusec = usage1.ru_stime.tv_usec - usage0.ru_stime.tv_usec;
    return(dsec*1000000.0 + dusec);

#endif
  }

  double Timer::all_usec() const
  {
#if defined (DOS) || defined (MSDOS)
    register clock_t  usage1;
    if ((usage1 = clock()) != (clock_t) -1) {
      return (usage1 - usage0);
    }
    // error: Processor time not available
    return (0.0);
 
#elif defined (AIPS_SOLARIS) || defined(AIPS_IRIX) || defined(AIPS_OSF) || defined(__hpux__) || defined(AIPS_LINUX) || defined(AIPS_DARWIN) || defined(AIPS_BSD)
# ifdef AIPS_CRAY_PGI
    double dsec, dusec;
    rusage usage1;       // current rusage structure
    getrusage(0, &usage1);
    dsec  = (usage1.ru_utime.tv_sec  - usage0.ru_utime.tv_sec)
      + (usage1.ru_stime.tv_sec  - usage0.ru_stime.tv_sec);
    dusec = (usage1.ru_utime.tv_usec - usage0.ru_utime.tv_usec)
      + (usage1.ru_stime.tv_usec - usage0.ru_stime.tv_usec);
    return(dsec*1000000.0 + dusec);
# else
    tms usage1;          // current tms structure
    times (&usage1);
    return 1000000.0 *
      ((double) (  (usage1.tms_utime - usage0.tms_utime)
                   + (usage1.tms_stime - usage0.tms_stime)  ))
      / ((double) sysconf(_SC_CLK_TCK));
# endif
#else
    register double dsec, dusec;
    rusage usage1;       // current rusage structure
    getrusage(0, &usage1);
    dsec  = (usage1.ru_utime.tv_sec  - usage0.ru_utime.tv_sec)
      + (usage1.ru_stime.tv_sec  - usage0.ru_stime.tv_sec);
    dusec = (usage1.ru_utime.tv_usec - usage0.ru_utime.tv_usec)
      + (usage1.ru_stime.tv_usec - usage0.ru_stime.tv_usec);
    return(dsec*1000000.0 + dusec);

#endif
}


  void Timer::show() const
  {
    show(cout);
  }

  void Timer::show (const String& s) const
  {
    show(cout, s);
  }

  void Timer::show(ostream &os) const
  {
    os << setw(11) << real() << " real "
       << setw(11) << user() << " user "
       << setw(11) << system() << " system" << endl;
  }

  void Timer::show (ostream &os, const String& s) const
  {
    os << s;
    show(os);
  }

} //# NAMESPACE CASACORE - END
