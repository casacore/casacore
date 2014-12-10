//# PrecTimer.h: Precision timer to measure elapsed times in a cumulative way
//# Copyright (C) 2006
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

#ifndef CASA_PRECTIMER_H
#define CASA_PRECTIMER_H


#include <casacore/casa/aips.h>
#include <cstdlib>
#include <iostream>

#if defined __ia64__ && defined __INTEL_COMPILER
#include <ia64regs.h>
#endif


namespace casacore { //# NAMESPACE CASACORE - BEGIN

// Forward Declaration.
class String;


// <summary>
// Precision timer to measure elapsed times in a cumulative way
// </summary>

// <use visibility=export>

// <reviewed reviewer="" date="" tests="tPrecTimer" demos="">
// </reviewed>

// <synopsis>
// The PrecTimer supplements the <linkto class=Timer>Timer</linkto> class.
// If offers a low-overhead and high-resolution interval timer for use
// on i386, x86_64, ia64, and powerpc platforms, using the processor's
// timestamp counter that is incremented each cycle.
// Put timer.start() and timer.stop() calls around the piece of
// code to be timed. Because the timer is cumulative, the total time of
// a particular piece of code can be timed.
// <note role=caution>
// Make sure that start() and stop() calls alternate,
// otherwise very strange times will be the result.
// </note>
//
// A timer can be started and stopped multiple times; both the average and
// total time, as well as the number of iterations are printed.
// The measured time is real time (as opposed to user or system time).
// The timer can be used to measure from 10 nanosecond to a century interval.
//
// Multiple timers can be used in a nested way as long as each of them
// has independent (matching) start and stop calls.
//
// The class is more or less a copy of the original written by John Romein
// at ASTRON, Dwingeloo, the Netherlands.
// </synopsis>

// <example>
// Here's how to create a timer, start it (the 'mark' member function)
// and display a breakdown.
// <srcblock>
//  PrecTimer ttimer;   // the timer is reset at construction time
//  PrecTimer ctimer;
//  ttimer.reset();     // if you want to reset the timer (not needed here)
//  ttimer.start();     // start the total timer
//  for (int i=0; i<n; ++i) {
//    ... do something ...
//    ctimer.start();   // start the calc timer
//    ...do some calculation which will be timed...
//    ctimer.stop();    // and stop it
//  }
//  ttimer.stop();
//  ttimer.show (cout, "Total       ");
//  ctimer.show (cout, "Calculations");
// </srcblock>
// </example>

  class PrecTimer {
  public:
    // Construct.
    PrecTimer();

    // Destruct.
    ~PrecTimer();

    // Restart the timer.
    void start();
    // Stop the timer
    void stop();

    // Reset the timer to zero.
    void reset();

    // Show real time on cout or a user supplied stream.
    // <group>
    void show() const;
    void show (std::ostream& os) const;
    // </group>

    // Show real time on cout or a user supplied
    // stream preceeded by the string parameter.
    // <group>
    void show (const String&) const;
    void show (std::ostream& os, const String& prefix) const;
    // </group>

    // Get the real time (in seconds).
    double getReal() const;

    // Get the total number of times start/stop is done.
    unsigned long long getCount() const;

  private:
    void print_time (std::ostream&, double time) const;

    struct TimeStruct {
#if defined __PPC__
      int	   total_time_high, total_time_low;
#else
      int	   total_time_low, total_time_high;
#endif
    };
    union Union1 {
      long long	   total_time;
      TimeStruct   s1;
    };

#if defined __i386__ && defined __INTEL_COMPILER && defined _OPENMP
    struct CountStruct {
      int count_low, count_high;
    };
    union Union2 {
      unsigned long long count;
      CountStruct        s2;
    };
#else
    struct Union2 {
      unsigned long long count;
    };
#endif

    Union1 u1;
    Union2 u2;

    static double CPU_speed_in_MHz;
    static double get_CPU_speed_in_MHz();
  };



  inline void PrecTimer::reset()
  {
    u1.total_time = 0;
    u2.count      = 0;
  }

  inline unsigned long long PrecTimer::getCount() const
  {
    return u2.count;
  }

  inline PrecTimer::PrecTimer()
  {
    reset();
  }

  inline PrecTimer::~PrecTimer()
  {}


  inline void PrecTimer::start()
  {
#if defined __x86_64__ && defined __INTEL_COMPILER && defined _OPENMP
    asm volatile
    (
	"rdtsc\n\t"
	"shlq $32,%%rdx\n\t"
	"leaq (%%rax,%%rdx),%%rax\n\t"
	"lock;subq %%rax,%0"
    :
	"+m" (u1.total_time)
    :
    :
	"rax", "rdx"
    );
#elif defined __i386__ && defined __INTEL_COMPILER && defined _OPENMP
    asm volatile
    (
	"rdtsc\n\t"
	"lock;subl %%eax,%0\n\t"
	"lock;sbbl %%edx,%1"
    :
	"+m" (u1.s1.total_time_low), "+m" (u1.s1total_time_high)
    :
    :
	"eax", "edx"
    );
#elif (defined __i386__ || defined __x86_64__) && (defined __PATHSCALE__ || (defined __APPLE__ && defined __APPLE_CC__ && __APPLE_CC__ == 5531))
    unsigned eax, edx;

    asm volatile ("rdtsc" : "=a" (eax), "=d" (edx));

    u1.total_time -= ((unsigned long long) edx << 32) + eax;
#elif (defined __i386__ || defined __x86_64__) && (defined __GNUC__ || defined __INTEL_COMPILER)
    asm volatile
    (
	"rdtsc\n\t"
	"subl %%eax, %0\n\t"
	"sbbl %%edx, %1"
    :
	"+m" (u1.s1.total_time_low), "+m" (u1.s1.total_time_high)
    :
    :
	"eax", "edx"
    );
#elif defined __ia64__ && defined __INTEL_COMPILER
    u1.total_time -= __getReg(_IA64_REG_AR_ITC);
#elif defined __ia64__ && defined __GNUC__
    long long time;
    asm volatile ("mov %0=ar.itc" : "=r" (time));
    u1.total_time -= time;
#elif defined __PPC__ && (defined __GNUC__ || defined __xlC__)
    int high, low, retry;

    asm
    (
	"0:\n\t"
	"mftbu %0\n\t"
	"mftb %1\n\t"
	"mftbu %2\n\t"
	"cmpw %2,%0\n\t"
	"bne 0b\n\t"
	"subfc %3,%1,%3\n\t"
	"subfe %4,%0,%4"
    :
	"=r" (high), "=r" (low), "=r" (retry),
	"=r" (u1.s1.total_time_low), "=r" (u1.s1.total_time_high)
    :
	"3" (u1.s1.total_time_low), "4" (u1.s1.total_time_high)
    );
#endif
  }


  inline void PrecTimer::stop()
  {
#if defined __x86_64__ && defined __INTEL_COMPILER && defined _OPENMP
    asm volatile
    (
	"rdtsc\n\t"
	"shlq $32,%%rdx\n\t"
	"leaq (%%rax,%%rdx),%%rax\n\t"
	"lock;addq %%rax,%0"
    :
	"+m" (u1.total_time)
    :
    :
	"rax", "rdx"
    );
#elif defined __i386__ && defined __INTEL_COMPILER && defined _OPENMP
    asm volatile
    (
	"rdtsc\n\t"
	"lock;addl %%eax, %0\n\t"
	"lock;adcl %%edx, %1"
    :
	"+m" (u1.s1.total_time_low), "+m" (u1.s1.total_time_high)
    :
    :
	"eax", "edx"
    );
#elif (defined __i386__ || defined __x86_64__) && (defined __PATHSCALE__ || (defined __APPLE__ && defined __APPLE_CC__ && __APPLE_CC__ == 5531))
    unsigned eax, edx;

    asm volatile ("rdtsc\n\t" : "=a" (eax), "=d" (edx));
    u1.total_time += ((unsigned long long) edx << 32) + eax;
#elif (defined __i386__ || defined __x86_64__) && (defined __GNUC__ || defined __INTEL_COMPILER)
    asm volatile
    (
	"rdtsc\n\t"
	"addl %%eax, %0\n\t"
	"adcl %%edx, %1"
    :
	"+m" (u1.s1.total_time_low), "+m" (u1.s1.total_time_high)
    :
    :
	"eax", "edx"
    );
#elif defined __ia64__ && defined __INTEL_COMPILER
    u1.total_time += __getReg(_IA64_REG_AR_ITC);
#elif defined __ia64__ && defined __GNUC__
    long long time;
    asm volatile ("mov %0=ar.itc" : "=r" (time));
    u1.total_time += time;
#elif defined __PPC__ && (defined __GNUC__ || defined __xlC__)
    int high, low, retry;

    asm
    (
	"0:\n\t"
	"mftbu %0\n\t"
	"mftb %1\n\t"
	"mftbu %2\n\t"
	"cmpw %2,%0\n\t"
	"bne 0b\n\t"
	"addc %3,%3,%1\n\t"
	"adde %4,%4,%0"
    :
	"=r" (high), "=r" (low), "=r" (retry),
	"=r" (u1.s1.total_time_low), "=r" (u1.s1.total_time_high)
    :
	"3" (u1.s1.total_time_low), "4" (u1.s1.total_time_high)
    );
#endif

#if defined __x86_64__ && defined __INTEL_COMPILER && defined _OPENMP
    asm volatile ("lock;addq $1,%0" : "+m" (u2.count));
#elif defined __i386__ && defined __INTEL_COMPILER && defined _OPENMP
    asm volatile
    (
	"lock;addl $1,%0\n\t"
	"lock;adcl $0,%1"
    :
	"+m" (u2.s2.count_low), "+m" (u2.s2.count_high)
    );
#else
    ++u2.count;
#endif
  }

} //# NAMESPACE CASACORE - END


#endif
