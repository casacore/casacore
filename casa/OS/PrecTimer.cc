//# PrecTimer.cc: Precision timer to measure elapsed times in a cumulative way
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

#include <casacore/casa/OS/PrecTimer.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/casa/iostream.h>
#include <casacore/casa/fstream.h>
#include <casacore/casa/iomanip.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Exceptions/Error.h>
#include <cstdlib>
#include <cstring>

#if defined __APPLE__
#include <sys/sysctl.h>
#include <sys/types.h>
#endif

namespace casacore { //# NAMESPACE CASACORE - BEGIN


double PrecTimer::CPU_speed_in_MHz = PrecTimer::get_CPU_speed_in_MHz();


double PrecTimer::get_CPU_speed_in_MHz()
{
  // first a few sanity checks
  AlwaysAssert(sizeof(int) == 4, AipsError);
  AlwaysAssert(sizeof(long long) == 8, AipsError);

#if (defined __linux__ || __APPLE__) &&                                 \
    (defined __i386__ || defined __x86_64__ || defined __ia64__ || defined __PPC__) && \
    (defined __GNUC__ || defined __INTEL_COMPILER || defined __PATHSCALE__ || defined __xlC__)
  ifstream infile("/proc/cpuinfo");
  char     buffer[256];

  while (infile.good()) {
    infile.getline(buffer, 256);

#if defined __PPC__
    char* colon;
    if (strcmp("cpu\t\t: 450 Blue Gene/P DD2", buffer) == 0) {
      return 850.0;
    }
    if (strcmp("machine\t\t: Blue Gene", buffer) == 0) {
      return 700.0;
    }
    if (strncmp("timebase", buffer, 8) == 0  &&
        (colon = strchr(buffer, ':')) != 0) {
      return atof(colon + 2) / 1e6;
    }
#elif defined __APPLE__ // Macintosh
    int mib[2] = { CTL_HW, HW_CPU_FREQ };
    double result = 0;
    size_t size = sizeof(result);
    if( sysctl(mib, 2, &result, &size, NULL, 0) != -1) {
      return result / 1e6;
    }
 #else
    char* colon;
    if (strncmp("cpu MHz", buffer, 7) == 0  &&
        (colon = strchr(buffer, ':')) != 0) {
      return atof(colon + 2);
    }
#endif
  }

  return 0.0;
#else
#warning partially supported architecture
  return 0.0;
#endif
}


double PrecTimer::getReal() const
{
  double time = u1.total_time / 1e6;
  if (CPU_speed_in_MHz > 0) {
    time /= CPU_speed_in_MHz;
  }
  return time;
}

void PrecTimer::show() const
{
  show (cout);
}

void PrecTimer::show (ostream& os) const
{
  if (u2.count == 0) {
    os << "not used\n";
  } else {
    double total = static_cast<double>(u1.total_time);
    if (CPU_speed_in_MHz == 0) {
      os << "avg = " << total / static_cast<double>(u2.count);
      os << ", total = " << u1.total_time << " cycles";
    } else {
      total /= 1e6 * CPU_speed_in_MHz;
      os << "avg = ";
      print_time(os, total / static_cast<double>(u2.count));
      os << ", total = ";
      print_time(os, total);
    }
    os << ", count = " << setw(9) << u2.count << endl;
  }
}

void PrecTimer::show (const String& s) const
{
  show (cout, s);
}

void PrecTimer::show (ostream &os, const String& s) const
{
  os << s << ":  ";
  show (os);
}

void PrecTimer::print_time (ostream& os, double time) const
{
  // Print the time in a suitable unit.
  static char units[] = { 'n', 'u', 'm', ' ', 'k' };
  time = 1000.0 * time / CPU_speed_in_MHz;
  int i=0;
  while (time >= 999.5 && i<5) {
    time /= 1000.0;
    ++i;
  }
  os << setprecision(3) << setw(5) << time << ' ' << units[i] << 's';
}

} //# NAMESPACE CASACORE - END

