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

#include <casa/OS/PrecTimer.h>
#include <casa/BasicSL/String.h>
#include <casa/iostream.h>
#include <casa/fstream.h>
#include <casa/iomanip.h>
#include <casa/Utilities/Assert.h>
#include <casa/Exceptions/Error.h>
#include <cstdlib>
#include <cstring>

namespace casa { //# NAMESPACE CASA - BEGIN


double PrecTimer::CPU_speed_in_MHz = PrecTimer::get_CPU_speed_in_MHz();


double PrecTimer::get_CPU_speed_in_MHz()
{
  // first a few sanity checks
  AlwaysAssert(sizeof(int) == 4, AipsError);
  AlwaysAssert(sizeof(long long) == 8, AipsError);

#if defined __linux__ && \
    (defined __i386__ || defined __x86_64__ || defined __ia64__ || defined __PPC__) && \
    (defined __GNUC__ || defined __INTEL_COMPILER || defined __PATHSCALE__ || defined __xlC__)
  ifstream infile("/proc/cpuinfo");
  char     buffer[256], *colon;

  while (infile.good()) {
    infile.getline(buffer, 256);

#if defined __PPC__
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
#else
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
  double time = total_time / 1e6;
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
  if (count == 0) {
    os << "not used\n";
  } else {
    double total = static_cast<double>(total_time);
    if (CPU_speed_in_MHz == 0) {
      os << "avg = " << total / static_cast<double>(count);
      os << ", total = " << total_time << " cycles";
    } else {
      total /= 1e6 * CPU_speed_in_MHz;
      os << "avg = ";
      print_time(os, total / static_cast<double>(count));
      os << ", total = ";
      print_time(os, total);
    }
    os << ", count = " << setw(9) << count << endl;
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

} //# NAMESPACE CASA - END

