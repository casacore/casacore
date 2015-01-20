//# tBlockTrace.cc: This program tests the BlockTrace class
//# Copyright (C) 2015
//# Associated Universities, Inc. Washington DC, USA.
//#
//# This program is free software; you can redistribute it and/or modify it
//# under the terms of the GNU General Public License as published by the Free
//# Software Foundation; either version 2 of the License, or (at your option)
//# any later version.
//#
//# This program is distributed in the hope that it will be useful, but WITHOUT
//# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
//# FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
//# more details.
//#
//# You should have received a copy of the GNU General Public License along
//# with this program; if not, write to the Free Software Foundation, Inc.,
//# 675 Massachusetts Ave, Cambridge, MA 02139, USA.
//#
//# Correspondence concerning AIPS++ should be addressed as follows:
//#        Internet email: aips2-request@nrao.edu.
//#        Postal address: AIPS++ Project Office
//#                        National Radio Astronomy Observatory
//#                        520 Edgemont Road
//#                        Charlottesville, VA 22903-2475 USA
//#
//# $Id: tBlock.cc 21021 2011-03-01 10:07:09Z gervandiepen $

//# Includes

#include <casacore/casa/Containers/Block.h>
#include <casacore/casa/OS/MemoryTrace.h>

#include <casacore/casa/namespace.h>

void doit()
{
  Block<Int> bl1(10);
  bl1.resize (20);
  Block<Double> bl2(3, 4.);
}

int main()
{
  traceMemoryBlockBegin ("1st no");
  MemoryTrace::open();
  traceMemoryBlockBegin ("2nd yes");
  MemoryTrace::open();
  traceMemoryBlockBegin ("3rd yes");
  MemoryTrace::close();
  traceMemoryBlockBegin ("4th no");
  // This should not do tracing.
  doit();
  BlockTrace::setTraceSize (10);
  traceMemoryBlockBegin ("5th yes start");
  // This should do tracing.
  doit();
  {
    MemoryTraceBlock mtb("6th yes endxxxxx");
  }
  // Now it should not trace.
  MemoryTrace::close();
  traceMemoryBlockBegin ("7th no");
  doit();
}

