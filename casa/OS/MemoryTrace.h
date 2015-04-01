//# MemoryTrace.h: Memory usage tracing mechanism
//# Copyright (C) 2015
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
//# $Id: Block.h 21120 2011-09-01 13:51:56Z gervandiepen $

#ifndef CASA_MEMORYTRACE_H
#define CASA_MEMORYTRACE_H

#include <casacore/casa/aips.h>
#include <casacore/casa/OS/Timer.h>
#include <fstream>
#include <string>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

  // <summary>memory usage tracing mechanism</summary>
  // <use visibility=export>
  //
  // <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="" demos="">
  // </reviewed>
  //
  // <synopsis>
  // The MemoryTrace class provides some means to trace the
  // memory usage of a program. It logs malloc and free messages in
  // a file which can be examined by the python script memorytrace.py.
  // <br>The tracing is done using hooks for malloc and free as explained
  // in 'man malloc_hook'.
  //
  // The tracing can be started and stopped at any time. On the first
  // start the trace file is created. The file can be closed at any time,
  // usually at the end of a program. Another start will recreate the file.
  //
  // The trace file consists of 3 types of lines:
  // <ul>
  //  <li> An allocation line like "a <address> <caller> <size>"
  //  <li> A deallocation line like "f <address> <caller>"
  //  <li> A line like "begin/end <name>" telling the script the beginning
  //   or end of a code block. It makes it possible to see how memory usage
  //   develops. Such lines can be inserted using the class MemoryTraceBlock.
  // </ul>
  // All lines start with the number of milliseconds since the start of
  // the program.
  // <p>
  // The script memorytrace.py can be used to interpret the log file and
  // to show the memory usage.
  // </synopsis>

  class MemoryTrace
  {
  public:
    // Start the tracing. Nothing is done if already started.
    // On the first time, it opens the trace file. The name of the
    // trace file can be given in the env.var. CASACORE_MEMORYTRACE.
    // If undefined, the name casacore_memorytrace.log will be used.
    static void start();

    // Stop the tracing.
    static void stop();

    // Open the trace file if not open yet.
    static void open();

    // Close the tracing output file.
    static void close();

    // Is tracing on?
    static Bool isOn()
      { return theirDoTrace; }

    // Is the tracing file opened?
    static Bool isOpen()
      { return theirFile.is_open(); }

    // Write a block line in the output file.
    static void writeBlock (const char* msg, const std::string& name);
    static void writeBlock (const char* msg, const char* name);

    // Write an alloc or free message.
    static std::ofstream& writeAlloc (const void* ptr, size_t);
    static std::ofstream& writeFree  (const void* ptr);

    // The hooks for malloc and free writing the trace messages.
    static void* mallocHook (size_t, const void* caller);
    static void freeHook (void*, const void* caller);

    // Make a string from a char* without tracing a possible malloc in
    // the string constructor.
    static std::string makeString (const char*);

  private:
    static Bool          theirDoTrace;
    static std::ofstream theirFile;
    static Timer         theirTimer;
    //# Variables to save original hooks.
    static void* (*theirOldMallocHook)(size_t, const void*);
    static void (*theirOldFreeHook)(void*, const void*);
  };


  // <summary> Class to write begin and end block message </summary>
  // <synopsis>
  // This class is meant to write memory trace messages indicating the
  // beginning and end of a code block. In this way it is known that the
  // (de)allocate messages between these messages belong to that code block.
  //
  // The constructor writes the begin message, while the destructor writes
  // the end message. Because the destructor is called automatically by the
  // compiler, the user does not have to worry about it; it will also
  // work fine in case of a premature exit from a function.
  //
  // It is possible to nest blocks as deeply as one likes.
  // </synopsis>
  class MemoryTraceBlock
  {
  public:
    // The constructor writes a block begin message.
    MemoryTraceBlock (const std::string& name);
    MemoryTraceBlock (const char* name);
    // The constructor writes a block end message.
    ~MemoryTraceBlock();
  private:
    std::string itsName;
  };

} //# NAMESPACE CASACORE - END


//# Trace memory (de)allocation.
#define traceMemoryAlloc(ptr,size,msg)     \
  if (casacore::MemoryTrace::isOpen()) { \
    casacore::MemoryTrace::writeAlloc (ptr, size) << msg << std::endl; \
  }
#define traceMemoryFree(ptr,msg)    \
  if (casacore::MemoryTrace::isOpen()) { \
    casacore::MemoryTrace::writeFree (ptr) << msg << std::endl;  \
  }

#define traceMemoryBlockBegin(name)   \
  if (casacore::MemoryTrace::isOpen()) { \
    casacore::MemoryTrace::writeBlock(" begin ", name); \
  }
#define traceMemoryBlockEnd(name)       \
  if (casacore::MemoryTrace::isOpen()) { \
    casacore::MemoryTrace::writeBlock(" end ", name); \
  }


#endif
