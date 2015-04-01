//# Sequence.h: provides sequences
//# Copyright (C) 1993,1994,1995,1999,2001
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

#ifndef CASA_SEQUENCE_H
#define CASA_SEQUENCE_H

#include <casacore/casa/aips.h>
#include <casacore/casa/OS/Mutex.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

// <summary> virtual templated base class for sequences </summary>
// <use visibility=export>
// <reviewed reviewer="Friso Olnon" date="1995/03/17" tests="" demos="">
// </reviewed>

// <synopsis>
// The virtual base class for sequences in the library.
// It is templated to allow users to derive sequences of any type,
// e.g. libg++'s Integers.
// </synopsis>

template<class t> class Sequence {
public:
    virtual ~Sequence(){};

    // Force derived classes to provide this function, to return the
    // next value in the sequence.
    virtual t getNext () = 0;
};


// <summary> uInt sequence for general use </summary>
// <use visibility=export>
// <reviewed reviewer="Friso Olnon" date="1995/03/17" tests="" demos="">
// </reviewed>

// <synopsis>
// This class provides a <src>uInt</src> based sequence for general use.
// </synopsis>

class uIntSequence : public Sequence<uInt> {

public:
    // Get the next <src>uInt</src> value in the sequence (thread-safe).
    // <group>
    uInt getNext()
      { return SgetNext(); }
    static uInt SgetNext();
    // </group>

private:
    static uInt num;
    static Mutex theirMutex;
};


} //# NAMESPACE CASACORE - END

#ifndef CASACORE_NO_AUTO_TEMPLATES
#include <casacore/casa/Utilities/Sequence.tcc>
#endif //# CASACORE_NO_AUTO_TEMPLATES
#endif
