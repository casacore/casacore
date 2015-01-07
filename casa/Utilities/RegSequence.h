//# RegSequence.h: Sequence for the Register template functions
//# Copyright (C) 1993,1994,1995,1999
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

#ifndef CASA_REGSEQUENCE_H
#define CASA_REGSEQUENCE_H

#include <casacore/casa/aips.h>
#include <casacore/casa/Utilities/Sequence.h>
#include <casacore/casa/OS/Mutex.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

// <summary> Sequence for the Register() template functions </summary>
// <use visibility=export>
// <reviewed reviewer="Friso Olnon" date="1995/03/17" tests="" demos="">
// </reviewed>

// <prerequisite>
// <li> class <linkto class=Sequence>Sequence</linkto>
// <li> global function <linkto group="Register.h#register">Register</linkto>
// </prerequisite>

// <synopsis> 
// This class provides a <linkto class=Sequence>Sequence</linkto> for the
// <linkto group="Register.h#register">Register()</linkto> template
// functions. Providing a separate sequence exclusively for 
// <src>Register()</src> makes it less likely that the <src>uInt</src>
// counter will overflow.
// </synopsis> 

class RegSequence : public Sequence<uInt> {
public:
    // Get the next <src>uInt</src> value in the sequence (thread-safe).
    // <group>
    uInt getNext();
    static uInt SgetNext() {return ++num;}
    // </group>

private:
    static uInt num;
    static Mutex theirMutex;
};


} //# NAMESPACE CASACORE - END

#endif


