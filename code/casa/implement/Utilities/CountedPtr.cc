//# CountedPtr.cc: Referenced counted pointer classes
//# Copyright (C) 1993,1994,1995,1996,1999
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

#include <aips/Utilities/CountedPtr.h>
#include <aips/Exceptions/Excp.h>

template<class t> 
void PtrRep<t>::freeVal() {
  if (val && deletable)
    {
    delete val;
    val = 0;
    }
}

template<class t>
SimpleCountedConstPtr<t> &SimpleCountedConstPtr<t>::operator=(t *v) {
  if (ref && --(*ref).count == 0){
    delete ref;
  }
  ref = new PtrRep<t>(v);
  return *this;
}

template<class t>
SimpleCountedConstPtr<t>::~SimpleCountedConstPtr() {
  if (ref && --(*ref).count == 0){
    delete ref;
    ref = 0;
  }
}

//
// All of these CountedPtr<t> ctors were moved out-of-line because
// the Sun Cfront compiler on Solaris generated statics for them.
// At some point, these should be moved back in line and tried with
// newer compilers.            (Tue Jan  3 15:30:07 EST 1995)
//
template<class t>
CountedPtr<t>::CountedPtr() : SimpleCountedPtr<t>(), CountedConstPtr<t>(),
					SimpleCountedConstPtr<t>() {}

template<class t>
CountedPtr<t>::CountedPtr(t *val, Bool delit) : SimpleCountedPtr<t>(val,delit),
					CountedConstPtr<t>(val,delit),
					SimpleCountedConstPtr<t>(val,delit) {}

template<class t>
CountedPtr<t>::CountedPtr(const CountedPtr<t> &val) : SimpleCountedPtr<t>(val),
					CountedConstPtr<t>(val),
					SimpleCountedConstPtr<t>(val) {}
