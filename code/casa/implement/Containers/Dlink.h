//# Dlink.h: Doubly linked list primitive
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

#ifndef CASA_DLINK_H
#define CASA_DLINK_H

#include <casa/aips.h>
#include <casa/Containers/Link.h>

namespace casa { //# NAMESPACE CASA - BEGIN

// <summary><b>Deprecated</b> use <linkto class=Link>Link</linkto> instead</summary>
// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="" demos="">
// </reviewed>
// <h2>Deprecated use <linkto class=Link><src>Link</src></linkto> instead.</h2>
//
template<class t> class Dlink : public Link<t> {
  public:
    Dlink(t e,Dlink<t> *p=0,Dlink<t> *n=0);
    Dlink<t> *&prev() {return (Dlink<t>*&) Prev;}
    const Dlink<t> *prev() const {return (Dlink<t>*) Prev;}
};


} //# NAMESPACE CASA - END

#endif
