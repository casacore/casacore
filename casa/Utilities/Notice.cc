//# Notice.cc: Classes for mainpulating notices
//# Copyright (C) 1993,1994,1995
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

#include <casacore/casa/Utilities/Notice.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

Notice::~Notice()
{
    // Nothing
}

NoticeSource::~NoticeSource(){
  while (head()) head()->val()->unlink();
}

void NoticeSource::notify(const Notice &note) {
  Link<NoticeTarget*> *t = 0;
  for (Link<NoticeTarget*> *tmp = curIters;tmp;tmp=t) {
     t = (*tmp).next();
     tmp->val()->notify(note);
   }
}

NoticeTarget::~NoticeTarget()
{ 
    unlink();  
}

void NoticeTarget::unlink() {
  if (ilink) {
    if (container && ilink == (*container).head()) 
      (*container).head() = (*ilink).unlink();
    else
      (*ilink).unlink();
    delete ilink;
    container = 0;
    ilink = 0;
  }
  valid = False;
}

void NoticeTarget::attach(NoticeSource *v) {
  if ( v ) {
    unlink();
    container = v;
    (*v).head() = ilink = new Link<NoticeTarget*>(this,0,(*v).head());
    valid = True;
  }
}

void NoticeTarget::attach(NoticeSource &v) {
  unlink();
  container = &v;
  v.head() = ilink = new Link<NoticeTarget*>(this,0,v.head());
  valid = True;
}

void NoticeTarget::link(const NoticeTarget &other) {
  if (other.isValid()) {
    unlink();
    container = other.container;
    (*container).head() = ilink = new Link<NoticeTarget*>(this,0,(*container).head());
    valid = True;
  }
}

void NoticeTarget::link(const NoticeTarget *other) {
  if (other && (*other).isValid()) {
    unlink();
    container = (*other).container;
    (*container).head() = ilink = new Link<NoticeTarget*>(this,0,(*container).head());
    valid = True;
  }
}


} //# NAMESPACE CASACORE - END

