//# SysEvent.cc: baseclass for system events
//# Copyright (C) 1994,1995
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
#include <aips/OS/SysEvent.h>
#include <aips/Exceptions/Excp.h>
#include <aips/Exceptions/Error.h>

SysEvent::~SysEvent() {
    if (rep) delete rep;
}

SysEvent::Group SysEvent::group() const {
    if (!rep) throw_invalid_sysevent();
    return (*rep).group();
}

String SysEvent::type() const {
    if (!rep) throw_invalid_sysevent();
    return (*rep).type();
}

Bool SysEvent::dispatch(SysEventTarget &e) {
    if (!rep) return False;
    return (*rep).dispatch(e);
}

Bool SysEvent::dispatch(SysEventTarget &e, SysEventSource &xs) {
    if (!rep) return False;
    return (*rep).dispatch(e,xs);
}

SysEvent *SysEvent::clone() const {
    if (!rep) throw_invalid_sysevent();
    return (*rep).clone();
}

SysEventSource::~SysEventSource() {}

Bool SysEventTarget::canHandle(SysEvent &event) {
    return event.group() == group() ? True : False;
}

Bool SysEventTarget::handle(SysEvent e) {
    return e.dispatch(*this);
}

SysEventTarget::~SysEventTarget() {}

//
// INTERNAL USE
//
SysEventTargetInfo::~SysEventTargetInfo() { }

SysEventTargetInfo::SysEventTargetInfo(SysEventTargetProc t,const String &r,void *u) : 
		tgt(0), tgtProc(t), regex(new Regex(r)),userData(u) { }
SysEventTargetInfo::SysEventTargetInfo(SysEventTargetProc t, void *u) : tgt(0),
		tgtProc(t), regex(new Regex()),userData(u) { }

SysEventTargetInfo::SysEventTargetInfo(SysEventTarget *t,const String &r) : tgt(t),
		tgtProc(0), regex(new Regex(r)), userData(0) { }
SysEventTargetInfo::SysEventTargetInfo(SysEventTarget *t) : tgt(t),
		tgtProc(0), regex(new Regex()), userData(0) { }


SysEventTargetInfo::SysEventTargetInfo(SysEventTargetInfo &other) : tgt(other.tgt),
		tgtProc(other.tgtProc), regex(other.regex), userData(other.userData) { }

SysEventTargetInfo &SysEventTargetInfo::operator=(SysEventTargetInfo &other) {
    tgt = other.tgt;
    tgtProc = other.tgtProc;
    regex = other.regex;
    userData = other.userData;
    return *this;
}

Regex &SysEventTargetInfo::getRegex() { return *regex; }


void throw_invalid_sysevent() {
    throw(AipsError("Invalid System Event"));
}

void throw_sysevent_systarget_mismatch() {
    throw(AipsError("Event Target Mismatch"));
}

void throw_sysevent_init_error() {
    throw(AipsError("Event Initialization Error"));
}
