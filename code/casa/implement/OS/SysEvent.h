//# SysEvent.h: abstract  base class for system events
//# Copyright (C) 1994,1995,1999,2000,2001
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

#if !defined(AIPS_SYSEVENT_H)
#define AIPS_SYSEVENT_H



#include <casa/Utilities/CountedPtr.h>
#include <casa/BasicSL/String.h>
#include <casa/Utilities/Regex.h>
#include <casa/Utilities/Register.h>
#include <sys/types.h>
#include <sys/time.h>

extern void throw_invalid_sysevent();
extern void throw_sysevent_systarget_mismatch();
extern void throw_sysevent_init_error();

class SysEventTarget;
class SysEventSource;

//#---------------------------------------------------------------------
//# SysEvent

// <summary> abstract base class for system events (from X, Glish, signals)
// </summary>

// <use visibility=local>

// <reviewed reviewer="Paul Shannon" date="1995/03/02" tests="">
//
// <prerequisite>
//   <li> some understanding of X and Glish events
// </prerequisite>

// <etymology>
//   Event is the class describing astronomical events, so 'SysEvent'
//   was picked to distinguish it.
// </etymology>
//
// <synopsis> 
//  This class is the base class for all system events. It is
//  simply a wrapper around the basic types of events which must
//  be handled in the system, e.g. X events, Glish events, signals,
//  etc. It provides a common interface to all events.
// </synopsis> 
//
// <motivation>
//  To write a windowing Glish client, it is necessary to mix the
//  X and Glish event loops. While this could have been done by
//  explicitly writing the X event loop each time, it is better
//  to encapsulate the event loop in a couple of classes. This
//  class does this for the actual events.
// </motivation>
//
class SysEvent {
public: 

    friend class XSysEventSource;
    friend class GlishSysEventSource;
    friend class SigSysEventSource;

    //
    // These are the various types of events which can be handled.
    // <ul>
    //	<li> XGroup -- X events
    //	<li> GlishGroup -- Glish events
    //	<li> SigGroup -- signals
    // </ul>
    //
    enum Group {UnknownGroup=0, XGroup, GlishGroup, SigGroup};

    //
    // Creates an unusable event, but the event can be assigned to.
    //
    SysEvent() : rep(0), src(0) {}
    //
    // Creates an event given another event.
    //
    SysEvent(const SysEvent &other) : rep(other.clone()), src(other.src) {}

    virtual ~SysEvent();

    //
    // Equality operator checks to see if two events are identical. 
    // This just uses the "group()" and the "type()" to determine
    // equality.
    //
    int operator==(const SysEvent &other) {
	return(group() == other.group() && 
	       type() == other.type());
    }
    SysEvent &operator=(const SysEvent &other) {
	if (rep) delete rep;
	rep = other.clone();
	src = other.src;
	return *this;
    }

    //
    // Returns the group to which this event belongs.
    //
    virtual Group group() const;
    //
    // Returns the type of this event represented as a
    // String.
    //
    virtual String type() const;
    //
    // Sends the event to the event target parameter.
    //
    virtual Bool dispatch(SysEventTarget &e);
    //
    // Dispatches the event to the event target parameter, but
    // it also carries along an extra event source in case it is
    // needed. This is needed, for example, when a non-X event
    // target wants to do something on the GUI, the X event 
    // source must be known.
    //
    virtual Bool dispatch(SysEventTarget &e, SysEventSource &xs);
    //
    // Returns the event source which generated this event.
    //
    SysEventSource *source() {
	return src ? src : (rep ? rep->source() : 0);
    }

protected:

    SysEvent *rep;
    SysEventSource *src;

    //
    // Protected constructor which is used by derived classes to
    // fill the letter (here rep).
    // 
    SysEvent(SysEvent *other, SysEventSource *s) : rep(other), src(s) { }
    SysEvent(SysEventSource *s) : rep(0), src(s) {}

    //
    // Returns a "deep" copy of the object.
    //
    virtual SysEvent *clone() const;
};

//
// Returns `True' if successfully handled
//
typedef Bool (*SysEventTargetProc)(SysEvent &, void *);
//#---------------------------------------------------------------------
//# SysEventSource

// <summary> abstract base class for system event sources 
// </summary>

// <reviewed reviewer="Paul Shannon" date="1995/03/02" tests="">
//
// <prerequisite>
//   <li>
//   <li>
// </prerequisite>

// <etymology>
//	See SysEvent
// </etymology>
//
// <synopsis> 
//	This class is the base class for all of the sources of
//	events within the system, e.g. X Window System, Glish.
//	These are the entities which generate the events which
//	must be handled.
// </synopsis> 
//
// <motivation>
//	See SysEvent
// </motivation>
//
class SysEventSource {
    friend class GlishSysEventSource;
public:
    SysEventSource() {}

    virtual ~SysEventSource();

    //
    // Returns the group to which the event source belongs.
    //
    virtual SysEvent::Group group() const = 0;
    //
    // Returns the next event which needs to be processed.
    // This function may blocks until an event is ready to
    // be returned.
    //
    virtual SysEvent nextEvent() = 0;
    //
    // Used to check to see if there are any events waiting to be 
    // processed.
    //
    virtual Bool waitingEvent() = 0;
    //
    // Used to check to see if the event source is still "connected",
    // i.e. whether it is still a valid source of events.
    //
    virtual Bool connected() = 0;

    // Part of the event driven interface to SysEventSource. These
    // return True upon success, and False upon failure.
    // <group>
    virtual Bool canCombine(const SysEventSource &src) const = 0;
    virtual Bool canCombine(const SysEventSource *src) const = 0;

    virtual Bool combine(SysEventSource &src) = 0;
    virtual Bool combine(SysEventSource *src) = 0;

    // Invoke the target whenever the event matches the regular
    // expression.
    // <group>
    virtual Bool addTarget(SysEventTarget &tgt, const String &regx) = 0;
    virtual Bool addTarget(SysEventTarget *tgt, const String &regx, Bool ownTarget=False) = 0;
    virtual Bool addTarget(SysEventTargetProc tgt, const String &regx, void *userData=0) = 0;
    // </group>

    // Specifies the default action
    // <group>
    virtual Bool setDefault(SysEventTarget &tgt) = 0;
    virtual Bool setDefault(SysEventTarget *tgt, Bool ownTarget=False) = 0;
    virtual Bool setDefault(SysEventTargetProc tgt, void *userData=0) = 0;
    // </group>
    virtual Bool loop() = 0;
    virtual void invokeTarget() = 0;
    // </group>
};

//#---------------------------------------------------------------------
//# SysEventTarget

// <summary> abstract base class for system event targets (handlers) 
// </summary>

// <reviewed reviewer="Paul Shannon" date="1995/03/02" tests="">
//
// <prerequisite>
//   <li> SysEvent
//   <li> the concept of event handlers
// </prerequisite>

// <etymology>
//	See SysEvent
// </etymology>
//
// <synopsis> 
//	This class is the base class for all of the handlers of
//	events within the system. The classes which derive from
//	this class are the ones which react to waiting events.
//	They are analogous to X event handlers.
// </synopsis> 
//
// <motivation>
//	See SysEvent
// </motivation>
//
class SysEventTarget {
public:

    SysEventTarget() {}
    virtual ~SysEventTarget();
    //
    // Returns the group to which the event target belongs.
    //
    virtual SysEvent::Group group() const = 0;
    //
    // Checks to see if a given SysEventTarget can handle a
    // particular event.
    //
    virtual Bool canHandle(SysEvent &event);

    virtual Bool handle(SysEvent);

    virtual SysEventTarget *clone() const = 0;
};

// <summary><b>Internal</b> class</summary>
// <use visibility=local>
//
// This class is used as a container of information for dispatching
// targets. It facilitates invoking targets based on regular
// expressions.
//
// This class is <b>not</b> intended for external use.
//
class SysEventTargetInfo {
protected:
    CountedPtr<SysEventTarget> tgt;
    SysEventTargetProc tgtProc;
    CountedPtr<Regex> regex;
    void *userData;
public:
    enum TargetType { Unknown=0, ClassTgt, ProcTgt };
    SysEventTargetInfo(SysEventTargetProc t,const String &r,void *u=0);
    SysEventTargetInfo(SysEventTargetProc t, void *u=0);

    SysEventTargetInfo(SysEventTarget *t,const String &r);
    SysEventTargetInfo(SysEventTarget *t);
    SysEventTargetInfo(SysEventTargetInfo &other);

    SysEventTargetInfo &operator=(SysEventTargetInfo &other);

    virtual ~SysEventTargetInfo();
    Regex &getRegex();
    void *getUserData() { return userData; }
    virtual SysEventTarget &getTarget() {return *tgt;}
    SysEventTargetProc getTargetProc() {return tgtProc;}
    TargetType type() const { return tgtProc ? ProcTgt : ClassTgt; }
    virtual uInt id() const { return Register(this); }
};

#endif
