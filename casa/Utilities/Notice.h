//# Notice.h: Classes for manipulating notices
//# Copyright (C) 1993,1994,1995,1999,2002
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

#ifndef CASA_NOTICE_H
#define CASA_NOTICE_H

#include <casacore/casa/aips.h>
#include <casacore/casa/Containers/Link.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declaration
class NoticeTarget;

// <summary>abstract base class for notices</summary> 
// <use visibility=export>
// <reviewed reviewer="Friso Olnon" date="1995/03/16" tests="" demos="">
// </reviewed>

// <synopsis> 
// A <src>Notice</src> is the piece of information passed around
// between a <src>NoticeSource</src> and a <src>NoticeTarget</src>. This
// abstract base class is only a skeleton intended to be derived from. It
// does not contain any relevant information -- that must be added by
// the derived classes --, but it enforces derived classes to implement
// the comparison operator <src>==</src> and the function
// <src>type()</src>.
// </synopsis> 

// <example>
// <linkto class=ListNotice>ListNotice</linkto>, derived from
// <src>Notice</src>, is the notification which is passed between
// <linkto class=List>List</linkto> and
// <linkto class=ListIter>ListIter</linkto>
// to keep cursors and container in sync.
// </example>

class Notice {
public:
    Notice() {}

    virtual ~Notice();

    // Return the identification number of the <src>Notice</src> type.
    virtual uInt type() const = 0;
   
    // Compare two <src>Notice</src>s.
    virtual int operator==(const Notice &) const = 0;
};

// <summary>base class for notice originators</summary> 
// <use visibility=export>
// <reviewed reviewer="Friso Olnon" date="1995/03/16" tests="" demos="">
// </reviewed>

// <synopsis>
// A <src>NoticeSource</src> maintains a list of all of the
// <src>NoticeTarget</src>s which are interested in <src>Notice</src>s
// from this <src>NoticeSource</src>. Its member function
// <src>notify()</src> sends the specified <src>Notice</src> to all the
// <src>NoticeTarget</src>s in the list.
//
// Classes which have many other dependent objects which need to be
// updated, should derive from this class.
// </synopsis> 

// <example>
// <linkto class=List>List</linkto>, the linked list class, is
// derived from <src>NoticeSource</src>. It mainly contains status
// information; all the manipulation functions are located in the
// <linkto class=ListIter>ListIter</linkto> classes. The linked
// list and its iterators communicate with each other via the notice
// system. <src>List</src> does not provide any further notice
// functionality; everything is taken care of by its base class
// <src>NoticeSource</src>.
// </example>

class NoticeSource {
public:
    friend class NoticeTarget;

    NoticeSource() : curIters(0) {}

    virtual ~NoticeSource();

    // Sends the <src>note</src> to all <src>NoticeTarget</src>s in the
    // target list.
    void notify(const Notice & note);

private:
    Link<NoticeTarget*> *curIters;     //# Do not Delete

    Link<NoticeTarget*> *&head() { return curIters;}
};

// <summary>abstract base class for notice receptors</summary> 
// <use visibility=export>
// <reviewed reviewer="Friso Olnon" date="1995/03/16" tests="" demos="">
// </reviewed>

// <synopsis> 
// A <src>NoticeTarget</src> receives the <src>Notice</src>s from the
// <src>NoticeSource</src> to which it is linked. A target can only be
// linked to one source.
//
// Classes which are dependent upon a particular
// <src>NoticeSource</src> should derive from this class.
// </synopsis> 

// <example>
// <linkto class=ListIter>ListIter</linkto> and its parent class
// <linkto class=ConstListIter>ConstListIter</linkto> are the iterators or
// "dynamic" cursors in the linked <linkto class=List>List</linkto>. They
// are derived from <src>NoticeTarget</src>, and the notice system ensures
// that multiple cursors are updated as elements are added and removed from
// the list, according to the following scheme:
// <ol>
//  <li> An iterator changes something to the underlying list.
//  <li> The iterator creates a <linkto class=ListNotice>ListNotice</linkto>
//       containing all the necessary information about the change.
//  <li> The iterator passes the notice to the <src>NoticeSource</src>
//       <linkto class=List>List</linkto>.
//  <li> The list relays the notice to all other iterators operating on the
//       list (kept in the "target list").
//  <li> Every iterator catches the notice and changes its state accordingly.
// </ol>
// </example>

class NoticeTarget {
public:
    friend class NoticeSource;
    
    // Destructs this <src>NoticeTarget</src>.
    virtual ~NoticeTarget();
    
    // Returns a boolean value telling whether this <src>NoticeTarget</src>
    // is still "valid".
    Bool isValid() const {return valid;}
    
    // Returns a boolean value telling whether this <src>NoticeTarget</src>
    // is still attached to a <src>NoticeSource</src> or not.
    Bool isAttached() const {return ilink ? True : False;}
    
    // Makes the current <src>NoticeTarget</src> "invalid".
    void invalidate() {valid = False;}
    
    // Hook through which <src>NoticeTarget</src>s are notified
    // (by <src>NoticeSource</src>s).
    virtual void notify(const Notice &) = 0;
    
protected:
    
    Link<NoticeTarget*> *ilink;
    NoticeSource *container;
    Bool valid;
    
    // Creates an unlinked, "invalid" <src>NoticeTarget</src>. An invalid
    // <src>NoticeTarget</src> does not occur in the target list of any
    // <src>NoticeSource</src>.
    NoticeTarget() : ilink(0), container(0), valid(False)  {}
    
    // Creates a "valid" <src>NoticeTarget</src> linked to the specified
    // <src>NoticeSource</src>. The <src>NoticeTarget</src> will be added
    // to the target list in that <src>NoticeSource</src>.
    // <group>
    NoticeTarget(NoticeSource *v) : ilink(0), container(0), valid(False) {attach(v);}
    NoticeTarget(NoticeSource &v) : ilink(0),container(0), valid(False) {attach(v);}
    // </group>
    
    // Creates a "valid" <src>NoticeTarget</src> linked to the same
    // <src>NoticeSource</src> as the <src>other NoticeTarget</src>.
    // So, both <src>NoticeTarget</src>s will occur in the same target
    // list.
    // <group>
    NoticeTarget(NoticeTarget &other) : ilink(0), container(0), valid(False)
		{ if (other.isValid()) attach(other.container);}
    NoticeTarget(NoticeTarget *other) : ilink(0), container(0), valid(False)
		{ if (other && other->isValid()) attach( (*other).container );}

    // </group>
    
    // Unlinks this <src>NoticeTarget</src> from its <src>NoticeSource</src>.
    // The <src>NoticeTarget</src> will be removed from the target list.
    void unlink(); 
    
    // Links this <src>NoticeTarget</src> to the same <src>NoticeSource</src>
    // as the <src>other NoticeTarget</src>. Any previous link will be undone.
    // <group>
    void link(const NoticeTarget &other);
    void link(const NoticeTarget *other);
    // </group>
    
    // Retrieves the next <src>NoticeTarget</src> in the target list
    // of the associated <src>NoticeSource</src>.
    // <group>
    Link<NoticeTarget*> *next() {
	return(ilink ? (*ilink).next() : 0);
    }
    const Link<NoticeTarget*> *next() const {
	return(ilink ? (*ilink).next() : 0);
    }
    // </group>
    
    // Adds this <src>NoticeTarget</src> to the target list in the
    // specified <src>NoticeSource</src>, so that it will receive all
    // notices sent out by that <src>NoticeSource</src>.
    // <group>
    void attach(NoticeSource *v);
    void attach(NoticeSource &v);
    // </group>
    
};


} //# NAMESPACE CASACORE - END

#endif





