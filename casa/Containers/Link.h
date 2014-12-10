//# Link.h: Doubly linked list primitive
//# Copyright (C) 1993,1994,1995,1999,2000,2001
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

#ifndef CASA_LINK_H
#define CASA_LINK_H

#include <casacore/casa/aips.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

// <summary>doubly linked list primitive</summary>
// <use visibility=export>
//
// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="" demos="">
// </reviewed>
//
// <etymology>
//    This class provides the primitives for creating a class of linked
//    data structures. Thus it is called <src>Link</src>.
// </etymology>
//
// <synopsis>
//  This class provides a minimal doubly linked list implementation. All of
//  the work is performed by the constructor. This class does not keep
//  track of the head of the list; this is left to the user of the class.
//  This class can be thought of as the "nodes" of a linked list, but
//  conceptually each of the nodes is a list itself. This class will
//  typically not be used by the average user because although it is
//  a functional doubly linked list implementation, <src>List<t></src>
//  provides a higher level of abstraction.
// </synopsis>
//
// <example>
//     This example makes <src>Link</src> behave like a stack:
//     <srcblock>
// #include <iostream>
//     #include <casacore/casa/Containers/Link.h>
//
//     main() {
//         Link<int> *hed = new Link<int>(23);
//
//         hed = new Link<int>(12,0,hed);
//         hed = new Link<int>(19,0,hed);
//         hed = new Link<int>(10,0,hed);
//
//         while (hed) {
//             Link<int> *cur = hed;
//             hed = hed->unlink();
//             cout << cur->val() << " ";
//             delete cur;
//         }
//         cout << endl;
//     }
//     </srcblock>
//     The output from the previous example would be:
//     <pre>
//           10 19 12 23
//     </pre>
//     As each new link is being created, the new element goes at the
//     beginning of the list because the previous head of the list,
//     <src>hed</src>, is being passed in as the <em>next</em> list
//     element.
//
//     This next example demonstrates how a queue could be created
//     instead of a stack:
//     <srcblock>
// #include <iostream>
//     #include <casacore/casa/Containers/Link.h>
//
//     main() {
//         Link<int> *hed = new Link<int>(23);
//         Link<int> *cur = hed;
//
//         cur = new Link<int>(12,cur);
//         cur = new Link<int>(19,cur);
//         cur = new Link<int>(10,cur);
//
//         while (hed) {
//             cur = hed;
//             hed = hed->unlink();
//             cout << cur->val() << " ";
//             delete cur;
//         }
//         cout << endl;
//     }
//     </srcblock>
//     The output here would be:
//     <pre>
//           23 12 19 10
//     </pre>
// </example>
template<class t> class Link {
protected:
    t store;
    Link<t> *Next;
    Link<t> *Prev;
public:
    //  The <src>val()</src> member function will return a reference to the
    //  contents of the current node.
    // <group>
    t &val() {return store;}
    const t &val() const {return store;}
    // </group>
  
    //  These member functions allow traversal of the list. the <src>next()</src>
    //  functions retrieve the next element in the list, and <src>prev()</src>
    //  retrieves the previous element.
    //
    //  <note role=tip> The <em>non-const</em> versions of these functions
    //      return a reference to the pointer to the next element in the list.
    //      This allows for modification of the list if necessary, e.g. for
    //      removal of elements.
    //  </note>
    // <group>
    Link<t> *&next() {return Next;}
    const Link<t> *next() const {return Next;}
    Link<t> *&prev() {return Prev;}
    const Link<t> *prev() const {return Prev;}
    // </group>

    //
    // This is where the maintenance of the list happens. The parameters are:
    // <ul>
    //    <li> <b>e</b> -- the element to be added
    //    <li> <b>p</b> -- the previous element of the list
    //    <li> <b>n</b> -- the next element of the list
    // </ul>
    // If the previous element is non-null it is used to get all of the 
    // information necessary to add this new element to the list. If 
    // the previous element is null and the next element is non-null
    // it is assumed that the new element is being added to the 
    // beginning of the list, i.e. before the next element but with
    // no previous element.
    //
    Link(t e,Link<t> *p=0,Link<t> *n=0) : store(e), Prev(p) {
	if (Prev) {
	    Next = (*Prev).Next;
	    (*Prev).Next = this;
	    if (Next) (*Next).Prev = this;
	} else {
	    Next = n;
	    if (Next) {
		//
		// Clean up previous list if inserting in the middle
		// of a list with "p==0".
		//
		if ((*Next).Prev) (*(*Next).Prev).Next = 0;
		(*Next).Prev = this;
	    }
	}
    }

    //
    // This destructor destroys the rest of the list, i.e. this object and all
    // that follow.
    // <note role=warning> If the destructor is called for a <src>Link<t></src> in
    //        the middle of a list the elements which occur before the object will
    //        be left dangling, and the objects which follow the deleted object
    //        will also be deleted.
    // </note>
    ~Link();

    //
    // This function unlinks a given element of the list. It requires
    // no parameters because the node has links to the previous and 
    // next elements in the list. This is useful when removing a 
    // single element from the list because the destructor, 
    // <src>Link::~Link</src>, will delete the rest of the list elements
    // if they are linked in with <src>this</src>. This function returns
    // the next element in the list.
    // <note role=tip> The <src>Link<t>*</src> parameter is unused. It is a
    //    historical artifact which <b>will</b> be removed.
    // </note>
    Link<t> *unlink(Link<t> * = 0);

};

typedef Link<int> Link_int;


} //# NAMESPACE CASACORE - END

#ifndef CASACORE_NO_AUTO_TEMPLATES
#include <casacore/casa/Containers/Link.tcc>
#endif //# CASACORE_NO_AUTO_TEMPLATES
#endif
