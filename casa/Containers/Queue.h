//# Queue.h: A First-In-First-Out (FIFO) data structure.
//# Copyright (C) 1995,1999
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

#ifndef CASA_QUEUE_H
#define CASA_QUEUE_H

#include <casacore/casa/aips.h>
#include <casacore/casa/Containers/Block.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

// 

// <summary> 
// A First-In-First-Out (FIFO) data structure.
// </summary>
//
// <reviewed reviewer="Gareth Hunt" date="94Jan06" tests="tQueue" demos="">
// </reviewed>
//
// <synopsis> 
// A Queue as implemented here is a simple container which can grow with time,
// and which returns its elements (only) in the order they are inserted. That
// is, the fundamental operations are to insert an element in the queue (at the
// "back") and retrieve an element (from the "front").
//
// <srcblock>
//     Queue<Int> queue;
//     Int x;
//     queue(1);                          // enqueue
//     queue.enqueue(2);                  // enqueue
//     queue(3);                          // enqueue
//     queue(4);                          // enqueue
//     queue.dequeue(x);                  // dequeue
//     cout << x << endl;
//     ...
//     while (queue.nelements() > 0) 
//         cout << queue() << " ";        // dequeue
//     cout << endl;
// </srcblock>
//
// Presently the implementation is rather simple. It stores the elements in
// a Block<T> which resizes (exponentially to avoid quadratic behaviour) when
// necessary. New elements are added to the end of the block, old elements are
// pulled off the front of the Block. The positions at the beginning are only
// reclaimed when the queue is empty or the compress() member is called.
//  This implementation is reasonably time
// efficient, but not necessarily space efficient. A more sophisticated
// implementation may be necessary eventually.
//
// To be used in a Queue, a class must have a default constructor, assignment
// operator, and copy constructor.
// </synopsis> 
//
// <motivation>
// This class was written for an application which thought it needed to queue
// up some Glish events while it processed other Glish events. In fact that
// application (Clean) was simplified so that it doesn't presently operate that
// way.
// </motivation>
//
// <todo asof="28OCT94">
//   <li> It is conceivable that an iterator might be useful for this class.
//   <li> If this class is ever heavily used, a more space efficient
//        implementation may be necessary.
// </todo>

template<class T> class Queue
{
public: 
    // Create a Queue with no elements.
    Queue();

    // Create a queue which is a copy of other. Compresses unused heap storage.
    Queue(const Queue<T> &other);

    ~Queue();

    // Create a queue which is a copy of other. Compresses unused heap storage.
    Queue<T> &operator=(const Queue<T> &other);

    // Place an element in the queue. After calling this, 
    // nelements() is increaed by one.
    // <group>
    void enqueue(const T &value);
    // Short-hand for enqueue();
    void operator()(const T &value);
    // </group>

    // Remove an element from the head of the queue and decrease
    // nelements() by one. If called when nelements() is zero, an
    // exception is thrown.
    // <group>
    void dequeue(T &value);
    // Short-hand for dequeue.
    T operator()();
    // </group>

    // Delete all the elements from the queue, and free up any resources.
    void clear();

    // Leave this queue logically unchanged, but remove unused storage. 
    // With the present Block<T> based implementation, removes
    // the unused entries at the beginning of the block.
    void compress();

    // How many elements are in the queue?
    uInt nelements() const {return next_p - first_p;}
private:
    Int first_p;
    Int next_p;
    Block<T> data_p;
};


} //# NAMESPACE CASACORE - END

#ifndef CASACORE_NO_AUTO_TEMPLATES
#include <casacore/casa/Containers/Queue.tcc>
#endif //# CASACORE_NO_AUTO_TEMPLATES
#endif
