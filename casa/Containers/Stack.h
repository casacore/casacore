//# Stack.h: Implementation of a stack using the doubly linked list class
//# Copyright (C) 1993,1994,1995,1999,2000
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

#ifndef CASA_STACK_H
#define CASA_STACK_H

#include <casacore/casa/aips.h>
#include <casacore/casa/Containers/Link.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

extern void throw_empty_Stack_error(const char *msg = 0);

//  This class, Stack<t>, defines an implementation of a stack using
//  the doubly linked list primitive, Link<t>. It has the standard
//  push and pop operations.
//
// <summary> 
// A Last-In-First-Out (LIFO) data structure.
// </summary>
//
// <reviewed reviewer="Gareth Hunt" date="94Jan06" tests="tQueue" demos="">
// </reviewed>
//
// <synopsis> 
// A Stack as implemented here is a simple container which can grow with time,
// and which returns its elements (only) in the inverse order which they are
// inserted. That is, the fundamental operations are to push (add) an element
// onto the top of the stack and to pop (remove) an element from the top of
// the stack. As a result, the last element placed on the stack will be the
// first element removed.
//
// <example>
// <srcblock>
//     Stack<int> one,two;
//     int count = 0;
//     one.push(1);                       // add
//     one.push(2);                       // add
//     one.push(3);                       // add
//     one.push(4);                       // add
//     while ( !one.empty() ) {
//         cout << one.top() << " ";      // top element
//         two.push(one.top());           // push = add
//         one.pop();                     // remove top element
//     }
//     cout << endl;
//     while ( !two.empty() ) {
//         one.push(two.top());
//         cout << two.popVal() << " ";   // remove and return top
//     }
//     while ( !one.empty() )
//         count += one.popVal();
//     cout << endl << count << endl;;
// </srcblock>
// This results in the following output:
// <pre>
//         4 3 2 1 
//         1 2 3 4 
//         10
// </pre>
// <example>
//
// Presently, this implementation is rather simple. It is built directly
// upon the Link class.
// </synopsis> 
//
// <motivation>
//    A simple stack was needed for the (now deprecated) CanDelete class.
// </motivation>
//
// <todo asof="28OCT94">
//   <li> It is conceivable that an iterator might be useful for this class.
//   <li> If this class is ever heavily used, a more space efficient
//        implementation may be necessary.
// </todo>
//
template<class elem> class Stack {
private:
  //  Pointer to the top of the stack.
  Link<elem> *topOfStack;
public:

  //
  // This creates an empty stack.
  //
  Stack() : topOfStack(0) {}

  //
  // Create a stack by making a copy of other.
  //
  Stack(const Stack<elem> &other);

  //
  // Create a stack which is a copy of other.
  //
  Stack<elem> &operator=(const Stack<elem> &other);

  ~Stack();

  //
  //  Add an element to the top of the stack.
  //
  void push(const elem &e) {topOfStack = new Link<elem>(e,0,topOfStack);}

  //
  //  Remove the top element from the top of the stack.
  //
  void pop() {
    if (topOfStack == 0)
      throw_empty_Stack_error("Invalid operation (pop) on an empty Stack.");
    Link<elem> *tmp = topOfStack;
    topOfStack = (*tmp).unlink();
    delete tmp;
  }

  //
  //  Remove the top element from the top of the stack, and return it
  //
  elem popVal() {
    if (topOfStack == 0)
      throw_empty_Stack_error("Invalid operation (popVal) on an empty Stack.");
    Link<elem> *tmp = topOfStack;
    elem ret = (*tmp).val();
    topOfStack = (*tmp).unlink();
    delete tmp;
    return ret;
  }

  //
  //  Retreive the top element on the stack.
  //
  // <group>
  elem &top() {
    if (topOfStack == 0)
      throw_empty_Stack_error("Invalid operation (top) on an empty Stack.");
    return((*topOfStack).val());}

  const elem &top() const {
    if (topOfStack == 0)
      throw_empty_Stack_error("Invalid operation (const top) on an empty Stack.");
    return((*topOfStack).val());}
  // </group>

  //
  //  Check to see if the stack is empty.
  //
  Bool empty() const { return(topOfStack ? False : True);}
};


} //# NAMESPACE CASACORE - END

#ifndef CASACORE_NO_AUTO_TEMPLATES
#include <casacore/casa/Containers/Stack.tcc>
#endif //# CASACORE_NO_AUTO_TEMPLATES
#endif
