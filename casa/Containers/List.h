//# List.h: Doubly linked list classes
//# Copyright (C) 1993,1994,1995,1996,1997,1998,1999,2000,2001
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

#ifndef CASA_LIST_H
#define CASA_LIST_H


//# Includes
#include <casacore/casa/aips.h>
#include <casacore/casa/Utilities/Register.h>
#include <casacore/casa/Utilities/Notice.h>
#include <casacore/casa/Containers/Link.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Containers/IterError.h>

namespace casacore { //#Begin casa namespace

// The function which throws an exception for advancing the internal
// cursor past the end of a list
void throw_list_end_error();
void throw_list_init_error();
void throw_list_start_error();
void throw_list_swapright_same_error();

//# Forward Declarations
template<class t> class ListIter;
template<class t> class ConstListIter;
template<class t> class List;


//
// <summary>Linked list update notice</summary> 
// <use visibility=local>
//
// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="" demos="">
// </reviewed>
//
// <synopsis>
// This class is the notification which is passed between <src>List<t></src>
// and <src>ListIter<t></src> in order to keep cursors and container in sync.
// This is the mechanism which allows multiple iterators to view the same
// list and automatically update as the list is changed. See the
// <linkto class=Notice:description>Notice</linkto> class for more information.
// </synopsis>
//
template<class t> class ListNotice : public Notice {
friend class ConstListIter<t>;
friend class ListIter<t>;
friend class List<t>;
public:
    enum modification { DELETE, ADD, REMOVE, SWAP };
    //
    // This function returns the Notice "type", which is retrieved
    // from the "type registry". The registry information is maintained 
    // automatically by the Notice constructors.  A form of run 
    // time type information, this function is mostly intended for advanced 
    // users.
    //
    uInt type() const;

    //
    // This operator can be used to compare two
    // ListNotices.
    //
    int operator==(const Notice &op) const;

private:
    modification mod;
    Link<t> *oprev;
    Link<t> *ocur;
    Link<t> *nprev;
    Link<t> *ncur;
    int off;
    int otherOff;

    //
    // This is used to construct a list notice. The parameters are:
    // <ul>
    //    <li> (m) what was done to the list
    //    <li> (oc) the old current position
    //    <li> (op) the old previous position
    //    <li> (nc) the new current position
    //    <li> (np) the new previous position
    //    <li> (of) current offset;
    //    <li> (nf) other offset (only used with SWAP mod)
    // </ul>
    //
    ListNotice(modification m, Link<t> *oc,Link<t> *op,Link<t> *nc,Link<t> *np, int of, int nf=0) : 
		mod(m),oprev(op),ocur(oc),nprev(np),ncur(nc), off(of), otherOff(nf) {}

    //
    // This constructor is used to initialize a notice for a deleted
    // "List". 
    //
    ListNotice() : mod(DELETE), oprev(0),ocur(0),
			nprev(0),ncur(0),off(0),otherOff(0) {}

};

//
// <summary>Doubly linked list</summary> 
// <use visibility=export>
//
// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="" demos="">
// </reviewed>
//
// <synopsis>
//     This class is a container which by itself has little functionality
//     because the iteration functionality is contained in the iterator
//     classes, <linkto class=ListIter>ListIter</linkto> and
//     <linkto class=ConstListIter>ConstListIterr</linkto>. These iterator
//     classes allow traversal, insertion into list, and removal from the list.
//
//     This group of classes, List and iterators, was designed to allow
//     multiple iterators to manipulate a list at the same time. However,
//     if only one iterator is required the <a href=#simple_example>simple
//     example</a> below shows how a simple list can be created and used
//     without complication. The <a href=#complete_example>more complete
//     example</a> below demonstrates all of the functionality of the List
//     classes.
// </synopsis>
//
// <anchor name=simple_example>
// <example>
//     <srcblock>
// #include <casacore/casa/Containers/List.h>
// #include <casacore/casa/Containers/ListIO.h>
// 
// main() {
//                                              // List, conceptual
//                                              //       cursor = "|"
//   ListIter<int> list(new List<int>(),True);  //  |
//   list.addRight(12);                         //  | 12
//   list.addRight(2);                          //  | 2 12
//   list.addRight(89);                         //  | 89 2 12
//   list++;                                    //  89 | 2 12
//   list.addRight(10);                         //  89 | 10 2 12
//   list++;                                    //  89 10 | 2 12
//   list.addRight(8);                          //  89 10 | 8 2 12
//   list--;                                    //  89 | 10 8 2 12
//   list.pos(0);                               //  | 89 10 8 2 12
//   list.pos(5);                               //  89 10 8 2 12 |
//   list.pos(4);                               //  89 10 8 2 | 12
//   list.step(3);                              //  89 | 10 8 2 12
//   list.step();                               //  89 10 | 8 2 12
//   list.step(-4);                             //  89 10 8 2 | 12
//   list.removeRight();                        //  89 10 8 2 |
//   cout << list << endl;
//   return 0;
// }
//     </srcblock>
//     <em>The output from this example looks like:</em>
//     <pre>
//          len=4 pos=4 89 10 8 2
//     </pre>
// </example>
// </anchor>
// 
template<class t> class List : public NoticeSource
{
friend class ConstListIter<t>;
friend class ListIter<t>;
public:
    //
    // Creates an empty list.
    //
    List() : head(0), tail(0), length(0){}
    //
    // Copy Semantics
    // <group>
    List(const List<t> &other);
    List(const List<t> *other);
    List<t> &operator=(const List<t> &other);
    List<t> &operator=(const List<t> *other);
    // </group>

    //*display 4
    //
    // Destructs the list.
    //
    ~List();

    //
    //    Returns the length of the list.
    //
    uInt len() const {return length;}

    //
    // List version
    //
    enum {ListVersion = 2};

protected:
    Link<t> *head;
    Link<t> *tail;
    uInt length;

    //
    // Updates the extreme pointers, head or tail
    // under the appropriate conditions
    //
    // <group>
    virtual void added(Link<t> *, Link<t> *);
    virtual void removed(Link<t> *, Link<t> *, Link<t> *);
    // </group>
};


// 
// <summary>Doubly linked constant list iterator</summary> 
//
//  <synopsis>
//  The <linkto class=List>List</linkto> class above only provides for
//  the list framework. This is one of two classes which allow list
//  iteration, insertion, and removal. This class <em>cannot</em> be
//  used to modify a list, but rather, it can only be used to look at
//  or observe a list. It provides <em>no</em> functions for modifying
//  the list.
//
//  All of the operations take place to the right of a conceptual cursor.
//  The cursor starts out before the first element of the list and can
//  be incremented past the last element of the list. Going further than
//  the end of the list results in an exception.
//
// <example>
//    In this example, assume that this function is called at the
//    end of the <a href=#simple_example>example above</a>, i.e.
//    assume that the line before the return, 
//    <a href=#simple_example>above</a>, is uncommented.
//
// <srcblock>
// void iterate(ListIter<int> &list) {
//                                             // List, conceptual
//                                             //       cursor = "|"
//   ConstListIter<int> li = list;             //  89 10 8 2 |
//   li--;                                     //  89 10 8 | 2
//   cout << li.getRight() << " ";             //  89 10 8 | 2
//   li--;                                     //  89 10 | 8 2
//   li.pos(0);                                //  | 89 10 8 2
//   li.pos(3);                                //  89 10 8 | 2
//   li.pos(1);                                //  89 | 10 8 2
//   li.step();                                //  89 10 | 8 2
//   li.pos(0);                                //  | 89 10 8 2
//   li.step(-3);                              //  89 10 | 8 2
//   cout << li.getRight() << endl;            //  89 10 | 8 2
//   cout << li << endl;                       //  89 10 | 8 2
// }
// </srcblock>
// The output which this function, <src>iterate()</src>, would
// produce would look like:
// <pre>
//         2 8
//         len=4 pos=2 89 10 8 2
// </pre>
//
// As shown above:
// <dl> 
//    <dt> <src>pos()</src>
//      <dd> allows for arbitrary positioning of the cursor
//    <dt> <src>step()</src>, <src>operator++()</src>, and <src>operator--()</src>
//      <dd> allow for relative positioning
//    <dt> <src>getRight()</src>
//      <dd> fetches the next element in the list. 
// </dl>
// In addition:
// <dl>
//    <dt> <src>atStart()</src>, <src>atEnd()</src>, and <src>pos()</src>
//      <dd> allow querying the position of the  cursor
//    <dt> <src>len()</src>
//      <dd> returns the number of elements in the list.
// </dl>
// </example>
//
// <note role=tip> This class uses the <linkto class=Notice>Notice
//        classes</linkto> to implement "dynamic" cursors so that
//        multiple cursors are updated as elements are added and
//        removed from the list.
// </note>
//    
template<class t> class ConstListIter : public NoticeTarget
{
public:

    //
    // This constructor creates a "ConstListIter" which tracks the
    // "List<t>" parameter.
    //
    // <group>
    ConstListIter(const List<t> *st);
    ConstListIter(const List<t> &st) : NoticeTarget((NoticeSource &)st),
                                       cur(st.head), prev(0), curPos(0), 
				       container_((List<t> *) (&st)) 
					 {}
    // </group>

    //
    // This constructor creates a "ConstListIter" which tracks the 
    // same list tracked by the "ConstListIter<t>" parameter.
    //
    // <group>
    ConstListIter(const ConstListIter<t> &other) : 
                        NoticeTarget((NoticeTarget &)other),
                        cur(other.cur), prev(other.prev), curPos(other.curPos),
			container_(other.container_) {}

    ConstListIter(const ConstListIter<t> *other);
    // </group>


    //
    // This is the default constructor. It allows one
    // to create an initially invalid empty ConstListIter.  The instantiated
    // class will accept assignment and thus become valid later.
    //
    ConstListIter() : NoticeTarget(),cur(0), prev(0), curPos(0), 
                      container_(0) {}

    //*display 4
    //
    // Destructor doesn\'t do anything special because
    // all of the "real" information is in the "List<t>".
    //
    ~ConstListIter();

    //*display 4
    //
    // This function is the hook through which iterators
    // are notified of important changes to the underlying
    // list. For advanced users. 
    //
    void notify(const Notice &);

    //
    // This functions allows one to checked if the cursor
    // is at an extreme list position. "atStart()" checks
    // to see if the cursor is at the beginning of the list,
    // and "atEnd()" checks to see if the cursor is at the
    // end of the list.
    //
    // <group>
    Bool atStart() const {
	AlwaysAssert(isValid(),InvalidIterError);
	if (prev == 0) return True;
	else return False;}

    Bool atEnd() const {
	AlwaysAssert(isValid(),InvalidIterError);
	if (cur == 0) return True;
	else return False;}
    // </group>

    //
    // This function is used to step the cursor forward through
    // the list.
    //
    // <group>
    void operator++() {
	AlwaysAssert(isValid(),InvalidIterError);
	if (cur) {
	    curPos++;
	    prev = cur;
	    cur = (*cur).next();
	} else throw_list_end_error();}

    inline void operator++(int) {
	AlwaysAssert(isValid(),InvalidIterError);
	if (cur != 0) {
	    curPos++;
	    prev = cur;
	    cur = (*cur).next();
	} else throw_list_end_error();}
    // </group>

    //
    // This function allow for stepping the cursor toward the 
    // front of the list. 
    //
    // <group>
    void operator--() {
	if (prev) {
	    curPos--;
	    cur = prev;
	    prev = (*prev).prev();
	} else throw_list_start_error();}

    void operator--(int) {
	if (prev) {
	    curPos--;
	    cur = prev;
	    prev = (*prev).prev();
	} else throw_list_start_error();}
    // </group>

    //
    // "pos()" without arguments returns the current postion
    // of the cursor.
    // "pos()" with an unsigned integer parameter
    // moves the cursor to an absolute position.
    //
    // <group>
    virtual uInt pos(uInt);

    uInt pos() const {
	AlwaysAssert(isValid(),InvalidIterError);
	return curPos;}
    // </group>

    //
    // This function returns the number of elements in the list.
    //
    uInt len() const {
	AlwaysAssert(isValid(),InvalidIterError);
	return (*container_).length;}

    //
    // "step()" with no parameters advances the cursor forward 
    // one element.
    // "step()" with a signed integer parameter moves the cursor
    // (forward or backward) by a relative offset indicated by the
    // parameter. 
    //
    // <group>
    inline uInt step(Int offset){
	Int toffset;
	AlwaysAssert(isValid(),InvalidIterError);
	//# Traps a negative offset because aparently some compilers
	//# do not handle modulo of a negative number correctly.
	toffset = offset < 0 && -offset > Int(curPos) ? -((- curPos - offset) % ((*container_).length + 1)) 
                                           : (curPos + offset) % ((*container_).length + 1);
	return(pos(toffset >= 0 ? toffset : (*container_).length + toffset + 1));}

    inline uInt step() {return(step(1));}
    // </group>

    //
    // Returns the element to the right of the cursor.
    //
    const t &getRight() const {
	AlwaysAssert(isValid(),InvalidIterError);
	if (!cur) throw_list_end_error();
	return((*cur).val());}

    //
    // This assignment operator substitutes the "List<t>" 
    // tracked by this iterator to the "List<t>" passed as an argument.
    //
    // <group>
    virtual ConstListIter<t> &operator=(const List<t> &other);
    virtual ConstListIter<t> &operator=(const List<t> *other);
    // </group>

    //
    // This assignment operator substitutes the "List<t>"
    // tracked by this iterator to the "List<t>" tracked by the
    // passed "ConstListIter<t>" argument.
    //
    // <group>
    virtual ConstListIter<t> &operator=(const ConstListIter<t> &other);
    virtual ConstListIter<t> &operator=(const ConstListIter<t> *other);
    // </group>

    //
    // This function moves the cursor to the beginning of the list.
    //
    void toStart() {
	AlwaysAssert(isValid(),InvalidIterError);
	cur = (*container_).head; prev = 0; curPos = 0;}
  
    //
    // This function moves the cursor to the end of the list.
    //
    void toEnd() {
	AlwaysAssert(isValid(),InvalidIterError);
	prev = (*container_).tail; 
	cur = 0;
	curPos = (*container_).length;
    }

    //
    // Get the container over which we are iterating, could be null...
    //
    const List<t> *container() const {return container_;}

    // enum outside class because of compiler errors on HPUX
    //enum {ConstListIterVersion = 1};

protected:

    Link<t> *cur;
    Link<t> *prev;
    uInt curPos;
    List<t> *container_;
};

// 
// <summary>Doubly linked non-constant list iterator</summary> 
//
//  The <linkto class=List>List</linkto> class above only provides for
//  the list framework. This is one of two classes which allow list
//  iteration, insertion, and removal. This class <em>can</em> be
//  used to modify a list. Unlike
//  <linkto class=ConstListIter>ConstListIter</linkto>, this class can
//  insert and remove elements from a list as well as look at
//  or observe a list. <linkto class=ConstListIter>ConstListIter</linkto>
//  should be used whenever the list is not modified.
//
//  All of the operations take place to the right of a conceptual cursor.
//  The cursor starts out before the first element of the list and can
//  be incremented past the last element of the list. Going further than
//  the end of the list results in an exception. All additions and deletions
//  occur to the right of this conceptual cursor. In addition, this class
//  uses the <linkto class=Notice>Notice</linkto> class to ensure that multiple
//  iterators which are observing the same list are updated as the list
//  changes. This is important when multiple iterators are used.
//
// <anchor name=complete_example>
// <example>
// <srcblock>
// #include <casacore/casa/Containers/List.h>
// #include <casacore/casa/Containers/ListIO.h>
//
// main() {
//                                             // The conceptual cursors are:
//                                             //   |  for one
//                                             //   ^  for two
//                                             //   _  for three
//     ListIter<int> one(new List<int>,True);
//     ListIter<int> three, two = one;
//     one.addRight(12);                       //  |^ 12
//     one.addRight(2);                        //  |^ 2 12
//     one.addRight(89);                       //  |^ 89 2 12
//     cout << one.getRight() << " " 
//          << two.getRight() << endl;
//     two.addRight(21);                       //  |^ 21 89 2 12
//     cout << one.getRight() << " " 
//          << two.getRight() << endl;
//     one++; two++; two++;                    //   21 | 89 ^ 2 12
//     three = one;                            //   21 |_ 89 ^ 2 12
//     one.removeRight();                      //   21 |^_ 2 12
//     cout << one.getRight() << " " 
//          << two.getRight() << " "
//          << three.getRight() << endl;
//     three.addRight(17);                     //   21 |^_ 17 2 12
//
//     cout << one.getRight() << " " 
//          << two.getRight() << " "
//          << three.getRight() << endl;
//
//     one.toEnd();                            //   21 ^_ 17 2 12 |
//     one.addRight(18);                       //   21 ^_ 17 2 12 | 18
//     two.pos(3);                             //   21 _ 17 2 ^ 12 | 18
//     three--;                                //   _ 21 17 2 ^ 12 | 18
//     two.step();                             //   _ 21 17 2 12 ^| 18
//     one.step(4);                            //   _ 21 17 | 2 12 ^ 18
//     cout << "one:   " << one << endl;
//     cout << "two:   " << two << endl;
//     cout << "three: " << three << endl;
//
//     return 0;
// }
// </srcblock>
//     The output from this example would look like:
//     <pre>
//           89 89
//           21 21
//           2 2 2
//           17 2 17
//           one:   len=5 pos=2 21 17 2 12 18
//           two:   len=5 pos=4 21 17 2 12 18
//           three: len=5 pos=0 21 17 2 12 18
//     </pre>
// </example>
// </anchor>
//
// <note role=tip> This class uses the "Notice" classes to implement "dynamic" cursors
//        so that multiple cursors are updated as elements are added and
//        removed from the list.
// </note>
//    
template<class t> class ListIter : virtual public ConstListIter<t> {
public:

    //
    // This constructor allows one to construct a ListIter and
    // attach it to the List parameter. The own flag can be
    // set to indicate that the List should be destroyed when
    // the ListIter is deleted.
    //
    ListIter(List<t> *st, Bool OWN = False) : ConstListIter<t>(st), own(OWN){}
    

    //
    // This constructor allows one to construct a ListIter and
    // attach it to the List parameter. 
    //
    ListIter(List<t> &st);

    //
    // These constructors allow for the creation of a ListIter from
    // another ListIter. This will attach this ListIter to the List
    // tracked by the ListIter parameter at the time of construction.
    //
    // <group>
    ListIter(const ListIter<t> &other);
    ListIter(const ListIter<t> *other) : ConstListIter<t>(other), own(False){}
    // </group>

    //
    // This is the default constructor. It allows one
    // to create an initially invalid empty ListIter.  The instantiated
    // class will accept assignment and thus become valid later.
    //
    ListIter() : ConstListIter<t>(), own(False){}


    //
    // This function adds the element to the right of the 
    // current cursor position.
    //
    void addRight(t e) { 
	AlwaysAssert(this->isValid(),InvalidIterError);
	Link<t> *c = this->cur;
	Link<t> *p = this->prev;
	this->cur = newLink(e,this->prev,this->cur);
	// Allow container to update
	(*this->container_).added(this->prev,this->cur);
	ListNotice<t> state(ListNotice<t>::ADD,c,p,this->cur,this->prev,
			    this->curPos);
	(*this->container_).notify(state);
    }

    //
    // This function removes the element to the right of the 
    // current cursor position.
    //
    void removeRight();

    //
    // This function swaps the list section after the
    // current position of the list with the right section
    // of the list of another iterator. This can be 
    // particularly useful for "remembering" the position 
    // of a cursor in a list.
    //
    virtual void swapRight(ListIter<t> &);


    //
    // Returns the element to the right of the cursor.
    //
    // <group>
    t &getRight() {
	AlwaysAssert(this->isValid(),InvalidIterError);
	if (!this->cur) throw_list_end_error();
	return((*this->cur).val());}

    const t &getRight() const { return(ConstListIter<t>::getRight());}
    // </group>

    //
    // This function changes the List
    // which this ListIter tracks and specifies that the List
    // should be deleted when this iterator is deleted.
    //
    virtual ListIter<t> &assign(List<t> *other,Bool OWN = False);

    //
    // This assignment operator changes the List which this
    // iterator tracks to the List parameter.
    //
    // <group>
    virtual ListIter<t> &operator=(List<t> &other);

    virtual ListIter<t> &operator=(List<t> *other);
    // </group>
  
    //
    // These assignment operators allow one to change the List
    // to which this iterator tracks to the List currently associated
    // with the argument ListIter.
    //
    // <group>
    virtual ListIter<t> &operator=(const ListIter<t> &other);

    virtual ListIter<t> &operator=(const ListIter<t> *other);
    // </group>

    ~ListIter();

//# **Seems to cause an internal compiler error on Sun's
//# **Cfront compiler. Remove when really need or compiler
//# **recovers from brain damage (Thu May  4 13:08:21 EDT 1995).
//#
//#  enum {ListIterVersion = 1};

protected:
    //
    // Indicates if this iterator "owns" the container it observes.
    //
    Bool own;

    //*display 1
    //
    // This function creates a new link. By separating link
    // creation out into "newLink", the "addRight(t)"
    // functionality can be performed in the base class.
    // 
    virtual Link<t> *newLink(t &e, Link<t> *p=0, Link<t> *n=0);

private:

    //*display 6
    //
    // These functions are for internal use.  They ONLY throw an exception
    // to prevent improper initialization of a constant OrderedMapIter.
    //
    // <group>
    ConstListIter<t> &operator=(const List<t> &);
    ConstListIter<t> &operator=(const List<t> *);
    ConstListIter<t> &operator=(const ConstListIter<t> &);
    ConstListIter<t> &operator=(const ConstListIter<t> *);
    // </group>
};  

// enum outside class because of compiler errors on HPUX
enum {ConstListIterVersion = 1};

} //#End casa namespace
#ifndef CASACORE_NO_AUTO_TEMPLATES
#include <casacore/casa/Containers/List.tcc>
#endif //# CASACORE_NO_AUTO_TEMPLATES
#endif
