//# CountedPtr.h: Referenced counted pointer classes
//# Copyright (C) 1993,1994,1995,1996,1999,2001
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

#ifndef CASA_COUNTEDPTR_H
#define CASA_COUNTEDPTR_H

#include <casa/aips.h>

//# Uncommenting out the following two lines will define USE_SHARED_PTR and
//# cause the implementation of CountedPtr to use a thread-safe smart
//# pointer implementation class inside (at this writing this will be
//# boost::shared_ptr but will eventually be std::shared_ptr).
//# Another effect is that portions of the measures framework will use
//# boost::recursive_mutex to make those classes friendly to multithreading.
//#
//# N.B.: I would not recommend mixing defined/undefined for the two symbols.
//#
//# Changing the setting will causes a major rebuild of both the casacore
//# and code projects since CountedPtr underlies most data structures and
//# because of the template nature of CountedPtr. (jjacobs 7/19/12)
//#
//# #define USE_SHARED_PTR
//# #define CASA_THREAD_NEUTRAL
//#
//# This switch is meant for the CASA group only.
//# Once most compilers support shared_ptr in their libstdc++, CountedPtr
//# will basically be typedef-ed to shared_ptr and SimpleCountedPtr, etc
//# will be deprecated.

#if ! defined (USE_SHARED_PTR)

//====================================================================
//====================================================================
//
// This is the original, thread-hostile implementatlow ion.
// For nonthreaded applications it's fine, though and as a bonus
// it does not incur and dependencies on boost.
//
//====================================================================
//====================================================================

#include <casa/aips.h>

namespace casa { //#Begin casa namespace

//# Forward Declarations
template<class t> class SimpleCountedPtr;
template<class t> class SimpleCountedConstPtr;
template<class t> class CountedPtr;
template<class t> class CountedConstPtr;

// <summary> act on dereference error </summary>
// <synopsis>
// Global function that throws an exception. It is called by the
// member functions of the counted pointer classes when an
// un-initialized (null) pointer is followed.
// </synopsis>
// <group name=dereference_error>
void throw_Null_CountedPtr_dereference_error();
// </group>

// <summary>Internal representation for <src>CountedPtr</src></summary>
// <use visibility=local>
// <reviewed reviewer="Friso Olnon" date="1995/03/15" tests="tCountedPtr" demos="">

// <prerequisite>
// <li> class <linkto class="SimpleCountedPtr:description">SimpleCountedPtr</linkto>
// <li> class <linkto class="SimpleCountedConstPtr:description">SimpleCountedConstPtr</linkto>
// </prerequisite>

// <synopsis>
// This class is a utility class for
// <linkto class="CountedConstPtr:description">CountedConstPtr</linkto>
// and <linkto class="CountedPtr:description">CountedPtr</linkto>.
// It stores the reference count and the pointer to the real data.
//
// <note role=tip> It is currently a template and is used such that
// <src>t</src> is the <em>true</em> type of the stored pointer. This
// means, however, that when it is used, a template instantiation must be
// done for each type which <src>t</src> assumes. This makes debugging
// easier, but in the future all of these pointers could be declared with
// <src>void</src> type to avoid template instantiations.
// </note>
// </synopsis>

// <motivation>
// This class isolates all of the low level management of the reference.
// </motivation>

template<class t> class PtrRep
{

public:

    friend class SimpleCountedPtr<t>;
    friend class SimpleCountedConstPtr<t>;
    friend class CountedPtr<t>;
    friend class CountedConstPtr<t>;

protected:

    // This constructor sets up the reference count to one and
    // initializes the pointer to the real data. The
    // <src>delit</src> flag can be passed in to indicate whether
    // the real data should be freed or not when the
    // reference count reaches zero.
    // <group>
    PtrRep(t *v) : val(v), count(1), deletable(True) {}
    PtrRep(t *v, Bool delit) : val(v), count(1), deletable(delit) {}
    // </group>

    // This deletes the real data if indeed it can be deleted.
    void freeVal();

    // This destructor uses the <src>deletable</src> flag to indicate if the
    // real data should be freed or not.
    //
    ~PtrRep() {
	freeVal();
    }

private:

    t *val;
    unsigned int count;
    Bool deletable;

};

// <summary>Simple referenced counted pointer for constant data</summary>
// <use visibility=export>
// <reviewed reviewer="Friso Olnon" date="1995/03/15" tests="tCountedPtr" demos="">

// <etymology>
// This class is <em>Simple</em> because it does not have the
// <src>operator->()</src> operator. This means that it puts less demands
// on the underlying type. It is <em>Counted</em> because it is reference
// counted, and it is <em>Const</em> because the underlying value is
// non-modifiable.
// </etymology>

// <synopsis>
// This class implements a simple reference counting mechanism. It
// allows <src>SimpleCountedConstPtr</src>s to be passed around freely,
// incrementing or decrementing the reference count as needed when one
// <src>SimpleCountedConstPtr</src> is assigned to another. When the
// reference count reaches zero the internal storage is deleted by
// default, but this behavior can be overridden.
//
// This class is used as a pointer to constant data. As such, it only
// has the subset of the
// <linkto class="CountedConstPtr:description">CountedConstPtr</linkto>
// functions which are relevant for constant data.
// </synopsis>

// <motivation>
// Reference counting
// </motivation>

template<class t> class SimpleCountedConstPtr
{
public:

    // This constructor allows for the creation of a null
    // <src>SimpleCountedConstPtr</src>. The assignment operator can be used
    // to assign a null <src>SimpleCountedConstPtr</src> from another
    // pointer.
    //
    SimpleCountedConstPtr() : ref(0) {}

    // This constructor sets up a reference count for the <src>val</src>
    // pointer.  By default, the data pointed to by <src>val</src>
    // will be deleted when it is no longer referenced. Passing in
    // <src>False</src> for <src>delit</src> will prevent the data
    // from being deleted when the reference count reaches zero.
    //
    // <note role=warning> After the counted pointer is initialized
    // the value should no longer be manipulated by the raw pointer of
    // type <src>t*</src>.
    // </note>
    //
    SimpleCountedConstPtr(t *val, Bool delit = True) {
	ref = new PtrRep<t>(val,delit);
    }

    // This constructor sets up a reference count for the
    // <src>val</src> pointer.  Since <src>val</src> is a pointer to
    // constant data, the data will not be deleted when the reference
    // count reaches zero.
    //
    // <note role=tip> Since the constant data will NOT be cleaned up
    // when the reference count reaches zero, the use of this class for
    // pointers to constant data may not be desirable.
    // </note>
    //
    SimpleCountedConstPtr(const t *val) {
	ref = new PtrRep<t>((t *) val,False);
    }

    // This copy constructor allows <src>SimpleCountedConstPtr</src>s to be
    // initialized from other <src>SimpleCountedConstPtr</src>s.
    //
    SimpleCountedConstPtr(const SimpleCountedConstPtr<t> &val) : ref(val.ref) {
	if (ref)
	    (*ref).count++;
    }

    // This destructor only deletes the really stored data when it was
    // initialized as deletable and the reference count is zero.
    //
    virtual ~SimpleCountedConstPtr();

    // The <src>SimpleCountedConstPtr</src> indirection operator simply
    // returns a reference to the value being protected. If the pointer
    // is un-initialized (null), an exception will be thrown. The member
    // function
    // <linkto class="SimpleCountedConstPtr:null()const">null</linkto>()
    // can be used to catch such a condition in time.
    //
    // <thrown>
    // <li> ExcpError
    // </thrown>
    //
    // <note role=tip> The address of the reference returned should
    // not be stored for later use.
    // </note>
    //
    const t &operator*() const {
	if (!ref) throw_Null_CountedPtr_dereference_error();
	return(*(*ref).val);
    }

    // Equality operator which checks to see if two
    // <src>SimpleCountedConstPtr</src>s are pointing at the same thing.
    //
    Bool operator==(const SimpleCountedConstPtr<t> &other) const {
	return (ref == other.ref ? True : False);
    }

    // Non-equality operator which checks to see if two
    // <src>SimpleCountedConstPtr</src>s are not pointing at the same thing.
    //
    Bool operator!=(const SimpleCountedConstPtr<t> &other) const {
	return (ref != other.ref ? True : False);
    }

    // This assignment operator allows <src>SimpleCountedConstPtr</src>s
    // to be freely assigned to each other.
    //
    SimpleCountedConstPtr<t> &operator=(const SimpleCountedConstPtr<t> &val) {
	if (ref && --(*ref).count == 0){
	    delete ref;
            ref = 0;
        }
	if ((ref = val.ref) != 0)
	    (*ref).count++;
	return *this;
    }

    // This assignment operator allows the object to which the current
    // <src>SimpleCountedConstPtr</src> points to be changed.
    //
    SimpleCountedConstPtr<t> &operator=(t *v);

    // Sometimes it is useful to know if there is more than one
    // reference made. This is a way of getting that. Of course the point
    // of these classes is that this information is normally not required.
    //
    uInt nrefs() const {return ref->count;}

    // Check to see if this <src>SimpleCountedConstPtr</src> is
    // un-initialized, null.
    //
    Bool null() const { return (ref==0 || (ref->val == 0));}

protected:

    PtrRep<t> *ref;

};

// <summary>Regular referenced counted pointer for constant data</summary>
// <use visibility=export>
// <reviewed reviewer="Friso Olnon" date="1995/03/15" tests="tCountedPtr" demos="">

// <prerequisite>
// <li> class <linkto class="SimpleCountedConstPtr:description">SimpleCountedConstPtr</linkto>
// </prerequisite>

// <synopsis>
// This class has the same objective as
// <linkto class="SimpleCountedConstPtr:description">SimpleCountedConstPtr</linkto>
// but it adds the <src>operator->()</src>. It still only contains a
// pointer whose underlying data cannot be changed. The destructor
// deletes the underlying data when the reference count reaches zero.
// </synopsis>

// <motivation>
// <src>operator->()</src> is useful, but not always available for
// every type.
// </motivation>

template<class t> class CountedConstPtr : virtual public SimpleCountedConstPtr<t> {
public:

    // This constructor allows for the creation of a null
    // <src>CountedConstPtr</src>. The assignment operator can be
    // used to assign a null <src>CountedConstPtr</src> from
    // another pointer.
    //
    CountedConstPtr() : SimpleCountedConstPtr<t>() {}

    // This constructor sets up a reference count for the <src>val</src>
    // pointer. By default, the data pointed to by <src>val</src> will
    // be deleted when it is no longer referenced. Passing in
    // <src>False</src> for <src>delit</src> will prevent the data
    // from being deleted when the reference count reaches zero.
    //
    // <note role=warning> After the counted pointer is initialized
    // the value should no longer be manipulated by the raw pointer
    // of type <src>t*</src>.
    // </note>
    //
    CountedConstPtr(t *val, Bool delit = True) : SimpleCountedConstPtr<t>(val,delit) {}

    // This copy constructor allows <src>CountedConstPtr</src>s to be
    // initialized from other <src>CountedConstPtr</src>s.
    //
    CountedConstPtr(const CountedConstPtr<t> &val) : SimpleCountedConstPtr<t>(val) {}

    // This assignment operator allows <src>CountedConstPtr</src>s to be
    // freely assigned to each other.
    //
    CountedConstPtr<t> &operator=(const CountedConstPtr<t> &val) {
	SimpleCountedConstPtr<t>::operator=(val);
	return *this;
    }

    // This assignment operator allows the object to which the current
    // <src>CountedConstPtr</src> points to be changed.
    //
    CountedConstPtr<t> &operator=(t *v) {
	SimpleCountedConstPtr<t>::operator=(v);
	return *this;
    }

    // This dereferencing operator behaves as expected; it returns the
    // pointer to the value being protected, and then its dereferencing
    // operator will be invoked as appropriate. If the pointer is
    // un-initialized (null), an exception will be thrown. The member
    // function
    // <linkto class="SimpleCountedConstPtr:null()const">null</linkto>()
    // can be used to catch such a condition in time.
    //
    // <thrown>
    // <li> ExcpError
    // </thrown>
    //
    const t *operator->() const {
	if (!this->ref) throw_Null_CountedPtr_dereference_error();
	return ((*this->ref).val);
    }
};

// <summary> Simple referenced counted pointer to non-constant data</summary>
// <use visibility=export>
// <reviewed reviewer="Friso Olnon" date="1995/03/15" tests="tCountedPtr" demos="">

// <prerequisite>
// <li> class <linkto class="SimpleCountedConstPtr:description">SimpleCountedConstPtr</linkto>
// </prerequisite>

// <synopsis>
// This class, like
// <linkto class="SimpleCountedConstPtr:description">SimpleCountedConstPtr</linkto>,
// does not define the <src>operator->()</src>. Thus it can point to
// simple data which does not have this operator defined. In contrast to
// <src>SimpleCountedConstPtr</src>, this class points at non-constant
// underlying data. The deletion properties are the same for both
// classes.
// </synopsis>

template<class t> class SimpleCountedPtr : virtual public SimpleCountedConstPtr<t> {
public:

    // This constructor allows for the creation of a null
    // <src>SimpleCountedPtr</src>. The assignment operator can be used
    // to assign a null <src>SimpleCountedPtr</src> from another pointer.
    //
    SimpleCountedPtr() : SimpleCountedConstPtr<t>() {}

    // This constructor sets up a reference count for the <src>val</src>
    // pointer.  By default, the data pointed to by <src>val</src>
    // will be deleted when it is no longer referenced. Passing in
    // <src>False</src> for <src>delit</src> will prevent the data
    // from being deleted when the reference count reaches zero.
    //
    // <note role=warning> After the counted pointer is initialized
    // the value should no longer be manipulated by the raw pointer
    // of type <src>t*</src>.
    // </note>
    //
    SimpleCountedPtr(t *val, Bool delit = True) : SimpleCountedConstPtr<t>(val,delit) {}

    // This copy constructor allows <src>SimpleCountedPtr</src>s to be
    // initialized from other <src>SimpleCountedPtr</src>s.
    //
    SimpleCountedPtr(const SimpleCountedPtr<t> &val) : SimpleCountedConstPtr<t>(val) {}

    // This assignment operator allows <src>SimpleCountedPtr</src>s to be
    // freely assigned to each other.
    //
    SimpleCountedPtr<t> &operator=(const SimpleCountedPtr<t> &val) {
	SimpleCountedConstPtr<t>::operator=(val);
	return *this;
    }

    // This assignment operator allows the object to which the current
    // <src>SimpleCountedPtr</src> points to be changed.
    //
    SimpleCountedPtr<t> &operator=(t *v) {
	SimpleCountedConstPtr<t>::operator=(v);
	return *this;
    }

    // The <src>SimpleCountedPtr</src> indirection operator simply
    // returns a reference to the value being protected. If the pointer
    // is un-initialized (null), an exception will be thrown. The member
    // function
    // <linkto class="SimpleCountedConstPtr:null()const">null</linkto>()
    // can be used to catch such a condition in time.
    //
    // <thrown>
    // <li> ExcpError
    // </thrown>
    //
    // <note role=tip> The address of the reference returned should
    // not be stored for later use.
    // </note>
    //
    // <group>
    const t &operator*() const {
	if (!this->ref) throw_Null_CountedPtr_dereference_error();
	return(*(*this->ref).val);
    }
    t &operator*() {
	if (!this->ref) throw_Null_CountedPtr_dereference_error();
	return(*(*this->ref).val);
    }
    // </group>

};

// <summary>Regular referenced counted pointer for non-constant data</summary>
// <use visibility=export>
// <reviewed reviewer="Friso Olnon" date="1995/03/15" tests="tCountedPtr" demos="">

// <prerequisite>
// <li> class <linkto class="SimpleCountedPtr:description">SimpleCountedPtr</linkto>
// <li> class <linkto class="CountedConstPtr:description">CountedConstPtr</linkto>
// </prerequisite>

// <synopsis>
// This class completes the lattice. It inherits much of the members
// which deal with non-constant data from
// <linkto class="SimpleCountedPtr:description">SimpleCountedPtr</linkto>,
// and it inherits the const <src>operator->()</src> from
// <linkto class="CountedConstPtr:description">CountedConstPtr</linkto>.
// What this class adds is the <src>operator->()</src> which returns a
// modifiable pointer.
// </synopsis>
//
template<class t> class CountedPtr : public SimpleCountedPtr<t>,
                                     public CountedConstPtr<t> {
public:

    // This constructor allows for the creation of a null
    // <src>CountedPtr</src>. The assignment operator can be used
    // to assign a null <src>CountedPtr</src> from another
    // pointer.
    //
    CountedPtr();

    // This constructor sets up a reference count for the
    // <src>val</src> pointer.  By default, the data pointed to by
    // <src>val</src> will be deleted when it is no longer
    // referenced. Passing in <src>False</src> for <src>delit</src> will
    // prevent the data from being deleted when the reference count
    // reaches zero.
    //
    // <note role=warning> After the counted pointer is initialized
    // the value should no longer be manipulated by the raw pointer of
    // type <src>t*</src>.
    // </note>
    //
    CountedPtr(t *val, Bool delit = True);

    // This copy constructor allows <src>CountedPtr</src>s to be
    // initialized from other <src>CountedPtr</src>s.
    //
    CountedPtr(const CountedPtr<t> &val);

    // This assignment operator allows <src>CountedPtr</src>s to be
    // freely assigned to each other.
    //
    CountedPtr<t> &operator=(const CountedPtr<t> &val) {
	SimpleCountedPtr<t>::operator=(val);
	return *this;
    }

    // This assignment operator allows the object to which the current
    // <src>CountedPtr</src> points to be changed.
    //
    CountedPtr<t> &operator=(t *v) {
	SimpleCountedPtr<t>::operator=(v);
	return *this;
    }

    // This dereferencing operator behaves as expected; it returns the
    // pointer to the value being protected, and then its dereferencing
    // operator will be invoked as appropriate. If the pointer is
    // un-initialized (null), an exception will be thrown. The member
    // function
    // <linkto class="SimpleCountedConstPtr:null()const">null</linkto>()
    // can be used to catch such a condition in time.
    //
    // <thrown>
    // <li> ExcpError
    // </thrown>
    //
    // <group>
    t *operator->() const {
	if (!this->ref) throw_Null_CountedPtr_dereference_error();
	return ((*this->ref).val);
    }
    t *operator->() {
	if (!this->ref) throw_Null_CountedPtr_dereference_error();
	return ((*this->ref).val);
    }
    // </group>
};

 } //#End casa namespace

// Keep this definition local

#else // when defined (USE_SHARED_PTR) is true


#include <boost/shared_ptr.hpp>

namespace casa { //#Begin casa namespace

//# Forward Declarations
template<class t> class SimpleCountedPtr;
template<class t> class SimpleCountedConstPtr;
template<class t> class CountedPtr;
template<class t> class CountedConstPtr;

// <summary> act on dereference error </summary>
// <synopsis>
// Global function that throws an exception. It is called by the
// member functions of the counted pointer classes when an
// un-initialized (null) pointer is followed.
// </synopsis>
// <group name=dereference_error>
void throw_Null_CountedPtr_dereference_error();
// </group>


// <summary>Simple referenced counted pointer for constant data</summary>
// <use visibility=export>
// <reviewed reviewer="Friso Olnon" date="1995/03/15" tests="tCountedPtr" demos="">

// <etymology>
// This class is <em>Simple</em> because it does not have the
// <src>operator->()</src> operator. This means that it puts less demands
// on the underlying type. It is <em>Counted</em> because it is reference
// counted, and it is <em>Const</em> because the underlying value is
// non-modifiable.
// </etymology>

// <synopsis>
// This class implements a simple reference counting mechanism. It
// allows <src>SimpleCountedConstPtr</src>s to be passed around freely,
// incrementing or decrementing the reference count as needed when one
// <src>SimpleCountedConstPtr</src> is assigned to another. When the
// reference count reaches zero the internal storage is deleted by
// default, but this behavior can be overridden.
//
// This class is used as a pointer to constant data. As such, it only
// has the subset of the
// <linkto class="CountedConstPtr:description">CountedConstPtr</linkto>
// functions which are relevant for constant data.
// </synopsis>

// <motivation>
// Reference counting
// </motivation>

template<class t>
class SimpleCountedConstPtr
{

protected:

    template <typename T>
    class Deleter {
    public:
        Deleter (Bool deleteIt) : reallyDeleteIt_p (deleteIt) {}
        void operator() (T * data) const { if (reallyDeleteIt_p) delete data;}
    private:
        Bool reallyDeleteIt_p;
    };

public:


    // This constructor allows for the creation of a null
    // <src>SimpleCountedConstPtr</src>. The assignment operator can be used
    // to assign a null <src>SimpleCountedConstPtr</src> from another
    // pointer.
    //
    SimpleCountedConstPtr() : pointerRep_p () {}

    // This constructor sets up a reference count for the <src>val</src>
    // pointer.  By default, the data pointed to by <src>val</src>
    // will be deleted when it is no longer referenced. Passing in
    // <src>False</src> for <src>delit</src> will prevent the data
    // from being deleted when the reference count reaches zero.
    //
    // <note role=warning> After the counted pointer is initialized
    // the value should no longer be manipulated by the raw pointer of
    // type <src>t*</src>.
    // </note>
    //
    SimpleCountedConstPtr(t *val, Bool delit = True)
    : pointerRep_p (val, Deleter<t> (delit))
    {}

    // This constructor sets up a reference count for the
    // <src>val</src> pointer.  Since <src>val</src> is a pointer to
    // constant data, the data will not be deleted when the reference
    // count reaches zero.
    //
    // <note role=tip> Since the constant data will NOT be cleaned up
    // when the reference count reaches zero, the use of this class for
    // pointers to constant data may not be desirable.
    // </note>
    //
    SimpleCountedConstPtr(const t *val)
    : pointerRep_p (val, Deleter<t> (False))
    {}

    // This copy constructor allows <src>SimpleCountedConstPtr</src>s to be
    // initialized from other <src>SimpleCountedConstPtr</src>s.
    //
//    SimpleCountedConstPtr(const SimpleCountedConstPtr<t> &val) : ref(val.ref) {
//	if (ref)
//	    (*ref).count++;
//    }

    // This destructor only deletes the really stored data when it was
    // initialized as deletable and the reference count is zero.
    //
    virtual ~SimpleCountedConstPtr() {}

    // The <src>SimpleCountedConstPtr</src> indirection operator simply
    // returns a reference to the value being protected. If the pointer
    // is un-initialized (null), an exception will be thrown. The member
    // function
    // <linkto class="SimpleCountedConstPtr:null()const">null</linkto>()
    // can be used to catch such a condition in time.
    //
    // <thrown>
    // <li> ExcpError
    // </thrown>
    //
    // <note role=tip> The address of the reference returned should
    // not be stored for later use.
    // </note>
    //
    const t &operator*() const {
	if (null()){
	    throw_Null_CountedPtr_dereference_error();
	}
	return pointerRep_p.operator* ();
    }

    // Equality operator which checks to see if two
    // <src>SimpleCountedConstPtr</src>s are pointing at the same thing.
    //
    Bool operator==(const SimpleCountedConstPtr<t> &other) const {
	return (this->get() == other.get() ? True : False);
    }

    // Non-equality operator which checks to see if two
    // <src>SimpleCountedConstPtr</src>s are not pointing at the same thing.
    //
    Bool operator!=(const SimpleCountedConstPtr<t> &other) const {
	return (this->get() != other.get()  ? True : False);
    }

    // This assignment operator allows the object to which the current
    // <src>SimpleCountedConstPtr</src> points to be changed.
    //
    SimpleCountedConstPtr<t> &
    operator=(t *v)
    {
        pointerRep_p = PointerRep (v);

        return * this;
    }

    // Sometimes it is useful to know if there is more than one
    // reference made. This is a way of getting that. Of course the point
    // of these classes is that this information is normally not required.
    //
    uInt nrefs() const {return pointerRep_p.use_count();}

    // Check to see if this <src>SimpleCountedConstPtr</src> is
    // un-initialized, null.
    //
    Bool null() const { return get() == 0;}

protected:

    typedef boost::shared_ptr<t> PointerRep;

    PointerRep pointerRep_p;

    t *
    get () const
    {
        return pointerRep_p.get();
    }
};

// <summary>Regular referenced counted pointer for constant data</summary>
// <use visibility=export>
// <reviewed reviewer="Friso Olnon" date="1995/03/15" tests="tCountedPtr" demos="">

// <prerequisite>
// <li> class <linkto class="SimpleCountedConstPtr:description">SimpleCountedConstPtr</linkto>
// </prerequisite>

// <synopsis>
// This class has the same objective as
// <linkto class="SimpleCountedConstPtr:description">SimpleCountedConstPtr</linkto>
// but it adds the <src>operator->()</src>. It still only contains a
// pointer whose underlying data cannot be changed. The destructor
// deletes the underlying data when the reference count reaches zero.
// </synopsis>

// <motivation>
// <src>operator->()</src> is useful, but not always available for
// every type.
// </motivation>

template<class t> class CountedConstPtr : virtual public SimpleCountedConstPtr<t> {
public:

    // This constructor allows for the creation of a null
    // <src>CountedConstPtr</src>. The assignment operator can be
    // used to assign a null <src>CountedConstPtr</src> from
    // another pointer.
    //
    CountedConstPtr() : SimpleCountedConstPtr<t>() {}

    // This constructor sets up a reference count for the <src>val</src>
    // pointer. By default, the data pointed to by <src>val</src> will
    // be deleted when it is no longer referenced. Passing in
    // <src>False</src> for <src>delit</src> will prevent the data
    // from being deleted when the reference count reaches zero.
    //
    // <note role=warning> After the counted pointer is initialized
    // the value should no longer be manipulated by the raw pointer
    // of type <src>t*</src>.
    // </note>
    //
    CountedConstPtr(t *val, Bool delit = True) : SimpleCountedConstPtr<t>(val,delit) {}

    // This copy constructor allows <src>CountedConstPtr</src>s to be
    // initialized from other <src>CountedConstPtr</src>s.
    //
    CountedConstPtr(const CountedConstPtr<t> &val) : SimpleCountedConstPtr<t>(val) {}

    // This assignment operator allows <src>CountedConstPtr</src>s to be
    // freely assigned to each other.
    //
    CountedConstPtr<t> &operator=(const CountedConstPtr<t> &val) {
	SimpleCountedConstPtr<t>::operator=(val);
	return *this;
    }

    // This assignment operator allows the object to which the current
    // <src>CountedConstPtr</src> points to be changed.
    //
    CountedConstPtr<t> &operator=(t *v) {
	SimpleCountedConstPtr<t>::operator=(v);
	return *this;
    }

    // This dereferencing operator behaves as expected; it returns the
    // pointer to the value being protected, and then its dereferencing
    // operator will be invoked as appropriate. If the pointer is
    // un-initialized (null), an exception will be thrown. The member
    // function
    // <linkto class="SimpleCountedConstPtr:null()const">null</linkto>()
    // can be used to catch such a condition in time.
    //
    // <thrown>
    // <li> ExcpError
    // </thrown>
    //
    const t
    *operator->() const {

	if (this->null()){
	    throw_Null_CountedPtr_dereference_error();
	}

	return this->get ();
    }
};

// <summary> Simple referenced counted pointer to non-constant data</summary>
// <use visibility=export>
// <reviewed reviewer="Friso Olnon" date="1995/03/15" tests="tCountedPtr" demos="">

// <prerequisite>
// <li> class <linkto class="SimpleCountedConstPtr:description">SimpleCountedConstPtr</linkto>
// </prerequisite>

// <synopsis>
// This class, like
// <linkto class="SimpleCountedConstPtr:description">SimpleCountedConstPtr</linkto>,
// does not define the <src>operator->()</src>. Thus it can point to
// simple data which does not have this operator defined. In contrast to
// <src>SimpleCountedConstPtr</src>, this class points at non-constant
// underlying data. The deletion properties are the same for both
// classes.
// </synopsis>

template<class t> class SimpleCountedPtr : virtual public SimpleCountedConstPtr<t> {
public:

    // This constructor allows for the creation of a null
    // <src>SimpleCountedPtr</src>. The assignment operator can be used
    // to assign a null <src>SimpleCountedPtr</src> from another pointer.
    //
    SimpleCountedPtr() : SimpleCountedConstPtr<t>() {}

    // This constructor sets up a reference count for the <src>val</src>
    // pointer.  By default, the data pointed to by <src>val</src>
    // will be deleted when it is no longer referenced. Passing in
    // <src>False</src> for <src>delit</src> will prevent the data
    // from being deleted when the reference count reaches zero.
    //
    // <note role=warning> After the counted pointer is initialized
    // the value should no longer be manipulated by the raw pointer
    // of type <src>t*</src>.
    // </note>
    //
    SimpleCountedPtr(t *val, Bool delit = True) : SimpleCountedConstPtr<t>(val,delit) {}

    // This copy constructor allows <src>SimpleCountedPtr</src>s to be
    // initialized from other <src>SimpleCountedPtr</src>s.
    //
    SimpleCountedPtr(const SimpleCountedPtr<t> &val) : SimpleCountedConstPtr<t>(val) {}

    // This assignment operator allows <src>SimpleCountedPtr</src>s to be
    // freely assigned to each other.
    //
    SimpleCountedPtr<t> &operator=(const SimpleCountedPtr<t> &val) {
	SimpleCountedConstPtr<t>::operator=(val);
	return *this;
    }

    // This assignment operator allows the object to which the current
    // <src>SimpleCountedPtr</src> points to be changed.
    //
    SimpleCountedPtr<t> &operator=(t *v) {
	SimpleCountedConstPtr<t>::operator=(v);
	return *this;
    }

    // The <src>SimpleCountedPtr</src> indirection operator simply
    // returns a reference to the value being protected. If the pointer
    // is un-initialized (null), an exception will be thrown. The member
    // function
    // <linkto class="SimpleCountedConstPtr:null()const">null</linkto>()
    // can be used to catch such a condition in time.
    //
    // <thrown>
    // <li> ExcpError
    // </thrown>
    //
    // <note role=tip> The address of the reference returned should
    // not be stored for later use.
    // </note>
    //
    // <group>
    const t &operator*() const {
	if (this->null()){
	    throw_Null_CountedPtr_dereference_error();
	}
	return * this->get();
    }
    t &operator*() {
	if (this->null()){
	    throw_Null_CountedPtr_dereference_error();
	}
	return * this->get();
    }
    // </group>

};

// <summary>Regular referenced counted pointer for non-constant data</summary>
// <use visibility=export>
// <reviewed reviewer="Friso Olnon" date="1995/03/15" tests="tCountedPtr" demos="">

// <prerequisite>
// <li> class <linkto class="SimpleCountedPtr:description">SimpleCountedPtr</linkto>
// <li> class <linkto class="CountedConstPtr:description">CountedConstPtr</linkto>
// </prerequisite>

// <synopsis>
// This class completes the lattice. It inherits much of the members
// which deal with non-constant data from
// <linkto class="SimpleCountedPtr:description">SimpleCountedPtr</linkto>,
// and it inherits the const <src>operator->()</src> from
// <linkto class="CountedConstPtr:description">CountedConstPtr</linkto>.
// What this class adds is the <src>operator->()</src> which returns a
// modifiable pointer.
// </synopsis>
//
template<class t> class CountedPtr : public SimpleCountedPtr<t>,
                                     public CountedConstPtr<t> {
public:

    // This constructor allows for the creation of a null
    // <src>CountedPtr</src>. The assignment operator can be used
    // to assign a null <src>CountedPtr</src> from another
    // pointer.
    //
    CountedPtr();

    // This constructor sets up a reference count for the
    // <src>val</src> pointer.  By default, the data pointed to by
    // <src>val</src> will be deleted when it is no longer
    // referenced. Passing in <src>False</src> for <src>delit</src> will
    // prevent the data from being deleted when the reference count
    // reaches zero.
    //
    // <note role=warning> After the counted pointer is initialized
    // the value should no longer be manipulated by the raw pointer of
    // type <src>t*</src>.
    // </note>
    //
    CountedPtr(t *val, Bool delit = True);

    // This copy constructor allows <src>CountedPtr</src>s to be
    // initialized from other <src>CountedPtr</src>s.
    //
    CountedPtr(const CountedPtr<t> &val);

    // This assignment operator allows <src>CountedPtr</src>s to be
    // freely assigned to each other.
    //
    CountedPtr<t> &operator=(const CountedPtr<t> &val) {
	SimpleCountedPtr<t>::operator=(val);
	return *this;
    }

    // This assignment operator allows the object to which the current
    // <src>CountedPtr</src> points to be changed.
    //
    CountedPtr<t> &operator=(t *v) {
	SimpleCountedPtr<t>::operator=(v);
	return *this;
    }

    // This dereferencing operator behaves as expected; it returns the
    // pointer to the value being protected, and then its dereferencing
    // operator will be invoked as appropriate. If the pointer is
    // un-initialized (null), an exception will be thrown. The member
    // function
    // <linkto class="SimpleCountedConstPtr:null()const">null</linkto>()
    // can be used to catch such a condition in time.
    //
    // <thrown>
    // <li> ExcpError
    // </thrown>
    //
    // <group>
    t *
    operator->() const {
	if (this->null()){
	    throw_Null_CountedPtr_dereference_error();
	}
	return this->get();
    }
    t *
    operator->() {
	if (this->null()){
	    throw_Null_CountedPtr_dereference_error();
	}

	return this->get();
    }
    // </group>
};

 } //#End casa namespace


#endif // defined (USE_BOOST_SHARED_PTR)


#ifndef CASACORE_NO_AUTO_TEMPLATES
#include <casa/Utilities/CountedPtr.tcc>
#endif //# CASACORE_NO_AUTO_TEMPLATES


#endif
