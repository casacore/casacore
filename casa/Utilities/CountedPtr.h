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

#include <casacore/casa/aips.h>

#if (defined(AIPS_CXX11) || (defined(__APPLE_CC__) && __APPLE_CC__ > 5621))
#include <memory>
#define SHARED_PTR std::shared_ptr
#define DYNAMIC_POINTER_CAST std::dynamic_pointer_cast
#define CONST_POINTER_CAST std::const_pointer_cast
#define STATIC_POINTER_CAST std::static_pointer_cast
#else
#include <tr1/memory>
#define SHARED_PTR std::tr1::shared_ptr
#define DYNAMIC_POINTER_CAST std::tr1::dynamic_pointer_cast
#define CONST_POINTER_CAST std::tr1::const_pointer_cast
#define STATIC_POINTER_CAST std::tr1::static_pointer_cast
#endif

namespace casacore { //#Begin casa namespace


// <summary> act on dereference error </summary>
// <synopsis>
// Global function that throws an exception. It is called by the
// member functions of the counted pointer classes when an
// un-initialized (null) pointer is followed.
// </synopsis>
// <group name=dereference_error>
void throw_Null_CountedPtr_dereference_error();
// </group>


// <summary>Referenced counted pointer for constant data</summary>
// <use visibility=export>
// <reviewed reviewer="Friso Olnon" date="1995/03/15" tests="tCountedPtr" demos="">

// <etymology>
// This class is <em>Counted</em> because it is reference counted.
// </etymology>

// <synopsis>
// This class implements a reference counting mechanism. It
// allows <src>CountedPtr</src>s to be passed around freely,
// incrementing or decrementing the reference count as needed when one
// <src>CountedPtr</src> is assigned to another. When the
// reference count reaches zero the internal storage is deleted by
// default, but this behavior can be overridden.
//
// Internally the class uses std::shared_ptr to be thread-safe. Note that
// tr1 is used if the compiler does not support C++11 yet.
// </synopsis>

// <motivation>
// Reference counting
// </motivation>

template<class t>
class CountedPtr
{

protected:
    // Helper class to make deletion of object optional.
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
    // <src>CountedPtr</src>. The assignment operator can be used
    // to assign a null <src>CountedPtr</src> from another
    // pointer.
    //
    CountedPtr() : pointerRep_p () {}

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
    CountedPtr(t *val, Bool delit = True)
    : pointerRep_p (val, Deleter<t> (delit))
    {}
    
    // This copy constructor allows <src>CountedPtr</src>s to be
    // initialized from other <src>CountedPtr</src>s for which the pointer TP*
    // is convertible to T*.
    template<typename TP>
    CountedPtr(const CountedPtr<TP>& that)
      : pointerRep_p(that.pointerRep_p)
    {}

    // Create from a shared_ptr.
    CountedPtr (const SHARED_PTR<t>& rep)
      : pointerRep_p (rep)
    {}

    // This destructor only deletes the really stored data when it was
    // initialized as deletable and the reference count is zero.
    ~CountedPtr() {}

    // This assignment operator allows <src>CountedPtr</src>s to be
    // copied from other <src>CountedPtr</src>s for which the pointer TP*
    // is convertible to t*.
    template<typename TP>
    CountedPtr<t>& operator=(const CountedPtr<TP>& that)
    {
      pointerRep_p = that.pointerRep_p;
      return *this;
    }

    // Reset the pointer.
    // <group>
    void reset (t *val, Bool delit=True)
      { pointerRep_p = PointerRep (val, Deleter<t>(delit)); }
    void reset()
      { pointerRep_p.reset(); }
    // </group>

    // The <src>CountedPtr</src> indirection operator simply
    // returns a reference to the value being protected. If the pointer
    // is un-initialized (null), an exception will be thrown. The member
    // function
    // <linkto class="CountedPtr:null()const">null</linkto>()
    // can be used to catch such a condition in time.
    // <note role=tip> The address of the reference returned should
    // not be stored for later use.
    // </note>
    t &operator*() const {
	if (null()){
	    throw_Null_CountedPtr_dereference_error();
	}
	return pointerRep_p.operator* ();
    }

    // This dereferencing operator behaves as expected; it returns the
    // pointer to the value being protected, and then its dereferencing
    // operator will be invoked as appropriate. If the pointer is
    // un-initialized (null), an exception will be thrown. The member
    // function
    // <linkto class="CountedPtr:null()const">null</linkto>()
    // can be used to catch such a condition in time.
    t *operator->() const {
	return get ();
    }

    // Get the underlying pointer.
    t* get () const {
        return pointerRep_p.get();
    }

    // Equality operator which checks to see if two
    // <src>CountedPtr</src>s are pointing at the same thing.
    Bool operator==(const CountedPtr<t> &other) const {
	return (get() == other.get());
    }
    //# Note: use of const void* gives ambiguius overload error.
    Bool operator==(int ptr) const {
        return (ptr == 0  &&  get() == 0);
    }

    // Non-equality operator which checks to see if two
    // <src>CountedPtr</src>s are not pointing at the same thing.
    Bool operator!=(const CountedPtr<t> &other) const {
	return (get() != other.get()  ? True : False);
    }
    //# Note: use of const void* gives ambiguius overload error.
    Bool operator!=(int ptr) const {
        return (ptr != 0  ||  get() != 0);
    }

    // This assignment operator allows the object to which the current
    // <src>CountedPtr</src> points to be changed.
    CountedPtr<t> &
    operator=(t *v)
    {
        pointerRep_p = PointerRep (v);
        return * this;
    }

    // Cast functions.
    // <group>
    template<typename U>
    CountedPtr<U> static_ptr_cast() const
      { return CountedPtr<U> (STATIC_POINTER_CAST<U> (pointerRep_p)); }
    template<typename U>
    CountedPtr<U> const_ptr_cast() const
      { return CountedPtr<U> (CONST_POINTER_CAST<U> (pointerRep_p)); }
    template<typename U>
    CountedPtr<U> dynamic_ptr_cast() const
      { return CountedPtr<U> (DYNAMIC_POINTER_CAST<U> (pointerRep_p)); }
    // </group>

    // Sometimes it is useful to know if there is more than one
    // reference made. This is a way of getting that. Of course the point
    // of these classes is that this information is normally not required.
    uInt nrefs() const
      { return pointerRep_p.use_count(); }

    // Check to see if this <src>CountedPtr</src> is
    // un-initialized, null.
    Bool null() const
      { return get() == 0; }

    // Test if it contains a valid pointer.
    operator bool() const
      { return get() != 0; }

private:
    // Make all types of CountedPtr a friend for the templated operator=.
    template<typename TP> friend class CountedPtr;

    typedef SHARED_PTR<t> PointerRep;

    PointerRep pointerRep_p;
};

// A shared_ptr is used as implementation.
inline Bool countedPtrShared()
  { return True; }

// Cast the CountedPtr from one pointer type to another.
template<typename T, typename U>
CountedPtr<T> static_pointer_cast (const CountedPtr<U>& that)
  { return that.template static_ptr_cast<T>(); }
template<typename T, typename U>
CountedPtr<T> const_pointer_cast (const CountedPtr<U>& that)
  { return that.template const_ptr_cast<T>(); }
template<typename T, typename U>
CountedPtr<T> dynamic_pointer_cast (const CountedPtr<U>& that)
  { return that.template dynamic_ptr_cast<T>(); }


} //#End casa namespace


#ifndef CASACORE_NO_AUTO_TEMPLATES
#include <casacore/casa/Utilities/CountedPtr.tcc>
#endif //# CASACORE_NO_AUTO_TEMPLATES

#endif
