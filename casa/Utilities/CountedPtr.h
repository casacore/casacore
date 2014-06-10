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

#if defined AIPS_CXX11
#include <memory>
///#elif defined HAVE_BOOST
///#include <boost/shared_ptr.hpp>
#else
#include <tr1/memory>
#endif

namespace casa { //#Begin casa namespace


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
// allows <src>CountedConstPtr</src>s to be passed around freely,
// incrementing or decrementing the reference count as needed when one
// <src>CountedConstPtr</src> is assigned to another. When the
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
    //
    CountedPtr(t *val, Bool delit = True)
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
    CountedPtr(const t *val)
    : pointerRep_p (val, Deleter<t> (False))
    {}

    // This copy constructor allows <src>CountedPtr</src>s to be
    // initialized from other <src>CountedPtr</src>s.
    //
//    CountedPtr(const CountedPtr<t> &val) : ref(val.ref) {
//	if (ref)
//	    (*ref).count++;
//    }

    // This destructor only deletes the really stored data when it was
    // initialized as deletable and the reference count is zero.
    //
    ~CountedPtr() {}

    // The <src>CountedPtr</src> indirection operator simply
    // returns a reference to the value being protected. If the pointer
    // is un-initialized (null), an exception will be thrown. The member
    // function
    // <linkto class="CountedPtr:null()const">null</linkto>()
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
    //
    // <thrown>
    // <li> ExcpError
    // </thrown>
    //
    t *operator->() const {

	if (null()){
	    throw_Null_CountedPtr_dereference_error();
	}

	return get ();
    }

    // Equality operator which checks to see if two
    // <src>CountedPtr</src>s are pointing at the same thing.
    //
    Bool operator==(const CountedPtr<t> &other) const {
	return (get() == other.get() ? True : False);
    }

    // Non-equality operator which checks to see if two
    // <src>CountedPtr</src>s are not pointing at the same thing.
    //
    Bool operator!=(const CountedPtr<t> &other) const {
	return (get() != other.get()  ? True : False);
    }

    // This assignment operator allows the object to which the current
    // <src>CountedPtr</src> points to be changed.
    //
    CountedPtr<t> &
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

    // Check to see if this <src>CountedPtr</src> is
    // un-initialized, null.
    //
    Bool null() const { return get() == 0;}

protected:

#ifdef AIPS_CXX11
    typedef std::shared_ptr<t> PointerRep;
  ///#elif HAVE_BOOST
  ///    typedef boost::shared_ptr<t> PointerRep;
#else
    typedef std::tr1::shared_ptr<t> PointerRep;
#endif

    PointerRep pointerRep_p;

    t *
    get () const
    {
        return pointerRep_p.get();
    }
};

inline Bool countedPtrShared()
  { return True; }

} //#End casa namespace


#ifndef CASACORE_NO_AUTO_TEMPLATES
#include <casa/Utilities/CountedPtr.tcc>
#endif //# CASACORE_NO_AUTO_TEMPLATES

#endif
