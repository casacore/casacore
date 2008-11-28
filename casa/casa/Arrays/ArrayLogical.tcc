//# ArrayLogical.cc: Element by element logical operations on arrays.
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

#include <casa/Arrays/ArrayLogical.h>
#include <casa/Arrays/ArrayUtil.h>
#include <casa/Arrays/ArrayError.h>
//# For scalar near() functions.
#include <casa/BasicMath/Functors.h>

namespace casa { //# NAMESPACE CASA - BEGIN

template<class T>
Bool allEQ (const Array<T> &l, const Array<T> &r)
{
    if (l.conform(r) == False) {
        return False;
    }
    uInt ntotal = l.nelements();
    Bool deleteL, deleteR;
    const T *ls = l.getStorage(deleteL);
    const T *rs = r.getStorage(deleteR);
    Bool retval = True;
    for (uInt i=0; i < ntotal; i++) {
	if (! (ls[i] == rs[i])) {
	    retval = False;
	    break;
	}
    }
    l.freeStorage(ls, deleteL);
    r.freeStorage(rs, deleteR);
    return retval;
}

#define ARRLOG_B_ALLFUNC_AA(ALLFUNC,OP,STRALLFUNC) \
template<class T> \
Bool ALLFUNC (const Array<T> &l, const Array<T> &r) \
{ \
    if (l.conform(r) == False) { \
	throw(ArrayConformanceError("::" STRALLFUNC "(const Array<T> &, " \
			    "const Array<T> &) - arrays do not conform")); \
    } \
    uInt ntotal = l.nelements(); \
    Bool deleteL, deleteR; \
    const T *ls = l.getStorage(deleteL); \
    const T *rs = r.getStorage(deleteR); \
\
    Bool retval = True; \
    for (uInt i=0; i < ntotal; i++) { \
	if (! (ls[i] OP rs[i])) { \
	    retval = False; \
	    break; \
	} \
    } \
    l.freeStorage(ls, deleteL); \
    r.freeStorage(rs, deleteR); \
    return retval; \
}

ARRLOG_B_ALLFUNC_AA ( allLE,  <,  "allLE" )
ARRLOG_B_ALLFUNC_AA ( allLT,  <,  "allLT" )
ARRLOG_B_ALLFUNC_AA ( allGE,  >=, "allGE" )
ARRLOG_B_ALLFUNC_AA ( allGT,  >,  "allGT" )
ARRLOG_B_ALLFUNC_AA ( allNE,  !=, "allNE" )
ARRLOG_B_ALLFUNC_AA ( allAND, &&, "allAND" )
ARRLOG_B_ALLFUNC_AA ( allOR,  ||, "allOR" )


#define ARRLOG_B_ANYFUNC_AA(ANYFUNC,OP,STRANYFUNC) \
template<class T> \
Bool ANYFUNC (const Array<T> &l, const Array<T> &r) \
{ \
    if (l.conform(r) == False) { \
	throw(ArrayConformanceError("::" STRANYFUNC "(const Array<T> &, " \
			    "const Array<T> &) - arrays do not conform")); \
    } \
    uInt ntotal = l.nelements(); \
    Bool deleteL, deleteR; \
    const T *ls = l.getStorage(deleteL); \
    const T *rs = r.getStorage(deleteR); \
\
    Bool retval = False; \
    for (uInt i=0; i < ntotal; i++) { \
	if (ls[i] OP rs[i]) { \
	    retval = True; \
	    break; \
	} \
    } \
    l.freeStorage(ls, deleteL); \
    r.freeStorage(rs, deleteR); \
    return retval; \
}


ARRLOG_B_ANYFUNC_AA ( anyLE,  <=, "anyLE" )
ARRLOG_B_ANYFUNC_AA ( anyLT,  <,  "anyLT" )
ARRLOG_B_ANYFUNC_AA ( anyGE,  >=, "anyGE" )
ARRLOG_B_ANYFUNC_AA ( anyGT,  >,  "anyGT" )
ARRLOG_B_ANYFUNC_AA ( anyEQ,  ==, "anyEQ" )
ARRLOG_B_ANYFUNC_AA ( anyNE,  !=, "anyNE" )
ARRLOG_B_ANYFUNC_AA ( anyAND, &&, "anyAND" )
ARRLOG_B_ANYFUNC_AA ( anyOR,  ||, "anyOR" )


#define ARRLOG_LA_OP_AA(OP,STROP) \
template<class T> \
LogicalArray operator OP (const Array<T> &l, const Array<T> &r) \
{ \
    if (l.conform(r) == False) { \
	throw(ArrayConformanceError( \
            "::" STROP "(const Array<T> &, const Array<T> &)" \
            " - arrays do not conform")); \
    } \
\
    Bool deleteL, deleteR; \
    const T *lStorage = l.getStorage(deleteL); \
    const T *ls = lStorage; \
    const T *rStorage = r.getStorage(deleteR); \
    const T *rs = rStorage; \
\
    LogicalArray retarr (l.shape()); \
    Bool deleteRet; \
    Bool *retStorage = retarr.getStorage(deleteRet); \
    Bool *rets = retStorage; \
\
    uInt ntotal = l.nelements(); \
    while (ntotal--) { \
	*rets = (*ls OP *rs) ? True : False; \
	rets++; \
        ls++; \
        rs++; \
    } \
\
    retarr.putStorage(retStorage, deleteRet); \
    l.freeStorage(lStorage, deleteL); \
    r.freeStorage(rStorage, deleteR); \
\
    return retarr; \
}


ARRLOG_LA_OP_AA ( <=, "<=" )
ARRLOG_LA_OP_AA ( <, "<" )
ARRLOG_LA_OP_AA ( >=, ">=" )
ARRLOG_LA_OP_AA ( >, ">" )
ARRLOG_LA_OP_AA ( ==, "==" )
ARRLOG_LA_OP_AA ( !=, "!=" )
ARRLOG_LA_OP_AA ( &&, "&&" )
ARRLOG_LA_OP_AA ( ||, "||" )


template<class T>
LogicalArray operator ! (const Array<T> &array)
{
    LogicalArray result (array.shape());

    Bool resultDelete;
    LogicalArrayElem *resultStorage =
        result.getStorage(resultDelete);
    LogicalArrayElem *resultS = resultStorage;

    Bool arrayDelete;
    const T *arrayStorage = array.getStorage(arrayDelete);
    const T *arrayS = arrayStorage;

    uInt ntotal = result.nelements();
    while (ntotal--) {
        *resultS = ((*arrayS) ? False : True);
        resultS++;
        arrayS++;
    }

    result.putStorage(resultStorage, resultDelete);
    array.freeStorage(arrayStorage, arrayDelete);

    return result;
}


#define ARRLOG_B_ALLFUNC_AS(ALLFUNC,OP) \
template<class T> Bool ALLFUNC (const Array<T> &array, const T &val) \
{ \
    uInt ntotal = array.nelements(); \
    Bool deleteIt; \
    const T *as = array.getStorage(deleteIt); \
\
    Bool retval = True; \
    for (uInt i=0; i < ntotal; i++) { \
	if (! (as[i] OP val)) { \
	    retval = False; \
	    break; \
	} \
    } \
\
    array.freeStorage(as, deleteIt); \
    return retval; \
}

#define ARRLOG_B_ALLFUNC_SA(ALLFUNC,OP) \
template<class T> \
Bool ALLFUNC (const T &val, const Array<T> &array) \
{ \
    uInt ntotal = array.nelements(); \
    Bool deleteIt; \
    const T *as = array.getStorage(deleteIt); \
\
    Bool retval = True; \
    for (uInt i=0; i < ntotal; i++) { \
	if (! (val OP as[i])) { \
	    retval = False; \
	    break; \
	} \
    } \
\
    array.freeStorage(as, deleteIt); \
    return retval; \
}


ARRLOG_B_ALLFUNC_AS ( allLE,  <= )
ARRLOG_B_ALLFUNC_SA ( allLE,  <= )
ARRLOG_B_ALLFUNC_AS ( allLT,  <  )
ARRLOG_B_ALLFUNC_SA ( allLT,  <  )
ARRLOG_B_ALLFUNC_AS ( allGE,  >= )
ARRLOG_B_ALLFUNC_SA ( allGE,  >= )
ARRLOG_B_ALLFUNC_AS ( allGT,  >  )
ARRLOG_B_ALLFUNC_SA ( allGT,  >  )
ARRLOG_B_ALLFUNC_AS ( allEQ,  == )
ARRLOG_B_ALLFUNC_SA ( allEQ,  == )
ARRLOG_B_ALLFUNC_AS ( allNE,  != )
ARRLOG_B_ALLFUNC_SA ( allNE,  != )


template<class T>
Bool allAND (const Array<T> &array, const T &val)
{
    if (!val) {
        return False;
    } else {

        uInt ntotal = array.nelements();
        Bool deleteIt;
        const T *as = array.getStorage(deleteIt);

        Bool retval = True;
        for (uInt i=0; i < ntotal; i++) {
	    if (! as[i]) {
	        retval = False;
	        break;
	    }
        }

        array.freeStorage(as, deleteIt);
        return retval;
    }
}

template<class T>
Bool allAND (const T &val, const Array<T> &array)
{
    if (!val) {
        return False;
    } else {

        uInt ntotal = array.nelements();
        Bool deleteIt;
        const T *as = array.getStorage(deleteIt);

        Bool retval = True;
        for (uInt i=0; i < ntotal; i++) {
	    if (! as[i]) {
	        retval = False;
	        break;
	    }
        }

        array.freeStorage(as, deleteIt);
        return retval;
    }
}


template<class T>
Bool allOR (const Array<T> &array, const T &val)
{
    if (val) {
        return True;
    } else {

        uInt ntotal = array.nelements();
        Bool deleteIt;
        const T *as = array.getStorage(deleteIt);

        Bool retval = True;
        for (uInt i=0; i < ntotal; i++) {
	    if (! as[i]) {
	        retval = False;
	        break;
	    }
        }

        array.freeStorage(as, deleteIt);
        return retval;
    }
}

template<class T>
Bool allOR (const T &val, const Array<T> &array)
{
    if (val) {
        return True;
    } else {

        uInt ntotal = array.nelements();
        Bool deleteIt;
        const T *as = array.getStorage(deleteIt);

        Bool retval = True;
        for (uInt i=0; i < ntotal; i++) {
	    if (! as[i]) {
	        retval = False;
	        break;
	    }
        }

        array.freeStorage(as, deleteIt);
        return retval;
    }
}


#define ARRLOG_B_ANYFUNC_AS(ANYFUNC,OP) \
template<class T> Bool ANYFUNC (const Array<T> &array, const T &val) \
{ \
    uInt ntotal = array.nelements(); \
    Bool deleteIt; \
    const T *as = array.getStorage(deleteIt); \
\
    Bool retval = False; \
    for (uInt i=0; i < ntotal; i++) { \
	if (as[i] OP val) { \
	    retval = True; \
	    break; \
	} \
    } \
\
    array.freeStorage(as, deleteIt); \
    return retval; \
}

#define ARRLOG_B_ANYFUNC_SA(ANYFUNC,OP) \
template<class T> \
Bool ANYFUNC (const T &val, const Array<T> &array) \
{ \
    uInt ntotal = array.nelements(); \
    Bool deleteIt; \
    const T *as = array.getStorage(deleteIt); \
\
    Bool retval = False; \
    for (uInt i=0; i < ntotal; i++) { \
	if (val OP as[i]) { \
	    retval = True; \
	    break; \
	} \
    } \
\
    array.freeStorage(as, deleteIt); \
    return retval; \
}


ARRLOG_B_ANYFUNC_AS ( anyLE,  <= )
ARRLOG_B_ANYFUNC_SA ( anyLE,  <= )
ARRLOG_B_ANYFUNC_AS ( anyLT,  <  )
ARRLOG_B_ANYFUNC_SA ( anyLT,  <  )
ARRLOG_B_ANYFUNC_AS ( anyGE,  >= )
ARRLOG_B_ANYFUNC_SA ( anyGE,  >= )
ARRLOG_B_ANYFUNC_AS ( anyGT,  >  )
ARRLOG_B_ANYFUNC_SA ( anyGT,  >  )
ARRLOG_B_ANYFUNC_AS ( anyEQ,  == )
ARRLOG_B_ANYFUNC_SA ( anyEQ,  == )
ARRLOG_B_ANYFUNC_AS ( anyNE,  != )
ARRLOG_B_ANYFUNC_SA ( anyNE,  != )


template<class T>
Bool anyAND (const Array<T> &array, const T &val)
{
    if (!val) {
        return False;
    } else {

        uInt ntotal = array.nelements();
        Bool deleteIt;
        const T *as = array.getStorage(deleteIt);

        Bool retval = False;
        for (uInt i=0; i < ntotal; i++) {
	    if (as[i]) {
	        retval = True;
	        break;
	    }
        }

        array.freeStorage(as, deleteIt);
        return retval;
    }
}

template<class T>
Bool anyAND (const T &val, const Array<T> &array)
{
    if (!val) {
        return False;
    } else {

        uInt ntotal = array.nelements();
        Bool deleteIt;
        const T *as = array.getStorage(deleteIt);

        Bool retval = False;
        for (uInt i=0; i < ntotal; i++) {
	    if (as[i]) {
	        retval = True;
	        break;
	    }
        }

        array.freeStorage(as, deleteIt);
        return retval;
    }
}


template<class T>
Bool anyOR (const Array<T> &array, const T &val)
{
    if (val) {
        return True;
    } else {

        uInt ntotal = array.nelements();
        Bool deleteIt;
        const T *as = array.getStorage(deleteIt);

        Bool retval = False;
        for (uInt i=0; i < ntotal; i++) {
	    if (as[i]) {
	        retval = True;
	        break;
	    }
        }

        array.freeStorage(as, deleteIt);
        return retval;
    }
}

template<class T>
Bool anyOR (const T &val, const Array<T> &array)
{
    if (val) {
        return True;
    } else {

        uInt ntotal = array.nelements();
        Bool deleteIt;
        const T *as = array.getStorage(deleteIt);

        Bool retval = False;
        for (uInt i=0; i < ntotal; i++) {
	    if (as[i]) {
	        retval = True;
	        break;
	    }
        }

        array.freeStorage(as, deleteIt);
        return retval;
    }
}


template<class T>
LogicalArray operator && (const Array<T> &array, const T &val)
{
    LogicalArray retarr (array.shape());

    if (!val) {
        retarr = False;
    } else {

        Bool deleteIt;
        const T *aStorage = array.getStorage(deleteIt);
        const T *as = aStorage;

        Bool deleteRet;
        Bool *retStorage = retarr.getStorage(deleteRet);
        Bool *rets = retStorage;

        uInt ntotal = array.nelements();
        while (ntotal--) {
	    *rets = (*as) ? True : False;
            rets++;
            as++;
        }

        retarr.putStorage(retStorage, deleteRet);
        array.freeStorage(aStorage, deleteIt);
    }

    return retarr;
}


template<class T>
LogicalArray operator && (const T &val, const Array<T> &array)
{
    LogicalArray retarr (array.shape());

    if (!val) {
        retarr = False;
    } else {

        Bool deleteIt;
        const T *aStorage = array.getStorage(deleteIt);
        const T *as = aStorage;

        Bool deleteRet;
        Bool *retStorage = retarr.getStorage(deleteRet);
        Bool *rets = retStorage;

        uInt ntotal = array.nelements();
        while (ntotal--) {
	    *rets = (*as) ? True : False;
            rets++;
            as++;
        }

        retarr.putStorage(retStorage, deleteRet);
        array.freeStorage(aStorage, deleteIt);
    }

    return retarr;
}


template<class T>
LogicalArray operator || (const Array<T> &array, const T &val)
{
    LogicalArray retarr (array.shape());

    if (val) {
        retarr = True;
    } else {

        Bool deleteIt;
        const T *aStorage = array.getStorage(deleteIt);
        const T *as = aStorage;

        Bool deleteRet;
        Bool *retStorage = retarr.getStorage(deleteRet);
        Bool *rets = retStorage;

        uInt ntotal = array.nelements();
        while (ntotal--) {
	    *rets = (*as) ? True : False;
            rets++;
            as++;
        }

        retarr.putStorage(retStorage, deleteRet);
        array.freeStorage(aStorage, deleteIt);
    }

    return retarr;
}


template<class T>
LogicalArray operator || (const T &val, const Array<T> &array)
{
    LogicalArray retarr (array.shape());

    if (val) {
        retarr = True;
    } else {

        Bool deleteIt;
        const T *aStorage = array.getStorage(deleteIt);
        const T *as = aStorage;

        Bool deleteRet;
        Bool *retStorage = retarr.getStorage(deleteRet);
        Bool *rets = retStorage;

        uInt ntotal = array.nelements();
        while (ntotal--) {
	    *rets = (*as) ? True : False;
            rets++;
            as++;
        }

        retarr.putStorage(retStorage, deleteRet);
        array.freeStorage(aStorage, deleteIt);
    }

    return retarr;
}


#define ARRLOG_LA_OP_AS(OP) \
template<class T> \
LogicalArray operator OP (const Array<T> &array, const T &val) \
{ \
    Bool deleteIt; \
    const T *aStorage = array.getStorage(deleteIt); \
    const T *as = aStorage; \
\
    LogicalArray retarr (array.shape()); \
    Bool deleteRet; \
    Bool *retStorage = retarr.getStorage(deleteRet); \
    Bool *rets = retStorage; \
\
    uInt ntotal = array.nelements(); \
    while (ntotal--) { \
	*rets = (*as OP val) ? True : False; \
        rets++; \
        as++; \
    } \
\
    retarr.putStorage(retStorage, deleteRet); \
    array.freeStorage(aStorage, deleteIt); \
\
    return retarr; \
}


#define ARRLOG_LA_OP_SA(OP) \
template<class T> \
LogicalArray operator OP (const T &val, const Array<T> &array) \
{ \
    Bool deleteIt; \
    const T *aStorage = array.getStorage(deleteIt); \
    const T *as = aStorage; \
\
    LogicalArray retarr (array.shape()); \
    Bool deleteRet; \
    Bool *retStorage = retarr.getStorage(deleteRet); \
    Bool *rets = retStorage; \
\
    uInt ntotal = array.nelements(); \
    while (ntotal--) { \
	*rets = (val OP *as) ? True : False; \
        rets++; \
        as++; \
    } \
\
    retarr.putStorage(retStorage, deleteRet); \
    array.freeStorage(aStorage, deleteIt); \
\
    return retarr; \
}


ARRLOG_LA_OP_AS ( <= )
ARRLOG_LA_OP_SA ( <= )
ARRLOG_LA_OP_AS ( < )
ARRLOG_LA_OP_SA ( < )
ARRLOG_LA_OP_AS ( >= )
ARRLOG_LA_OP_SA ( >= )
ARRLOG_LA_OP_AS ( > )
ARRLOG_LA_OP_SA ( > )
ARRLOG_LA_OP_AS ( == )
ARRLOG_LA_OP_SA ( == )
ARRLOG_LA_OP_AS ( != )
ARRLOG_LA_OP_SA ( != )


template<class T>
LogicalArray isNaN (const Array<T> &array)
{
  LogicalArray result(array.shape());
  if (array.contiguousStorage()) {
    std::transform (array.begin(),  array.end(),  result.cbegin(), IsNaN<T>());
  } else {
    std::transform (array.cbegin(), array.cend(), result.cbegin(), IsNaN<T>());
  }
  return result;
}

template<class T>
LogicalArray isInf (const Array<T> &array)
{
  LogicalArray result(array.shape());
  if (array.contiguousStorage()) {
    std::transform (array.cbegin(), array.cend(), result.cbegin(), IsInf<T>());
  } else {
    std::transform (array.begin(),  array.end(),  result.cbegin(), IsInf<T>());
  }
  return result;
}

template<class T>
LogicalArray near (const Array<T> &l, const Array<T>& r, Double tol)
{
  if (! l.shape().isEqual(r.shape())) {
	throw ArrayConformanceError(
            "::near(const Array<T> &, const Array<T> &, Double tol=1.0e-5)"
            " - arrays do not conform");
  }
  LogicalArray res(l.shape());
  if (l.contiguousStorage()) {
    if (r.contiguousStorage()) {
      std::transform (l.cbegin(), l.cend(), r.cbegin(),
                      res.cbegin(), Near<T>(tol));
    } else {
      std::transform (l.cbegin(), l.cend(), r.begin(),
                      res.cbegin(), Near<T>(tol));
    }
  } else {
    if (r.contiguousStorage()) {
      std::transform (r.cbegin(), r.cend(), l.begin(),
                      res.cbegin(), Near<T>(tol));
    } else {
      std::transform (r.begin(),  r.end(),  l.begin(),
                      res.cbegin(), Near<T>(tol));
    }
  }
  return res;
}


template<class T> LogicalArray nearAbs(const Array<T> &l, const Array<T> &r,
              				    Double tol)
{
  if (! l.shape().isEqual(r.shape())) {
	throw ArrayConformanceError(
            "::nearAbs(const Array<T> &, const Array<T> &, Double tol=1.0e-5)"
            " - arrays do not conform");
  }
  LogicalArray res(l.shape());
  if (l.contiguousStorage() && r.contiguousStorage()) {
    std::transform (l.cbegin(), l.cend(), r.cbegin(),
                    res.cbegin(), NearAbs<T>(tol));
  } else {
    std::transform (r.begin(),  r.end(),  l.begin(),
                    res.cbegin(), NearAbs<T>(tol));
  }
  return res;
}

template<class T> LogicalArray near (const Array<T> &array, const T &val,
				     Double tol)
{
    Bool deleteIt;
    const T *aStorage = array.getStorage(deleteIt);
    const T *as = aStorage;

    LogicalArray retarr (array.shape());
    Bool deleteRet;
    Bool *retStorage = retarr.getStorage(deleteRet);
    Bool *rets = retStorage;

    uInt ntotal = array.nelements();
    while (ntotal--) {
	*rets = near(*as, val, tol);
        rets++;
        as++;
    }

    retarr.putStorage(retStorage, deleteRet);
    array.freeStorage(aStorage, deleteIt);

    return retarr;
}

template<class T> LogicalArray near (const T &val, const Array<T> &array,
				      Double tol)
{
    Bool deleteIt;
    const T *aStorage = array.getStorage(deleteIt);
    const T *as = aStorage;

    LogicalArray retarr (array.shape());
    Bool deleteRet;
    Bool *retStorage = retarr.getStorage(deleteRet);
    Bool *rets = retStorage;

    uInt ntotal = array.nelements();
    while (ntotal--) {
	*rets = near(val, *as, tol);
        rets++;
        as++;
    }

    retarr.putStorage(retStorage, deleteRet);
    array.freeStorage(aStorage, deleteIt);

    return retarr;
}

template<class T> LogicalArray nearAbs (const Array<T> &array, const T &val,
				     Double tol)
{
    Bool deleteIt;
    const T *aStorage = array.getStorage(deleteIt);
    const T *as = aStorage;

    LogicalArray retarr (array.shape());
    Bool deleteRet;
    Bool *retStorage = retarr.getStorage(deleteRet);
    Bool *rets = retStorage;

    uInt ntotal = array.nelements();
    while (ntotal--) {
	*rets = nearAbs(*as, val, tol);
        rets++;
        as++;
    }

    retarr.putStorage(retStorage, deleteRet);
    array.freeStorage(aStorage, deleteIt);

    return retarr;
}

template<class T> LogicalArray nearAbs (const T &val, const Array<T> &array,
				      Double tol)
{
    Bool deleteIt;
    const T *aStorage = array.getStorage(deleteIt);
    const T *as = aStorage;

    LogicalArray retarr (array.shape());
    Bool deleteRet;
    Bool *retStorage = retarr.getStorage(deleteRet);
    Bool *rets = retStorage;

    uInt ntotal = array.nelements();
    while (ntotal--) {
	*rets = nearAbs(val, *as, tol);
        rets++;
        as++;
    }

    retarr.putStorage(retStorage, deleteRet);
    array.freeStorage(aStorage, deleteIt);

    return retarr;
}

template<class T> Bool allNear (const Array<T> &l, const Array<T> &r,
				Double tol)
{
    if (l.conform(r) == False) {
	throw(ArrayConformanceError("::allNear(const Array<T> &, const Array<T>"
			    " &, Double tol) - arrays do not conform"));
    }
    uInt ntotal = l.nelements();
    Bool deleteL, deleteR;
    const T *ls = l.getStorage(deleteL);
    const T *rs = r.getStorage(deleteR);

    Bool retval = True;
    for (uInt i=0; i < ntotal; i++) {
	if (! near(ls[i], rs[i], tol)) {
	    retval = False;
	    break;
	}
    }
    l.freeStorage(ls, deleteL);
    r.freeStorage(rs, deleteR);
    return retval;
}

template<class T> Bool allNearAbs (const Array<T> &l, const Array<T> &r,
				   Double tol)
{
    if (l.conform(r) == False) {
	throw(ArrayConformanceError("::allNear(const Array<T> &, const Array<T>"
			    " &, Double tol) - arrays do not conform"));
    }
    uInt ntotal = l.nelements();
    Bool deleteL, deleteR;
    const T *ls = l.getStorage(deleteL);
    const T *rs = r.getStorage(deleteR);

    Bool retval = True;
    for (uInt i=0; i < ntotal; i++) {
	if (! nearAbs(ls[i], rs[i], tol)) {
	    retval = False;
	    break;
	}
    }
    l.freeStorage(ls, deleteL);
    r.freeStorage(rs, deleteR);
    return retval;
}

template<class T> Bool allNear (const Array<T> &array, const T &val, Double tol)
{
    uInt ntotal = array.nelements();
    Bool deleteIt;
    const T *as = array.getStorage(deleteIt);

    Bool retval = True;
    for (uInt i=0; i < ntotal; i++) {
	if (! near(as[i], val, tol)) {
	    retval = False;
	    break;
	}
    }

    array.freeStorage(as, deleteIt);
    return retval;
}

template<class T> Bool allNear (const T &val, const Array<T> &array, Double tol)
{
    uInt ntotal = array.nelements();
    Bool deleteIt;
    const T *as = array.getStorage(deleteIt);

    Bool retval = True;
    for (uInt i=0; i < ntotal; i++) {
	if (! near(val, as[i], tol)) {
	    retval = False;
	    break;
	}
    }

    array.freeStorage(as, deleteIt);
    return retval;
}

template<class T> Bool allNearAbs (const Array<T> &array, const T &val,
				   Double tol)
{
    uInt ntotal = array.nelements();
    Bool deleteIt;
    const T *as = array.getStorage(deleteIt);

    Bool retval = True;
    for (uInt i=0; i < ntotal; i++) {
	if (! nearAbs(as[i], val, tol)) {
	    retval = False;
	    break;
	}
    }

    array.freeStorage(as, deleteIt);
    return retval;
}

template<class T> Bool allNearAbs (const T &val, const Array<T> &array,
				   Double tol)
{
    uInt ntotal = array.nelements();
    Bool deleteIt;
    const T *as = array.getStorage(deleteIt);

    Bool retval = True;
    for (uInt i=0; i < ntotal; i++) {
	if (! nearAbs(val, as[i], tol)) {
	    retval = False;
	    break;
	}
    }

    array.freeStorage(as, deleteIt);
    return retval;
}

template<class T> Bool anyNear (const Array<T> &l, const Array<T> &r, 
				Double tol)
{
    if (l.conform(r) == False) {
	throw(ArrayConformanceError("::anyNear(const Array<T> &, const Array<T>"
			    " &, Double tol) - arrays do not conform"));
    }
    uInt ntotal = l.nelements();
    Bool deleteL, deleteR;
    const T *ls = l.getStorage(deleteL);
    const T *rs = r.getStorage(deleteR);

    Bool retval = False;
    for (uInt i=0; i < ntotal; i++) {
	if (near(ls[i],rs[i], tol)) {
	    retval = True;
	    break;
	}
    }
    l.freeStorage(ls, deleteL);
    r.freeStorage(rs, deleteR);
    return retval;
}

template<class T> Bool anyNearAbs (const Array<T> &l, const Array<T> &r,
				   Double tol)
{
    if (l.conform(r) == False) {
	throw(ArrayConformanceError("::anyNear(const Array<T> &, const Array<T>"
			    " &, Double tol) - arrays do not conform"));
    }
    uInt ntotal = l.nelements();
    Bool deleteL, deleteR;
    const T *ls = l.getStorage(deleteL);
    const T *rs = r.getStorage(deleteR);

    Bool retval = False;
    for (uInt i=0; i < ntotal; i++) {
	if (nearAbs(ls[i],rs[i], tol)) {
	    retval = True;
	    break;
	}
    }
    l.freeStorage(ls, deleteL);
    r.freeStorage(rs, deleteR);
    return retval;
}

template<class T> Bool anyNear (const Array<T> &array, const T &val, Double tol)
{
    uInt ntotal = array.nelements();
    Bool deleteIt;
    const T *as = array.getStorage(deleteIt);

    Bool retval = False;
    for (uInt i=0; i < ntotal; i++) {
	if (near(as[i], val, tol)) {
	    retval = True;
	    break;
	}
    }

    array.freeStorage(as, deleteIt);
    return retval;
}

template<class T> Bool anyNear (const T &val, const Array<T> &array, Double tol)
{
    uInt ntotal = array.nelements();
    Bool deleteIt;
    const T *as = array.getStorage(deleteIt);

    Bool retval = False;
    for (uInt i=0; i < ntotal; i++) {
	if (near(val, as[i], tol)) {
	    retval = True;
	    break;
	}
    }

    array.freeStorage(as, deleteIt);
    return retval;
}

template<class T> Bool anyNearAbs (const Array<T> &array, const T &val,
				   Double tol)
{
    uInt ntotal = array.nelements();
    Bool deleteIt;
    const T *as = array.getStorage(deleteIt);

    Bool retval = False;
    for (uInt i=0; i < ntotal; i++) {
	if (nearAbs(as[i], val, tol)) {
	    retval = True;
	    break;
	}
    }

    array.freeStorage(as, deleteIt);
    return retval;
}

template<class T> Bool anyNearAbs (const T &val, const Array<T> &array,
				   Double tol)
{
    uInt ntotal = array.nelements();
    Bool deleteIt;
    const T *as = array.getStorage(deleteIt);

    Bool retval = False;
    for (uInt i=0; i < ntotal; i++) {
	if (nearAbs(val, as[i], tol)) {
	    retval = True;
	    break;
	}
    }

    array.freeStorage(as, deleteIt);
    return retval;
}

template<class T> uInt ntrue (const Array<T> &array)
{
    uInt ntotal = array.nelements();
    Bool deleteIt;
    const T *as = array.getStorage(deleteIt);
    uInt retval = 0;
    for (uInt i=0; i<ntotal; i++) {
	if (as[i]) {
	    retval++;
	}
    }
    array.freeStorage(as, deleteIt);
    return retval;
}

template<class T> Array<uInt> partialNTrue (const Array<T>& array,
					    const IPosition& collapseAxes)
{
  const IPosition& shape = array.shape();
  uInt ndim = shape.nelements();
  if (ndim == 0) {
    return Array<uInt>();
  }
  IPosition resShape, incr;
  Int nelemCont = 0;
  uInt stax = partialFuncHelper (nelemCont, resShape, incr, shape,
				 collapseAxes);
  Array<uInt> result (resShape);
  result = 0;
  Bool deleteData, deleteRes;
  const T* arrData = array.getStorage (deleteData);
  const T* data = arrData;
  uInt* resData = result.getStorage (deleteRes);
  uInt* res = resData;
  // Find out how contiguous the data is, i.e. if some contiguous data
  // end up in the same output element.
  // const tells if any data are contiguous.
  // stax gives the first non-contiguous axis.
  // no gives the number of contiguous elements.
  Bool cont = True;
  uInt n0 = nelemCont;
  Int incr0 = incr(0);
  if (nelemCont <= 1) {
    cont = False;
    n0 = shape(0);
    stax = 1;
  }
  // Loop through all data and assemble as needed.
  IPosition pos(ndim, 0);
  while (True) {
    if (cont) {
      uInt tmp = *res;
      for (uInt i=0; i<n0; i++) {
	if (*data++) {
	  tmp++;
	}
      }
      *res = tmp;
    } else {
      for (uInt i=0; i<n0; i++) {
	if (*data++) {
	  (*res)++;
	}
	res += incr0;
      }
    }
    uInt ax;
    for (ax=stax; ax<ndim; ax++) {
      res += incr(ax);
      if (++pos(ax) < shape(ax)) {
	break;
      }
      pos(ax) = 0;
    }
    if (ax == ndim) {
      break;
    }
  }
  array.freeStorage (arrData, deleteData);
  result.putStorage (resData, deleteRes);
  return result;
}

template<class T> Array<uInt> partialNFalse (const Array<T>& array,
					     const IPosition& collapseAxes)
{
  Array<uInt> result = partialNTrue (array, collapseAxes);
  uInt nr = result.nelements();
  if (nr > 0) {
    uInt factor = array.nelements() / nr;
    Bool deleteRes;
    uInt* res = result.getStorage (deleteRes);
    for (uInt i=0; i<nr; i++) {
      res[i] = factor - res[i];
    }
    result.putStorage (res, deleteRes);
  }
  return result;
}

} //# NAMESPACE CASA - END

