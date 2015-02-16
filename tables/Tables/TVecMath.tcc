//# TVecMath.cc: Global helper functions for table vector mathematics
//# Copyright (C) 1994,1995,1997
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

#ifndef TABLES_TVECMATH_TCC
#define TABLES_TVECMATH_TCC

#include <casacore/tables/Tables/TVecMath.h>
#include <casacore/tables/Tables/TVec.h>
#include <casacore/tables/Tables/TVecTemp.h>
#include <casacore/tables/Tables/TableError.h>
#include <casacore/casa/Arrays/ArrayError.h>
#include <casacore/casa/BasicMath/Math.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Add, subtract, multiply, divide table vector.
//# Define it for a vector and scalar, 2 vectors, 2 vectors with assign.
#define TVECMATHOPER(NAME,OP,OPA) \
template<class T> \
TabVecRep<T>& aips_name2(tabVecRepvalr,NAME) (const TabVecRep<T>& tv, \
                                              const T& val) \
{ \
    uInt nr = tv.nelements(); \
    TabVecTemp<T>& vec = *(TabVecTemp<T>*)tv.newVec(); \
    for (uInt i=0; i<nr; i++) { \
	tv.getVal (i, vec(i)); \
	vec(i) OPA val; \
    } \
    return vec; \
} \
template<class T> \
TabVecRep<T>& aips_name2(tabVecRepvall,NAME) (const T& val, \
                                              const TabVecRep<T>& tv) \
{ \
    uInt nr = tv.nelements(); \
    TabVecTemp<T>& vec = *(TabVecTemp<T>*)tv.newVec(); \
    T tmp; \
    for (uInt i=0; i<nr; i++) { \
	tv.getVal (i, tmp); \
	vec(i) = val OP tmp; \
    } \
    return vec; \
} \
template<class T> \
TabVecRep<T>& aips_name2(tabVecReptv,NAME) (const TabVecRep<T>& tvl, \
                                            const TabVecRep<T>& tvr) \
{ \
    uInt nr = tvr.nelements(); \
    tvl.validateConformance(nr); \
    TabVecTemp<T>& vec = *(TabVecTemp<T>*)tvl.newVec(); \
    for (uInt i=0; i<nr; i++) { \
	tvl.getVal (i, vec(i)); \
	vec(i) OPA tvr.value(i); \
    } \
    return vec; \
} \
template<class T> \
void aips_name2(tabVecRepvalass,NAME) (TabVecRep<T>& tv, const T& val) \
{ \
    uInt nr = tv.nelements(); \
    T tmp; \
    for (uInt i=0; i<nr; i++) { \
	tv.getVal (i, tmp); \
	tmp OPA val; \
	tv.putVal (i, tmp); \
    } \
} \
template<class T> \
void aips_name2(tabVecReptvass,NAME) (TabVecRep<T>& tvl, \
                                      const TabVecRep<T>& tvr) \
{ \
    uInt nr = tvr.nelements(); \
    tvl.validateConformance(nr); \
    T tmp; \
    for (uInt i=0; i<nr; i++) { \
	tvl.getVal (i, tmp); \
	tmp OPA tvr.value(i); \
	tvl.putVal (i, tmp); \
    } \
}

TVECMATHOPER(add,+,+=)
TVECMATHOPER(sub,-,-=)
TVECMATHOPER(tim,*,*=)
TVECMATHOPER(div,/,/=)



template<class T>
TabVecRep<T>& tabVecRepnegate(const TabVecRep<T>& tv)
{
    uInt nr = tv.nelements();
    TabVecTemp<T>& vec = *(TabVecTemp<T>*)tv.newVec();
    T tmp;
    for (uInt i=0; i<nr; i++) {
	tv.getVal (i, tmp);
	vec(i) = -tmp;
    }
    return vec;
}


template<class T>
void tabVecRepminmax (T& min, T& max, const TabVecRep<T>& tv)
{
    uInt nr = tv.nelements();
    if (nr == 0) {
	throw(ArrayError("void minMax(T& min, T& max, const TabVecRep<T>&) - "
			 "TabVecRep has no elements"));
    }
    T tmp;
    tv.getVal (0, min);
    max = min;
    for (uInt i=1; i<nr; i++) {
        tv.getVal (i, tmp);
	if (tmp < min)
	    min = tmp;
	if (tmp > max)
	    max = tmp;
    }
}


template<class T>
void tabVecRepindgen(TabVecRep<T>& tv, Int start, Int inc)
{
    uInt nr = tv.nelements();
    for (uInt i=0; i<nr; i++) {
	tv.putVal (i, start + i*inc);
    }
}


#define TABVECFUNC(NAME) \
template<class T> \
TabVecRep<T>& aips_name2(tabVecRep,NAME) (const TabVecRep<T>& tv) \
{ \
    uInt nr = tv.nelements(); \
    TabVecTemp<T>& vec = *(TabVecTemp<T>*)tv.newVec(); \
    T tmp; \
    for (uInt i=0; i<nr; i++) { \
	tv.getVal (i, tmp); \
	vec(i) = NAME(tmp); \
    } \
    return vec; \
}

#define TABVECFUNC2(NAME) \
template<class T> \
TabVecRep<T>& aips_name2(tabVecRep,NAME) (const TabVecRep<T>& tvl, \
                                          const TabVecRep<T>& tvr) \
{ \
    uInt nr = tvr.nelements(); \
    tvl.validateConformance(nr); \
    TabVecTemp<T>& vec = *(TabVecTemp<T>*)tvl.newVec(); \
    T tmpl, tmpr; \
    for (uInt i=0; i<nr; i++) { \
	tvl.getVal (i, tmpl); \
	tvr.getVal (i, tmpr); \
	vec(i) = NAME(tmpl, tmpr); \
    } \
    return vec; \
}

TABVECFUNC (cos)
TABVECFUNC (cosh)
TABVECFUNC (exp)
TABVECFUNC (log)
TABVECFUNC (log10)
TABVECFUNC2(pow)
TABVECFUNC (sin)
TABVECFUNC (sinh)
TABVECFUNC (sqrt)
TABVECFUNC (acos)
TABVECFUNC (asin)
TABVECFUNC (atan)
TABVECFUNC2(atan2)
TABVECFUNC (ceil)
TABVECFUNC (fabs)
TABVECFUNC (floor)
TABVECFUNC2(fmod)
TABVECFUNC (tan)
TABVECFUNC (tanh)


template<class T>
TabVecRep<T>& tabVecReppowd (const TabVecRep<T>& tv, const double& exp)
{
    uInt nr = tv.nelements();
    TabVecTemp<T>& vec = *(TabVecTemp<T>*)tv.newVec();
    T tmp;
    for (uInt i=0; i<nr; i++) {
	tv.getVal (i, tmp);
	vec(i) = pow(tmp, exp);
    }
    return vec;
}


template<class T>
T tabVecRepsum (const TabVecRep<T>& tv)
{
    uInt nr = tv.nelements();
    if (nr == 0) {
	throw(ArrayError("T sum(const TabVecRep<T>&) - "
			 "TabVecRep has no elements"));
    }
    T tmp, res;
    tv.getVal (0, res);
    for (uInt i=1; i<nr; i++) {
	tv.getVal (i, tmp);
	res += tmp;
    }
    return res;
}

template<class T>
T tabVecRepproduct (const TabVecRep<T>& tv)
{
    uInt nr = tv.nelements();
    if (nr == 0) {
	throw(ArrayError("T product(const TabVecRep<T>&) - "
			 "TabVecRep has no elements"));
    }
    T tmp, res;
    tv.getVal (0, res);
    for (uInt i=1; i<nr; i++) {
	tv.getVal (i, tmp);
	res *= tmp;
    }
    return res;
}


template<class T>
T tabVecRepinnerproduct (const TabVecRep<T>& tvl, const TabVecRep<T>& tvr)
{
    uInt nr = tvr.nelements();
    tvl.validateConformance(nr);
    T res = 0;
    for (uInt i=0; i<nr; i++) {
	res += tvl.value(i) * tvr.value(i);
    }
    return res;
}

template<class T>
T tabVecRepnorm (const TabVecRep<T>& tv)
{
    uInt nr = tv.nelements();
    T tmp;
    T res = 0;
    for (uInt i=0; i<nr; i++) {
	tv.getVal (i, tmp);
	res += tmp*tmp;
    }
    return sqrt(res);
}

//# the 3-space cross/vector product
template <class T>
TabVecRep<T>& tabVecRepcrossproduct (const TabVecRep<T>& tvl,
				     const TabVecRep<T>& tvr)
{
    tvl.validateConformance(3);
    tvr.validateConformance(3);
    TabVecTemp<T>& vec = *(TabVecTemp<T>*)tvl.newVec();
    vec(0) = tvl.value(1) * tvr.value(2)  -  tvl.value(2) * tvr.value(1);
    vec(1) = tvl.value(2) * tvr.value(0)  -  tvl.value(0) * tvr.value(2);
    vec(2) = tvl.value(0) * tvr.value(1)  -  tvl.value(1) * tvr.value(0);
    return vec;
}

} //# NAMESPACE CASACORE - END


#endif
