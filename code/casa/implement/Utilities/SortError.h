//# SortError.h: Error classes for the sort class
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

#if !defined (AIPS_SORTERROR_H)
#define AIPS_SORTERROR_H

#include <aips/aips.h>
#include <aips/Exceptions/Error.h>


// <summary> Generic Sort exception </summary>
// <use visibility=export>
// <reviewed reviewer="Friso Olnon" date="1995/03/01" tests="" demos="">
// <prerequisite>
//   <li> <linkto class=Sort>Sort</linkto>
// </prerequisite>
// <synopsis> 
// SortError is the generic Sort exception; catching this one means catching
// all Sort exceptions. Note that you have to catch AipsError to catch
// all possible exceptions.
// </synopsis> 
class SortError : public AipsError {
public:
    SortError ();
    SortError (const String&);
    ~SortError ();
};


// <summary> Invalid data type used for this sort key </summary>
// <use visibility=export>
// <reviewed reviewer="Friso Olnon" date="1995/03/01" tests="" demos="">
// <prerequisite>
//   <li> <linkto class=Sort>Sort</linkto>
// </prerequisite>
// <synopsis> 
// Invalid data type used for this sort key
// </synopsis> 
class SortInvDT : public SortError {
public:
    SortInvDT ();
    ~SortInvDT ();
};

// <summary> Invalid increment used for this sort key </summary>
// <use visibility=export>
// <reviewed reviewer="Friso Olnon" date="1995/03/01" tests="" demos="">
// <prerequisite>
//   <li> <linkto class=Sort>Sort</linkto>
// </prerequisite>
// <synopsis> 
// Invalid increment used for this sort key.
// The increment should be >= size of sort key.
// </synopsis> 
class SortInvIncr : public SortError {
public:
    SortInvIncr ();
    ~SortInvIncr ();
};

// <summary> No data array given to Sort constructor. </summary>
// <use visibility=export>
// <reviewed reviewer="Friso Olnon" date="1995/03/01" tests="" demos="">
// <prerequisite>
//   <li> <linkto class=Sort>Sort</linkto>
// </prerequisite>
// <synopsis> 
// No data array has been given to Sort constructor.
// </synopsis> 
class SortNoData : public SortError {
public:
    SortNoData ();
    ~SortNoData ();
};

// <summary> Invalid sort option given to routine dosort. </summary>
// <use visibility=export>
// <reviewed reviewer="Friso Olnon" date="1995/03/01" tests="" demos="">
// <prerequisite>
//   <li> <linkto class=Sort>Sort</linkto>
// </prerequisite>
// <synopsis> 
// Invalid sort option has been given to routine dosort.
// </synopsis> 
class SortInvOpt : public SortError {
public:
    SortInvOpt ();
    ~SortInvOpt ();
};

#endif























