//# <ClassFileName.h>: this defines <ClassName>, which ...
//# Copyright (C) 1996,2000
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
//#
//# $Id$

#if !defined(AIPS_RECORD_FIELD_WRITER)
#define AIPS_RECORD_FIELD_WRITER

#include <aips/aips.h>
#include <aips/Containers/RecordField.h>
#include <aips/Arrays/Array.h>

// <summary> Record field writer
// </summary>

// <use visibility=local>   or   <use visibility=export>

// <reviewed reviewer="" date="yyyy/mm/dd" tests="" demos="">
// </reviewed>

// <prerequisite>
//   <li> SomeClass
//   <li> SomeOtherClass
//   <li> some concept
// </prerequisite>
//
// <etymology>
// </etymology>
//
// <synopsis>
// </synopsis>
//
// <example>
// </example>
//
// <motivation>
// </motivation>
//
// <templating arg=T>
//    <li>
//    <li>
// </templating>
//
// <thrown>
//    <li>
//    <li>
// </thrown>
//
// <todo asof="yyyy/mm/dd">
//   <li> add this feature
//   <li> fix this bug
//   <li> start discussion of this possible extension
// </todo>

class RecordFieldWriter
{
public:
    virtual ~RecordFieldWriter();
    virtual void writeField() = 0;
};

// <summary> Record field copier
// </summary>

template<class outType, class inType> 
class RecordFieldCopier : public RecordFieldWriter
{
public:
    RecordFieldCopier(RecordInterface &outRecord, 
		      RecordFieldId whichOutField,
		      const RecordInterface &inRecord, 
		      RecordFieldId whichInField);
    void copy() {*out_p = outType(*in_p);}
    virtual void writeField();
private:
    RecordFieldPtr<outType>   out_p;
    RORecordFieldPtr<inType> in_p;
};

// <summary> Unequal shape copier
// </summary>

template<class T> class UnequalShapeCopier : public RecordFieldWriter
{
public:
    UnequalShapeCopier(RecordInterface &outRecord, 
		       RecordFieldId whichOutField,
		       const RecordInterface &inRecord, 
		       RecordFieldId whichInField);
    virtual void writeField();
private:
    RecordFieldPtr<Array<T> >   out_p;
    RORecordFieldPtr<Array<T> > in_p;
};

// <summary> Multi field writer
// </summary>

class MultiRecordFieldWriter
{
public:
  void addWriter(RecordFieldWriter *fromNew);
  void copy();
  ~MultiRecordFieldWriter();
private:
  // Make faster by having the RecordFieldCopiers split out so straight copying
  // is inline.
  PtrBlock<RecordFieldWriter *> writers_p;
};

#endif
