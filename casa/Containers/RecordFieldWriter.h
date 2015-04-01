//# RecordFieldWriter.h: Various copiers to move fields between records.
//# Copyright (C) 1996,2000,2001
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

#ifndef CASA_RECORDFIELDWRITER_H
#define CASA_RECORDFIELDWRITER_H

#include <casacore/casa/aips.h>
#include <casacore/casa/Containers/RecordField.h>
#include <casacore/casa/Arrays/Array.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

// <summary> Record field writer.  Base class for the copiers.
// </summary>

// <use visibility=local>

// <reviewed reviewer="" date="yyyy/mm/dd" tests="" demos="">
// </reviewed>

// <etymology>
// These classes write values to a field or fields in a record.
// </etymology>
//
// <synopsis>
// These classes are used in the ms2sdfits conversion code.
// It might be better if they were moved there.
// </synopsis>
//
// <motivation>
// It was useful to set up a number of copiers and invoke them as appropriate
// via a single function call.  Some copiers may be more complicate than a
// direct field to field copy.
// </motivation>
//
// <todo asof="2001/07/10">
//   <li> Either make this generally useful here or move them out of containers
//   <li> fully document this
// </todo>

class RecordFieldWriter
{
public:
    virtual ~RecordFieldWriter();
    virtual void writeField() = 0;
};

// <summary> Record field copier.  Copies field to field as is.
// </summary>

// <use visibility=local>

// <reviewed reviewer="" date="yyyy/mm/dd" tests="" demos="">
// </reviewed>

// <etymology>
// Copies a field from a record to another record with a field
// of the same type.
// </etymology>

// <motivation>
// This type of copy can be inlined.
// </motivation>

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

// <summary> Unequal shape copier.
// </summary>

// <use visibility=local>

// <reviewed reviewer="" date="yyyy/mm/dd" tests="" demos="">
// </reviewed>

// <etymology>
// Copy fields where the two fields fields do not have the same shape,
// however, the number of elements must match.  Copying is done element
// by element in vector order.
// </etymology>
//
// <motivation>
// Sometimes the shapes need to change even though the number of elements
// stays the same.
// </motivation>
//

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

// <summary> Multi field writer.  Copy many fields with a single call.
// </summary>

// <use visibility=local>

// <reviewed reviewer="" date="yyyy/mm/dd" tests="" demos="">
// </reviewed>

// <etymology>
// This class contains other copiers and copies multiple fields at a time.
// </etymology>
//
// <motivation>
// It was useful to set up a number of copiers and invoke them as appropriate
// via a single function call.
// </motivation>
//

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


} //# NAMESPACE CASACORE - END

#ifndef CASACORE_NO_AUTO_TEMPLATES
#include <casacore/casa/Containers/RecordFieldWriter.tcc>
#endif //# CASACORE_NO_AUTO_TEMPLATES
#endif
