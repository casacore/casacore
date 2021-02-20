//# RecordField.cc: Access to an individual field from a record
//# Copyright (C) 1995,1996,1997,1999
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

#ifndef CASA_RECORDFIELD_TCC
#define CASA_RECORDFIELD_TCC


#include <casacore/casa/Containers/RecordField.h>
#include <casacore/casa/Utilities/Assert.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

template<class T>
RecordFieldPtr<T>::RecordFieldPtr()
: fieldPtr_p   (nullptr),
  parent_p     (nullptr),
  fieldNumber_p(-1)
{
    // Nothing
}

template<class T>
RecordFieldPtr<T>::RecordFieldPtr (RecordInterface& record, Int whichField)
{
    attachToRecord (record, whichField);
}

template<class T>
RecordFieldPtr<T>::RecordFieldPtr (RecordInterface& record,
				   const RecordFieldId& id)
{
    attachToRecord (record, record.idToNumber (id));
}

template<class T>
void RecordFieldPtr<T>::attachToRecord (RecordInterface& record,
					const RecordFieldId& id)
{
    attachToRecord (record, record.idToNumber (id));
}
template<class T>
void RecordFieldPtr<T>::attachToRecord (RecordInterface& record,
					Int whichField)
{
    parent_p      = &record;
    fieldNumber_p = whichField;
    // Cast to correct type, because a void* is returned.
    // This cast is fully safe.
    fieldPtr_p = (T*)(attachRecordFieldPtr (parent_p, whichField, 
					    whatType(static_cast<T*>(nullptr)), static_cast<T*>(nullptr)));
}

template<class T>
void RecordFieldPtr<T>::detach()
{
    fieldPtr_p    = nullptr;
    parent_p      = nullptr;
    fieldNumber_p = -1;
}

template<class T>
T& RecordFieldPtr<T>::operator*()
{
    parent_p->makeUnique();
    return *fieldPtr_p;
}

template<class T>
void RecordFieldPtr<T>::define (const T& value)
{
    defineRecordFieldPtr (parent_p, fieldNumber_p, whatType(static_cast<T*>(0)), &value);
}

template<class T>
const String& RecordFieldPtr<T>::comment() const
{
    return parent_p->comment (fieldNumber_p);
}

template<class T>
void RecordFieldPtr<T>::setComment (const String& comment)
{
    parent_p->setComment (fieldNumber_p, comment);
}

} //# NAMESPACE CASACORE - END


#endif
