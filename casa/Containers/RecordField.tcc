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

#ifndef CASA_RECORDFIELD_TCC
#define CASA_RECORDFIELD_TCC


#include <casacore/casa/Containers/RecordField.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

template<typename T>
RecordFieldPtr<T>::RecordFieldPtr()
: parent_p     (nullptr),
  fieldNumber_p(-1)
{
    // Nothing
}

template<typename T>
RecordFieldPtr<T>::RecordFieldPtr (RecordInterface& record, Int whichField)
{
    attachToRecord (record, whichField);
}

template<typename T>
RecordFieldPtr<T>::RecordFieldPtr (RecordInterface& record,
				   const RecordFieldId& id)
{
    attachToRecord (record, record.idToNumber (id));
}

template<typename T>
void RecordFieldPtr<T>::attachToRecord (RecordInterface& record,
					const RecordFieldId& id)
{
    attachToRecord (record, record.idToNumber (id));
}
template<typename T>
void RecordFieldPtr<T>::attachToRecord (RecordInterface& record,
					Int whichField)
{
    parent_p      = &record;
    fieldNumber_p = whichField;
    fieldName_p   = record.name(whichField);
    get(); // check type
}

template<typename T>
void RecordFieldPtr<T>::detach()
{
    parent_p      = nullptr;
    fieldNumber_p = -1;
    fieldName_p.clear();
}

template<typename T>
T& RecordFieldPtr<T>::operator*()
{
    parent_p->makeUnique();
    return const_cast<T&>(get());
}


template<>
inline const Table* RecordFieldPtr<Table>::get_typed_ptr(RecordInterface* record, Int fieldNumber) const
{
  validate();
  return static_cast<const Table*>(record->get_pointer(fieldNumber, TpTable));
}

template<>
inline const Record* RecordFieldPtr<Record>::get_typed_ptr(RecordInterface* record, Int fieldNumber)const
{
  validate();
  return static_cast<const Record*>(record->get_pointer(fieldNumber, TpRecord, "Record"));
}

template<>
inline const TableRecord* RecordFieldPtr<TableRecord>::get_typed_ptr(RecordInterface* record, Int fieldNumber) const
{
  validate();
  return static_cast<const TableRecord*>(record->get_pointer(fieldNumber, TpRecord, "TableRecord"));
}

template<typename T>
inline const T* RecordFieldPtr<T>::get_typed_ptr(RecordInterface* record, Int fieldNumber) const
{
  validate();
  return static_cast<const T*>(record->get_pointer(fieldNumber, whatType<T>()));
}

template<typename T>
inline void RecordFieldPtr<T>::define (const T& value)
{
  validate();
  parent_p->defineDataField (fieldNumber_p, whatType<T>(), &value);
}

template<>
inline void RecordFieldPtr<TableRecord>::define (const TableRecord& value)
{
  validate();
  parent_p->defineDataField (fieldNumber_p, TpRecord, &value);
}

template<typename T>
const String& RecordFieldPtr<T>::comment() const
{
  validate();
  return parent_p->comment (fieldNumber_p);
}

template<typename T>
void RecordFieldPtr<T>::setComment (const String& comment)
{
  validate();
  parent_p->setComment (fieldNumber_p, comment);
}

} //# NAMESPACE CASACORE - END


#endif
