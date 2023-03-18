//# TableRecordData.cc: The representation of the data in a TableRecord field
//# Copyright (C) 2023
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


//# Includes
#include <casacore/tables/Tables/TableRecordData.h>
#include <casacore/tables/Tables/TableRecord.h>
#include <casacore/tables/Tables/TableKeyword.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

  RecordData<TableRecord>::RecordData (const TableRecord& rec)
    : RecordDataBase (TpRecord),
      itsRecPtr      (new TableRecord(rec))
  {
    setData (itsRecPtr.get());
  }

  void RecordData<TableRecord>::doCopyData (const void* rec)
  {
    *itsRecPtr = *static_cast<const TableRecord*>(rec);
  }
  
  void RecordData<TableRecord>::printData (std::ostream& os, const String& indent,
                                           Int maxNrValues)
  {
    os << '{' << endl;
    itsRecPtr->print (os, maxNrValues, indent+"  ");
    os << indent << '}';
  }
  
  void RecordData<TableRecord>::putData (AipsIO& os, Bool empty, const TableAttr*)
  {
    if (empty) {
      os << *itsRecPtr;
    } else {
      itsRecPtr->putData (os, TableAttr());
    }
  }

  void RecordData<TableRecord>::getData (AipsIO& os, uInt version, Bool empty,
                                         const TableAttr*)
  {
    if (empty) {
      os >> *itsRecPtr;
    } else {
      itsRecPtr->getData (os, version, TableAttr());
    }
  }

  void RecordData<TableRecord>::doClear()
  {
    itsRecPtr.reset();
  }


  RecordData<TableKeyword>::RecordData (const TableKeyword& tab)
    : RecordDataBase (TpTable),
      itsTabPtr      (new TableKeyword(tab))
  {
    setData (itsTabPtr.get());
  }

  void RecordData<TableKeyword>::doCopyData (const void* tab)
  {
    *itsTabPtr = *static_cast<const TableKeyword*>(tab);
  }
  
  void RecordData<TableKeyword>::printData (std::ostream& os, const String&, Int)
  {
    os << "Table " << itsTabPtr->tableName();
  }
  
  void RecordData<TableKeyword>::putData (AipsIO& os, Bool, const TableAttr* parentAttr)
  {
    os << itsTabPtr->tableName (*parentAttr);
  }

  void RecordData<TableKeyword>::getData (AipsIO& os, uInt, Bool,
                                          const TableAttr* parentAttr)
  {
    String name;
    os >> name;
    itsTabPtr->set (name, *parentAttr);
  }

  void RecordData<TableKeyword>::doClear()
  {
    itsTabPtr.reset();
  }


} //# NAMESPACE CASACORE - END
