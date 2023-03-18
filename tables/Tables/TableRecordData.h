//# TableRecordData.h: The representation of the data in a TableRecord field
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


#ifndef TABLES_TABLERECORDDATA_H
#define TABLES_TABLERECORDDATA_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/casa/Containers/RecordData.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

  //# Forward Declarations
  class TableRecord;
  class TableKeyword;

  // <summary>
  // The representation of a nested TableRecord in a TableRecord field
  // </summary>
  // <use visibility=local>
  // <synopsis>
  // RecordData is a templated class to hold the data of a Record field.
  // This specialization holds a nested TableRecord.
  // </synopsis>
  template<>
  class RecordData<TableRecord> : public RecordDataBase
  {
  public:
    RecordData (const TableRecord&);
    ~RecordData() override = default;
    void doCopyData (const void* rec) override;
    void printData (std::ostream& os, const String& indent,
                    Int maxNrValues) override;
    void putData (AipsIO& os, Bool empty, const TableAttr*) override;
    void getData (AipsIO& os, uInt version, Bool empty, const TableAttr*) override;
    void doClear() override;
  private:
    //# Cannot include TableRecord.h, so a pointer is needed
    std::unique_ptr<TableRecord> itsRecPtr;
  };

  // <summary>
  // The representation of a TableKeyword (a table) in a TableRecord field
  // </summary>
  template<>
  class RecordData<TableKeyword> : public RecordDataBase
  {
  public:
    RecordData (const TableKeyword&);
    ~RecordData() override = default;
    void doCopyData (const void* tab) override;
    void printData (std::ostream& os, const String& indent,
                    Int maxNrValues) override;
    void putData (AipsIO& os, Bool empty, const TableAttr*) override;
    void getData (AipsIO& os, uInt version, Bool empty, const TableAttr*) override;
    void doClear() override;
  private:
    //# Cannot include TableKeyword.h, so a pointer is needed
    std::unique_ptr<TableKeyword> itsTabPtr;
  };


} //# NAMESPACE CASACORE - END

#endif
