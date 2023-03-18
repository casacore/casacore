//# TableRecordRep.cc: A hierarchical collection of named fields of various types
//# Copyright (C) 1996,1997,1999,2000,2001,2002
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

#include <casacore/tables/Tables/TableRecordRep.h>
#include <casacore/tables/Tables/TableRecord.h>
#include <casacore/tables/Tables/TableKeyword.h>
#include <casacore/tables/Tables/TableAttr.h>
#include <casacore/tables/Tables/Table.h>
#include <casacore/tables/Tables/TableDesc.h>
#include <casacore/casa/Arrays/Array.h>
#include <casacore/casa/Arrays/ArrayError.h>
#include <casacore/casa/Arrays/IPosition.h>
#include <casacore/casa/IO/AipsIO.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Exceptions/Error.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

  TableRecordRep::TableRecordRep (const RecordDesc& description)
    : RecordRep()
  {
    restructure (description, True);
  }

  TableRecordRep::TableRecordRep (const TableRecordRep& other)
    : RecordRep()
  {
    restructure (other.desc_p, False);
    copy_other (other);
  }

  TableRecordRep::~TableRecordRep()
  {}
  
  TableRecordRep& TableRecordRep::operator= (const TableRecordRep& other)
  {
    if (this != &other) {
      restructure (other.desc_p, False);
      copy_other (other);
    }
    return *this;
  }

  void TableRecordRep::restructure (const RecordDesc& newDescription,
                                    Bool recursive)
  {
    desc_p = newDescription;
    data_p.clear();
    data_p.reserve (desc_p.nfields());
    for (uInt i=0; i<desc_p.nfields(); ++i) {
      DataType dtype = desc_p.type(i);
      if (dtype == TpRecord) {
        if (recursive) {
          data_p.push_back (std::unique_ptr<RecordData<TableRecord>>
                            (new RecordData<TableRecord>
                             (TableRecord(this, desc_p.subRecord(i)))));
        } else {
          data_p.push_back (std::unique_ptr<RecordData<TableRecord>>
                            (new RecordData<TableRecord>
                             (TableRecord(this, RecordDesc()))));
        }
      } else if (desc_p.type(i) == TpTable) {
        data_p.push_back (std::unique_ptr<RecordData<TableKeyword>>
                          (new RecordData<TableKeyword>
                           (TableKeyword(desc_p.tableDescName(i)))));
      } else {
        data_p.push_back (RecordDataBase::makeData (dtype, desc_p.shape(i), True));
      }
    }
  }

  void TableRecordRep::addField (const String& name, const TableRecord& rec,
                                 RecordInterface::RecordType type)
  {
    // If the record is empty, it is variable structured.
    if (rec.nfields() == 0) {
      type = RecordInterface::Variable;
    }
    // If the new field is fixed, add its description too.
    if (type == RecordInterface::Fixed) {
      desc_p.addField (name, rec.description());
    } else {
      desc_p.addField (name, TpRecord);
    }
    // Use default ctor and assignment to be sure that the
    // new record gets the correct record type.
    data_p.push_back (std::unique_ptr<RecordData<TableRecord>>
                      (new RecordData<TableRecord>(TableRecord(this, type))));
    data_p.back()->copyData (&rec);
  }

  void TableRecordRep::addField (const String& name, const Table& value,
                                 RecordInterface::RecordType type)
  {
    String tableDescName;
    // When the new field is fixed, add its description name too.
    if (type == RecordInterface::Fixed) {
      tableDescName = value.tableDesc().getType();
    }
    desc_p.addTable (name, tableDescName);
    data_p.push_back (std::unique_ptr<RecordData<TableKeyword>>
                      (new RecordData<TableKeyword>
                       (TableKeyword(value, tableDescName))));
  }
  /*
    void TableRecordRep::defineDataField (Int whichField, DataType type,
    const void* value)
    {
    AlwaysAssert (whichField >= 0  &&  whichField < Int(data_p.size()), AipsError);
    DataType descDtype = desc_p.type(whichField);
    if (type == descDtype) {
    if (type == TpTable) {
    *static_cast<TableKeyword*>(data_p[whichField]).data() =
    *static_cast<const Table*>(value);
    return;
    }
    }
    RecordRep::defineDataField (whichField, type, value);
    }
  */
  Bool TableRecordRep::conform (const TableRecordRep& other) const
  {
    // First check (non-recursively) if the descriptions conform.
    if (! desc_p.conform (other.desc_p)) {
      return False;
    }
    // Now check for each fixed sub-record and table if it conforms.
    for (uInt i=0; i<data_p.size(); ++i) {
      if (desc_p.type(i) == TpRecord) {
        const TableRecord& thisRecord =
          *static_cast<const TableRecord*>(data_p[i]->data());
        if (thisRecord.isFixed()) {
          const TableRecord& thatRecord =
            *static_cast<const TableRecord*>(other.data_p[i]->data());
          if (! thisRecord.conform (thatRecord)) {
            return False;
          }
        }
      } else if (desc_p.type(i) == TpTable) {
        const TableKeyword& thisKey =
          *static_cast<const TableKeyword*>(data_p[i]->data());
        if (thisKey.isFixed()) {
          const TableKeyword& thatKey =
            *static_cast<const TableKeyword*>(other.data_p[i]->data());
          if (! thisKey.conform (thatKey)) {
            return False;
          }
        }
      }
    }
    return True;
  }

  void* TableRecordRep::get_pointer (Int whichField, DataType type,
                                     const String& recordType) const
  {
    if (recordType != "TableRecord") {
      throw (AipsError ("TableRecordRep::get_pointer - field " +
                        desc_p.name(whichField) +
                        " is not of type TableRecord"));
    }
    return get_pointer (whichField, type);
  }

  void* TableRecordRep::get_pointer (Int whichField, DataType type) const
  {
    return RecordRep::get_pointer (whichField, type);
  }

  void TableRecordRep::closeTable (Int whichField) const
  {
    AlwaysAssert (whichField >= 0  &&  whichField < Int(desc_p.nfields())
		  &&  desc_p.type(whichField) == TpTable, AipsError);
    static_cast<TableKeyword*>(data_p[whichField]->data())->close();
  }


  void TableRecordRep::doMergeField (DataType type, const void* otherPtr,
                                     const IPosition& shape)
  {
    if (type == TpRecord) {
      data_p.push_back (std::unique_ptr<RecordData<TableRecord>>
                        (new RecordData<TableRecord>
                         (*static_cast<const TableRecord*>(otherPtr))));
    } else if (type == TpTable) {
      data_p.push_back (std::unique_ptr<RecordData<TableKeyword>>
                        (new RecordData<TableKeyword>
                         (*static_cast<const TableKeyword*>(otherPtr))));
    } else {
      data_p.push_back (RecordDataBase::makeData (type, shape, False));
    }
    data_p.back()->copyData (otherPtr);
  }


  void TableRecordRep::merge (const TableRecordRep& other,
                              RecordInterface::DuplicatesFlag flag)
  {
    Int n = other.desc_p.nfields();
    for (Int i=0; i<n; ++i) {
      mergeField (other, i, flag);
    }
  }
    

  void TableRecordRep::renameTables (const String& newParentName,
                                     const String& oldParentName)
  {
    for (uInt i=0; i<data_p.size(); ++i) {
      if (desc_p.type(i) == TpTable) {
        static_cast<TableKeyword*>(data_p[i]->data())->renameTable
          (newParentName, oldParentName);
      }
    }
  }


  void TableRecordRep::closeTables() const
  {
    for (uInt i=0; i<data_p.size(); ++i) {
      if (desc_p.type(i) == TpTable) {
        static_cast<TableKeyword*>(data_p[i]->data())->close();
      }
    }
  }


  void TableRecordRep::flushTables (Bool fsync) const
  {
    for (uInt i=0; i<data_p.size(); ++i) {
      if (desc_p.type(i) == TpTable) {
        static_cast<TableKeyword*>(data_p[i]->data())->flush(fsync);
      }
    }
  }


  Bool TableRecordRep::areTablesMultiUsed() const
  {
    for (uInt i=0; i<data_p.size(); ++i) {
      if (desc_p.type(i) == TpTable) {
        if (static_cast<TableKeyword*>(data_p[i]->data())->isMultiUsed(True)) {
          return True;
        }
      }
    }
    return False;
  }


  void TableRecordRep::putRecord (AipsIO& os, Int recordType,
                                  const TableAttr& parentAttr) const
  {
    os.putstart ("TableRecord", 1);              // version 1
    os << desc_p;
    os << recordType;
    putData (os, parentAttr);
    os.putend();
  }

  void TableRecordRep::putData (AipsIO& os, const TableAttr& parentAttr) const
  {
    for (uInt i=0; i<desc_p.nfields(); ++i) {
      if (desc_p.type(i) == TpRecord) {
        data_p[i]->putData (os, desc_p.subRecord(i).nfields()==0, nullptr);
      } else {
        data_p[i]->putData (os, False, &parentAttr);
      }
    }
  }

  void TableRecordRep::getRecord (AipsIO& os, Int& recordType,
                                  const TableAttr& parentAttr)
  {
    // Support reading scalar, array, and table keyword sets as records.
    uInt version;
    String type = os.getNextType();
    if (type == "ScalarKeywordSet") {
      version = os.getstart ("ScalarKeywordSet");
      getTableKeySet (os, version, parentAttr, 0);
    } else if (type == "ArrayKeywordSet") {
      version = os.getstart ("ArrayKeywordSet");
      getTableKeySet (os, version, parentAttr, 1);
    } else if (type == "TableKeywordSet") {
      version = os.getstart ("TableKeywordSet");
      getTableKeySet (os, version, parentAttr, 2);
      recordType = RecordInterface::Variable;
    }else{
      uInt version = os.getstart ("TableRecord");
      // Get the description and restructure the record.
      RecordDesc desc;
      os >> desc;
      os >> recordType;
      restructure (desc, True);
      // Read the data.
      getData (os, version, parentAttr);
    }
    os.getend();
  }

  void TableRecordRep::getData (AipsIO& os, uInt version,
                                const TableAttr& parentAttr)
  {
    for (uInt i=0; i<desc_p.nfields(); ++i) {
      if (desc_p.type(i) == TpRecord) {
        data_p[i]->getData (os, version, desc_p.subRecord(i).nfields()==0, nullptr);
      } else {
        data_p[i]->getData (os, version, False, &parentAttr);
      }
    }
  }

  void TableRecordRep::reopenRW()
  {
    for (uInt i=0; i<data_p.size(); ++i) {
      DataType type = desc_p.type(i);
      if (type == TpRecord) {
        static_cast<TableRecord*>(data_p[i]->data())->reopenRW();
      }else if (type == TpTable) {
        static_cast<TableKeyword*>(data_p[i]->data())->setRW();
      }
    }
  }

  void TableRecordRep::getTableKeySet (AipsIO& os, uInt version,
                                       const TableAttr& parentAttr,
                                       uInt type)
  {
    // First build the description from the map of keyword names and
    // attributes.
    RecordDesc desc;
    getKeyDesc (os, desc);
    // Define the record from the description.
    // Read the keyword values and define the corresponding record value.
    restructure (desc, True);
    getScalarKeys (os);
    if (type > 0) {
      getArrayKeys (os);
    }
    if (type > 1) {
      String key, name;
      uInt i, n;
      os >> n;
      for (i=0; i<n; ++i) {
        os >> key;               // keyword name
        os >> name;              // table name
        static_cast<TableKeyword*>(data_p[desc_p.fieldNumber(key)]->data())->set
          (name, parentAttr);
      }
    }
    // Newer keyword sets may contain nested keyword sets.
    // We do not support reading those, so throw an exception when they exist.
    if (version > 1) {
      uInt n;
      os >> n;
      AlwaysAssert (n==0, AipsError);
    }
  }

} //# NAMESPACE CASACORE - END

