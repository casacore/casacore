//# TableExprId.h: The identification of a TaQL selection subject
//# Copyright (C) 2000
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


#ifndef TABLES_TABLEEXPRID_H
#define TABLES_TABLEEXPRID_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/casa/stdvector.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations
class RecordInterface;
class TableExprData;

// <summary>
// The identification of a TaQL selection subject.
// </summary>

// <use visibility=export>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="">
// </reviewed>

// <prerequisite>
//   <li> <linkto class="TableExprNode">TableExprNode</linkto>.
// </prerequisite>

// <synopsis>
// This class provides the user the ability to identify the data objects
// to test in a TaQL expression. In this way a TaQL expression is not
// limited to tables, but can be used for any set of data.
// Three types are available:
// <ol>
//  <li> A row number giving the row to test in a table.
//       It also contains a sequence number (0..n) which is used to get the
//       result calculated by aggregate functions.
//  <li> A <linkto class=RecordInterface>RecordInterface</linkto>
//       object giving the record to test.
//  <li> A <linkto class=TableExprData>TableExprData</linkto>
//       object giving the abstract base class of an object holding
//       the data to test. In this way any data can be used.
// </ol>
// The TaQL expression must be setup with this in mind by constructing
// the appropriate <linkto class=TableExprNode>TableExprNode</linkto>
// leaf objects.
// <br>
// When used for tables, the function <linkto class=Table>Table::col</linkto>
// should be used to create the <src>TableExprNode</src> objects for the
// table columns to be used in the expression.
// <br>
// For the other cases class
// <linkto class=TableExprNodeRecordField>TableExprNodeRecordField</linkto>
// has to be used to know the index of the fields in the expression.
// It uses a record (description) for this purpose.
// </synopsis>

// <example>
// <srcblock>
// </srcBlock>
// </example>

// <motivation>
// This class makes it possible that TaQL can be used in a very versatile way.
// </motivation>

//# <todo asof="1996/03/12">
//# </todo>


class TableExprId
{
public:
    // Default constructor sets rownr to -1.
    TableExprId();

    // Construct it from a row number.
    TableExprId (uInt rowNumber);

    // Construct it from a Record object.
    TableExprId (const RecordInterface&);

    // Construct it from pointers to data.
    TableExprId (const TableExprData& data);

    ~TableExprId()
    {}

    // Is the id given by row number?
    Bool byRow() const;

    // Is the id given as a RecordInterface?
    Bool byRecord() const;

    // Is the id given as a TableExprData?
    Bool byData() const;

    // Get the row number.
    Int64 rownr() const;

    // Get the Record reference.
    const RecordInterface& record() const;

    // Get the data reference.
    const TableExprData& data() const;
    // Set the row number.

    void setRownr (uInt rownr);

    // Set the record.
    void setRecord (const RecordInterface&);

private:
    Int                          type_p;
    union {
      Int64                      row_p;
      const RecordInterface*     record_p;
      const TableExprData*       data_p;
    };
};



inline TableExprId::TableExprId()
  : type_p (0),
    row_p  (-1)
{}

inline TableExprId::TableExprId (uInt rowNumber)
  : type_p (0),
    row_p  (rowNumber)
{}

inline TableExprId::TableExprId (const RecordInterface& record)
  : type_p   (-1),
    record_p (&record)
{}

inline TableExprId::TableExprId (const TableExprData& data)
  : type_p (-2),
    data_p (&data)
{}

inline Int64 TableExprId::rownr() const
{
    return row_p;
}

inline const RecordInterface& TableExprId::record() const
{
    return *record_p;
}

inline const TableExprData& TableExprId::data() const
{
    return *data_p;
}

inline void TableExprId::setRownr (uInt rownr)
{
    row_p = rownr;
}

inline void TableExprId::setRecord (const RecordInterface& record)
{
    record_p = &record;
}

inline Bool TableExprId::byRow() const
{
    return type_p >= 0;
}

inline Bool TableExprId::byRecord() const
{
    return type_p == -1;
}

inline Bool TableExprId::byData() const
{
    return type_p == -2;
}


} //# NAMESPACE CASACORE - END

#endif
