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


#if !defined(AIPS_TABLEEXPRID_H)
#define AIPS_TABLEEXPRID_H

//# Includes
#include <aips/aips.h>

//# Forward Declarations
class RecordInterface;


// <summary>
// The identification of a record field.
// </summary>

// <use visibility=export>

// <reviewed reviewer="" date="" tests="">
// </reviewed>

// <prerequisite>
//   <li> <linkto class="TableExprNode">TableExprNode</linkto>.
// </prerequisite>

// <etymology>
// TableExprId gives the identification of a field in a record.
// </etymology>

// <synopsis>
// This class provides the user to identify a field in a record.
// Identification can be done by means of the field name or by means
// of its field number.
// <br>For the programmer the most convenient way is probably the name,
// because that is the natural identification.
// However, identification by means of field number is much faster
// and could be used when it is known.
// </synopsis>

// <example>
// <srcblock>
// void someFunc (const Record& record)
// {
//     float value1 = record.asfloat ("name");     // identify by name
//     float value2 = record.asfloat (0);          // identify by number
// }
// </srcBlock>
// </example>

// <motivation>
// This class makes it possible that many functions in Record classes
// have to be defined only once. The constructors of TableExprId
// make it possible that a number and a string are automatically
// converted, so the user does not have to instantiate a TableExprId
// object explicitly.
// </motivation>

//# <todo asof="1996/03/12">
//# </todo>


class TableExprId
{
public:
    // Construct it from a row number.
    TableExprId (uInt rowNumber);

    // Construct it from a Record object.
    TableExprId (const RecordInterface&);

    // Is the id given by row number (or by record)?
    Bool byRow() const;

    // Get the row number.
    uInt rownr() const;

    // Get the Record reference.
    const RecordInterface& record() const;

    // Set the row number.
    void setRownr (uInt rownr);

    // Set the record.
    void setRecord (const RecordInterface&);

private:
    uInt    row_p;
    const RecordInterface* record_p;
};



inline TableExprId::TableExprId (uInt rowNumber)
: row_p    (rowNumber),
  record_p (0)
{}

inline TableExprId::TableExprId (const RecordInterface& record)
: row_p    (32768*32768),
  record_p (&record)
{}

inline uInt TableExprId::rownr() const
{
    return row_p;
}

inline const RecordInterface& TableExprId::record() const
{
    return *record_p;
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
    return record_p == 0;
}



#endif
