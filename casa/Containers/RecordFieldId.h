//# RecordFieldId.h: The identification of a record field
//# Copyright (C) 1995,1996
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


#ifndef CASA_RECORDFIELDID_H
#define CASA_RECORDFIELDID_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/casa/BasicSL/String.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations
class RecordInterface;


// <summary>
// The identification of a record field.
// </summary>

// <use visibility=export>
// <reviewed reviewer="Mark Wieringa" date="1996/04/15" tests="tRecord">
// </reviewed>

// <prerequisite>
//   <li> <linkto class="RecordInterface">RecordInterface</linkto>.
// </prerequisite>

// <etymology>
// RecordFieldId gives the identification of a field in a record.
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
// have to be defined only once. The constructors of RecordFieldId
// make it possible that a number and a string are automatically
// converted, so the user does not have to instantiate a RecordFieldId
// object explicitly.
// </motivation>

//# <todo asof="1996/03/12">
//# </todo>


class RecordFieldId
{
public:
    // Construct it from a field number.
    RecordFieldId (Int fieldNumber);

    // Construct it from a field name.
    // <group>
    RecordFieldId (const String& name);
    RecordFieldId (const std::string& name);
    RecordFieldId (const Char* name);
    // </group>

    // Get the field number.
    Int fieldNumber() const;

    // Get the field name.
    const String& fieldName() const;

    // Is the id given by name?
    Bool byName() const;

private:
    Bool    byName_p;
    Int     number_p;
    String  name_p;
};



inline RecordFieldId::RecordFieldId (Int fieldNumber)
: byName_p (False),
  number_p (fieldNumber)
{}

inline RecordFieldId::RecordFieldId (const String& fieldName)
: byName_p (True),
  number_p (-1),
  name_p   (fieldName)
{}

inline RecordFieldId::RecordFieldId (const std::string& fieldName)
: byName_p (True),
  number_p (-1),
  name_p   (fieldName)
{}

inline RecordFieldId::RecordFieldId (const Char* fieldName)
: byName_p (True),
  number_p (-1),
  name_p   (fieldName)
{}

inline Int RecordFieldId::fieldNumber() const
{
    return number_p;
}

inline const String& RecordFieldId::fieldName() const
{
    return name_p;
}

inline Bool RecordFieldId::byName() const
{
    return byName_p;
}



} //# NAMESPACE CASACORE - END

#endif
