//# NewFile: Constrain a string to be a new (non-existent) file
//# Copyright (C) 1996,1999
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

#if !defined(AIPS_NEW_FILE_H)
#define AIPS_NEW_FILE_H

#include <aips/aips.h>
#include <trial/Tasking/ParameterConstraint.h>
#include <aips/Utilities/String.h>

// <summary>
// </summary>

// <use visibility=export>

// <reviewed reviewer="" date="yyyy/mm/dd" tests="" demos="">
// </reviewed>

// <prerequisite>
//   <li> <linkto class=ParameterConstraint>ParameterConstraint</linkto>
//   <li> <linkto class=Parameter>Parameter</linkto>
// </prerequisite>
//
// <etymology>
// Use this if you want a New File.
// </etymology>
//
// <synopsis>
// NewFile is a parameter constraint that is intended to be used for String
// parameters which are interpreted as output file names. If the file exists,
// then the user is asked via ApplicationEnvironment::choice whether or not
// he or she wants to delete the file before using it.
// </synopsis>
//
// <example>
// Parameter<String> outfile(inputRecord, "outfile", ParameterSet::In);
// outfile.setConstraint(NewFile());
// </example>
//
// <example>
// NewFile validFile;
// String newFileName("bigone"), error;
// Bool ok = validFile.valueOK(newFileName, error);
// if (!ok) {
//    cout << error << endl;
// }
// </example>
//
// <motivation>
// Output file names are fairly common parameters, this consolidates the error
// checking and "remove if it already exists" logic.
// </motivation>
//
// <todo asof="1996/12/10">
//   <li> We should probably make sure that the file is writable
// </todo>

class NewFile : public ParameterConstraint<String>
{
public:
//  Currently the deleteIfExists argument has no affect
    NewFile(Bool deleteIfExists = True);

//  Copy constructor (copy semantics)
    NewFile(const NewFile &other);

//  Assignment (copy semantics)
    NewFile &operator=(const NewFile &other);

// Destructor
    ~NewFile();

// Indicates whether the specified string is a valid new file,
// invoking the choice GUI.  If it returns False, an error 
// message is returned.
    virtual Bool valueOK(const String &value, String &error) const;

// Set the constraint
    virtual ParameterConstraint<String> *clone() const;
public:
    Bool delete_p;
};

#endif
