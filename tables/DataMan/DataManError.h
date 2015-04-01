//# DataManError.h: Data manager error classes
//# Copyright (C) 1994,1995,1996,1999,2000,2003
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
//# $Id$

#ifndef TABLES_DATAMANERROR_H
#define TABLES_DATAMANERROR_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/casa/Exceptions/Error.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# This header file defines the error classes used in the
//# data manager classes.


// <summary>
// Base error class for table data manager
// </summary>
// <use visibility=export>
// <reviewed reviewer="Gareth Hunt" date="94Nov17" tests="">
// </reviewed>

// <synopsis> 
// This is the generic data manager exception; catching this one means
// catching all DataMan* exceptions.
// Note that you have to catch AipsError to catch all possible exceptions.
// </synopsis> 

class DataManError : public AipsError {
public:
    // The default constructor generates the message "Table DataManager error".
    DataManError ();
    // Construct with given message.
    DataManError (const String& message);
    ~DataManError () throw();
};


// <summary>
// Internal table data manager error
// </summary>
// <use visibility=export>
// <reviewed reviewer="Gareth Hunt" date="94Nov17" tests="">
// </reviewed>

// <synopsis> 
// Internal data manager error (should never be thrown).
// If this is thrown, something is terribly wrong.
// </synopsis> 

class DataManInternalError : public DataManError {
public:
    // Add given message to string "Internal Table DataManager error: ".
    DataManInternalError (const String& message);
    ~DataManInternalError () throw();
};


// <summary>
// Table DataManager error; invalid data manager
// </summary>
// <use visibility=export>
// <reviewed reviewer="Gareth Hunt" date="94Nov17" tests="">
// </reviewed>

// <synopsis> 
// A data manager is unknown (i.e. not registered in DataManReg.cc).
// This means that the data manager object cannot be recreated.
// </synopsis> 

class DataManUnknownCtor : public DataManError {
public:
    // This constructor generates a message that a data manager
    // with the given name is unknown (i.e. not registered).
    DataManUnknownCtor (const String& columnName);
    ~DataManUnknownCtor () throw();
};


// <summary>
// Table DataManager error; invalid data type
// </summary>
// <use visibility=export>
// <reviewed reviewer="Gareth Hunt" date="94Nov17" tests="">
// </reviewed>

// <synopsis> 
// Invalid data type used in the data manager.
// The data manager found an unknown data type when doing a get or put.
// In principle this error should never occur.
// </synopsis> 

class DataManInvDT : public DataManError {
public:
    // The default constructor generates a generic "invalid data type" message.
    DataManInvDT ();
    // Put the name of the offending column in the "invalid data type" message.
    DataManInvDT (const String& columnName);
    ~DataManInvDT () throw();
};



// <summary>
// Table DataManager error; invalid operation
// </summary>
// <use visibility=export>
// <reviewed reviewer="Gareth Hunt" date="94Nov17" tests="">
// </reviewed>

// <synopsis> 
// Invalid operation on a data manager.
// A request was done that the data manager could not handle.
// In principle the table system should already test on such operations
// and it should not bother the data manager with invalid requests.
// However, the data manager still tests them for safety.
// </synopsis> 

class DataManInvOper : public DataManError {
public:
    // The default constructor generates a generic "invalid operation" message.
    DataManInvOper ();
    // Add given message to string "Invalid DataMan operation: ".
    DataManInvOper (const String& message);
    ~DataManInvOper () throw();
};


// <summary>
// Table DataManager error; unknown virtual column
// </summary>
// <use visibility=export>
// <reviewed reviewer="Gareth Hunt" date="94Nov17" tests="">
// </reviewed>

// <synopsis> 
// A column is unknown to the virtual column engine.
// This error is caused by binding a column to a virtual column engine
// which does not know the column name or data type.
// </synopsis> 

class DataManUnknownVirtualColumn : public DataManError {
public:
    // Issue a message containing the column name.
    DataManUnknownVirtualColumn (const String& columnName,
				 const String& engineName);
    ~DataManUnknownVirtualColumn () throw();
};


// <summary>
// Table DataManager error; error in TiledStMan
// </summary>
// <use visibility=export>
// <reviewed reviewer="Gareth Hunt" date="94Nov17" tests="">
// </reviewed>

// <synopsis> 
// An error was made when using the TiledStMan.
// The TiledStMan is quite complex, so it is easy to make mistakes.
// The <linkto class=TiledStMan>TiledStMan</linkto> and related
// classes should be studied carefully.
// </synopsis> 

class TSMError : public DataManError {
public:
    // Issue the message prefixed by "TiledStMan: ".
    TSMError (const String& message);
    ~TSMError () throw();
};



} //# NAMESPACE CASACORE - END

#endif
