//# FunctionFactoryErrors:  Exception classes for use by FunctionFactories & clients
//# Copyright (C) 2002
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

#ifndef SCIMATH_FUNCTIONFACTORYERRORS_H
#define SCIMATH_FUNCTIONFACTORYERRORS_H

#include <casacore/casa/aips.h>
#include <casacore/casa/Exceptions/Error.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

class FunctionFactoryError : public AipsError {
public:
    FunctionFactoryError(const String& message,Category c=GENERAL) : AipsError(message,c) {}
    virtual ~FunctionFactoryError() throw();
};

class UnrecognizedFunctionError : public FunctionFactoryError {
public:

    // create an exception indicating that the a function of the given name
    // is not recognized
    UnrecognizedFunctionError(const String& name, Category c=INVALID_ARGUMENT) : 
	FunctionFactoryError(String("Unrecognized function: ") + name,c), 
	fname(name) 
    {}
    virtual ~UnrecognizedFunctionError() throw();

    const String& getName() { return fname; }

private:
    String fname;
};

class InvalidSerializationError : public FunctionFactoryError {
public:
    // create an exception indicating a Record serialization of a 
    // Function is invalid.  The error message will be a "Invalid function 
    // description in  record: " + reason.
    InvalidSerializationError(const String& reason,Category c=GENERAL) : 
	FunctionFactoryError(preamble + reason,c), reas(reason) {}

    virtual ~InvalidSerializationError() throw();
    const String& getReason() { return reas; } 

    static const String preamble;

private:
    String reas;
};

class FieldNotFoundError : public InvalidSerializationError {
public:
    FieldNotFoundError(const String& field,Category c=GENERAL) : 
	InvalidSerializationError(String("No ") + field + " defined",c),
	fname(field)
    {}
    virtual ~FieldNotFoundError() throw();

private:
    String fname;
};


} //# NAMESPACE CASACORE - END

#endif
