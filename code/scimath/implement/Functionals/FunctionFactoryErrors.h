//# FunctionFactoryErrors:  Exception classes for use by FunctionFactories & clients
//# Copyright (C) 2002,2003
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

#if !defined(AIPS_FUNCTIONFACTORYERRORS_H)
#define AIPS_FUNCTIONFACTORYERRORS_H

#include <aips/Exceptions/Error.h>

class FunctionFactoryError : public AipsError {
public:
    FunctionFactoryError(const String& message) : AipsError(message) {}

    virtual ~FunctionFactoryError() throw();
};

class UnrecognizedFunctionError : public FunctionFactoryError {
public:

    // create an exception indicating that the a function of the given name
    // is not recognized
    UnrecognizedFunctionError(const String& name) : 
	FunctionFactoryError(String("Unrecognized function: ") + name), 
	fname(name) 
    {}

    const String& getName() { return fname; }

    virtual ~UnrecognizedFunctionError() throw();

private:
    String fname;
};

class InvalidGlishSerializationError : public FunctionFactoryError {
public:
    // create an exception indicating a GlishRecord serialization of a 
    // Function is invalid.  The error message will be a "Invalid function 
    // description in Glish record: " + reason.
    InvalidGlishSerializationError(const String& reason) : 
	FunctionFactoryError(preamble + reason), reas(reason) {}

    const String& getReason() { return reas; } 

    virtual ~InvalidGlishSerializationError() throw();

    static const String preamble;

private:
    String reas;
};

class GlishFieldNotFoundError : public InvalidGlishSerializationError {
public:
    GlishFieldNotFoundError(const String& field) : 
	InvalidGlishSerializationError(String("No ") + field + " defined"),
	fname(field)
    {}

    virtual ~GlishFieldNotFoundError() throw();

private:
    String fname;
};

#endif
