//# tDataType.cc: This program tests the DataType related functions
//# Copyright (C) 1995,1996
//# Associated Universities, Inc. Washington DC, USA.
//#
//# This program is free software; you can redistribute it and/or modify it
//# under the terms of the GNU General Public License as published by the Free
//# Software Foundation; either version 2 of the License, or (at your option)
//# any later version.
//#
//# This program is distributed in the hope that it will be useful, but WITHOUT
//# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
//# FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
//# more details.
//#
//# You should have received a copy of the GNU General Public License along
//# with this program; if not, write to the Free Software Foundation, Inc.,
//# 675 Massachusetts Ave, Cambridge, MA 02139, USA.
//#
//# Correspondence concerning AIPS++ should be addressed as follows:
//#        Internet email: aips2-request@nrao.edu.
//#        Postal address: AIPS++ Project Office
//#                        National Radio Astronomy Observatory
//#                        520 Edgemont Road
//#                        Charlottesville, VA 22903-2475 USA
//#
//# $Id$

#include <aips/Utilities/DataType.h>
#include <aips/Utilities/String.h>
#include <aips/Utilities/Assert.h>

#include <strstream.h>
#include <iostream.h>

int main()
{
    // First check the << operator
    char null = 0;
    char buffer[100];
    {
	ostrstream formatter(buffer, 100);
	formatter << TpBool << null; 
	AlwaysAssertExit(String(buffer) == "Bool");
    }
    {
	ostrstream formatter(buffer, 100);
	formatter << TpChar << null; 
	AlwaysAssertExit(String(buffer) == "Char");
    }
    {
	ostrstream formatter(buffer, 100);
	formatter << TpUChar << null; 
	AlwaysAssertExit(String(buffer) == "uChar");
    }
    {
	ostrstream formatter(buffer, 100);
	formatter << TpShort << null; 
	AlwaysAssertExit(String(buffer) == "Short");
    }
    {
	ostrstream formatter(buffer, 100);
	formatter << TpUShort << null; 
	AlwaysAssertExit(String(buffer) == "uShort");
    }
    {
	ostrstream formatter(buffer, 100);
	formatter << TpInt << null; 
	AlwaysAssertExit(String(buffer) == "Int");
    }
    {
	ostrstream formatter(buffer, 100);
	formatter << TpUInt << null; 
	AlwaysAssertExit(String(buffer) == "uInt");
    }
    {
	ostrstream formatter(buffer, 100);
	formatter << TpFloat << null; 
	AlwaysAssertExit(String(buffer) == "float");
    }
    {
	ostrstream formatter(buffer, 100);
	formatter << TpDouble << null; 
	AlwaysAssertExit(String(buffer) == "double");
    }
    {
	ostrstream formatter(buffer, 100);
	formatter << TpComplex << null; 
	AlwaysAssertExit(String(buffer) == "Complex");
    }
    {
	ostrstream formatter(buffer, 100);
	formatter << TpDComplex << null; 
	AlwaysAssertExit(String(buffer) == "DComplex");
    }
    {
	ostrstream formatter(buffer, 100);
	formatter << TpString << null; 
	AlwaysAssertExit(String(buffer) == "String");
    }
    {
	ostrstream formatter(buffer, 100);
	formatter << TpTable << null; 
	AlwaysAssertExit(String(buffer) == "Table");
    }
    {
	ostrstream formatter(buffer, 100);
	formatter << TpArrayBool << null; 
	AlwaysAssertExit(String(buffer) == "Array<Bool>");
    }
    {
	ostrstream formatter(buffer, 100);
	formatter << TpArrayChar << null; 
	AlwaysAssertExit(String(buffer) == "Array<Char>");
    }
    {
	ostrstream formatter(buffer, 100);
	formatter << TpArrayUChar << null; 
	AlwaysAssertExit(String(buffer) == "Array<uChar>");
    }
    {
	ostrstream formatter(buffer, 100);
	formatter << TpArrayShort << null; 
	AlwaysAssertExit(String(buffer) == "Array<Short>");
    }
    {
	ostrstream formatter(buffer, 100);
	formatter << TpArrayUShort << null; 
	AlwaysAssertExit(String(buffer) == "Array<uShort>");
    }
    {
	ostrstream formatter(buffer, 100);
	formatter << TpArrayInt << null; 
	AlwaysAssertExit(String(buffer) == "Array<Int>");
    }
    {
	ostrstream formatter(buffer, 100);
	formatter << TpArrayUInt << null; 
	AlwaysAssertExit(String(buffer) == "Array<uInt>");
    }
    {
	ostrstream formatter(buffer, 100);
	formatter << TpArrayFloat << null; 
	AlwaysAssertExit(String(buffer) == "Array<float>");
    }
    {
	ostrstream formatter(buffer, 100);
	formatter << TpArrayDouble << null; 
	AlwaysAssertExit(String(buffer) == "Array<double>");
    }
    {
	ostrstream formatter(buffer, 100);
	formatter << TpArrayComplex << null; 
	AlwaysAssertExit(String(buffer) == "Array<Complex>");
    }
    {
	ostrstream formatter(buffer, 100);
	formatter << TpArrayDComplex << null; 
	AlwaysAssertExit(String(buffer) == "Array<DComplex>");
    }
    {
	ostrstream formatter(buffer, 100);
	formatter << TpArrayString << null; 
	AlwaysAssertExit(String(buffer) == "Array<String>");
    }
    {
	ostrstream formatter(buffer, 100);
	formatter << TpRecord << null; 
	AlwaysAssertExit(String(buffer) == "Record");
    }
    {
	ostrstream formatter(buffer, 100);
	formatter << TpOther << null; 
	AlwaysAssertExit(String(buffer) == "Other");
    }

    // Now check the whatType() functions
    class Goofy;
    AlwaysAssertExit( whatType((Goofy *)0) == TpOther );
    AlwaysAssertExit( whatType((Bool *)0) == TpBool );
    AlwaysAssertExit( whatType((Char *)0) == TpChar );
    AlwaysAssertExit( whatType((uChar *)0) == TpUChar );
    AlwaysAssertExit( whatType((Short *)0) == TpShort );
    AlwaysAssertExit( whatType((uShort *)0) == TpUShort );
    AlwaysAssertExit( whatType((Int *)0) == TpInt );
    AlwaysAssertExit( whatType((uInt *)0) == TpUInt );
    AlwaysAssertExit( whatType((Float *)0) == TpFloat );
    AlwaysAssertExit( whatType((Double *)0) == TpDouble );
    AlwaysAssertExit( whatType((Complex *)0) == TpComplex );
    AlwaysAssertExit( whatType((DComplex *)0) == TpDComplex );
    AlwaysAssertExit( whatType((String *)0) == TpString );
    AlwaysAssertExit( whatType((Table *)0) == TpTable );
    AlwaysAssertExit( whatType((Array<Bool> *)0) == TpArrayBool );
    AlwaysAssertExit( whatType((Array<Char> *)0) == TpArrayChar );
    AlwaysAssertExit( whatType((Array<uChar> *)0) == TpArrayUChar );
    AlwaysAssertExit( whatType((Array<Short> *)0) == TpArrayShort );
    AlwaysAssertExit( whatType((Array<uShort> *)0) == TpArrayUShort );
    AlwaysAssertExit( whatType((Array<Int> *)0) == TpArrayInt );
    AlwaysAssertExit( whatType((Array<uInt> *)0) == TpArrayUInt );
    AlwaysAssertExit( whatType((Array<Float> *)0) == TpArrayFloat );
    AlwaysAssertExit( whatType((Array<Double> *)0) == TpArrayDouble );
    AlwaysAssertExit( whatType((Array<Complex> *)0) == TpArrayComplex );
    AlwaysAssertExit( whatType((Array<DComplex> *)0) == TpArrayDComplex );
    AlwaysAssertExit( whatType((Array<String> *)0) == TpArrayString );
    AlwaysAssertExit( whatType((Record *)0) == TpRecord );

    AlwaysAssertExit (isScalar(TpBool) && isScalar(TpChar) && isScalar(TpUChar)&&
      isScalar(TpShort) && isScalar(TpUShort) && isScalar(TpInt) && 
      isScalar(TpUInt) && isScalar(TpFloat) && isScalar(TpDouble) &&
      isScalar(TpComplex) && isScalar(TpDComplex) && isScalar(TpString) &&
      !isScalar(TpTable) && !isScalar(TpRecord) && !isScalar(TpOther) &&
      !isScalar(TpArrayBool) && !isScalar(TpArrayChar) && 
      !isScalar(TpArrayUChar)&& !isScalar(TpArrayShort) && 
      !isScalar(TpArrayUShort) && !isScalar(TpArrayInt) && 
      !isScalar(TpArrayUInt) && !isScalar(TpArrayFloat) && 
      !isScalar(TpArrayDouble) && !isScalar(TpArrayComplex) &&
      !isScalar(TpArrayDComplex) && !isScalar(TpArrayString));
    AlwaysAssertExit (!isArray(TpBool) && !isArray(TpChar) && !isArray(TpUChar)&&
      !isArray(TpShort) && !isArray(TpUShort) && !isArray(TpInt) && 
      !isArray(TpUInt) && !isArray(TpFloat) && !isArray(TpDouble) &&
      !isArray(TpComplex) && !isArray(TpDComplex) && !isArray(TpString) &&
      !isArray(TpTable) && !isArray(TpRecord) && !isArray(TpOther) &&
      isArray(TpArrayBool) && isArray(TpArrayChar) && 
      isArray(TpArrayUChar)&& isArray(TpArrayShort) && 
      isArray(TpArrayUShort) && isArray(TpArrayInt) && 
      isArray(TpArrayUInt) && isArray(TpArrayFloat) && 
      isArray(TpArrayDouble) && isArray(TpArrayComplex) &&
      isArray(TpArrayDComplex) && isArray(TpArrayString));

    cout << "OK" << endl;
    return 0;
}
