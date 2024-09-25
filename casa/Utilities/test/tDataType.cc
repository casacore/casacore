//# tDataType.cc: This program tests the DataType related functions
//# Copyright (C) 1995,1996,1999,2000,2001,2003
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
//#        Internet email: casa-feedback@nrao.edu.
//#        Postal address: AIPS++ Project Office
//#                        National Radio Astronomy Observatory
//#                        520 Edgemont Road
//#                        Charlottesville, VA 22903-2475 USA

#include <casacore/casa/Utilities/DataType.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Quanta/Quantum.h>
#include <casacore/casa/sstream.h>
#include <casacore/casa/iostream.h>

#include <casacore/casa/namespace.h>
void simpleTests()
{
    // First check the << operator
    {
	ostringstream formatter;
	formatter << TpBool;
	AlwaysAssertExit(String(formatter) == "Bool");
    }
    {
	ostringstream formatter;
	formatter << TpChar;
	AlwaysAssertExit(formatter.str() == "Char");
    }
    {
	ostringstream formatter;
	formatter << TpUChar; 
	AlwaysAssertExit(formatter.str() == "uChar");
    }
    {
	ostringstream formatter;
	formatter << TpShort; 
	AlwaysAssertExit(formatter.str() == "Short");
    }
    {
	ostringstream formatter;
	formatter << TpUShort; 
	AlwaysAssertExit(formatter.str() == "uShort");
    }
    {
	ostringstream formatter;
	formatter << TpInt; 
	AlwaysAssertExit(formatter.str() == "Int");
    }
    {
	ostringstream formatter;
	formatter << TpUInt; 
	AlwaysAssertExit(formatter.str() == "uInt");
    }
    {
	ostringstream formatter;
	formatter << TpInt64; 
	AlwaysAssertExit(formatter.str() == "Int64");
    }
    {
	ostringstream formatter;
	formatter << TpFloat; 
	AlwaysAssertExit(formatter.str() == "float");
    }
    {
	ostringstream formatter;
	formatter << TpDouble; 
	AlwaysAssertExit(formatter.str() == "double");
    }
    {
	ostringstream formatter;
	formatter << TpComplex; 
	AlwaysAssertExit(formatter.str() == "Complex");
    }
    {
	ostringstream formatter;
	formatter << TpDComplex; 
	AlwaysAssertExit(formatter.str() == "DComplex");
    }
    {
	ostringstream formatter;
	formatter << TpString; 
	AlwaysAssertExit(formatter.str() == "String");
    }
    {
	ostringstream formatter;
	formatter << TpTable; 
	AlwaysAssertExit(formatter.str() == "Table");
    }
    {
	ostringstream formatter;
	formatter << TpArrayBool; 
	AlwaysAssertExit(formatter.str() == "Array<Bool>");
    }
    {
	ostringstream formatter;
	formatter << TpArrayChar; 
	AlwaysAssertExit(formatter.str() == "Array<Char>");
    }
    {
	ostringstream formatter;
	formatter << TpArrayUChar; 
	AlwaysAssertExit(formatter.str() == "Array<uChar>");
    }
    {
	ostringstream formatter;
	formatter << TpArrayShort; 
	AlwaysAssertExit(formatter.str() == "Array<Short>");
    }
    {
	ostringstream formatter;
	formatter << TpArrayUShort; 
	AlwaysAssertExit(formatter.str() == "Array<uShort>");
    }
    {
	ostringstream formatter;
	formatter << TpArrayInt; 
	AlwaysAssertExit(formatter.str() == "Array<Int>");
    }
    {
	ostringstream formatter;
	formatter << TpArrayUInt; 
	AlwaysAssertExit(formatter.str() == "Array<uInt>");
    }
    {
	ostringstream formatter;
	formatter << TpArrayInt64; 
	AlwaysAssertExit(formatter.str() == "Array<Int64>");
    }
    {
	ostringstream formatter;
	formatter << TpArrayFloat; 
	AlwaysAssertExit(formatter.str() == "Array<float>");
    }
    {
	ostringstream formatter;
	formatter << TpArrayDouble; 
	AlwaysAssertExit(formatter.str() == "Array<double>");
    }
    {
	ostringstream formatter;
	formatter << TpArrayComplex; 
	AlwaysAssertExit(formatter.str() == "Array<Complex>");
    }
    {
	ostringstream formatter;
	formatter << TpArrayDComplex; 
	AlwaysAssertExit(formatter.str() == "Array<DComplex>");
    }
    {
	ostringstream formatter;
	formatter << TpArrayString; 
	AlwaysAssertExit(formatter.str() == "Array<String>");
    }
    {
	ostringstream formatter;
	formatter << TpRecord; 
	AlwaysAssertExit(formatter.str() == "Record");
    }
    {
	ostringstream formatter;
	formatter << TpOther; 
	AlwaysAssertExit(formatter.str() == "Other");
    }
    {
	ostringstream formatter;
	formatter << TpQuantity; 
	AlwaysAssertExit(formatter.str() == "Quantity");
    }
    {
	ostringstream formatter;
	formatter << TpArrayQuantity; 
	AlwaysAssertExit(formatter.str() == "Array<Quantity>");
    }

    // Now check the whatType() functions
    class Goofy;
    AlwaysAssertExit( whatType<Goofy>() == TpOther );
    AlwaysAssertExit( whatType<Bool>() == TpBool );
    AlwaysAssertExit( whatType<Char>() == TpChar );
    AlwaysAssertExit( whatType<uChar>() == TpUChar );
    AlwaysAssertExit( whatType<Short>() == TpShort );
    AlwaysAssertExit( whatType<uShort>() == TpUShort );
    AlwaysAssertExit( whatType<Int>() == TpInt );
    AlwaysAssertExit( whatType<uInt>() == TpUInt );
    AlwaysAssertExit( whatType<Int64>() == TpInt64 );
    AlwaysAssertExit( whatType<Float>() == TpFloat );
    AlwaysAssertExit( whatType<Double>() == TpDouble );
    AlwaysAssertExit( whatType<Complex>() == TpComplex );
    AlwaysAssertExit( whatType<DComplex>() == TpDComplex );
    AlwaysAssertExit( whatType<String>() == TpString );
    AlwaysAssertExit( whatType<Table>() == TpTable );
    AlwaysAssertExit( whatType<Array<Bool>>() == TpArrayBool );
    AlwaysAssertExit( whatType<Array<Char>>() == TpArrayChar );
    AlwaysAssertExit( whatType<Array<uChar>>() == TpArrayUChar );
    AlwaysAssertExit( whatType<Array<Short>>() == TpArrayShort );
    AlwaysAssertExit( whatType<Array<uShort>>() == TpArrayUShort );
    AlwaysAssertExit( whatType<Array<Int>>() == TpArrayInt );
    AlwaysAssertExit( whatType<Array<uInt>>() == TpArrayUInt );
    AlwaysAssertExit( whatType<Array<Int64>>() == TpArrayInt64 );
    AlwaysAssertExit( whatType<Array<Float>>() == TpArrayFloat );
    AlwaysAssertExit( whatType<Array<Double>>() == TpArrayDouble );
    AlwaysAssertExit( whatType<Array<Complex>>() == TpArrayComplex );
    AlwaysAssertExit( whatType<Array<DComplex>>() == TpArrayDComplex );
    AlwaysAssertExit( whatType<Array<String>>() == TpArrayString );
    AlwaysAssertExit( whatType<Record>() == TpRecord );
    AlwaysAssertExit( whatType<Quantity>() == TpQuantity ) ;
    AlwaysAssertExit( whatType<Array<Quantity>>() == TpArrayQuantity );

    AlwaysAssertExit (isScalar(TpBool) && isScalar(TpChar) && isScalar(TpUChar)&&
		      isScalar(TpShort) && isScalar(TpUShort) && isScalar(TpInt) && 
		      isScalar(TpUInt) && isScalar(TpFloat) && isScalar(TpDouble) &&
		      isScalar(TpComplex) && isScalar(TpDComplex) && isScalar(TpString) &&
		      isScalar(TpQuantity) && isScalar(TpInt64) &&
		      !isScalar(TpTable) && !isScalar(TpRecord) && !isScalar(TpOther) &&
		      !isScalar(TpArrayBool) && !isScalar(TpArrayChar) && 
		      !isScalar(TpArrayUChar)&& !isScalar(TpArrayShort) && 
		      !isScalar(TpArrayUShort) && !isScalar(TpArrayInt) && 
		      !isScalar(TpArrayUInt) && !isScalar(TpArrayFloat) && 
		      !isScalar(TpArrayDouble) && !isScalar(TpArrayComplex) &&
		      !isScalar(TpArrayDComplex) && !isScalar(TpArrayString) &&
		      !isScalar(TpArrayQuantity) && !isScalar(TpArrayInt64));
    AlwaysAssertExit (!isArray(TpBool) && !isArray(TpChar) && !isArray(TpUChar)&&
		      !isArray(TpShort) && !isArray(TpUShort) && !isArray(TpInt) && 
		      !isArray(TpUInt) && !isArray(TpFloat) && !isArray(TpDouble) &&
		      !isArray(TpComplex) && !isArray(TpDComplex) && !isArray(TpString) &&
		      !isArray(TpTable) && !isArray(TpRecord) && !isArray(TpOther) &&
		      !isArray(TpQuantity) && !isArray(TpInt64) &&
		      isArray(TpArrayBool) && isArray(TpArrayChar) && 
		      isArray(TpArrayUChar)&& isArray(TpArrayShort) && 
		      isArray(TpArrayUShort) && isArray(TpArrayInt) && 
		      isArray(TpArrayUInt) && isArray(TpArrayFloat) && 
		      isArray(TpArrayDouble) && isArray(TpArrayComplex) &&
		      isArray(TpArrayDComplex) && isArray(TpArrayString) &&
		      isArray(TpArrayQuantity) && isArray(TpArrayInt64));
    AlwaysAssertExit(asScalar(TpBool) == TpBool &&
		     asScalar(TpChar) == TpChar &&
		     asScalar(TpUChar) == TpUChar &&
		     asScalar(TpShort) == TpShort &&
		     asScalar(TpUShort) == TpUShort &&
		     asScalar(TpInt) == TpInt &&
		     asScalar(TpUInt) == TpUInt &&
		     asScalar(TpInt64) == TpInt64 &&
		     asScalar(TpFloat) == TpFloat &&
		     asScalar(TpDouble) == TpDouble &&
		     asScalar(TpComplex) == TpComplex &&
		     asScalar(TpDComplex) == TpDComplex &&
		     asScalar(TpString) == TpString &&
		     asScalar(TpQuantity) == TpQuantity &&
		     asScalar(TpArrayBool) == TpBool &&
		     asScalar(TpArrayChar) == TpChar &&
		     asScalar(TpArrayUChar) == TpUChar &&
		     asScalar(TpArrayShort) == TpShort &&
		     asScalar(TpArrayUShort) == TpUShort &&
		     asScalar(TpArrayInt) == TpInt &&
		     asScalar(TpArrayUInt) == TpUInt &&
		     asScalar(TpArrayInt64) == TpInt64 &&
		     asScalar(TpArrayFloat) == TpFloat &&
		     asScalar(TpArrayDouble) == TpDouble &&
		     asScalar(TpArrayComplex) == TpComplex &&
		     asScalar(TpArrayDComplex) == TpDComplex &&
		     asScalar(TpArrayString) == TpString &&
		     asScalar(TpArrayQuantity) == TpQuantity);

    AlwaysAssertExit(asArray(TpBool) == TpArrayBool &&
		     asArray(TpChar) == TpArrayChar &&
		     asArray(TpUChar) == TpArrayUChar &&
		     asArray(TpShort) == TpArrayShort &&
		     asArray(TpUShort) == TpArrayUShort &&
		     asArray(TpInt) == TpArrayInt &&
		     asArray(TpUInt) == TpArrayUInt &&
		     asArray(TpInt64) == TpArrayInt64 &&
		     asArray(TpFloat) == TpArrayFloat &&
		     asArray(TpDouble) == TpArrayDouble &&
		     asArray(TpComplex) == TpArrayComplex &&
		     asArray(TpDComplex) == TpArrayDComplex &&
		     asArray(TpString) == TpArrayString &&
		     asArray(TpQuantity) == TpArrayQuantity &&
		     asArray(TpArrayBool) == TpArrayBool &&
		     asArray(TpArrayChar) == TpArrayChar &&
		     asArray(TpArrayUChar) == TpArrayUChar &&
		     asArray(TpArrayShort) == TpArrayShort &&
		     asArray(TpArrayUShort) == TpArrayUShort &&
		     asArray(TpArrayInt) == TpArrayInt &&
		     asArray(TpArrayUInt) == TpArrayUInt &&
		     asArray(TpArrayInt64) == TpArrayInt64 &&
		     asArray(TpArrayFloat) == TpArrayFloat &&
		     asArray(TpArrayDouble) == TpArrayDouble &&
		     asArray(TpArrayComplex) == TpArrayComplex &&
		     asArray(TpArrayDComplex) == TpArrayDComplex &&
		     asArray(TpArrayString) == TpArrayString &&
		     asArray(TpArrayQuantity) == TpArrayQuantity);
}

// to be called using types for which an exception from asScalar is expected
void excpAsScalar(DataType type)
{
    Bool hadExcp = False;
    try {
	asScalar(type);
    } catch (std::exception& x) {
	hadExcp = True;
    } 
    AlwaysAssert(hadExcp, AipsError);
}

void excpAsArray(DataType type)
{
    Bool hadExcp = False;
    try {
	asArray(type);
    } catch (std::exception& x) {
	hadExcp = True;
    } 
    AlwaysAssert(hadExcp, AipsError);
}

void excpTests() 
{
    excpAsScalar(TpTable);
    excpAsScalar(TpRecord);
    excpAsScalar(TpOther);
    excpAsArray(TpTable);
    excpAsArray(TpRecord);
    excpAsArray(TpOther);
}

int main()
{
    try {
	simpleTests();
	excpTests();
    } catch (std::exception& x) {
	cout << "Caught an exception: " << x.what() << endl;
	return 1;
    } 

    cout << "OK" << endl;
    return 0;
}
