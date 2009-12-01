//# CanonicalIO.cc: Class for IO in canonical format
//# Copyright (C) 1996,1999,2001
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

#include <casa/IO/CanonicalIO.h>
#include <casa/OS/CanonicalConversion.h>
#include <casa/IO/ByteIO.h>

namespace casa { //# NAMESPACE CASA - BEGIN

CanonicalIO::CanonicalIO (ByteIO* byteIO, uInt bufferLength, Bool takeOver)
: TypeIO          (byteIO, takeOver), 
  itsBuffer       (new char[bufferLength]),
  itsBufferLength (bufferLength)
{}

CanonicalIO::CanonicalIO (const CanonicalIO& that)
: TypeIO          (that), 
  itsBuffer       (new char[that.itsBufferLength]),
  itsBufferLength (that.itsBufferLength)
{}

CanonicalIO& CanonicalIO::operator= (const CanonicalIO& that)
{
    if (this != &that) {
        TypeIO::operator= (that);
	if (itsBufferLength != that.itsBufferLength) {
	    delete [] itsBuffer;
	    itsBufferLength = that.itsBufferLength;
	    itsBuffer = new char[itsBufferLength];
	}
    }
    return *this;
}

CanonicalIO::~CanonicalIO()
{
    delete [] itsBuffer;
}


uInt CanonicalIO::write (uInt nvalues, const Bool* value)
{
    return TypeIO::write (nvalues, value);
}

uInt CanonicalIO::write (uInt nvalues, const Char* value)
{
    if (CONVERT_CAN_CHAR) {
	if (nvalues * SIZE_CAN_CHAR <= itsBufferLength) {
	    CanonicalConversion::fromLocal (itsBuffer, value, nvalues);
	    itsByteIO->write (nvalues * SIZE_CAN_CHAR, itsBuffer);
    	} else {
	    char* tempBuffer = new char [nvalues * SIZE_CAN_CHAR];
	    CanonicalConversion::fromLocal (tempBuffer, value, nvalues);
	    itsByteIO->write (nvalues * SIZE_CAN_CHAR, tempBuffer);
	    delete [] tempBuffer;
	}
    } else {
	itsByteIO->write (nvalues * sizeof(Char), value);
    }
    return nvalues * SIZE_CAN_CHAR;
}

uInt CanonicalIO::write (uInt nvalues, const uChar* value)
{
    if (CONVERT_CAN_UCHAR) {
	if (nvalues * SIZE_CAN_UCHAR <= itsBufferLength) {
	    CanonicalConversion::fromLocal (itsBuffer, value, nvalues);
	    itsByteIO->write (nvalues * SIZE_CAN_UCHAR, itsBuffer);
    	} else {
	    char* tempBuffer = new char [nvalues * SIZE_CAN_UCHAR];
	    CanonicalConversion::fromLocal (tempBuffer, value, nvalues);
	    itsByteIO->write (nvalues * SIZE_CAN_UCHAR, tempBuffer);
	    delete [] tempBuffer;
	}
    } else {
	itsByteIO->write (nvalues * sizeof(uChar), value);
    }
    return nvalues * SIZE_CAN_UCHAR;
}

uInt CanonicalIO::write (uInt nvalues, const Short* value)
{
    if (CONVERT_CAN_SHORT) {
	if (nvalues * SIZE_CAN_SHORT <= itsBufferLength) {
	    CanonicalConversion::fromLocal (itsBuffer, value, nvalues);
	    itsByteIO->write (nvalues * SIZE_CAN_SHORT, itsBuffer);
    	} else {
	    char* tempBuffer = new char [nvalues * SIZE_CAN_SHORT];
	    CanonicalConversion::fromLocal (tempBuffer, value, nvalues);
	    itsByteIO->write (nvalues * SIZE_CAN_SHORT, tempBuffer);
	    delete [] tempBuffer;
	}
    } else {
	itsByteIO->write (nvalues * sizeof(Short), value);
    }
    return nvalues * SIZE_CAN_SHORT;
}

uInt CanonicalIO::write (uInt nvalues, const uShort* value)
{
    if (CONVERT_CAN_USHORT) {
	if (nvalues * SIZE_CAN_USHORT <= itsBufferLength) {
	    CanonicalConversion::fromLocal (itsBuffer, value, nvalues);
	    itsByteIO->write (nvalues * SIZE_CAN_USHORT, itsBuffer);
    	} else {
	    char* tempBuffer = new char [nvalues * SIZE_CAN_USHORT];
	    CanonicalConversion::fromLocal (tempBuffer, value, nvalues);
	    itsByteIO->write (nvalues * SIZE_CAN_USHORT, tempBuffer);
	    delete [] tempBuffer;
	}
    } else {
	itsByteIO->write (nvalues * sizeof(uShort), value);
    }
    return nvalues * SIZE_CAN_USHORT;
}

uInt CanonicalIO::write(uInt nvalues, const Int* value)
{
    if (CONVERT_CAN_INT) {
	if (nvalues * SIZE_CAN_INT <= itsBufferLength) {
	    CanonicalConversion::fromLocal (itsBuffer, value, nvalues);
	    itsByteIO->write (nvalues * SIZE_CAN_INT, itsBuffer);
    	} else {
	    char* tempBuffer = new char [nvalues * SIZE_CAN_INT];
	    CanonicalConversion::fromLocal (tempBuffer, value, nvalues);
	    itsByteIO->write (nvalues * SIZE_CAN_INT, tempBuffer);
	    delete [] tempBuffer;
	}
    } else {
	itsByteIO->write (nvalues * sizeof(Int), value);
    }
    return nvalues * SIZE_CAN_INT;
}

uInt CanonicalIO::write(uInt nvalues, const uInt* value)
{
    if (CONVERT_CAN_UINT) {
	if (nvalues * SIZE_CAN_UINT <= itsBufferLength) {
	    CanonicalConversion::fromLocal (itsBuffer, value, nvalues);
	    itsByteIO->write (nvalues * SIZE_CAN_UINT, itsBuffer);
    	} else {
	    char* tempBuffer = new char [nvalues * SIZE_CAN_UINT];
	    CanonicalConversion::fromLocal (tempBuffer, value, nvalues);
	    itsByteIO->write (nvalues * SIZE_CAN_UINT, tempBuffer);
	    delete [] tempBuffer;
	}
    } else {
	itsByteIO->write (nvalues * sizeof(uInt), value);
    }
    return nvalues * SIZE_CAN_UINT;
}

uInt CanonicalIO::write(uInt nvalues, const Int64* value)
{
    if (CONVERT_CAN_INT64) {
	if (nvalues * SIZE_CAN_INT64 <= itsBufferLength) {
	    CanonicalConversion::fromLocal (itsBuffer, value, nvalues);
	    itsByteIO->write (nvalues * SIZE_CAN_INT64, itsBuffer);
    	} else {
	    char* tempBuffer = new char [nvalues * SIZE_CAN_INT64];
	    CanonicalConversion::fromLocal (tempBuffer, value, nvalues);
	    itsByteIO->write (nvalues * SIZE_CAN_INT64, tempBuffer);
	    delete [] tempBuffer;
	}
    } else {
	itsByteIO->write (nvalues * sizeof(Int64), value);
    }
    return nvalues * SIZE_CAN_INT64;
}

uInt CanonicalIO::write(uInt nvalues, const uInt64* value)
{
    if (CONVERT_CAN_UINT64) {
	if (nvalues * SIZE_CAN_UINT64 <= itsBufferLength) {
	    CanonicalConversion::fromLocal (itsBuffer, value, nvalues);
	    itsByteIO->write (nvalues * SIZE_CAN_UINT64, itsBuffer);
    	} else {
	    char* tempBuffer = new char [nvalues * SIZE_CAN_UINT64];
	    CanonicalConversion::fromLocal (tempBuffer, value, nvalues);
	    itsByteIO->write (nvalues * SIZE_CAN_UINT64, tempBuffer);
	    delete [] tempBuffer;
	}
    } else {
	itsByteIO->write (nvalues * sizeof(uInt64), value);
    }
    return nvalues * SIZE_CAN_UINT64;
}

uInt CanonicalIO::write(uInt nvalues, const float* value)
{
    if (CONVERT_CAN_FLOAT) {
	if (nvalues * SIZE_CAN_FLOAT <= itsBufferLength) {
	    CanonicalConversion::fromLocal (itsBuffer, value, nvalues);
	    itsByteIO->write (nvalues * SIZE_CAN_FLOAT, itsBuffer);
    	} else {
	    char* tempBuffer = new char [nvalues * SIZE_CAN_FLOAT];
	    CanonicalConversion::fromLocal (tempBuffer, value, nvalues);
	    itsByteIO->write (nvalues * SIZE_CAN_FLOAT, tempBuffer);
	    delete [] tempBuffer;
	}
    } else {
	itsByteIO->write (nvalues * sizeof(float), value);
    }
    return nvalues * SIZE_CAN_FLOAT;
}

uInt CanonicalIO::write(uInt nvalues, const double* value)
{
    if (CONVERT_CAN_DOUBLE) {
	if (nvalues * SIZE_CAN_DOUBLE <= itsBufferLength) {
	    CanonicalConversion::fromLocal (itsBuffer, value, nvalues);
	    itsByteIO->write (nvalues * SIZE_CAN_DOUBLE, itsBuffer);
    	} else {
	    char* tempBuffer = new char [nvalues * SIZE_CAN_DOUBLE];
	    CanonicalConversion::fromLocal (tempBuffer, value, nvalues);
	    itsByteIO->write (nvalues * SIZE_CAN_DOUBLE, tempBuffer);
	    delete [] tempBuffer;
	}
    } else {
	itsByteIO->write (nvalues * sizeof(double), value);
    }
    return nvalues * SIZE_CAN_DOUBLE;
}

uInt CanonicalIO::write (uInt nvalues, const Complex* value)
{
    return TypeIO::write (nvalues, value);
}

uInt CanonicalIO::write (uInt nvalues, const DComplex* value)
{
    return TypeIO::write (nvalues, value);
}

uInt CanonicalIO::write (uInt nvalues, const String* value)
{
    return TypeIO::write (nvalues, value);
}


uInt CanonicalIO::read (uInt nvalues, Bool* value)
{
    return TypeIO::read (nvalues, value);
}

uInt CanonicalIO::read (uInt nvalues, Char* value)
{
    if (CONVERT_CAN_CHAR) {
	if (nvalues * SIZE_CAN_CHAR <= itsBufferLength) {
	    itsByteIO->read (nvalues * SIZE_CAN_CHAR, itsBuffer);
	    CanonicalConversion::toLocal (value, itsBuffer, nvalues);
	} else {
	    char* tempBuffer = new char[nvalues * SIZE_CAN_CHAR];
	    itsByteIO->read (nvalues * SIZE_CAN_CHAR, tempBuffer);
	    CanonicalConversion::toLocal (value, tempBuffer, nvalues);
	    delete [] tempBuffer;
	}
    } else {
	itsByteIO->read (nvalues * sizeof(Char), value);
    }
    return nvalues * SIZE_CAN_CHAR;
}

uInt CanonicalIO::read (uInt nvalues, uChar* value)
{
    if (CONVERT_CAN_UCHAR) {
	if (nvalues * SIZE_CAN_UCHAR <= itsBufferLength) {
	    itsByteIO->read (nvalues * SIZE_CAN_UCHAR, itsBuffer);
	    CanonicalConversion::toLocal (value, itsBuffer, nvalues);
	} else {
	    char* tempBuffer = new char[nvalues * SIZE_CAN_UCHAR];
	    itsByteIO->read (nvalues * SIZE_CAN_UCHAR, tempBuffer);
	    CanonicalConversion::toLocal (value, tempBuffer, nvalues);
	    delete [] tempBuffer;
	}
    } else {
	itsByteIO->read (nvalues * sizeof(uChar), value);
    }
    return nvalues * SIZE_CAN_UCHAR;
}

uInt CanonicalIO::read (uInt nvalues, Short* value)
{
    if (CONVERT_CAN_SHORT) {
	if (nvalues * SIZE_CAN_SHORT <= itsBufferLength) {
	    itsByteIO->read (nvalues * SIZE_CAN_SHORT, itsBuffer);
	    CanonicalConversion::toLocal (value, itsBuffer, nvalues);
	} else {
	    char* tempBuffer = new char[nvalues * SIZE_CAN_SHORT];
	    itsByteIO->read (nvalues * SIZE_CAN_SHORT, tempBuffer);
	    CanonicalConversion::toLocal (value, tempBuffer, nvalues);
	    delete [] tempBuffer;
	}
    } else {
	itsByteIO->read (nvalues * sizeof(Short), value);
    }
    return nvalues * SIZE_CAN_SHORT;
}

uInt CanonicalIO::read (uInt nvalues, uShort* value)
{
    if (CONVERT_CAN_USHORT) {
	if (nvalues * SIZE_CAN_USHORT <= itsBufferLength) {
	    itsByteIO->read (nvalues * SIZE_CAN_USHORT, itsBuffer);
	    CanonicalConversion::toLocal (value, itsBuffer, nvalues);
	} else {
	    char* tempBuffer = new char[nvalues * SIZE_CAN_USHORT];
	    itsByteIO->read (nvalues * SIZE_CAN_USHORT, tempBuffer);
	    CanonicalConversion::toLocal (value, tempBuffer, nvalues);
	    delete [] tempBuffer;
	}
    } else {
	itsByteIO->read (nvalues * sizeof(uShort), value);
    }
    return nvalues * SIZE_CAN_USHORT;
}

uInt CanonicalIO::read (uInt nvalues, Int* value)
{
    if (CONVERT_CAN_INT) {
	if (nvalues * SIZE_CAN_INT <= itsBufferLength) {
	    itsByteIO->read (nvalues * SIZE_CAN_INT, itsBuffer);
	    CanonicalConversion::toLocal (value, itsBuffer, nvalues);
	} else {
	    char* tempBuffer = new char[nvalues * SIZE_CAN_INT];
	    itsByteIO->read (nvalues * SIZE_CAN_INT, tempBuffer);
	    CanonicalConversion::toLocal (value, tempBuffer, nvalues);
	    delete [] tempBuffer;
	}
    } else {
	itsByteIO->read (nvalues * sizeof(Int), value);
    }
    return nvalues * SIZE_CAN_INT;
}

uInt CanonicalIO::read (uInt nvalues, uInt* value)
{
    if (CONVERT_CAN_UINT) {
	if (nvalues * SIZE_CAN_UINT <= itsBufferLength) {
	    itsByteIO->read (nvalues * SIZE_CAN_UINT, itsBuffer);
	    CanonicalConversion::toLocal (value, itsBuffer, nvalues);
	} else {
	    char* tempBuffer = new char[nvalues * SIZE_CAN_UINT];
	    itsByteIO->read (nvalues * SIZE_CAN_UINT, tempBuffer);
	    CanonicalConversion::toLocal (value, tempBuffer, nvalues);
	    delete [] tempBuffer;
	}
    } else {
	itsByteIO->read (nvalues * sizeof(uInt), value);
    }
    return nvalues * SIZE_CAN_UINT;
}

uInt CanonicalIO::read (uInt nvalues, Int64* value)
{
    if (CONVERT_CAN_INT64) {
	if (nvalues * SIZE_CAN_INT64 <= itsBufferLength) {
	    itsByteIO->read (nvalues * SIZE_CAN_INT64, itsBuffer);
	    CanonicalConversion::toLocal (value, itsBuffer, nvalues);
	} else {
	    char* tempBuffer = new char[nvalues * SIZE_CAN_INT64];
	    itsByteIO->read (nvalues * SIZE_CAN_INT64, tempBuffer);
	    CanonicalConversion::toLocal (value, tempBuffer, nvalues);
	    delete [] tempBuffer;
	}
    } else {
	itsByteIO->read (nvalues * sizeof(Int64), value);
    }
    return nvalues * SIZE_CAN_INT64;
}

uInt CanonicalIO::read (uInt nvalues, uInt64* value)
{
    if (CONVERT_CAN_UINT64) {
	if (nvalues * SIZE_CAN_UINT64 <= itsBufferLength) {
	    itsByteIO->read (nvalues * SIZE_CAN_UINT64, itsBuffer);
	    CanonicalConversion::toLocal(value, itsBuffer, nvalues);
	} else {
	    char* tempBuffer = new char[nvalues * SIZE_CAN_UINT64];
	    itsByteIO->read (nvalues * SIZE_CAN_UINT64, tempBuffer);
	    CanonicalConversion::toLocal (value, tempBuffer, nvalues);
	    delete [] tempBuffer;
	}
    } else {
	itsByteIO->read (nvalues * sizeof(uInt64), value);
    }
    return nvalues * SIZE_CAN_UINT64;
}

uInt CanonicalIO::read (uInt nvalues, float* value)
{
    if (CONVERT_CAN_FLOAT) {
	if (nvalues * SIZE_CAN_FLOAT <= itsBufferLength) {
	    itsByteIO->read (nvalues * SIZE_CAN_FLOAT, itsBuffer);
	    CanonicalConversion::toLocal (value, itsBuffer, nvalues);
	} else {
	    char* tempBuffer = new char[nvalues * SIZE_CAN_FLOAT];
	    itsByteIO->read (nvalues * SIZE_CAN_FLOAT, tempBuffer);
	    CanonicalConversion::toLocal (value, tempBuffer, nvalues);
	    delete [] tempBuffer;
	}
    } else {
	itsByteIO->read (nvalues * sizeof(float), value);
    }
    return nvalues * SIZE_CAN_FLOAT;
}

uInt CanonicalIO::read (uInt nvalues, double* value)
{
    if (CONVERT_CAN_DOUBLE) {
	if (nvalues * SIZE_CAN_DOUBLE <= itsBufferLength) {
	    itsByteIO->read (nvalues * SIZE_CAN_DOUBLE, itsBuffer);
	    CanonicalConversion::toLocal (value, itsBuffer, nvalues);
	} else {
	    char* tempBuffer = new char[nvalues * SIZE_CAN_DOUBLE];
	    itsByteIO->read (nvalues * SIZE_CAN_DOUBLE, tempBuffer);
	    CanonicalConversion::toLocal (value, tempBuffer, nvalues);
	    delete [] tempBuffer;
	}
    } else {
	itsByteIO->read (nvalues * sizeof(double), value);
    }
    return nvalues * SIZE_CAN_DOUBLE;
}

uInt CanonicalIO::read (uInt nvalues, Complex* value)
{
    return TypeIO::read (nvalues, value);
}

uInt CanonicalIO::read (uInt nvalues, DComplex* value)
{
    return TypeIO::read (nvalues, value);
}

uInt CanonicalIO::read (uInt nvalues, String* value)
{
    return TypeIO::read (nvalues, value);
}

} //# NAMESPACE CASA - END

