//# LECanonicalIO.cc: Class for IO in little endian canonical format
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
//# $Id$

#include <casacore/casa/IO/LECanonicalIO.h>
#include <casacore/casa/OS/LECanonicalConversion.h>
#include <casacore/casa/IO/ByteIO.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

LECanonicalIO::LECanonicalIO (ByteIO* byteIO, uInt bufferLength, Bool takeOver)
: TypeIO          (byteIO, takeOver), 
  itsBuffer       (new char[bufferLength]),
  itsBufferLength (bufferLength)
{}

LECanonicalIO::LECanonicalIO (const LECanonicalIO& that)
: TypeIO          (that), 
  itsBuffer       (new char[that.itsBufferLength]),
  itsBufferLength (that.itsBufferLength)
{}

LECanonicalIO& LECanonicalIO::operator= (const LECanonicalIO& that)
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

LECanonicalIO::~LECanonicalIO()
{
    delete [] itsBuffer;
}


size_t LECanonicalIO::write (size_t nvalues, const Bool* value)
{
    return TypeIO::write (nvalues, value);
}

size_t LECanonicalIO::write (size_t nvalues, const Char* value)
{
    if (CONVERT_LECAN_CHAR) {
	if (nvalues * SIZE_LECAN_CHAR <= itsBufferLength) {
	    LECanonicalConversion::fromLocal (itsBuffer, value, nvalues);
	    itsByteIO->write (nvalues * SIZE_LECAN_CHAR, itsBuffer);
    	} else {
	    char* tempBuffer = new char [nvalues * SIZE_LECAN_CHAR];
	    LECanonicalConversion::fromLocal (tempBuffer, value, nvalues);
	    itsByteIO->write (nvalues * SIZE_LECAN_CHAR, tempBuffer);
	    delete [] tempBuffer;
	}
    } else {
	itsByteIO->write (nvalues * sizeof(Char), value);
    }
    return nvalues * SIZE_LECAN_CHAR;
}

size_t LECanonicalIO::write (size_t nvalues, const uChar* value)
{
    if (CONVERT_LECAN_UCHAR) {
	if (nvalues * SIZE_LECAN_UCHAR <= itsBufferLength) {
	    LECanonicalConversion::fromLocal (itsBuffer, value, nvalues);
	    itsByteIO->write (nvalues * SIZE_LECAN_UCHAR, itsBuffer);
    	} else {
	    char* tempBuffer = new char [nvalues * SIZE_LECAN_UCHAR];
	    LECanonicalConversion::fromLocal (tempBuffer, value, nvalues);
	    itsByteIO->write (nvalues * SIZE_LECAN_UCHAR, tempBuffer);
	    delete [] tempBuffer;
	}
    } else {
	itsByteIO->write (nvalues * sizeof(uChar), value);
    }
    return nvalues * SIZE_LECAN_UCHAR;
}

size_t LECanonicalIO::write (size_t nvalues, const Short* value)
{
    if (CONVERT_LECAN_SHORT) {
	if (nvalues * SIZE_LECAN_SHORT <= itsBufferLength) {
	    LECanonicalConversion::fromLocal (itsBuffer, value, nvalues);
	    itsByteIO->write (nvalues * SIZE_LECAN_SHORT, itsBuffer);
    	} else {
	    char* tempBuffer = new char [nvalues * SIZE_LECAN_SHORT];
	    LECanonicalConversion::fromLocal (tempBuffer, value, nvalues);
	    itsByteIO->write (nvalues * SIZE_LECAN_SHORT, tempBuffer);
	    delete [] tempBuffer;
	}
    } else {
	itsByteIO->write (nvalues * sizeof(Short), value);
    }
    return nvalues * SIZE_LECAN_SHORT;
}

size_t LECanonicalIO::write (size_t nvalues, const uShort* value)
{
    if (CONVERT_LECAN_USHORT) {
	if (nvalues * SIZE_LECAN_USHORT <= itsBufferLength) {
	    LECanonicalConversion::fromLocal (itsBuffer, value, nvalues);
	    itsByteIO->write (nvalues * SIZE_LECAN_USHORT, itsBuffer);
    	} else {
	    char* tempBuffer = new char [nvalues * SIZE_LECAN_USHORT];
	    LECanonicalConversion::fromLocal (tempBuffer, value, nvalues);
	    itsByteIO->write (nvalues * SIZE_LECAN_USHORT, tempBuffer);
	    delete [] tempBuffer;
	}
    } else {
	itsByteIO->write (nvalues * sizeof(uShort), value);
    }
    return nvalues * SIZE_LECAN_USHORT;
}

size_t LECanonicalIO::write(size_t nvalues, const Int* value)
{
    if (CONVERT_LECAN_INT) {
	if (nvalues * SIZE_LECAN_INT <= itsBufferLength) {
	    LECanonicalConversion::fromLocal (itsBuffer, value, nvalues);
	    itsByteIO->write (nvalues * SIZE_LECAN_INT, itsBuffer);
    	} else {
	    char* tempBuffer = new char [nvalues * SIZE_LECAN_INT];
	    LECanonicalConversion::fromLocal (tempBuffer, value, nvalues);
	    itsByteIO->write (nvalues * SIZE_LECAN_INT, tempBuffer);
	    delete [] tempBuffer;
	}
    } else {
	itsByteIO->write (nvalues * sizeof(Int), value);
    }
    return nvalues * SIZE_LECAN_INT;
}

size_t LECanonicalIO::write(size_t nvalues, const uInt* value)
{
    if (CONVERT_LECAN_UINT) {
	if (nvalues * SIZE_LECAN_UINT <= itsBufferLength) {
	    LECanonicalConversion::fromLocal (itsBuffer, value, nvalues);
	    itsByteIO->write (nvalues * SIZE_LECAN_UINT, itsBuffer);
    	} else {
	    char* tempBuffer = new char [nvalues * SIZE_LECAN_UINT];
	    LECanonicalConversion::fromLocal (tempBuffer, value, nvalues);
	    itsByteIO->write (nvalues * SIZE_LECAN_UINT, tempBuffer);
	    delete [] tempBuffer;
	}
    } else {
	itsByteIO->write (nvalues * sizeof(uInt), value);
    }
    return nvalues * SIZE_LECAN_UINT;
}

size_t LECanonicalIO::write(size_t nvalues, const Int64* value)
{
    if (CONVERT_LECAN_INT64) {
	if (nvalues * SIZE_LECAN_INT64 <= itsBufferLength) {
	    LECanonicalConversion::fromLocal (itsBuffer, value, nvalues);
	    itsByteIO->write (nvalues * SIZE_LECAN_INT64, itsBuffer);
    	} else {
	    char* tempBuffer = new char [nvalues * SIZE_LECAN_INT64];
	    LECanonicalConversion::fromLocal (tempBuffer, value, nvalues);
	    itsByteIO->write (nvalues * SIZE_LECAN_INT64, tempBuffer);
	    delete [] tempBuffer;
	}
    } else {
	itsByteIO->write (nvalues * sizeof(Int64), value);
    }
    return nvalues * SIZE_LECAN_INT64;
}

size_t LECanonicalIO::write(size_t nvalues, const uInt64* value)
{
    if (CONVERT_LECAN_UINT64) {
	if (nvalues * SIZE_LECAN_UINT64 <= itsBufferLength) {
	    LECanonicalConversion::fromLocal (itsBuffer, value, nvalues);
	    itsByteIO->write (nvalues * SIZE_LECAN_UINT64, itsBuffer);
    	} else {
	    char* tempBuffer = new char [nvalues * SIZE_LECAN_UINT64];
	    LECanonicalConversion::fromLocal (tempBuffer, value, nvalues);
	    itsByteIO->write (nvalues * SIZE_LECAN_UINT64, tempBuffer);
	    delete [] tempBuffer;
	}
    } else {
	itsByteIO->write (nvalues * sizeof(uInt64), value);
    }
    return nvalues * SIZE_LECAN_UINT64;
}

size_t LECanonicalIO::write(size_t nvalues, const float* value)
{
    if (CONVERT_LECAN_FLOAT) {
	if (nvalues * SIZE_LECAN_FLOAT <= itsBufferLength) {
	    LECanonicalConversion::fromLocal (itsBuffer, value, nvalues);
	    itsByteIO->write (nvalues * SIZE_LECAN_FLOAT, itsBuffer);
    	} else {
	    char* tempBuffer = new char [nvalues * SIZE_LECAN_FLOAT];
	    LECanonicalConversion::fromLocal (tempBuffer, value, nvalues);
	    itsByteIO->write (nvalues * SIZE_LECAN_FLOAT, tempBuffer);
	    delete [] tempBuffer;
	}
    } else {
	itsByteIO->write (nvalues * sizeof(float), value);
    }
    return nvalues * SIZE_LECAN_FLOAT;
}

size_t LECanonicalIO::write(size_t nvalues, const double* value)
{
    if (CONVERT_LECAN_DOUBLE) {
	if (nvalues * SIZE_LECAN_DOUBLE <= itsBufferLength) {
	    LECanonicalConversion::fromLocal (itsBuffer, value, nvalues);
	    itsByteIO->write (nvalues * SIZE_LECAN_DOUBLE, itsBuffer);
    	} else {
	    char* tempBuffer = new char [nvalues * SIZE_LECAN_DOUBLE];
	    LECanonicalConversion::fromLocal (tempBuffer, value, nvalues);
	    itsByteIO->write (nvalues * SIZE_LECAN_DOUBLE, tempBuffer);
	    delete [] tempBuffer;
	}
    } else {
	itsByteIO->write (nvalues * sizeof(double), value);
    }
    return nvalues * SIZE_LECAN_DOUBLE;
}

size_t LECanonicalIO::write (size_t nvalues, const Complex* value)
{
    return TypeIO::write (nvalues, value);
}

size_t LECanonicalIO::write (size_t nvalues, const DComplex* value)
{
    return TypeIO::write (nvalues, value);
}

size_t LECanonicalIO::write (size_t nvalues, const String* value)
{
    return TypeIO::write (nvalues, value);
}


size_t LECanonicalIO::read (size_t nvalues, Bool* value)
{
    return TypeIO::read (nvalues, value);
}

size_t LECanonicalIO::read (size_t nvalues, Char* value)
{
    if (CONVERT_LECAN_CHAR) {
	if (nvalues * SIZE_LECAN_CHAR <= itsBufferLength) {
	    itsByteIO->read (nvalues * SIZE_LECAN_CHAR, itsBuffer);
	    LECanonicalConversion::toLocal (value, itsBuffer, nvalues);
	} else {
	    char* tempBuffer = new char[nvalues * SIZE_LECAN_CHAR];
	    itsByteIO->read (nvalues * SIZE_LECAN_CHAR, tempBuffer);
	    LECanonicalConversion::toLocal (value, tempBuffer, nvalues);
	    delete [] tempBuffer;
	}
    } else {
	itsByteIO->read (nvalues * sizeof(Char), value);
    }
    return nvalues * SIZE_LECAN_CHAR;
}

size_t LECanonicalIO::read (size_t nvalues, uChar* value)
{
    if (CONVERT_LECAN_UCHAR) {
	if (nvalues * SIZE_LECAN_UCHAR <= itsBufferLength) {
	    itsByteIO->read (nvalues * SIZE_LECAN_UCHAR, itsBuffer);
	    LECanonicalConversion::toLocal (value, itsBuffer, nvalues);
	} else {
	    char* tempBuffer = new char[nvalues * SIZE_LECAN_UCHAR];
	    itsByteIO->read (nvalues * SIZE_LECAN_UCHAR, tempBuffer);
	    LECanonicalConversion::toLocal (value, tempBuffer, nvalues);
	    delete [] tempBuffer;
	}
    } else {
	itsByteIO->read (nvalues * sizeof(uChar), value);
    }
    return nvalues * SIZE_LECAN_UCHAR;
}

size_t LECanonicalIO::read (size_t nvalues, Short* value)
{
    if (CONVERT_LECAN_SHORT) {
	if (nvalues * SIZE_LECAN_SHORT <= itsBufferLength) {
	    itsByteIO->read (nvalues * SIZE_LECAN_SHORT, itsBuffer);
	    LECanonicalConversion::toLocal (value, itsBuffer, nvalues);
	} else {
	    char* tempBuffer = new char[nvalues * SIZE_LECAN_SHORT];
	    itsByteIO->read (nvalues * SIZE_LECAN_SHORT, tempBuffer);
	    LECanonicalConversion::toLocal (value, tempBuffer, nvalues);
	    delete [] tempBuffer;
	}
    } else {
	itsByteIO->read (nvalues * sizeof(Short), value);
    }
    return nvalues * SIZE_LECAN_SHORT;
}

size_t LECanonicalIO::read (size_t nvalues, uShort* value)
{
    if (CONVERT_LECAN_USHORT) {
	if (nvalues * SIZE_LECAN_USHORT <= itsBufferLength) {
	    itsByteIO->read (nvalues * SIZE_LECAN_USHORT, itsBuffer);
	    LECanonicalConversion::toLocal (value, itsBuffer, nvalues);
	} else {
	    char* tempBuffer = new char[nvalues * SIZE_LECAN_USHORT];
	    itsByteIO->read (nvalues * SIZE_LECAN_USHORT, tempBuffer);
	    LECanonicalConversion::toLocal (value, tempBuffer, nvalues);
	    delete [] tempBuffer;
	}
    } else {
	itsByteIO->read (nvalues * sizeof(uShort), value);
    }
    return nvalues * SIZE_LECAN_USHORT;
}

size_t LECanonicalIO::read (size_t nvalues, Int* value)
{
    if (CONVERT_LECAN_INT) {
	if (nvalues * SIZE_LECAN_INT <= itsBufferLength) {
	    itsByteIO->read (nvalues * SIZE_LECAN_INT, itsBuffer);
	    LECanonicalConversion::toLocal (value, itsBuffer, nvalues);
	} else {
	    char* tempBuffer = new char[nvalues * SIZE_LECAN_INT];
	    itsByteIO->read (nvalues * SIZE_LECAN_INT, tempBuffer);
	    LECanonicalConversion::toLocal (value, tempBuffer, nvalues);
	    delete [] tempBuffer;
	}
    } else {
	itsByteIO->read (nvalues * sizeof(Int), value);
    }
    return nvalues * SIZE_LECAN_INT;
}

size_t LECanonicalIO::read (size_t nvalues, uInt* value)
{
    if (CONVERT_LECAN_UINT) {
	if (nvalues * SIZE_LECAN_UINT <= itsBufferLength) {
	    itsByteIO->read (nvalues * SIZE_LECAN_UINT, itsBuffer);
	    LECanonicalConversion::toLocal (value, itsBuffer, nvalues);
	} else {
	    char* tempBuffer = new char[nvalues * SIZE_LECAN_UINT];
	    itsByteIO->read (nvalues * SIZE_LECAN_UINT, tempBuffer);
	    LECanonicalConversion::toLocal (value, tempBuffer, nvalues);
	    delete [] tempBuffer;
	}
    } else {
	itsByteIO->read (nvalues * sizeof(uInt), value);
    }
    return nvalues * SIZE_LECAN_UINT;
}

size_t LECanonicalIO::read (size_t nvalues, Int64* value)
{
    if (CONVERT_LECAN_INT64) {
	if (nvalues * SIZE_LECAN_INT64 <= itsBufferLength) {
	    itsByteIO->read (nvalues * SIZE_LECAN_INT64, itsBuffer);
	    LECanonicalConversion::toLocal (value, itsBuffer, nvalues);
	} else {
	    char* tempBuffer = new char[nvalues * SIZE_LECAN_INT64];
	    itsByteIO->read (nvalues * SIZE_LECAN_INT64, tempBuffer);
	    LECanonicalConversion::toLocal (value, tempBuffer, nvalues);
	    delete [] tempBuffer;
	}
    } else {
	itsByteIO->read (nvalues * sizeof(Int64), value);
    }
    return nvalues * SIZE_LECAN_INT64;
}

size_t LECanonicalIO::read (size_t nvalues, uInt64* value)
{
    if (CONVERT_LECAN_UINT64) {
	if (nvalues * SIZE_LECAN_UINT64 <= itsBufferLength) {
	    itsByteIO->read (nvalues * SIZE_LECAN_UINT64, itsBuffer);
	    LECanonicalConversion::toLocal(value, itsBuffer, nvalues);
	} else {
	    char* tempBuffer = new char[nvalues * SIZE_LECAN_UINT64];
	    itsByteIO->read (nvalues * SIZE_LECAN_UINT64, tempBuffer);
	    LECanonicalConversion::toLocal (value, tempBuffer, nvalues);
	    delete [] tempBuffer;
	}
    } else {
	itsByteIO->read (nvalues * sizeof(uInt64), value);
    }
    return nvalues * SIZE_LECAN_UINT64;
}

size_t LECanonicalIO::read (size_t nvalues, float* value)
{
    if (CONVERT_LECAN_FLOAT) {
	if (nvalues * SIZE_LECAN_FLOAT <= itsBufferLength) {
	    itsByteIO->read (nvalues * SIZE_LECAN_FLOAT, itsBuffer);
	    LECanonicalConversion::toLocal (value, itsBuffer, nvalues);
	} else {
	    char* tempBuffer = new char[nvalues * SIZE_LECAN_FLOAT];
	    itsByteIO->read (nvalues * SIZE_LECAN_FLOAT, tempBuffer);
	    LECanonicalConversion::toLocal (value, tempBuffer, nvalues);
	    delete [] tempBuffer;
	}
    } else {
	itsByteIO->read (nvalues * sizeof(float), value);
    }
    return nvalues * SIZE_LECAN_FLOAT;
}

size_t LECanonicalIO::read (size_t nvalues, double* value)
{
    if (CONVERT_LECAN_DOUBLE) {
	if (nvalues * SIZE_LECAN_DOUBLE <= itsBufferLength) {
	    itsByteIO->read (nvalues * SIZE_LECAN_DOUBLE, itsBuffer);
	    LECanonicalConversion::toLocal (value, itsBuffer, nvalues);
	} else {
	    char* tempBuffer = new char[nvalues * SIZE_LECAN_DOUBLE];
	    itsByteIO->read (nvalues * SIZE_LECAN_DOUBLE, tempBuffer);
	    LECanonicalConversion::toLocal (value, tempBuffer, nvalues);
	    delete [] tempBuffer;
	}
    } else {
	itsByteIO->read (nvalues * sizeof(double), value);
    }
    return nvalues * SIZE_LECAN_DOUBLE;
}

size_t LECanonicalIO::read (size_t nvalues, Complex* value)
{
    return TypeIO::read (nvalues, value);
}

size_t LECanonicalIO::read (size_t nvalues, DComplex* value)
{
    return TypeIO::read (nvalues, value);
}

size_t LECanonicalIO::read (size_t nvalues, String* value)
{
    return TypeIO::read (nvalues, value);
}

} //# NAMESPACE CASACORE - END

