//# StArrayFile.cc: Read/write array in external format for a storage manager
//# Copyright (C) 1994,1995,1996,1997,1999,2000,2001,2002
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

#include <casacore/tables/DataMan/StArrayFile.h>
#include <casacore/casa/OS/RegularFile.h>
#include <casacore/casa/IO/MFFileIO.h>
#include <casacore/casa/IO/CanonicalIO.h>
#include <casacore/casa/IO/LECanonicalIO.h>
#include <casacore/casa/OS/CanonicalConversion.h>
#include <casacore/casa/OS/LECanonicalConversion.h>
#include <casacore/casa/Arrays/IPosition.h>
#include <casacore/casa/Containers/Block.h>
#include <casacore/casa/BasicSL/Complex.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/casa/Utilities/ValType.h>
#include <casacore/casa/OS/Path.h>
#include <casacore/tables/DataMan/DataManError.h>
#include <casacore/casa/Utilities/Assert.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

StManArrayFile::StManArrayFile (const String& fname, ByteIO::OpenOption fop,
				uint32_t version, bool bigEndian,
				uint32_t bufferSize,
                                const std::shared_ptr<MultiFileBase>& mfile)
: leng_p    (16),
  version_p (version),
  hasPut_p  (false)
{
    // The maximum version is 1.
    if (version_p > 1) {
	version_p = 1;
    }
    if (bufferSize == 0) {
      bufferSize = 65536;
    }
    //# Open file name as input and/or output; throw exception if it fails.
    if (mfile) {
      file_p = new MFFileIO (mfile, fname, fop);
    } else {
      file_p = new RegularFileIO (RegularFile(fname), fop, bufferSize);
    }
    if (bigEndian) {
	iofil_p = new CanonicalIO (file_p);
    }else{
	iofil_p = new LECanonicalIO (file_p);
    }
    AlwaysAssert (iofil_p != 0, AipsError);
    swput_p = iofil_p->isWritable();
    //# Get the version and length for an existing file.
    //# Otherwise set put-flag.
    resync();
    sizeChar_p   = ValType::getCanonicalSize (TpChar,   bigEndian);
    sizeuChar_p  = ValType::getCanonicalSize (TpUChar,  bigEndian);
    sizeShort_p  = ValType::getCanonicalSize (TpShort,  bigEndian);
    sizeuShort_p = ValType::getCanonicalSize (TpUShort, bigEndian);
    sizeInt_p    = ValType::getCanonicalSize (TpInt,    bigEndian);
    sizeuInt_p   = ValType::getCanonicalSize (TpUInt,   bigEndian);
    sizeInt64_p  = ValType::getCanonicalSize (TpInt64,  bigEndian);
    sizeFloat_p  = ValType::getCanonicalSize (TpFloat,  bigEndian);
    sizeDouble_p = ValType::getCanonicalSize (TpDouble, bigEndian);
}


//# Close the file.
//# Delete it if required.
StManArrayFile::~StManArrayFile ()
{
    //# Write the version and file length at the beginning.
    flush (false);
    delete iofil_p;
    delete file_p;
}


bool StManArrayFile::flush (bool)
{
    if (hasPut_p) {
	setpos (0);
	put (version_p);
	iofil_p->write (1, &leng_p);
	hasPut_p = false;
	file_p->flush();
	setpos (leng_p);
	return true;
    }
    return false;
}

// Resync the file (i.e. clear possible cache information).
void StManArrayFile::resync()
{
    file_p->resync();
    if (iofil_p->seek (0, ByteIO::End) > 0) {
        setpos (0);
	get (version_p);
	iofil_p->read (1, &leng_p);
    }else{
        setpos (0);
	put (version_p);
	iofil_p->write (1, &leng_p);
	//# Put a 0 to fill up the buffer and make valgrind happy.
	put (int32_t(0));
        hasPut_p = true;
    }
}

void StManArrayFile::setpos (int64_t pos)
{
    int64_t newpos = iofil_p->seek (pos);
    if (newpos != pos) {
	throw (DataManError ("StManArrayFile::setpos failed in file " +
                             file_p->fileName()));
    }
}


void StManArrayFile::put (int64_t fileOff, int64_t arrayOff, uint64_t nr,
			  const float* data)
{
    setpos (fileOff + arrayOff*sizeFloat_p);
    iofil_p->write (nr, data);
    hasPut_p = true;
}

uint32_t StManArrayFile::putShape (const IPosition& shape, int64_t& offset,
			       const float*)
    { return putRes (shape, offset, sizeFloat_p); }

void StManArrayFile::get (int64_t fileOff, int64_t arrayOff, uint64_t nr,
                          float* data)
{
    setpos (fileOff + arrayOff*sizeFloat_p);
    iofil_p->read (nr, data);
}

void StManArrayFile::copyArrayFloat (int64_t to, int64_t from, uint64_t nr)
    { copyData (to, from, nr*sizeFloat_p); }


//# Put a vector at the given offset.
#define STMANARRAYFILE_PUTGET(T,NM,SIZEDTYPE)                           \
void StManArrayFile::put (int64_t fileOff, int64_t arrayOff, uint64_t nr, \
			  const T* data) \
{ \
    setpos (fileOff + arrayOff*SIZEDTYPE); \
    iofil_p->write (nr, data); \
    hasPut_p = true; \
} \
uint32_t StManArrayFile::putShape (const IPosition& shape, int64_t& offset, \
			       const T*) \
    { return putRes (shape, offset, SIZEDTYPE); } \
void StManArrayFile::get (int64_t fileOff, int64_t arrayOff, uint64_t nr, T* data) \
{ \
    setpos (fileOff + arrayOff*SIZEDTYPE); \
    iofil_p->read (nr, data); \
} \
void StManArrayFile::aips_name2(copyArray,NM) (int64_t to, int64_t from, uint64_t nr)\
    { copyData (to, from, nr*SIZEDTYPE); }

STMANARRAYFILE_PUTGET(char, Char, sizeChar_p)
STMANARRAYFILE_PUTGET(unsigned char, uChar, sizeuChar_p)
STMANARRAYFILE_PUTGET(int16_t, Short, sizeShort_p)
STMANARRAYFILE_PUTGET(uint16_t, uShort, sizeuShort_p)
STMANARRAYFILE_PUTGET(int32_t, Int, sizeInt_p)
STMANARRAYFILE_PUTGET(uint32_t, uInt, sizeuInt_p)
STMANARRAYFILE_PUTGET(int64_t, Int64, sizeInt64_p)
STMANARRAYFILE_PUTGET(uint64_t, uInt64, sizeuInt64_p)
//#//STMANARRAYFILE_PUTGET(float, Float, sizeFloat_p)
STMANARRAYFILE_PUTGET(double, Double, sizeDouble_p)
//#//STMANARRAYFILE_PUTGET(long double, sizeLDouble_p)


//# Handle it for bool.
void StManArrayFile::put (int64_t fileOff, int64_t arrayOff, uint64_t nr,
			  const bool* data)
{
    //# Bools are stored as bits, thus a bit more complex.
    uint64_t start  = arrayOff / 8;
    uint32_t   stbit  = arrayOff - 8 * start;
    uint64_t end    = (arrayOff + nr) / 8;
    uint32_t   endbit = arrayOff + nr - 8 * end;
    if (endbit != 0) {
	end++;
    }
    //# Allocate a buffer of the required length.
    //# Read first and/or last byte when partially accessed.
    unsigned char* buf = new unsigned char[end - start];
    if (endbit != 0) {
	setpos (fileOff + end - 1);
	iofil_p->read (1, buf+end-start-1);
    }
    //# Avoid reading same byte twice if all bits are in 1 byte.
    if (stbit != 0  &&  (endbit == 0  ||  start < end-1)) {
	setpos (fileOff + start);
	iofil_p->read (1, buf);
    }
    Conversion::boolToBit (buf, data, stbit, nr);
    setpos (fileOff + start);
    iofil_p->write (end - start, buf);
    hasPut_p = true;
    delete [] buf;
}
uint32_t StManArrayFile::putShape (const IPosition& shape, int64_t& offset,
			       const bool*)
    { return putRes (shape, offset, 0.125); }
void StManArrayFile::get (int64_t fileOff, int64_t arrayOff, uint64_t nr, bool* data)
{
    //# Bools are stored as bits, thus a bit more complex.
    uint64_t start  = arrayOff / 8;
    uint32_t   stbit  = arrayOff - 8 * start;
    uint64_t end    = (arrayOff + nr) / 8;
    uint32_t   endbit = arrayOff + nr - 8 * end;
    if (endbit != 0) {
	end++;
    }
    //# Allocate a buffer of the required length.
    unsigned char* buf = new unsigned char[end - start];
    setpos (fileOff + start);
    iofil_p->read (end - start, buf);
    Conversion::bitToBool (data, buf, stbit, nr);
    delete [] buf;
}
void StManArrayFile::copyArrayBool (int64_t to, int64_t from, uint64_t nr)
    { copyData (to, from, (nr+7)/8); }


//# Handle it for Complex and String.
//# A Complex consists of 2 float values.
//# For a string its file offset gets stored (as a uint32_t), while the
//# string itself will be put at the end of the file.
uint32_t StManArrayFile::putShape (const IPosition& shape, int64_t& offset,
			       const Complex*)
    { return putRes (shape, offset, 2*sizeFloat_p); }
uint32_t StManArrayFile::putShape (const IPosition& shape, int64_t& offset,
			       const DComplex*)
    { return putRes (shape, offset, 2*sizeDouble_p); }
uint32_t StManArrayFile::putShape (const IPosition& shape, int64_t& offset,
			       const String*)
{
    uint32_t   n  = putRes (shape, offset, sizeuInt_p);
    uint64_t nr = shape.product();
    Block<uint32_t> data(nr, 0u);
    put (offset+n, 0, nr, data.storage());
    return n;
}

//# Put a complex vector at the given file offset.
void StManArrayFile::put (int64_t fileOff, int64_t arrayOff, uint64_t nr,
			  const Complex* data)
{
    setpos (fileOff + arrayOff*2*sizeFloat_p);
    iofil_p->write (2*nr, (const float*)data);
    hasPut_p = true;
}
void StManArrayFile::put (int64_t fileOff, int64_t arrayOff, uint64_t nr,
			  const DComplex* data)
{
    setpos (fileOff + arrayOff*2*sizeDouble_p);
    iofil_p->write (2*nr, (const double*)data);
    hasPut_p = true;
}

//# Put a string at the given file offset.
void StManArrayFile::put (int64_t fileOff, int64_t arrayOff, uint64_t nr,
			  const String* data)
{
    //# Get file offset for string offset array.
    //# Allocate a buffer to hold 4096 string offsets.
    int64_t offs = fileOff + arrayOff*sizeuInt_p;
    uint32_t buf[4096];
    uint64_t n;
    while (nr > 0) {
	n = (nr < 4096  ?  nr : 4096);
	setpos (leng_p);                            // position at end of file
	for (uint64_t i=0; i<n; i++) {
            // The offset in the file is an uint32_t.
            // Note: this should be fixed one time and make it uint64_t.
            AlwaysAssert (leng_p < int64_t(65536)*65536, DataManError);
	    buf[i] = leng_p;
	    leng_p += put (uint32_t(data->length()));   // write string length
	    leng_p += iofil_p->write (data->length(), data->chars());
	    data++;
	}
	//# Write the offsets.
	setpos (offs);
	offs += iofil_p->write (n, buf);
	hasPut_p = true;
	nr   -= n;
    }
}


//# Get a complex vector at the given file offset.
void StManArrayFile::get (int64_t fileOff, int64_t arrayOff, uint64_t nr,
			  Complex* data)
{
    setpos (fileOff + arrayOff*2*sizeFloat_p);
    iofil_p->read (2*nr, (float*)data);
}
void StManArrayFile::get (int64_t fileOff, int64_t arrayOff, uint64_t nr,
			  DComplex* data)
{
    setpos (fileOff + arrayOff*2*sizeDouble_p);
    iofil_p->read (2*nr, (double*)data);
}

//# Get a string at the given file offset.
void StManArrayFile::get (int64_t fileOff, int64_t arrayOff, uint64_t nr,
			  String* data)
{
    //# Get file offset for string offset array.
    //# Allocate a buffer to hold 4096 string offsets.
    int64_t offs = fileOff + arrayOff*sizeuInt_p;
    uint32_t buf[4096];
    uint64_t n;
    uint32_t l;
    while (nr > 0) {
	n = (nr < 4096  ?  nr : 4096);
	setpos (offs);
	offs += iofil_p->read (n, buf);
	for (uint64_t i=0; i<n; i++) {
	    if (buf[i] == 0) {
	        *data = String();
	    } else {
	        setpos (buf[i]);
		get (l);                    // read string length
		data->resize (l);   // resize storage which adds trailing 0
		char* ptr = &((*data)[0]);  // get actual string
		iofil_p->read (data->length(), ptr);
#ifdef USE_OLD_STRING
		ptr[l] = '\0';
#endif
	    }
	    data++;
	}
	nr -= n;
    }
}


void StManArrayFile::copyArrayComplex (int64_t to, int64_t from, uint64_t nr)
    { copyData (to, from, nr*2*sizeFloat_p); }
void StManArrayFile::copyArrayDComplex (int64_t to, int64_t from, uint64_t nr)
    { copyData (to, from, nr*2*sizeDouble_p); }
void StManArrayFile::copyArrayString (int64_t to, int64_t from, uint64_t nr)
{
    String data[4096];
    uint64_t ndone = 0;
    for (uint64_t n=0; nr>0; nr-=n) {
	n = (nr < 4096  ?  nr : 4096);
	get (from, ndone, n, data);
	put (to, ndone, n, data);
	ndone += n;
    }
}

//# Copy the data of the given length from one file offset to another.
void StManArrayFile::copyData (int64_t to, int64_t from, uint64_t length)
{
    unsigned char buffer[32768];
    for (uint64_t n=0; length>0; length-=n) {
	n = (length < 32768  ?  length : 32768);
	setpos (from);
	from += iofil_p->read (n, buffer);
	setpos (to);
	to += iofil_p->write  (n, buffer);
	hasPut_p = true;
    }
}

//# Write shape and reserve file space for the array by
//# increasing the length.
//# Take care it is at 8 byte boundary.
//# Write something in the last byte to make sure the file is extended.
uint32_t StManArrayFile::putRes (const IPosition& shape, int64_t& offset,
			     float lenElem)
{
    leng_p = 8 * ((leng_p+7) / 8);
    offset = leng_p;
    uint32_t n = 0;
    setpos (leng_p);
    // Put reference count in higher versions only.
    if (version_p > 0) {
	n += put (uint32_t(1));
    }
    n += put (uint32_t(shape.nelements()));
    for (uint32_t i=0; i<shape.nelements(); i++) {
      n += put (int32_t(shape(i)));
    }
    // Add length of shape and of entire array to file length.
    // Take care of rounding (needed for bool case).
    leng_p += n;
    leng_p += int64_t (double(shape.product()) * lenElem + 0.95);
    setpos (leng_p - 1);
    char c = 0;
    iofil_p->write (1, &c);
    hasPut_p = true;
    return n;
}

//# Get the shape at the given file offset
//# and returns its length in the file.
uint32_t StManArrayFile::getShape (int64_t fileOff, IPosition& shape)
{
    setpos (fileOff);
    uint32_t n=0;
    if (version_p > 0) {
	uint32_t refCount;
	n = get (refCount);
    }
    uint32_t nr;
    n += get (nr);
    shape.resize (nr);
    int32_t tmp;
    for (uint32_t i=0; i<nr; i++) {
	n += get (tmp);
        shape(i) = tmp;
    }
    return n;
}

//# Update the reference count.
uint32_t StManArrayFile::getRefCount (int64_t offset)
{
    if (version_p == 0) {
	return 1;
    }
    setpos (offset);
    uint32_t refCount;
    get (refCount);
    return refCount;
}

//# Update the reference count.
void StManArrayFile::putRefCount (uint32_t refCount, int64_t offset)
{
    // For version 0 only a dummy put (i.e. value 1) is allowed.
    if (version_p == 0) {
	AlwaysAssert (refCount==1, AipsError);
    }else{
	setpos (offset);
	put (refCount);
    }
}

} //# NAMESPACE CASACORE - END
