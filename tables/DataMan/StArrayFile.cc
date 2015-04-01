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
//#
//# $Id$

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
				uInt version, Bool bigEndian,
				uInt bufferSize, MultiFileBase* mfile)
: leng_p    (16),
  version_p (version),
  hasPut_p  (False)
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
      file_p = new MFFileIO (*mfile, fname, fop);
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
    sizeFloat_p  = ValType::getCanonicalSize (TpFloat,  bigEndian);
    sizeDouble_p = ValType::getCanonicalSize (TpDouble, bigEndian);
    if (bigEndian) {
        sizeInt64_p =
            CanonicalConversion::canonicalSize(static_cast<Int64*>(0));
	sizeuInt64_p =
            CanonicalConversion::canonicalSize(static_cast<uInt64*>(0));
    } else {
        sizeInt64_p =
            LECanonicalConversion::canonicalSize(static_cast<Int64*>(0));
	sizeuInt64_p =
            LECanonicalConversion::canonicalSize(static_cast<uInt64*>(0));
    }
}


//# Close the file.
//# Delete it if required.
StManArrayFile::~StManArrayFile ()
{
    //# Write the version and file length at the beginning.
    flush (False);
    delete iofil_p;
    delete file_p;
}


Bool StManArrayFile::flush (Bool)
{
    if (hasPut_p) {
	setpos (0);
	put (version_p);
	iofil_p->write (1, &leng_p);
	hasPut_p = False;
	file_p->flush();
	setpos (leng_p);
	return True;
    }
    return False;
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
	put (int(0));
        hasPut_p = True;
    }
}

void StManArrayFile::setpos (Int64 pos)
{
    Int64 newpos = iofil_p->seek (pos);
    if (newpos != pos) {
	throw (DataManError ("StManArrayFile::setpos failed"));
    }
}


void StManArrayFile::put (Int64 fileOff, uInt arrayOff, uInt nr,
			  const Float* data)
{
    setpos (fileOff + Int64(arrayOff)*sizeFloat_p);
    iofil_p->write (nr, data);
    hasPut_p = True;
}

uInt StManArrayFile::putShape (const IPosition& shape, Int64& offset,
			       const Float*)
    { return putRes (shape, offset, sizeFloat_p); }

void StManArrayFile::get (Int64 fileOff, uInt arrayOff, uInt nr, Float* data)
{
    setpos (fileOff + Int64(arrayOff)*sizeFloat_p);
    iofil_p->read (nr, data);
}

void StManArrayFile::copyArrayFloat (Int64 to, Int64 from, uInt nr)
    { copyData (to, from, nr*sizeFloat_p); }


//# Put a vector at the given offset.
#define STMANARRAYFILE_PUTGET(T,SIZEDTYPE) \
void StManArrayFile::put (Int64 fileOff, uInt arrayOff, uInt nr, \
			  const T* data) \
{ \
    setpos (fileOff + Int64(arrayOff)*SIZEDTYPE); \
    iofil_p->write (nr, data); \
    hasPut_p = True; \
} \
uInt StManArrayFile::putShape (const IPosition& shape, Int64& offset, \
			       const T*) \
    { return putRes (shape, offset, SIZEDTYPE); } \
void StManArrayFile::get (Int64 fileOff, uInt arrayOff, uInt nr, T* data) \
{ \
    setpos (fileOff + Int64(arrayOff)*SIZEDTYPE); \
    iofil_p->read (nr, data); \
} \
void StManArrayFile::aips_name2(copyArray,T) (Int64 to, Int64 from, uInt nr)\
    { copyData (to, from, nr*SIZEDTYPE); }

STMANARRAYFILE_PUTGET(Char, sizeChar_p)
STMANARRAYFILE_PUTGET(uChar, sizeuChar_p)
STMANARRAYFILE_PUTGET(Short, sizeShort_p)
STMANARRAYFILE_PUTGET(uShort, sizeuShort_p)
STMANARRAYFILE_PUTGET(Int, sizeInt_p)
STMANARRAYFILE_PUTGET(uInt, sizeuInt_p)
STMANARRAYFILE_PUTGET(Int64, sizeInt64_p)
STMANARRAYFILE_PUTGET(uInt64, sizeuInt64_p)
//#//STMANARRAYFILE_PUTGET(Float, sizeFloat_p)
STMANARRAYFILE_PUTGET(Double, sizeDouble_p)
//#//STMANARRAYFILE_PUTGET(long double, sizeLDouble_p)


//# Handle it for Bool.
void StManArrayFile::put (Int64 fileOff, uInt arrayOff, uInt nr,
			  const Bool* data)
{
    //# Bools are stored as bits, thus a bit more complex.
    uInt start  = arrayOff / 8;
    uInt stbit  = arrayOff - 8 * start;
    uInt end    = (arrayOff + nr) / 8;
    uInt endbit = arrayOff + nr - 8 * end;
    if (endbit != 0) {
	end++;
    }
    //# Allocate a buffer of the required length.
    //# Read first and/or last byte when partially accessed.
    uChar* buf = new uChar[end - start];
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
    hasPut_p = True;
    delete [] buf;
}
uInt StManArrayFile::putShape (const IPosition& shape, Int64& offset,
			       const Bool*)
    { return putRes (shape, offset, 0.125); }
void StManArrayFile::get (Int64 fileOff, uInt arrayOff, uInt nr, Bool* data)
{
    //# Bools are stored as bits, thus a bit more complex.
    uInt start  = arrayOff / 8;
    uInt stbit  = arrayOff - 8 * start;
    uInt end    = (arrayOff + nr) / 8;
    uInt endbit = arrayOff + nr - 8 * end;
    if (endbit != 0) {
	end++;
    }
    //# Allocate a buffer of the required length.
    uChar* buf = new uChar[end - start];
    setpos (fileOff + start);
    iofil_p->read (end - start, buf);
    Conversion::bitToBool (data, buf, stbit, nr);
    delete [] buf;
}
void StManArrayFile::copyArrayBool (Int64 to, Int64 from, uInt nr)
    { copyData (to, from, (nr+7)/8); }


//# Handle it for Complex and String.
//# A Complex consists of 2 float values.
//# For a string its file offset gets stored (as a uInt), while the
//# string itself will be put at the end of the file.
uInt StManArrayFile::putShape (const IPosition& shape, Int64& offset,
			       const Complex*)
    { return putRes (shape, offset, 2*sizeFloat_p); }
uInt StManArrayFile::putShape (const IPosition& shape, Int64& offset,
			       const DComplex*)
    { return putRes (shape, offset, 2*sizeDouble_p); }
uInt StManArrayFile::putShape (const IPosition& shape, Int64& offset,
			       const String*)
{
    uInt n = putRes (shape, offset, sizeuInt_p);
    uInt nr = shape.product();
    Block<uInt> data(nr, 0u);
    put (offset+n, 0, nr, data.storage());
    return n;
}

//# Put a complex vector at the given file offset.
void StManArrayFile::put (Int64 fileOff, uInt arrayOff, uInt nr,
			  const Complex* data)
{
    setpos (fileOff + Int64(arrayOff)*2*sizeFloat_p);
    iofil_p->write (2*nr, (const Float*)data);
    hasPut_p = True;
}
void StManArrayFile::put (Int64 fileOff, uInt arrayOff, uInt nr,
			  const DComplex* data)
{
    setpos (fileOff + Int64(arrayOff)*2*sizeDouble_p);
    iofil_p->write (2*nr, (const Double*)data);
    hasPut_p = True;
}

//# Put a string at the given file offset.
void StManArrayFile::put (Int64 fileOff, uInt arrayOff, uInt nr,
			  const String* data)
{
    //# Get file offset for string offset array.
    //# Allocate a buffer to hold 4096 string offsets.
    Int64 offs = fileOff + Int64(arrayOff)*sizeuInt_p;
    uInt buf[4096];
    uInt i, n;
    while (nr > 0) {
	n = (nr < 4096  ?  nr : 4096);
	setpos (leng_p);                            // position at end of file
	for (i=0; i<n; i++) {
            // Note: this should be fixed one time.
            AlwaysAssert (leng_p < Int64(65536)*65536, DataManError);
	    buf[i] = leng_p;
                //
                // Converted the string->length into an uInt so
                // the SGI compiler could find the right overloaded function
                // some problem with string::size_t conversion
                //
	    leng_p += put (uInt(data->length()));         // write string length
	    leng_p += iofil_p->write (data->length(), data->chars());
	    data++;
	}
	//# Write the offsets.
	setpos (offs);
	offs += iofil_p->write (n, buf);
	hasPut_p = True;
	nr   -= n;
    }
}


//# Get a complex vector at the given file offset.
void StManArrayFile::get (Int64 fileOff, uInt arrayOff, uInt nr,
			  Complex* data)
{
    setpos (fileOff + Int64(arrayOff)*2*sizeFloat_p);
    iofil_p->read (2*nr, (Float*)data);
}
void StManArrayFile::get (Int64 fileOff, uInt arrayOff, uInt nr,
			  DComplex* data)
{
    setpos (fileOff + Int64(arrayOff)*2*sizeDouble_p);
    iofil_p->read (2*nr, (Double*)data);
}

//# Get a string at the given file offset.
void StManArrayFile::get (Int64 fileOff, uInt arrayOff, uInt nr,
			  String* data)
{
    //# Get file offset for string offset array.
    //# Allocate a buffer to hold 4096 string offsets.
    Int64 offs = fileOff + Int64(arrayOff)*sizeuInt_p;
    uInt buf[4096];
    uInt i, n, l;
    while (nr > 0) {
	n = (nr < 4096  ?  nr : 4096);
	setpos (offs);
	offs += iofil_p->read (n, buf);
	for (i=0; i<n; i++) {
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


void StManArrayFile::copyArrayComplex (Int64 to, Int64 from, uInt nr)
    { copyData (to, from, nr*2*sizeFloat_p); }
void StManArrayFile::copyArrayDComplex (Int64 to, Int64 from, uInt nr)
    { copyData (to, from, nr*2*sizeDouble_p); }
void StManArrayFile::copyArrayString (Int64 to, Int64 from, uInt nr)
{
    String data[4096];
    uInt ndone = 0;
    for (uInt n=0; nr>0; nr-=n) {
	n = (nr < 4096  ?  nr : 4096);
	get (from, ndone, n, data);
	put (to, ndone, n, data);
	ndone += n;
    }
}

//# Copy the data of the given length from one file offset to another.
void StManArrayFile::copyData (Int64 to, Int64 from, uInt length)
{
    uChar buffer[32768];
    for (uInt n=0; length>0; length-=n) {
	n = (length < 32768  ?  length : 32768);
	setpos (from);
	from += iofil_p->read (n, buffer);
	setpos (to);
	to += iofil_p->write  (n, buffer);
	hasPut_p = True;
    }
}

//# Write shape and reserve file space for the array by
//# increasing the length.
//# Take care it is at 8 byte boundary.
//# Write something in the last byte to make sure the file is extended.
uInt StManArrayFile::putRes (const IPosition& shape, Int64& offset,
			     float lenElem)
{
    leng_p = 8 * ((leng_p+7) / 8);
    offset = leng_p;
    uInt n = 0;
    setpos (leng_p);
    // Put reference count in higher versions only.
    if (version_p > 0) {
	n += put (uInt(1));
    }
    n += put (shape.nelements());
    for (uInt i=0; i<shape.nelements(); i++) {
      n += put (Int(shape(i)));
    }
    // Add length of shape and of entire array to file length.
    // Take care of rounding (needed for Bool case).
    leng_p += n;
    leng_p += Int64 (double(shape.product()) * lenElem + 0.95);
    setpos (leng_p - 1);
    Char c = 0;
    iofil_p->write (1, &c);
    hasPut_p = True;
    return n;
}

//# Get the shape at the given file offset
//# and returns its length in the file.
uInt StManArrayFile::getShape (Int64 fileOff, IPosition& shape)
{
    setpos (fileOff);
    uInt n=0;
    if (version_p > 0) {
	uInt refCount;
	n = get (refCount);
    }
    uInt nr;
    n += get (nr);
    shape.resize (nr);
    Int tmp;
    for (uInt i=0; i<nr; i++) {
	n += get (tmp);
        shape(i) = tmp;
    }
    return n;
}

//# Update the reference count.
uInt StManArrayFile::getRefCount (Int64 offset)
{
    if (version_p == 0) {
	return 1;
    }
    setpos (offset);
    uInt refCount;
    get (refCount);
    return refCount;
}

//# Update the reference count.
void StManArrayFile::putRefCount (uInt refCount, Int64 offset)
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
