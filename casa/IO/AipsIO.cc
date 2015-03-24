//# AipsIO.cc: AipsIO is the object persistency mechanism of Casacore
//# Copyright (C) 1993,1994,1995,1996,1997,1998,2001
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

#include <casacore/casa/IO/AipsIO.h>
#include <casacore/casa/IO/TypeIO.h>
#include <casacore/casa/IO/CanonicalIO.h>
#include <casacore/casa/IO/ByteIO.h>
#include <casacore/casa/IO/RegularFileIO.h>
#include <casacore/casa/IO/MFFileIO.h>
#include <casacore/casa/BasicSL/Complex.h>
#include <casacore/casa/Utilities/Assert.h>
#include <cstring>                  //# for strcmp with gcc-4.3

namespace casacore { //# NAMESPACE CASACORE - BEGIN

// This is the implementation of the AipsIO class.
// Operator << and >> for the built-in data types are inline functions
// and defined in AipsIO.h.

// Define the magic value used to check if the get of objects
// is in synchronization with the file objetcs.
const uInt AipsIO::magicval_p = 0xbebebebe;

AipsIO::AipsIO()
: opened_p (0),
  swput_p  (-1),
  swget_p  (-1),
  maxlev_p (10),
  objlen_p (10),
  objtln_p (10),
  objptr_p (10),
  hasCachedType_p(False)
{}

AipsIO::AipsIO (const String& fileName, ByteIO::OpenOption fop,
		uInt filebufSize, MultiFileBase* mfile)
: opened_p (0),
  maxlev_p (10),
  objlen_p (10),
  objtln_p (10),
  objptr_p (10)
{
    // Open the file.
  open (fileName, fop, filebufSize, mfile);
}

AipsIO::AipsIO (ByteIO* file)
: opened_p   (0),
  maxlev_p   (10),
  objlen_p   (10),
  objtln_p   (10),
  objptr_p   (10)
{
    open (file);
}

AipsIO::AipsIO (TypeIO* file)
: opened_p   (0),
  maxlev_p   (10),
  objlen_p   (10),
  objtln_p   (10),
  objptr_p   (10)
{
    open (file);
}

AipsIO::~AipsIO()
{
    try {
       close();
    } catch (...) {}
}



void AipsIO::open (const String& fileName, ByteIO::OpenOption fop,
		   uInt filebufSize, MultiFileBase* mfile)
{
    // Initialize everything for the open.
    openInit (fop);
    if (mfile) {
      file_p = new MFFileIO (*mfile, fileName, fopt_p);
    } else {
      file_p = new RegularFileIO (fileName, fopt_p, filebufSize);
    }
    io_p = new CanonicalIO (file_p);
    seekable_p = True;
    opened_p   = 1;
}

void AipsIO::open (ByteIO* file)
{
    // Initialize everything for the open.
    openInit (ByteIO::New);
    file_p = 0;
    io_p   = new CanonicalIO (file);
    AlwaysAssert (io_p != 0, AipsError);
    seekable_p = io_p->isSeekable();
    if (! io_p->isReadable()) {
	swget_p = -1;
    }
    if (! io_p->isWritable()) {
	swput_p = -1;
    }
    opened_p = 1;
}

void AipsIO::open (TypeIO* file)
{
    // Initialize everything for the open.
    openInit (ByteIO::New);
    file_p = 0;
    io_p   = file;
    AlwaysAssert (io_p != 0, AipsError);
    seekable_p = io_p->isSeekable();
    if (! io_p->isReadable()) {
	swget_p = -1;
    }
    if (! io_p->isWritable()) {
	swput_p = -1;
    }
    opened_p = -1;
}

// Only open if not already open.
void AipsIO::openInit (ByteIO::OpenOption fop)
{
    if (opened_p != 0) {
	throw (AipsError ("AipsIO: already open"));
    }
    hasCachedType_p = False;
    fopt_p  = fop;
    swget_p = 0;
    swput_p = 0;
    level_p = 0;
    objtln_p[0] = 0xffffffff;                  // highest possible uInt value
    // Determine if put is possible.
    if (fopt_p == ByteIO::Old) {
	swput_p = -1;                          // put not possible
    }
}

// Close the file and delete filebuf buffer space.
// Delete the file if required.
void AipsIO::close()
{
    if (opened_p == 1) {
	delete io_p;
	delete file_p;
    }
    io_p     = 0;
    file_p   = 0;
    opened_p = 0;
    swput_p  = -1;
    swget_p  = -1;
    hasCachedType_p = False;
}


// getpos allows you to get the position of an object in a file by
// calling it before calling putstart.
// setpos allows you to position on an object.
// Note that these functions only return a valid result if a disk file
// is used (it will not work for IPC).

Int64 AipsIO::getpos()
{
    return io_p->seek (0, ByteIO::Current);
}

Int64 AipsIO::setpos (Int64 pos)
{
    if (level_p != 0) {
        throw (AipsError("AipsIO::setpos cannot be done while "
			 "accessing objects"));
    }
    return io_p->seek (pos);
}


// The primitive put functions (operator <<) put one value at a time.
// The value is converted to canonical format.
// They test if a put is allowed; an exception is thrown if not.
// The vector FromLocal functions are used to avoid align problems,
// because the destination (the output buffer) can be non-aligned.

AipsIO& AipsIO::operator<< (const Bool& var)
{
    testput();
    objlen_p[level_p] += io_p->write (1, &var);
    return (*this);
}
    
AipsIO& AipsIO::operator<< (const Char& var)
{
    testput();
    objlen_p[level_p] += io_p->write (1, &var);
    return (*this);
}
    
AipsIO& AipsIO::operator<< (const uChar& var)
{
    testput();
    objlen_p[level_p] += io_p->write (1, &var);
    return (*this);
}

AipsIO& AipsIO::operator<< (const short& var)
{
    testput();
    objlen_p[level_p] += io_p->write (1, &var);
    return (*this);
}
    
AipsIO& AipsIO::operator<< (const unsigned short& var)
{
    testput();
    objlen_p[level_p] += io_p->write (1, &var);
    return (*this);
}
    
AipsIO& AipsIO::operator<< (const int& var)
{
    testput();
    objlen_p[level_p] += io_p->write (1, &var);
    return (*this);
}
    
AipsIO& AipsIO::operator<< (const unsigned int& var)
{
    testput();
    objlen_p[level_p] += io_p->write (1, &var);
    return (*this);
}

AipsIO& AipsIO::operator<< (const Int64& var)
{
    testput();
    objlen_p[level_p] += io_p->write (1, &var);
    return (*this);
}

AipsIO& AipsIO::operator<< (const uInt64& var)
{
    testput();
    objlen_p[level_p] += io_p->write (1, &var);
    return (*this);
}
    
AipsIO& AipsIO::operator<< (const float& var)
{
    testput();
    objlen_p[level_p] += io_p->write (1, &var);
    return (*this);
}
    
AipsIO& AipsIO::operator<< (const double& var)
{
    testput();
    objlen_p[level_p] += io_p->write (1, &var);
    return (*this);
}

AipsIO& AipsIO::operator<< (const Complex& var)
{
    testput();
    objlen_p[level_p] += io_p->write (1, &var);
    return (*this);
}

AipsIO& AipsIO::operator<< (const DComplex& var)
{
    testput();
    objlen_p[level_p] += io_p->write (1, &var);
    return (*this);
}

AipsIO& AipsIO::operator<< (const String& var)
{
    testput();
    objlen_p[level_p] += io_p->write (1, &var);
    return (*this);
}

AipsIO& AipsIO::operator<< (const Char* var)
{
    testput();
    String str(var);
    objlen_p[level_p] += io_p->write (1, &str);
    return (*this);
}


// The following routines put an entire vector in one go.
// They test if a put is allowed.
// The data is stored in canonical format in an intermediate buffer

AipsIO& AipsIO::put (uInt nrv, const Bool* var, Bool putNR)
{
    testput();
    if (putNR) {
	operator<< (nrv);                        // store #values
    }
    objlen_p[level_p] += io_p->write (nrv, var);
    return (*this);
}

AipsIO& AipsIO::put (uInt nrv, const Char* var, Bool putNR)
{
    testput();
    if (putNR) {
	operator<< (nrv);                        // store #values
    }
    objlen_p[level_p] += io_p->write (nrv, var);
    return (*this);
}

AipsIO& AipsIO::put (uInt nrv, const uChar* var, Bool putNR)
{
    testput();
    if (putNR) {
	operator<< (nrv);                        // store #values
    }
    objlen_p[level_p] += io_p->write (nrv, var);
    return (*this);
}

AipsIO& AipsIO::put (uInt nrv, const short* var, Bool putNR)
{
    testput();
    if (putNR) {
	operator<< (nrv);                        // store #values
    }
    objlen_p[level_p] += io_p->write (nrv, var);
    return (*this);
}

AipsIO& AipsIO::put (uInt nrv, const unsigned short* var, Bool putNR)
{
    testput();
    if (putNR) {
	operator<< (nrv);                        // store #values
    }
    objlen_p[level_p] += io_p->write (nrv, var);
    return (*this);
}

AipsIO& AipsIO::put (uInt nrv, const int* var, Bool putNR)
{
    testput();
    if (putNR) {
	operator<< (nrv);                        // store #values
    }
    objlen_p[level_p] += io_p->write (nrv, var);
    return (*this);
}

AipsIO& AipsIO::put (uInt nrv, const unsigned int* var, Bool putNR)
{
    testput();
    if (putNR) {
	operator<< (nrv);                        // store #values
    }
    objlen_p[level_p] += io_p->write (nrv, var);
    return (*this);
}

AipsIO& AipsIO::put (uInt nrv, const Int64* var, Bool putNR)
{
    testput();
    if (putNR) {
	operator<< (nrv);                        // store #values
    }
    objlen_p[level_p] += io_p->write (nrv, var);
    return (*this);
}

AipsIO& AipsIO::put (uInt nrv, const uInt64* var, Bool putNR)
{
    testput();
    if (putNR) {
	operator<< (nrv);                        // store #values
    }
    objlen_p[level_p] += io_p->write (nrv, var);
    return (*this);
}

AipsIO& AipsIO::put (uInt nrv, const float* var, Bool putNR)
{
    testput();
    if (putNR) {
	operator<< (nrv);                        // store #values
    }
    objlen_p[level_p] += io_p->write (nrv, var);
    return (*this);
}

AipsIO& AipsIO::put (uInt nrv, const double* var, Bool putNR)
{
    testput();
    if (putNR) {
	operator<< (nrv);                        // store #values
    }
    objlen_p[level_p] += io_p->write (nrv, var);
    return (*this);
}

AipsIO& AipsIO::put (uInt nrv, const Complex* var, Bool putNR)
{
    testput();
    if (putNR) {
	operator<< (nrv);                        // store #values
    }
    objlen_p[level_p] += io_p->write (nrv, var);
    return (*this);
}

AipsIO& AipsIO::put (uInt nrv, const DComplex* var, Bool putNR)
{
    testput();
    if (putNR) {
	operator<< (nrv);                        // store #values
    }
    objlen_p[level_p] += io_p->write (nrv, var);
    return (*this);
}

AipsIO& AipsIO::put (uInt nrv, const String* var, Bool putNR)
{
    testput();
    if (putNR) {
	operator<< (nrv);                        // store #values
    }
    for (uInt i=0; i<nrv; i++) {
	operator<< (var[i]);
    }
    return (*this);
}


AipsIO& AipsIO::put (const vector<Bool>& vec)
{
    // std::vector<bool> uses bits instead of bytes. So copy first.
    Block<Bool> var(vec.size());
    std::copy (vec.begin(), vec.end(), var.begin());
    put (var.size(), var.storage(), True);
    return *this;
}


// putstart starts writing an object.
// This is not possible if there is no file, if the file is not opened
// for output or if there is a get in operation.
// If it is the root object, it initializes the dynamic buffers.
// It puts the object type and version and reserves space for the length.
// It increases the level for each object to hold the length.

uInt AipsIO::putstart (const Char* type, uInt vers)
{
    return (putstart (String(type), vers));
}

uInt AipsIO::putstart (const String& type, uInt vers)
{
    if (opened_p == 0  ||  swput_p < 0  ||  swget_p > 0) {
	throw (AipsError ("AipsIO::putstart: not open or not writable"));
    }
    if (level_p == 0) {
	swput_p = 1;                           // indicate putting is possible
	objlen_p[0] = 0;
	operator<< (magicval_p);               // write magic value
    }
    level_p++;
    if (level_p >= maxlev_p) {
	maxlev_p += 10;                        // increase size of blocks
	objlen_p.resize (maxlev_p);
	objtln_p.resize (maxlev_p);
	objptr_p.resize (maxlev_p);
    }
    objlen_p[level_p] = 0;                     // initialize object length
    objptr_p[level_p] = getpos();              // remember where to put
    operator<< (magicval_p);
    operator<< (type);                         // write object type
    operator<< (vers);                         // write object version
    return level_p;
}


// putend ends putting an object. It decreases the level and writes
// the object length if the file is seekable.
uInt AipsIO::putend()
{
    if (level_p == 0) {
	testputerr();                        // no corresponding putstart
    }
    uInt len = objlen_p[level_p];            // object length
    if (seekable_p) {
	Int64 pos = getpos();
	io_p->seek (objptr_p[level_p]);
	operator<< (objlen_p[level_p]);
	io_p->seek (pos);
    }
    level_p--;
    if (level_p == 0) {
	swput_p = 0;                     // putting is not possible anymore
    }else{
	objlen_p[level_p] += len;        // add length to parent object
    }
    return len;
}


// The primitive get functions (operator >>) get one value at a time.
// They test if a get is allowed; an exception is thrown if not.
// They convert from canonical to local format.
// Note that the variables in the >> functions are always aligned, so
// we can use the scalar ToLocal function which is doing a simple assign.

AipsIO& AipsIO::operator>> (Bool& var)
{
    testget();
    objlen_p[level_p] += io_p->read (1, &var);
    testgetLength();
    return (*this);
}
    
AipsIO& AipsIO::operator>> (Char& var)
{
    testget();
    objlen_p[level_p] += io_p->read (1, &var);
    testgetLength();
    return (*this);
}
    
AipsIO& AipsIO::operator>> (uChar& var)
{
    testget();
    objlen_p[level_p] += io_p->read (1, &var);
    testgetLength();
    return (*this);
}
    
AipsIO& AipsIO::operator>> (short& var)
{
    testget();
    objlen_p[level_p] += io_p->read (1, &var);
    testgetLength();
    return (*this);
}
    
AipsIO& AipsIO::operator>> (unsigned short& var)
{
    testget();
    objlen_p[level_p] += io_p->read (1, &var);
    testgetLength();
    return (*this);
}
    
AipsIO& AipsIO::operator>> (int& var)
{
    testget();
    objlen_p[level_p] += io_p->read (1, &var);
    testgetLength();
    return (*this);
}
    
AipsIO& AipsIO::operator>> (unsigned int& var)
{
    testget();
    objlen_p[level_p] += io_p->read (1, &var);
    testgetLength();
    return (*this);
}
    
AipsIO& AipsIO::operator>> (Int64& var)
{
    testget();
    objlen_p[level_p] += io_p->read (1, &var);
    testgetLength();
    return (*this);
}
    
AipsIO& AipsIO::operator>> (uInt64& var)
{
    testget();
    objlen_p[level_p] += io_p->read (1, &var);
    testgetLength();
    return (*this);
}

AipsIO& AipsIO::operator>> (float& var)
{
    testget();
    objlen_p[level_p] += io_p->read (1, &var);
    testgetLength();
    return (*this);
}

AipsIO& AipsIO::operator>> (double& var)
{
    testget();
    objlen_p[level_p] += io_p->read (1, &var);
    testgetLength();
    return (*this);
}

AipsIO& AipsIO::operator>> (Complex& var)
{
    testget();
    objlen_p[level_p] += io_p->read (1, &var);
    testgetLength();
    return (*this);
}

AipsIO& AipsIO::operator>> (DComplex& var)
{
    testget();
    objlen_p[level_p] += io_p->read (1, &var);
    testgetLength();
    return (*this);
}

AipsIO& AipsIO::operator>> (String& var)
{
    testget();
    objlen_p[level_p] += io_p->read (1, &var);
    testgetLength();
    return (*this);
}


// The following routines get an entire vector in one go.
// They convert from canonical to local format.
// If the format sizes differ, some special has to be done.
// The compiler will remove redundant code in that test.
// They also test if a get is allowed.
// The user has to supply the buffer and the given nr of values is read.

AipsIO& AipsIO::get (uInt nrv, Bool* var)
{
    testget();
    objlen_p[level_p] += io_p->read (nrv, var);
    testgetLength();
    return (*this);
}

AipsIO& AipsIO::get (uInt nrv, Char* var)
{
    testget();
    objlen_p[level_p] += io_p->read (nrv, var);
    testgetLength();
    return (*this);
}

AipsIO& AipsIO::get (uInt nrv, uChar* var)
{
    testget();
    objlen_p[level_p] += io_p->read (nrv, var);
    testgetLength();
    return (*this);
}

AipsIO& AipsIO::get (uInt nrv, short* var)
{
    testget();
    objlen_p[level_p] += io_p->read (nrv, var);
    testgetLength();
    return (*this);
}

AipsIO& AipsIO::get (uInt nrv, unsigned short* var)
{
    testget();
    objlen_p[level_p] += io_p->read (nrv, var);
    testgetLength();
    return (*this);
}

AipsIO& AipsIO::get (uInt nrv, int* var)
{
    testget();
    objlen_p[level_p] += io_p->read (nrv, var);
    testgetLength();
    return (*this);
}

AipsIO& AipsIO::get (uInt nrv, unsigned int* var)
{
    testget();
    objlen_p[level_p] += io_p->read (nrv, var);
    testgetLength();
    return (*this);
}

AipsIO& AipsIO::get (uInt nrv, Int64* var)
{
    testget();
    objlen_p[level_p] += io_p->read (nrv, var);
    testgetLength();
    return (*this);
}

AipsIO& AipsIO::get (uInt nrv, uInt64* var)
{
    testget();
    objlen_p[level_p] += io_p->read (nrv, var);
    testgetLength();
    return (*this);
}

AipsIO& AipsIO::get (uInt nrv, float* var)
{
    testget();
    objlen_p[level_p] += io_p->read (nrv, var);
    testgetLength();
    return (*this);
}

AipsIO& AipsIO::get (uInt nrv, double* var)
{
    testget();
    objlen_p[level_p] += io_p->read (nrv, var);
    testgetLength();
    return (*this);
}

AipsIO& AipsIO::get (uInt nrv, Complex* var)
{
    testget();
    objlen_p[level_p] += io_p->read (nrv, var);
    testgetLength();
    return (*this);
}

AipsIO& AipsIO::get (uInt nrv, DComplex* var)
{
    testget();
    objlen_p[level_p] += io_p->read (nrv, var);
    testgetLength();
    return (*this);
}

AipsIO& AipsIO::get (uInt nrv, String* var)
{
    testget();
    objlen_p[level_p] += io_p->read (nrv, var);
    testgetLength();
    return (*this);
}

AipsIO& AipsIO::get (vector<Bool>& vec)
{
    uInt nrv;
    Bool* var;
    getnew (nrv, var);
    vec.resize (nrv);
    std::copy (var, var+nrv, vec.begin());
    delete [] var;
    return *this;
}


// The following routines get an entire vector in one go.
// The routine will allocate a buffer of the appropriate size.
// It returns a pointer to that buffer and the nr of values read.

AipsIO& AipsIO::getnew (uInt& nrv, Bool*& var)
{
    operator>> (nrv);
    var = new Bool[nrv];
    get (nrv, var);
    return (*this);
}

AipsIO& AipsIO::getnew (uInt& nrv, Char*& var)
{
    operator>> (nrv);
    var = new Char[nrv];
    get (nrv, var);
    return (*this);
}

AipsIO& AipsIO::getnew (uInt& nrv, uChar*& var)
{
    operator>> (nrv);
    var = new uChar[nrv];
    get (nrv, var);
    return (*this);
}

AipsIO& AipsIO::getnew (uInt& nrv, short*& var)
{
    operator>> (nrv);
    var = new short[nrv];
    get (nrv, var);
    return (*this);
}

AipsIO& AipsIO::getnew (uInt& nrv, unsigned short*& var)
{
    operator>> (nrv);
    var = new unsigned short[nrv];
    get (nrv, var);
    return (*this);
}

AipsIO& AipsIO::getnew (uInt& nrv, int*& var)
{
    operator>> (nrv);
    var = new int[nrv];
    get (nrv, var);
    return (*this);
}

AipsIO& AipsIO::getnew (uInt& nrv, unsigned int*& var)
{
    operator>> (nrv);
    var = new unsigned int[nrv];
    get (nrv, var);
    return (*this);
}

AipsIO& AipsIO::getnew (uInt& nrv, Int64*& var)
{
    operator>> (nrv);
    var = new Int64[nrv];
    get (nrv, var);
    return (*this);
}

AipsIO& AipsIO::getnew (uInt& nrv, uInt64*& var)
{
    operator>> (nrv);
    var = new uInt64[nrv];
    get (nrv, var);
    return (*this);
}

AipsIO& AipsIO::getnew (uInt& nrv, float*& var)
{
    operator>> (nrv);
    var = new float[nrv];
    get (nrv, var);
    return (*this);
}

AipsIO& AipsIO::getnew (uInt& nrv, double*& var)
{
    operator>> (nrv);
    var = new double[nrv];
    get (nrv, var);
    return (*this);
}

AipsIO& AipsIO::getnew (uInt& nrv, Complex*& var)
{
    operator>> (nrv);
    var = new Complex[nrv];
    get (nrv, var);
    return (*this);
}

AipsIO& AipsIO::getnew (uInt& nrv, DComplex*& var)
{
    operator>> (nrv);
    var = new DComplex[nrv];
    get (nrv, var);
    return (*this);
}

AipsIO& AipsIO::getnew (uInt& nrv, String*& var)
{
    operator>> (nrv);
    var = new String[nrv];
    get (nrv, var);
    return (*this);
}


// getNextType gets the object type of the next piece of
// information to read. It can only be used if a file has been
// opened and if no put is in operation.
// It checks if it finds the correct magic value preceeding
// the object type.
const String& AipsIO::getNextType()
{
    if (opened_p == 0  ||  swget_p < 0  ||  swput_p > 0) {
        String message;
        if (file_p) message = file_p->fileName() + " - ";
	throw (AipsError ("AipsIO::getNextType: " + message + 
                          "not opened or not readable"));
    }
    if (hasCachedType_p) {
	return objectType_p;
    }
    uInt swgetOld = swget_p;
    uInt mval;
    if (level_p == 0) {
        swget_p = 1;                       // getting is possible (temporarily)
	objlen_p[0] = 0;                   // length already read
	operator>> (mval);
	if (mval != magicval_p) {
            String message;
            if (file_p) message = file_p->fileName() + " - ";
	    throw (AipsError ("AipsIO::getNextType: " + message + 
                              "no magic value found"));
	}
    }
    level_p++;
    if (level_p >= maxlev_p) {
	maxlev_p += 10;
	objlen_p.resize (maxlev_p);
	objtln_p.resize (maxlev_p);
	objptr_p.resize (maxlev_p);
    }
    objlen_p[level_p] = 0;                 // length already read
    objtln_p[level_p] = 16;                // to satisfy test in read
    operator>> (objtln_p[level_p]);        // total object length to read
    operator>> (objectType_p);             // object type
    // Getting may not be possible till getstart has been done.
    swget_p = swgetOld;
    hasCachedType_p = True;
    return objectType_p;
}

// getstart starts reading an object.
// It checks the object type and returns the version.
// getend ends reading an object. It decreases the level and removes
// possible DynBuffer buffers (although they should not be present).
// It checks if the entire object has been read.

uInt AipsIO::getstart (const String& type)
{
    return (getstart (type.chars()));
}

uInt AipsIO::getstart (const Char* type)
{
    uInt vers;
    if (strcmp (type, getNextType().chars()) != 0) {
	throw (AipsError ("AipsIO::getstart: found object type " +
			  getNextType() + ", expected " + type));
    }
    swget_p = 1;                           // getting is possible now
    hasCachedType_p = False;               // type is not cached anymore
    operator>> (vers);                     // read object version
    return vers;
}

uInt AipsIO::getend()
{
    if (level_p > 0) {
	uInt len = objlen_p[level_p];      // length of object read
	if (len != objtln_p[level_p]  &&  objtln_p[level_p] != magicval_p) {
	    throw (AipsError ("AipsIO::getend: part of object not read"));
	}
        if (--level_p == 0) {
            swget_p = 0;                   // reading not possible anymore
	}else{
	    objlen_p[level_p] += len;      // increase length read of parent
	}
	return len;
    }else{
        testgeterr();                      // no corresponding getstart
        return 0;
    }
}


// Throw errors on behalf of testput and testget.
// testget and testput are inline and it would be a bit expensive to
// get these (expanded) statements in all instances.
void AipsIO::testputerr()
    { throw (AipsError ("AipsIO: no putstart done")); }
void AipsIO::testgeterr()
    { throw (AipsError ("AipsIO: no getstart done")); }
void AipsIO::testgeterrLength()
    { throw (AipsError ("AipsIO: read beyond end of object")); }

} //# NAMESPACE CASACORE - END

