// # tByteIO.cc: Test program for class ByteIO and derived classes
// # Copyright (C) 1996,1997,2000,2001,2002
// # Associated Universities, Inc. Washington DC, USA.
// #
// # This library is free software; you can redistribute it and/or modify it
// # under the terms of the GNU Library General Public License as published by
// # the Free Software Foundation; either version 2 of the License, or (at your
// # option) any later version.
// #
// # This library is distributed in the hope that it will be useful, but WITHOUT
// # ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
// # FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Library General Public
// # License for more details.
// #
// # You should have received a copy of the GNU Library General Public License
// # along with this library; if not, write to the Free Software Foundation,
// # Inc., 675 Massachusetts Ave, Cambridge, MA 02139, USA.
// #
// # Correspondence concerning AIPS++ should be addressed as follows:
// #        Internet email: casa-feedback@nrao.edu.
// #        Postal address: AIPS++ Project Office
// #                        National Radio Astronomy Observatory
// #                        520 Edgemont Road
// #                        Charlottesville, VA 22903-2475 USA

#include <casacore/casa/aips.h>
#include <casacore/casa/IO/FiledesIO.h>
#include <casacore/casa/IO/RegularFileIO.h>
#include <casacore/casa/IO/MemoryIO.h>
#include <casacore/casa/OS/RegularFile.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/iostream.h>
#include <unistd.h>
#include <fcntl.h>

#include <casacore/casa/namespace.h>
void checkLength(ByteIO& fio, unsigned int& curLength, unsigned int addLength) {
  curLength += addLength;
  AlwaysAssertExit(fio.length() == curLength);
}

static bool valb = true;
static short vals = -3;
static unsigned short valus = 2;
static int vali = 1000;
static unsigned int valui = 32768;
static int64_t vall = -14793;
static uint64_t valul = 17;
static float valf = 1.2;
static double vald = -3.14;

void checkValues(ByteIO& fio, unsigned short incr) {
  fio.seek(0);
  unsigned int curLength = fio.length();
  bool resb;
  AlwaysAssertExit(fio.read(sizeof(bool), &resb) == sizeof(bool));
  AlwaysAssertExit(resb == valb);
  short ress;
  AlwaysAssertExit(fio.read(sizeof(short), &ress) == sizeof(short));
  AlwaysAssertExit(ress == vals - incr);
  unsigned short resus;
  AlwaysAssertExit(fio.read(sizeof(unsigned short), &resus) == sizeof(unsigned short));
  AlwaysAssertExit(resus == valus + incr);
  int resi;
  AlwaysAssertExit(fio.read(sizeof(int), &resi) == sizeof(int));
  AlwaysAssertExit(resi == vali);
  unsigned int resui;
  AlwaysAssertExit(fio.read(sizeof(unsigned int), &resui) == sizeof(unsigned int));
  AlwaysAssertExit(resui == valui);
  int64_t resl;
  AlwaysAssertExit(fio.read(sizeof(int64_t), &resl) == sizeof(int64_t));
  AlwaysAssertExit(resl == vall);
  uint64_t resul;
  AlwaysAssertExit(fio.read(sizeof(uint64_t), &resul) == sizeof(uint64_t));
  AlwaysAssertExit(resul == valul);
  float resf;
  AlwaysAssertExit(fio.read(sizeof(float), &resf) == sizeof(float));
  AlwaysAssertExit(resf == valf);
  double resd;
  AlwaysAssertExit(fio.read(sizeof(double), &resd) == sizeof(double));
  AlwaysAssertExit(resd == vald);
  AlwaysAssertExit(fio.length() == curLength);
}

void doIt(ByteIO& fio) {
  unsigned int length = 0;
  AlwaysAssertExit(fio.length() == 0);
  fio.write(sizeof(bool), &valb);
  checkLength(fio, length, sizeof(bool));
  fio.write(sizeof(short), &vals);
  checkLength(fio, length, sizeof(short));
  fio.write(sizeof(unsigned short), &valus);
  checkLength(fio, length, sizeof(unsigned short));
  fio.write(sizeof(int), &vali);
  checkLength(fio, length, sizeof(int));
  fio.write(sizeof(unsigned int), &valui);
  checkLength(fio, length, sizeof(unsigned int));
  fio.write(sizeof(int64_t), &vall);
  checkLength(fio, length, sizeof(int64_t));
  fio.write(sizeof(uint64_t), &valul);
  checkLength(fio, length, sizeof(uint64_t));
  fio.write(sizeof(float), &valf);
  checkLength(fio, length, sizeof(float));
  fio.write(sizeof(double), &vald);
  checkLength(fio, length, sizeof(double));

  checkValues(fio, 0);

  fio.seek(int(sizeof(bool)));
  AlwaysAssertExit(fio.length() == length);
  unsigned short incr = 100;
  short vals1 = vals - incr;
  short ress;
  fio.write(sizeof(short), &vals1);
  fio.seek(int(sizeof(bool)));
  AlwaysAssertExit(fio.read(sizeof(short), &ress) == sizeof(short));
  AlwaysAssertExit(ress == vals1);
  unsigned short valus1 = valus + incr;
  unsigned short resus;
  fio.write(sizeof(unsigned short), &valus1);
  fio.seek(int(-sizeof(unsigned short)), ByteIO::Current);
  AlwaysAssertExit(fio.read(sizeof(unsigned short), &resus) == sizeof(unsigned short));
  AlwaysAssertExit(resus == valus1);
  int resi;
  AlwaysAssertExit(fio.read(sizeof(int), &resi) == sizeof(int));
  AlwaysAssertExit(resi == vali);
  AlwaysAssertExit(fio.length() == length);

  checkValues(fio, incr);

  AlwaysAssertExit(fio.length() == length);
  int64_t offset = sizeof(bool);
  incr = 100;
  vals1 = vals - incr;
  fio.pwrite(sizeof(short), offset, &vals1);
  AlwaysAssertExit(fio.pread(sizeof(short), offset, &ress) == sizeof(short));
  AlwaysAssertExit(ress == vals1);
  fio.seek(offset);
  AlwaysAssertExit(fio.read(sizeof(short), &ress) == sizeof(short));
  AlwaysAssertExit(ress == vals1);
  offset += sizeof(short);
  valus1 = valus + incr;
  fio.pwrite(sizeof(unsigned short), offset, &valus1);
  AlwaysAssertExit(fio.pread(sizeof(unsigned short), offset, &resus) == sizeof(unsigned short));
  AlwaysAssertExit(resus == valus1);
  AlwaysAssertExit(fio.read(sizeof(unsigned short), &ress) == sizeof(unsigned short));
  AlwaysAssertExit(resus == valus1);

  checkValues(fio, incr);
}

void checkReopen() {
  RegularFile rfile("tByteIO_tmp.data");
  {
    RegularFileIO fio(rfile);
    checkValues(fio, 100);
    fio.reopenRW();
    fio.seek(int64_t(sizeof(bool)));
    short vals;
    fio.read(sizeof(short), &vals);
    vals -= 50;
    fio.seek(int64_t(sizeof(bool)));
    fio.write(sizeof(short), &vals);
    unsigned short valus;
    fio.read(sizeof(unsigned short), &valus);
    valus += 50;
    fio.seek(int(-sizeof(unsigned short)), ByteIO::Current);
    fio.write(sizeof(unsigned short), &valus);
    checkValues(fio, 150);
  }

  rfile.setPermissions(0444);
  RegularFileIO fio2(rfile);
  checkValues(fio2, 150);
  bool flag = false;
  try {
    fio2.reopenRW();
  } catch (std::exception& x) {
    flag = true;
  }
  // When the user is root, the file permission test doesn't work: root may still open the
  // file rw. This causes this code inside a Docker container not to throw. So skip the
  // test when the user is root.
  if (geteuid() != 0) {
    AlwaysAssertExit(flag);
  }
  checkValues(fio2, 150);
  rfile.setPermissions(0644);
}

void testMemoryIO() {
  {
    unsigned char buf[10];
    MemoryIO membuf(buf, sizeof(buf), ByteIO::New, 6);
    doIt(membuf);
    AlwaysAssertExit(membuf.getBuffer() != (const unsigned char*)&buf);
    int64_t length = membuf.length();
    int incr = 20;
    membuf.seek(incr, ByteIO::End);
    AlwaysAssertExit(membuf.length() == length + incr);
    checkValues(membuf, 100);
    char val;
    int64_t lincr = incr;
    membuf.seek(-lincr, ByteIO::End);
    for (int i = 0; i < incr; i++) {
      membuf.read(1, &val);
      AlwaysAssertExit(val == 0);
    }
    try {
      membuf.read(1, &val);
    } catch (std::exception& x) {
      cout << x.what() << endl;  // read beyond object
    }
    try {
      membuf.seek(int(-(length + incr + 1)), ByteIO::Current);
    } catch (std::exception& x) {
      cout << x.what() << endl;  // negative seek
    }
    membuf.seek(int(-(length + incr)), ByteIO::Current);
  }
  {
    char buf[10];
    MemoryIO membuf(buf, sizeof(buf), ByteIO::New, 0);
    try {
      doIt(membuf);
    } catch (std::exception& x) {  // not expandable
      cout << x.what() << endl;
    }
    try {
      membuf.seek(10, ByteIO::End);
    } catch (std::exception& x) {  // not expandable
      cout << x.what() << endl;
    }
    AlwaysAssertExit(membuf.getBuffer() == (const unsigned char*)buf);
  }
  {
    char* buf = new char[10];
    MemoryIO membuf(buf, 10, ByteIO::Scratch, 0, true);
    try {
      doIt(membuf);
    } catch (std::exception& x) {  // not expandable
      cout << x.what() << endl;
    }
    try {
      membuf.seek(10, ByteIO::End);
    } catch (std::exception& x) {  // not expandable
      cout << x.what() << endl;
    }
    AlwaysAssertExit(membuf.getBuffer() == (const unsigned char*)buf);
  }
}

int main() {
  try {
    testMemoryIO();

    MemoryIO file2;
    doIt(file2);

    MemoryIO file3(file2.getBuffer(), file2.length());
    try {
      file3.write(0, nullptr);
    } catch (std::exception& x) {
      cout << x.what() << endl;  // readonly
    }
    checkValues(file3, 100);

    {
      RegularFileIO file1(RegularFile("tByteIO_tmp.data"), ByteIO::New);
      doIt(file1);
    }
    checkReopen();
    // Do regular io for various buffer sizes.
    for (unsigned int bs = 1; bs < 100; bs++) {
      RegularFileIO file1(RegularFile("tByteIO_tmp.data"), ByteIO::New, bs);
      doIt(file1);
    }

    int fd = open("tByteIO_tmp.data2", O_CREAT | O_TRUNC | O_RDWR, 0644);
    int flags = fcntl(fd, F_GETFL);
    if (flags & O_RDWR) {
      cout << "read/write" << endl;
    } else if (flags & O_WRONLY) {
      cout << "writeonly" << endl;
    } else {
      cout << "readonly" << endl;
    }
    FiledesIO file4(fd, "");
    doIt(file4);
    close(fd);

    int fd1 = open("tByteIO_tmp.data2", O_RDONLY, 0644);
    int flags1 = fcntl(fd1, F_GETFL);
    if (flags1 & O_RDWR) {
      cout << "read/write" << endl;
    } else if (flags1 & O_WRONLY) {
      cout << "writeonly" << endl;
    } else {
      cout << "readonly" << endl;
    }
    FiledesIO file5(fd1, "");
    checkValues(file5, 100);
    close(fd1);

    int fd2 = creat("tByteIO_tmp.data2", 0644);
    int flags2 = fcntl(fd2, F_GETFL);
    if (flags2 & O_RDWR) {
      cout << "read/write" << endl;
    } else if (flags2 & O_WRONLY) {
      cout << "writeonly" << endl;
    } else {
      cout << "readonly" << endl;
    }
    FiledesIO file6(fd2, "");
    close(fd2);
  } catch (std::exception& x) {
    cout << "Caught an exception: " << x.what() << endl;
    return 1;
  }
  return 0;  // exit with success status
}
