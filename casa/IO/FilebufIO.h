// # FilebufIO.h: Class for buffered IO on a file
// # Copyright (C) 1996,1997,1999,2001,2002
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

#ifndef CASA_FILEBUFIO_H
#define CASA_FILEBUFIO_H

// # Includes
#include <casacore/casa/aips.h>
#include <casacore/casa/IO/ByteIO.h>
#include <casacore/casa/BasicSL/String.h>

#include <vector>

namespace casacore {  // # NAMESPACE CASACORE - BEGIN

// <summary> Class for buffered IO on a file.</summary>

// <use visibility=export>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="tByteIO" demos="">
// </reviewed>

// <prerequisite>
//  <li> <linkto class=ByteIO>ByteIO</linkto>
// </prerequisite>

// <synopsis>
// This class is a specialization of class
// <linkto class=ByteIO>ByteIO</linkto>.
// This class is doing IO on a file in a buffered way to reduce the number
// of file accesses as much as possible.
// It is part of the entire IO framework. It can for
// instance be used to store data in canonical format in a file
// in an IO-efficient way.
// <br>
// The buffer size is dynamic, so any time it can be set as needed.
// <p>
// It is also possible to construct a <src>FilebufIO</src> object
// from a file descriptor (e.g. for a pipe or socket).
// The constructor will determine automatically if the file is
// readable, writable and seekable.
// </synopsis>

// <example>
// This example shows how FilebufIO can be used with an fd.
// It uses the fd for a regular file, which could be done in an easier
// way using class <linkto class=RegularFileIO>RegularFileIO</linkto>.
// However, when using pipes or sockets, this would be the only way.
// <srcblock>
//    // Get a file descriptor for the file.
//    int fd = open ("file.name");
//    // Use that as the source of AipsIO (which will also use CanonicalIO).
//    FilebufIO fio (fd);
//    AipsIO stream (&fio);
//    // Read the data.
//    Int vali;
//    bool valb;
//    stream >> vali >> valb;
// </srcblock>
// </example>

// <motivation>
// The stdio package was used, but it proved to be very slow on Solaris.
// After a seek the buffer was refreshed, which increased the number
// of file accesses enormously.
// Also the interaction between reads and writes in stdio was poor.
// </motivation>

class FilebufIO : public ByteIO {
 public:
  static constexpr unsigned int kDefaultBufferSize = 16384;

  // Default constructor.
  // A stream can be attached using the attach function.
  FilebufIO();

  // Construct from the given file descriptor.
  // Note that the destructor and the detach function implicitly close
  // the file descriptor.
  explicit FilebufIO(int fd, unsigned int bufferSize = kDefaultBufferSize);

  // Attach to the given file descriptor.
  // Note that the destructor and the detach function implicitly close
  // the file descriptor.
  void attach(int fd, unsigned int bufferSize = kDefaultBufferSize);

  // The destructor closes the file when it was owned and opened and not
  // closed yet.
  ~FilebufIO() override;

  // Write the number of bytes.
  void write(int64_t size, const void* buf) override;

  // Read <src>size</src> bytes from the File. Returns the number of bytes
  // actually read. Will throw an exception (AipsError) if the requested
  // number of bytes could not be read unless throwException is set to
  // false. Will always throw an exception if the file is not readable or
  // the system call returns an undocumented value.
  int64_t read(int64_t size, void* buf, bool throwException = true) override;

  // Flush the current buffer.
  void flush() override;

  // Resync the file (i.e. empty the current buffer).
  void resync() override;

  // Truncate the file to the given size.
  void truncate(int64_t size) override;

  // Get the length of the byte stream.
  int64_t length() override;

  // Is the IO stream readable?
  bool isReadable() const override;

  // Is the IO stream writable?
  bool isWritable() const override;

  // Is the IO stream seekable?
  bool isSeekable() const override;

  // Get the file name of the file attached.
  String fileName() const override;

  // Get the buffer size.
  unsigned int bufferSize() const { return itsBuffer.size(); }

 protected:
  // Detach the FILE. Close it when needed.
  void detach(bool closeFile = false);

  // Determine if the file descriptor is readable and/or writable.
  void fillRWFlags(int fd);

  // Determine if the file is seekable.
  void fillSeekable();

  // Reset the position pointer to the given value. It returns the
  // new position.
  int64_t doSeek(int64_t offset, ByteIO::SeekOption) override;

  // Set a new buffer size.
  // If a buffer was already existing, flush and delete it.
  void setBuffer(int64_t bufSize);

  // Write a buffer of given length into the file at given offset.
  void writeBuffer(int64_t offset, const char* buf, int64_t size);

  // Read a buffer of given length from the file at given offset.
  int64_t readBuffer(int64_t offset, char* buf, int64_t size, bool throwException);

  // Write a partial block into the stream at the current offset.
  // It is guaranteed by the caller that the block fits in a single buffer.
  void writeBlock(int64_t size, const char* buf);

  // Read a partial block from the stream at the current offset.
  // It is guaranteed by the caller that the block fits in a single buffer.
  int64_t readBlock(int64_t size, char* buf, bool throwException);

 private:
  void checkedSeek(int64_t offset, int whence);

  bool itsSeekable = false;
  bool itsReadable = false;
  bool itsWritable = false;
  int itsFile = -1;
  int64_t itsBufLen = 0;  // the current buffer length used
  std::vector<char> itsBuffer;
  int64_t itsBufOffset = -1;   // file offset of current buffer
  int64_t itsOffset = -1;      // current file offset
  int64_t itsSeekOffset = -1;  // offset last seeked
  bool itsDirty = false;       // data written into current buffer?

  FilebufIO(const FilebufIO& that) = delete;
  FilebufIO& operator=(const FilebufIO& that) = delete;
};

}  // namespace casacore

#endif
