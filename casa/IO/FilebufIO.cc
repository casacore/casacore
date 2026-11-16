// # FilebufIO.cc: Class for buffered IO on a file.
// # Copyright (C) 1996,1997,1998,1999,2000,2001,2002
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
#include <casacore/casa/IO/LargeIOFuncDef.h>
#include <casacore/casa/IO/FilebufIO.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Exceptions/Error.h>
#include <unistd.h>
#include <fcntl.h>
#include <errno.h>                 // needed for errno
#include <casacore/casa/string.h>  // needed for strerror

namespace casacore {  // # NAMESPACE CASACORE - BEGIN

FilebufIO::FilebufIO() = default;

FilebufIO::FilebufIO(int fd, unsigned int bufferSize) { attach(fd, bufferSize); }

FilebufIO::~FilebufIO() { detach(); }

void FilebufIO::attach(int fd, unsigned int bufSize) {
  AlwaysAssert(itsFile == -1, AipsError);
  itsFile = fd;
  itsOffset = 0;
  itsSeekOffset = -1;
  itsDirty = false;
  fillRWFlags(fd);
  fillSeekable();
  setBuffer(bufSize);
}

void FilebufIO::setBuffer(int64_t bufSize) {
  if (!itsBuffer.empty()) {
    flush();
    itsBuffer.clear();
    itsBufLen = 0;
    itsBufOffset = -1;
  }
  if (bufSize > 0) {
    itsBuffer.assign(bufSize, 0);
    itsBufOffset = -int(bufferSize() + 1);
  }
}

void FilebufIO::detach(bool closeFile) {
  setBuffer(0);

  if (closeFile && itsFile >= 0) {
    ::traceCLOSE(itsFile);
  }
  itsFile = -1;
  itsSeekable = false;
  itsReadable = false;
  itsWritable = false;
}

void FilebufIO::fillRWFlags(int fd) {
  itsReadable = false;
  itsWritable = false;
  const int flags = fcntl(fd, F_GETFL);
  if(flags >= 0) {
    if ((flags & O_RDWR) == O_RDWR) {
      itsReadable = true;
      itsWritable = true;
    } else if ((flags & O_WRONLY) == O_WRONLY) {
      itsWritable = true;
    } else {
      itsReadable = true;
    }
  }
}

void FilebufIO::fillSeekable() {
  int64_t curOff = itsOffset;
  itsSeekable = (doSeek(0, ByteIO::End) >= 0);
  itsOffset = curOff;
}

String FilebufIO::fileName() const { return ""; }

void FilebufIO::flush() {
  if (itsDirty) {
    writeBuffer(itsBufOffset, itsBuffer.data(), itsBufLen);
    itsDirty = false;
  }
}

void FilebufIO::truncate(int64_t size) {
  if (!itsBuffer.empty()) {
    flush();
    itsBuffer.clear();
    itsBufLen = 0;
    itsBufOffset = -1;
  }
  if (ftruncate(itsFile, size) == -1) throw std::runtime_error("ftruncate reported an error");
}

void FilebufIO::resync() {
  AlwaysAssert(!itsDirty, AipsError);
  itsBufLen = 0;
  itsBufOffset = -int(bufferSize() + 1);
  itsOffset = 0;
  itsSeekOffset = -1;
}

void FilebufIO::writeBuffer(int64_t offset, const char* buf, int64_t size) {
  if (size > 0) {
    if (offset != itsSeekOffset) {
      ::traceLSEEK(itsFile, offset, SEEK_SET);
      itsSeekOffset = offset;
    }
    if (::traceWRITE(itsFile, const_cast<char*>(buf), size) != size) {
      int error = errno;
      itsSeekOffset = -1;
      throw AipsError(String("FilebufIO: write error for file ") + fileName() + ": " +
                      strerror(error));
    }
    itsSeekOffset += size;
  }
}

int64_t FilebufIO::readBuffer(int64_t offset, char* buf, int64_t size, bool throwException) {
  if (offset != itsSeekOffset) {
    ::traceLSEEK(itsFile, offset, SEEK_SET);
    itsSeekOffset = offset;
  }
  int64_t bytesRead = ::traceREAD(itsFile, buf, size);
  int error = errno;
  if (bytesRead > int(size)) {  // Should never be executed
    itsSeekOffset = -1;
    throw AipsError(
        "FilebufIO::read - read returned a bad value"
        " for file " +
        fileName());
  }
  if (bytesRead != int(size) && throwException == true) {
    if (bytesRead < 0) {
      itsSeekOffset = -1;
      throw AipsError(String("FilebufIO::read error for file ") + fileName() + ": " +
                      strerror(error));
    } else if (bytesRead < int(size)) {
      itsSeekOffset = -1;
      throw AipsError("FilebufIO::read - incorrect number of bytes (" + std::to_string(bytesRead) +
                      " out of " + std::to_string(size) + ") read for file " +
                      std::string(fileName()));
    }
  }
  itsSeekOffset += bytesRead;
  return bytesRead;
}

void FilebufIO::write(int64_t size, const void* buf) {
  // Throw an exception if not writable.
  if (!itsWritable) {
    throw AipsError("FilebufIO object (file " + fileName() + ") is not writable");
  }
  const char* bufc = static_cast<const char*>(buf);
  // Determine blocknr of first and last full block.
  int64_t st = (itsOffset + bufferSize() - 1) / bufferSize();
  int64_t end = (itsOffset + size) / bufferSize();
  int64_t blkst = st * bufferSize() - itsOffset;
  int64_t sz = 0;
  if (st < end) {
    sz = (end - st) * bufferSize();
    // There are one or more full blocks.
    // Write them all.
    writeBuffer(st * bufferSize(), bufc + blkst, sz);
    // Discard the current buffer if within these full blocks.
    if (st * bufferSize() <= itsBufOffset && end * bufferSize() >= itsBufOffset + bufferSize()) {
      itsDirty = false;
      itsBufOffset = -int(bufferSize() + 1);
      itsBufLen = 0;
    }
  }
  // Write the start of the user buffer (if needed).
  // Note that by doing this after the full block write, we avoid a
  // possible flush of the current buffer if it is within the full blocks.
  if (blkst > 0) {
    if (blkst > size) {
      blkst = size;
    }
    writeBlock(blkst, bufc);
  }
  // Write the remainder of the user buffer (if needed).
  // First update the offset in the stream.
  blkst += sz;
  itsOffset += blkst;
  if (blkst < size) {
    size -= blkst;
    writeBlock(size, bufc + blkst);
    itsOffset += size;
  }
}

int64_t FilebufIO::read(int64_t size, void* buf, bool throwException) {
  // Throw an exception if not readable.
  if (!itsReadable) {
    throw AipsError("FilebufIO object (file " + fileName() + ") is not readable");
  }
  // Determine blocknr of start and end full block.
  int64_t start_block = (itsOffset + bufferSize() - 1) / bufferSize();
  int64_t end_block = (itsOffset + size) / bufferSize();
  // Offset of the first full block relative to the start of the requested read
  int64_t first_block_offset = start_block * bufferSize() - itsOffset;
  int64_t full_blocks_size = 0;
  if (start_block < end_block) {
    // There are one or more full blocks.
    // Read them all.
    // Handle the case that one of them is the current buffer.
    int64_t start_offset = start_block * bufferSize();
    int64_t end_offset = end_block * bufferSize();
    char* buffer_ptr = static_cast<char*>(buf) + first_block_offset;
    if (start_offset <= itsBufOffset && end_offset >= itsBufOffset + bufferSize()) {
      if (start_offset < itsBufOffset) {
        full_blocks_size = itsBufOffset - start_offset;
        AlwaysAssert(readBuffer(start_offset, buffer_ptr, full_blocks_size, throwException) ==
                         full_blocks_size,
                     AipsError);
        buffer_ptr += full_blocks_size;
      }
      // Do the get of the current block via readBlock to handle
      // the (hardly possible) case that bufferSize()!=itsBufLen.
      int64_t savoff = itsOffset;
      itsOffset = itsBufOffset;
      const int64_t bytes_read = readBlock(bufferSize(), buffer_ptr, throwException);
      full_blocks_size += bytes_read;
      buffer_ptr += bytes_read;
      itsOffset = savoff;
      start_offset = itsBufOffset + bufferSize();
    }
    // Read the remaining full blocks.
    full_blocks_size +=
        readBuffer(start_offset, buffer_ptr, end_offset - start_offset, throwException);
  }
  // Read the start of the user buffer (if needed).
  int64_t total = full_blocks_size;
  if (first_block_offset > 0) {
    if (first_block_offset > size) {
      first_block_offset = size;
    }
    total += readBlock(first_block_offset, static_cast<char*>(buf), throwException);
  }
  // Read the remainder of the user buffer (if needed).
  // First update the offset in the stream.
  first_block_offset += full_blocks_size;
  itsOffset += first_block_offset;
  if (first_block_offset < size) {
    const uint64_t remaining_size = size - first_block_offset;
    total +=
        readBlock(remaining_size, static_cast<char*>(buf) + first_block_offset, throwException);
    itsOffset += remaining_size;
  }
  return total;
}

void FilebufIO::writeBlock(int64_t size, const char* buf) {
  // Write a part of a block.
  // It is ensured that the buffer fits in a single block and that it
  // is not a full block.
  // Flush current buffer if needed.
  if (itsOffset < itsBufOffset || itsOffset >= itsBufOffset + bufferSize()) {
    if (itsDirty) {
      flush();
    }
    // Read the new buffer.
    itsBufOffset = itsOffset / bufferSize() * bufferSize();
    itsBufLen = readBuffer(itsBufOffset, itsBuffer.data(), bufferSize(), false);
    if (itsBufLen < 0) {
      itsBuffer.assign(bufferSize(), 0);
    } else if (itsBufLen < bufferSize()) {
      memset(itsBuffer.data() + itsBufLen, 0, bufferSize() - itsBufLen);
    }
  }
  int64_t st = itsOffset - itsBufOffset;
  memcpy(&itsBuffer[st], buf, size);
  itsDirty = true;
  if (st + size > itsBufLen) {
    itsBufLen = st + size;
  }
}

int64_t FilebufIO::readBlock(int64_t size, char* buf, bool throwException) {
  // Read a part of a block.
  // It is ensured that the buffer fits in a single block and that it
  // is not a full block.
  // Read current buffer if needed. Flush if current block is dirty.
  if (itsOffset < itsBufOffset || itsOffset >= itsBufOffset + bufferSize()) {
    if (itsDirty) {
      flush();
    }
    // Read the new buffer.
    itsBufOffset = itsOffset / bufferSize() * bufferSize();
    itsBufLen = readBuffer(itsBufOffset, itsBuffer.data(), bufferSize(), false);
    if (itsBufLen < 0) {
      itsBuffer.assign(bufferSize(), 0);
    } else if (itsBufLen < bufferSize()) {
      memset(&itsBuffer[itsBufLen], 0, bufferSize() - itsBufLen);
    }
  }
  const int64_t st = itsOffset - itsBufOffset;
  if (st + size > itsBufLen) {
    if (throwException) {
      throw AipsError(
          "FilebufIO::readBlock - incorrect number of bytes"
          " read for file " +
          fileName());
    }
    if (itsBufLen > st) {
      size = itsBufLen - st;
    } else {
      size = 0;
    }
  }
  memcpy(buf, &itsBuffer[0] + st, size);
  return size;
}

int64_t FilebufIO::doSeek(int64_t offset, ByteIO::SeekOption dir) {
  switch (dir) {
    case ByteIO::Begin:
      itsOffset = offset;
      break;
    case ByteIO::End:
      itsSeekOffset = ::traceLSEEK(itsFile, offset, SEEK_END);
      itsOffset = itsSeekOffset;
      break;
    default:
      itsOffset += offset;
      break;
  }
  return itsOffset;
}

int64_t FilebufIO::length() {
  itsSeekOffset = ::traceLSEEK(itsFile, 0, SEEK_END);
  int64_t len = itsBufOffset + itsBufLen;
  if (len < itsSeekOffset) {
    len = itsSeekOffset;
  }
  return len;
}

bool FilebufIO::isReadable() const { return itsReadable; }

bool FilebufIO::isWritable() const { return itsWritable; }

bool FilebufIO::isSeekable() const { return itsSeekable; }

}  // namespace casacore
