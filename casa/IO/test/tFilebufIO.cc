// # tFilebufIO.cc: Test program for class FilebufIO
// # Copyright (C) 2002
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

#include <casacore/casa/IO/RegularFileIO.h>
#include <casacore/casa/OS/RegularFile.h>
#include <casacore/casa/Utilities/Assert.h>

#include <boost/test/unit_test.hpp>

#include <sys/stat.h>
#include <cstring>
#include <vector>

#include <unistd.h>
#include <fcntl.h>

using casacore::FilebufIO;

namespace {
constexpr size_t kWriteSize = 2000;

class TestFilebufIO : public FilebufIO {
public:
  TestFilebufIO(int fd, size_t buffer_size) : FilebufIO(fd, buffer_size) {}
  using FilebufIO::detach;
};

}

BOOST_AUTO_TEST_SUITE(file_buf_io)

void checkRead(int fd, size_t read_buffer_size, const char* expected) {
  FilebufIO fio(fd, read_buffer_size);
  fio.seek(0);
  std::vector<char> buffer(read_buffer_size);

  for (size_t j = 0; j < kWriteSize / read_buffer_size; j++) {
    BOOST_REQUIRE_EQUAL(fio.read(read_buffer_size, buffer.data(), true), read_buffer_size);
    for (size_t i = 0; i < read_buffer_size; i++) {
      BOOST_REQUIRE_EQUAL(buffer[i], *expected++);
    }
  }
  const size_t remaining = kWriteSize % read_buffer_size;
  BOOST_REQUIRE_EQUAL(fio.read(read_buffer_size, buffer.data(), false), remaining);
  for (size_t i = 0; i < remaining; i++) {
    BOOST_REQUIRE_EQUAL(buffer[i], *expected++);
  }
}

BOOST_AUTO_TEST_CASE(various_read_sizes) {
  char write_data[kWriteSize];
  for (size_t i = 0; i < kWriteSize; i++) {
    write_data[i] = i;
  }
  {
    casacore::RegularFileIO file(casacore::RegularFile("tFilebufIO_tmp.dat"), casacore::ByteIO::New);
    file.write(kWriteSize, write_data);
  }
  int fd = open("tFilebufIO_tmp.dat", O_RDONLY);
  constexpr size_t kReadSizes[] = { 2000, 100, 1, 3, 101, 2001, 10000 };
  for(size_t read_size : kReadSizes) {
    checkRead(fd, read_size, write_data);
  }
  close(fd);
}


BOOST_AUTO_TEST_CASE(read_with_cached_middle_block) {
  constexpr int64_t kBufferSize = 4;
  constexpr size_t kFileSize = 16;

  char write_data[kFileSize];
  for (size_t i = 0; i < kFileSize; ++i) {
    write_data[i] = static_cast<char>(i);
  }

  int fd = open("tFilebufIO_cached.dat", O_CREAT | O_TRUNC | O_RDWR, 0600);
  BOOST_REQUIRE(fd >= 0);
  BOOST_REQUIRE_EQUAL(write(fd, write_data, kFileSize), kFileSize);

  {
    TestFilebufIO fio(fd, kBufferSize);

    // Load block 1 into the internal buffer.
    constexpr int64_t kBlock1Offset = kBufferSize;
    fio.seek(kBlock1Offset);
    char cached;
    BOOST_REQUIRE_EQUAL(fio.read(1, &cached, true), 1);
    BOOST_REQUIRE_EQUAL(cached, write_data[kBlock1Offset]);

    // Read blocks 0, 1 and 2. Block 1 is cached, while
    // blocks 0 and 2 need to be read directly.
    fio.seek(0);
    char buffer[3 * kBufferSize];
    BOOST_REQUIRE_EQUAL(fio.read(sizeof(buffer), buffer, true), sizeof(buffer));

    for (size_t i = 0; i < sizeof(buffer); ++i) {
      BOOST_CHECK_EQUAL(buffer[i], write_data[i]);
    }

    fio.detach(false);
  }

  close(fd);
}

BOOST_AUTO_TEST_CASE(truncate_with_dirty_buffer) {
  constexpr size_t kBufferSize = 8;
  constexpr size_t kFileSize = 16;

  char write_data[kFileSize] = {};

  int fd = open("tFilebufIO_truncate.dat", O_CREAT | O_TRUNC | O_RDWR, 0600);
  BOOST_REQUIRE(fd >= 0);
  BOOST_REQUIRE_EQUAL(::write(fd, write_data, kFileSize), kFileSize);

  {
    TestFilebufIO fio(fd, kBufferSize);

    // Modify bytes 6 and 7. This leaves the buffer dirty.
    fio.seek(6);
    const char replacement[] = {'A', 'B'};
    fio.write(sizeof(replacement), replacement);

    // Truncate the file before the dirty buffer is flushed.
    fio.truncate(4);

    // Detaching flushes the buffer.
    fio.detach(false);
  }

  struct stat st {};
  BOOST_REQUIRE_EQUAL(fstat(fd, &st), 0);

  // The dirty buffer must not extend the file again.
  BOOST_CHECK_EQUAL(st.st_size, 4);

  close(fd);
}

BOOST_AUTO_TEST_CASE(non_throwing_read_at_eof) {
  constexpr size_t kBufferSize = 4;
  constexpr size_t kFileSize = 10;

  char write_data[kFileSize];
  for (size_t i = 0; i < kFileSize; ++i) {
    write_data[i] = static_cast<char>(i);
  }

  int fd = open("tFilebufIO_eof.dat", O_CREAT | O_TRUNC | O_RDWR, 0600);
  BOOST_REQUIRE(fd >= 0);
  BOOST_REQUIRE_EQUAL(::write(fd, write_data, kFileSize), kFileSize);

  {
    class TestFilebufIO : public FilebufIO {
    public:
      TestFilebufIO(int fd) : FilebufIO(fd, kBufferSize) {}
      using FilebufIO::detach;
    };

    TestFilebufIO fio(fd);

    char buffer[16] = {};

    // Request more bytes than are available.
    fio.seek(0);
    BOOST_CHECK_EQUAL(fio.read(sizeof(buffer), buffer, false), kFileSize);

    for (size_t i = 0; i < kFileSize; ++i) {
      BOOST_CHECK_EQUAL(buffer[i], write_data[i]);
    }

    // Reading again at EOF should return zero.
    BOOST_CHECK_EQUAL(fio.read(1, buffer, false), 0);

    fio.detach(false);
  }

  close(fd);
}

BOOST_AUTO_TEST_CASE(detach_resets_access_flags) {
  int fd = open("tFilebufIO_detach.dat", O_CREAT | O_TRUNC | O_RDONLY, 0600);
  BOOST_REQUIRE(fd >= 0);

  {
    TestFilebufIO fio(fd, 4);
    BOOST_CHECK(fio.isReadable());
    BOOST_CHECK(!fio.isWritable());

    fio.detach(false);

    BOOST_CHECK(!fio.isReadable());
    BOOST_CHECK(!fio.isWritable());
    BOOST_CHECK(!fio.isSeekable());
  }

  close(fd);
}

BOOST_AUTO_TEST_CASE(invalid_file_descriptor) {
  FilebufIO fio(-1, 4);

  // An invalid descriptor must not be considered readable or writable.
  BOOST_CHECK(!fio.isReadable());
  BOOST_CHECK(!fio.isWritable());
  BOOST_CHECK(!fio.isSeekable());
}

BOOST_AUTO_TEST_SUITE_END()
