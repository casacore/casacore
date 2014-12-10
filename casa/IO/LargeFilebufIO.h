//# Added for backward compatibility

#ifndef CASA_LARGEFILEBUFIO_H
#define CASA_LARGEFILEBUFIO_H

#warning "LargeFilebufIO.h has been deprecated; use FilebufIO.h instead."

#include <casacore/casa/IO/FilebufIO.h>

namespace casacore {
  typedef FilebufIO LargeFilebufIO;
}

#endif
