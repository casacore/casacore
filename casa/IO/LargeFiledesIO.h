//# Added for backward compatibility

#ifndef CASA_LARGEFILEDESIO_H
#define CASA_LARGEFILEDESIO_H

#warning "LargeFiledesIO.h has been deprecated; use FiledesIO.h instead."

#include <casacore/casa/IO/FiledesIO.h>

namespace casacore {
  typedef FiledesIO LargeFiledesIO;
}

#endif
