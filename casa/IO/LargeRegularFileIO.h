//# Added for backward compatibility

#ifndef CASA_LARGEREGULARFILEIO_H
#define CASA_LARGEREGULARFILEIO_H

#warning "LargeRegularFileIO.h has been deprecated; use RegularFileIO.h instead."

#include <casacore/casa/IO/RegularFileIO.h>

namespace casacore {
  typedef RegularFileIO LargeRegularFileIO;
}

#endif
