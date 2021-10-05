#ifndef DYSCO_STMAN_ERROR_H
#define DYSCO_STMAN_ERROR_H

#include <casacore/tables/DataMan/DataManError.h>

namespace dyscostman {

/**
 * Represents a runtime exception that occured within the DyscoStMan.
 */
class DyscoStManError : public casacore::DataManError {
 public:
  /** Constructor */
  DyscoStManError() : casacore::DataManError() {}
  /** Construct with message.
   * @param message The exception message. */
  explicit DyscoStManError(const std::string &message)
      : casacore::DataManError(
            message + " -- Error occured inside the Dysco Storage Manager") {}
};

}  // namespace dyscostman

#endif
