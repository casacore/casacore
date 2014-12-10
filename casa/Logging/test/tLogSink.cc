#include <casacore/casa/Logging/LogIO.h>
#include <casacore/casa/Logging/LogSink.h>

using namespace casacore;

int main()
{
  LogSink *sink = new LogSink(LogMessage::SEVERE);
  LogIO io_p(*sink);

  io_p << "Something happened";

  delete sink;
  return 0;
}
