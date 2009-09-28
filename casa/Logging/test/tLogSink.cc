#include <casa/Logging/LogIO.h>
#include <casa/Logging/LogSink.h>

using namespace casa;

int main()
{
  LogSink *sink = new LogSink(LogMessage::SEVERE);
  LogIO io_p(*sink);

  io_p << "Something happened";

  delete sink;
  return 0;
}
