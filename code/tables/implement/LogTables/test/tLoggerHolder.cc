//# tImageLogger.cc: Test program for class ImageLogger
//# Copyright (C) 2001
//# Associated Universities, Inc. Washington DC, USA.
//#
//# This library is free software; you can redistribute it and/or modify it
//# under the terms of the GNU Library General Public License as published by
//# the Free Software Foundation; either version 2 of the License, or (at your
//# option) any later version.
//#
//# This library is distributed in the hope that it will be useful, but WITHOUT
//# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
//# FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Library General Public
//# License for more details.
//#
//# You should have received a copy of the GNU Library General Public License
//# along with this library; if not, write to the Free Software Foundation,
//# Inc., 675 Massachusetts Ave, Cambridge, MA 02139, USA.
//#
//# Correspondence concerning AIPS++ should be addressed as follows:
//#        Internet email: aips2-request@nrao.edu.
//#        Postal address: AIPS++ Project Office
//#                        National Radio Astronomy Observatory
//#                        520 Edgemont Road
//#                        Charlottesville, VA 22903-2475 USA
//#
//#
//# $Id$

#include <trial/Images/ImageLogger.h>
#include <aips/Tables/Table.h>
#include <aips/Exceptions/Error.h>
#include <aips/Utilities/Assert.h>

void doIt (Bool tempClose)
{
  ImageLogger sublogger (False);
  ImageLogger sublogger1 (False);
  ImageLogger logger (False);
  logger.addParent (&sublogger);
  logger.addParent (&sublogger1);
  sublogger.logio() << "subtest1" << LogIO::POST;
  logger.logio() << "test1" << LogIO::POST;

  // Make sure we have a new table.
  if (Table::isReadable ("tImageLogger_tmp.log")) {
    Table::deleteTable ("tImageLogger_tmp.log");
  }
  // Create with a TableLogSink.
  // Test copy ctor and assignment.
  ImageLogger logger2a ("tImageLogger_tmp.log", True);
  ImageLogger logger2 (logger);
  logger2 = logger2a;
  logger2.addParent (&logger);
  logger2.addParent (&sublogger);
  logger2.logio() << "testtable" << LogIO::POST;
  logger.logio() << "test2" << LogIO::POST;
  AlwaysAssertExit (logger.logio().localSink().nelements() == 2);
  AlwaysAssertExit (logger2.logio().localSink().nelements() == 1);
  AlwaysAssertExit (logger.sink().nelements() == 2);
  AlwaysAssertExit (logger2.sink().nelements() == 1);

  if (tempClose) {
    logger2.tempClose();
  }
  AlwaysAssertExit (logger2.isTempClosed() == tempClose);

  {
    Table tab("tImageLogger_tmp.log");
    AlwaysAssertExit (tab.nrow() == 1);
  }

  for (ImageLogger::const_iterator iter = logger2.begin();
       iter != logger2.end();
       iter++) {
    cout << iter->time() << ' ' << (*iter).message() << endl;
  }
  AlwaysAssertExit (logger2.isTempClosed() == tempClose);
  for (ImageLogger::const_iterator iter = sublogger1.begin();
       iter != sublogger.end();
       iter++) {
    cout << iter->time() << ' ' << (*iter).message() << endl;
  }
}

int main()
{
  try {
    doIt (False);
    doIt (True);
  } catch (AipsError& x) {
    cout << "Unexpected exception: " << x.getMesg() << endl;
    return 1;
  } catch (...) {
    cout << "Unexpected unknown exception" << endl;
    return 1;
  }
  cout << "OK" << endl;
  return 0;
}
