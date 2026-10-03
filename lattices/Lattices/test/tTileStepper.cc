// # tTileStepper.cc: Test program for class TileStepper
// # Copyright (C) 1997,1998,1999,2000,2001
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

#include <casacore/lattices/Lattices/TileStepper.h>
#include <casacore/casa/Inputs/Input.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/iostream.h>

#include <casacore/casa/namespace.h>
int main(int argc, const char* argv[]) {
  Input inp(1);
  inp.version(" ");
  inp.create("nx", "128", "Number of pixels along the x-axis", "int");
  inp.create("ny", "128", "Number of pixels along the y-axis", "int");
  inp.create("nz", "128", "Number of pixels along the z-axis", "int");
  inp.create("tx", "32", "Tile size along the x-axis", "int");
  inp.create("ty", "32", "Tile size along the y-axis", "int");
  inp.create("tz", "32", "Tile size along the z-axis", "int");
  inp.create("blcx", "0", "Blc along the x-axis", "int");
  inp.create("blcy", "0", "Blc along the y-axis", "int");
  inp.create("blcz", "0", "Blc along the z-axis", "int");
  inp.create("trcx", "1000000", "Trc along the x-axis", "int");
  inp.create("trcy", "1000000", "Trc along the y-axis", "int");
  inp.create("trcz", "1000000", "Trc along the z-axis", "int");
  inp.create("incx", "1", "Inc along the x-axis", "int");
  inp.create("incy", "1", "Inc along the y-axis", "int");
  inp.create("incz", "1", "Inc along the z-axis", "int");
  inp.readArguments(argc, argv);

  const unsigned int nx = inp.getInt("nx");
  const unsigned int ny = inp.getInt("ny");
  const unsigned int nz = inp.getInt("nz");
  const unsigned int tx = inp.getInt("tx");
  const unsigned int ty = inp.getInt("ty");
  const unsigned int tz = inp.getInt("tz");
  const unsigned int blcx = inp.getInt("blcx");
  const unsigned int blcy = inp.getInt("blcy");
  const unsigned int blcz = inp.getInt("blcz");
  const unsigned int trcx = inp.getInt("trcx");
  const unsigned int trcy = inp.getInt("trcy");
  const unsigned int trcz = inp.getInt("trcz");
  const unsigned int incx = inp.getInt("incx");
  const unsigned int incy = inp.getInt("incy");
  const unsigned int incz = inp.getInt("incz");

  // Check/adapt the values.
  IPosition shape(3, nx, ny, nz);
  IPosition tileShape(3, tx, ty, tz);
  IPosition blc(3, blcx, blcy, blcz);
  IPosition trc(3, trcx, trcy, trcz);
  IPosition inc(3, incx, incy, incz);
  for (unsigned int i = 0; i < 3; i++) {
    AlwaysAssertExit(shape(i) > 0);
    AlwaysAssertExit(tileShape(i) > 0);
    AlwaysAssertExit(blc(i) >= 0);
    AlwaysAssertExit(inc(i) > 0);
    if (tileShape(i) > shape(i)) tileShape(i) = shape(i);
    if (blc(i) > shape(i) - 1) blc(i) = shape(i) - 1;
    if (trc(i) > shape(i) - 1) trc(i) = shape(i) - 1;
    if (trc(i) < blc(i)) trc(i) = blc(i);
  }
  cout << "shape = " << shape << endl;
  cout << "tileshape = " << tileShape << endl;
  cout << "blc = " << blc << endl;
  cout << "trc = " << trc << endl;
  cout << "inc = " << inc << endl;
  {
    TileStepper stepper(shape, tileShape);
    stepper.subSection(blc, trc, inc);
    while (!stepper.atEnd()) {
      cout << stepper.position() << ' ' << stepper.endPosition() << ' ' << stepper.cursorShape()
           << endl;
      stepper++;
    }
    cout << "nsteps = " << stepper.nsteps() << endl;
    while (!stepper.atStart()) {
      cout << stepper.position() << ' ' << stepper.endPosition() << ' ' << stepper.cursorShape()
           << endl;
      stepper--;
    }
    cout << "nsteps = " << stepper.nsteps() << endl;
  }
  {
    TileStepper stepper(shape, tileShape, IPosition(3, 2, 1, 0));
    stepper.subSection(blc, trc, inc);
    while (!stepper.atEnd()) {
      cout << stepper.position() << ' ' << stepper.endPosition() << ' ' << stepper.cursorShape()
           << endl;
      stepper++;
    }
    cout << "nsteps = " << stepper.nsteps() << endl;
    while (!stepper.atStart()) {
      cout << stepper.position() << ' ' << stepper.endPosition() << ' ' << stepper.cursorShape()
           << endl;
      stepper--;
    }
    cout << "nsteps = " << stepper.nsteps() << endl;
  }
}
