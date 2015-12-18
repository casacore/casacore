# next release

changes since 2.0.3

- Add experimental Python3 support. Read the README for instructions
- Changed behavior from 2.0.3: Use PYTHON2_* configuration options to set
  your python2 interpreter.
- The default search path for measures data is now smaller. The path can
  be set at cmake time by specifying `-DDATA_DIR=/path/to/data`. This path
  can contain `%CASAROOT%` which is expanded at run time, to support 
  relocatable installations. The measures path can also still be set in
  `.casarc`.

# 2.0.3

## General
- Merge of CASA work into casacore, CASA should compile with casacore now
- Under-the-hood optimizations, like using allocator features (#132)
- Building with gcc 5 is now possible (#166)
- Compatibility with more versions of wcslib and cfitsio
- SOFA is now an external dependency (#105)

## Tables
- Arrays can now sometimes be reshaped while keeping allocated memory (#113)

## MS
- Selection of baselines with a regexp between stations is now possible (#99)
- Several new methods to MSMetaData (#138)

## Bug fixes
- Fix a bug which caused an error with LOFAR measurement sets with 
  the LOFAR tool msoverview (#140)
- Fix a bug where TaQL would not write output when an expression was 
  used (#184)

# 2.0.2

This version was not released

# 2.0.1

changes since 2.0.0

## Bug fixes
- Does not build if checkout root folder is not named casacore (#79)


# 2.0.0

changes since 1.7.0

## General
- Fixed all build problems for GCC and clang on various platforms
  (up to OS-X 10.10)
- Fixed all valgrind issues found in test programs
- Made the statics in all of casacore thread-safe. Similar to
  libstdc++ the user is responsible for thread-safety when using
  casacore ojbects.
- Added casacore/ to the #include path of all header files
- Changed namespace from casa to casacore (but still #defined as
  casa for time being)
- Split Tables, MeasurementSets and Lattices into smaller parts
- The 3 changes above give rise to backward incompatibilities. Scripts
  in casacore/changescripts (notably updateall) can be used to fix
  client code.

## Tables
- Added GROUPBY/HAVING/aggregation function to TaQL
- Optimized TaQL's IN operator (linear time for integer sets)
- Added table tracing (class TableTrace)
- Fixed TiledStMan issue where BucketCache was not shrinked
- Added MultiFile option to combine table files in a single one

## Components
- This module has been removed (too CASA specific)

## Images
- Added support for beam per frequency channel and Stokes
- Added persistency for ImageExpr and ImageCocat objects

## Python
- Moved converters from pyrap to casacore/Python
