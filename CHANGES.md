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
