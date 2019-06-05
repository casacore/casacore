# 3.1.1

## General
 - Only use DataManager.get/putSlice if possible (#901)
 - User docker for Linux build on Travis for CI  (#856)
 - improve unicode support (#853)
 - Changed MadfmFunc so that it inherits from ArrayFunctorBase (#905)
 - Fix memory leak in TableProxy::getColumnDescription  (#900)
 - Allow colon and fix recursive bison parsing  (#894)
 - ObjectID's hash function behavior fix (#897)
 - Add a public getter for the mask array to LCRegionFixed  (#895)
 - Replace STL-like containers by their STL counterparts  (#890)

## Python support
 - Boost 1.67 Python components require a Python version suffix (#844)
 - Handle numpy Unicode arrays  (#912)
 - Fix memory leak when converting Python unicode to casa string (#910)
 - Possible leak bug (#908)
 - Always use TpInt64 for PyLong (#916)
 
## Build system
 - Travis macOS fails on Python3 OSX build system (#778)
 - Switch travis to docker /xenial (#847)

# 3.1

## General
- Removed many warnings issued by newer compiler versions (#798, #809, #819, #820, #866, #883)
- Further improved thread-safety (#817, #869, #877, #886)
- Added variance and standard deviation of complex numbers (#851)

## Tables
- Improved parallel storage manager based on ADIOS2
- Support of Int64 table columns (#859)
- Added O_DIRECT support when using MultiFile (#885)
- TaQL improvements (#813, #851, #867)
- Small CTDS fixes (#840, #868)

## Measurement Set
- Small improvements (#858, #864, #872)
- Better handling of FITS-IDI data bit sampling (#836)

## Images
- Support for non-chunked HDF5 data sets (#879, #880)
- Fixed count overflow on very large images (#849)



# 3.0

## General
- Building with C++11 is now required
- Improved thread-safety of statics (#775)
- Improve statistics code (#785, #776, #662, #663)

## Tables
- Avoid growing the size of tables when overwriting data (#768)
- TaQL support for radial velocity, earth magnetic field and Doppler (#750)
- Added performance tests for tables (#658)

## Measurement Set
- Improvements to MSConcat (#779, #701)
- Better handling of FITS-IDI flag history (#748)


## Images
- Better support for double precision images (#752, #751)


## Other

- A full list of changes can be found on the [issue tracker](https://github.com/casacore/casacore/milestone/8?closed=1).


 
# 2.4.1

## General
- This release fixes a bug where TableIterator would skip a row

# 2.4.0

## General
- Bug fixes and improvements
- This version can be used to compile CASA 5.0
- Improved error checking when parsing dates/times (#619)
- Statistics: allow data provider to specify number of threads (#645)

## MS
- Disable caching of MS main table columns (#597)
- Properly copy the `SIGMA_SPECTRUM` column, if present (#599)
- Implement getTimesForSpws (#600)

## FITS and FITS-IDI
- Implement digital corrections for DiFX/VLBA (#602)
- Several improvements for WEIGHTS when reading FITS-IDI (#590, #608)
- Parallactic angle calculation for Nasmyth mounts (#627)
- Modernised matrix syntax in FITS files now follows standard 3.0 (#606)


# 2.3.0

## General
- Bug fixes and improvements
- Improved installation documentation in README
- Add an option to use Ccache (`-DUseCcache`) (#549)
- Add some statistics functionality (#569)

## Python
- Make some of the TableProxy functionality publicly available (#559)
- Make version checking from python (or plain C) possible (#583)

## TaQL
- Fix transpose in TaQL (#563)
- Added functions `delay` and `uvwapp` to `mscal` (#562)

## FITS
- MSFitsOutput now writes ant diams (#536)
- Improvements to FITS-IDI conversion (#538, #590, #579)


# 2.2.0

## General
- Lots of bug fixes and improvements
- Tests are not built by default anymore, use `-DBUILD_TESTING=True`
  to build them
- Building with C++11 is now default, use `-DCXX11=False` if you do
  not have a recent compiler (#533)
- Added JSON support (#506)

## TaQL
- Major improvements in TaQL, such as masked arrays, new commands
  `ALTER TABLE`, `SHOW TABLE`, `HELP` (#388)

## Images
- ImageConcat and ImageExpr now use JSON export (#517)


# 2.1.0

## General
- Lots of code improvement, optimization and added tests

## Measures
- The default search path for measures data is now smaller. The path can
  be set at cmake time by specifying `-DDATA_DIR=/path/to/data`. This path
  can contain `%CASAROOT%` which is expanded at run time, to support 
  relocatable installations. The measures path can also still be set in
  `.casarc`. (#277)
- Inserted fix to leap second handling problem (#290)
- Various coordinate performance improvements  (#258)

## Tables
- Renamed showtable to showtableinfo (#320)
- Added several TaQL functions (#229)

## MS
- Improve testing of MSSummary (#330)
- Many improvements for the MS related functions (#318, #291, #228, #208, ...)
- Fix multithreaded MS creation (#298)

## Python
- Build python bindings by default (not for CASA build)
- Add experimental Python3 support. Read the README for instructions (#280)
- Changed behavior from 2.0.3: Use PYTHON2_* configuration options to set
  your python2 interpreter. (#280)

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
