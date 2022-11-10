# 3.5.0

## Highlights

Apart from many bug fixes and modernizations, a few new features were introduced:

 - TaQL now supports copying columns to new columns (e.g. to make a backup of the `FLAG` column)
 - Non-zero coordinates in JPL frames, e.g. the `SUN` frame, are now interpreted as an offset in right ascension and declination. Previously, coordinates in a  JPL coordinate frame were ignored.
 - The Dysco storage manager is now built by default as part of casacore.


## General

 - Improve continuous integration (#1180, #1182), move from Travis CI to Github Actions (#1086, #1097, #1098, #1161)
 - Modernizations: remove Mutex (#1095, #1127, #1128), typing system (#1172, #1176)
 - Allow building with newer compilers (#1134, #1137, #1145, #1206, #1208, #1210, #1211)
 - Build python3 by default, not python2 (#1209)


## MeasurementSet
 - Improve reading of DataDescriptionId (#1103)
 - Compute feed information (#1104) and field attributes (#1109) on demand
 - UVFits improvements: support X-Y mounts (#1115), allow large antenna numbers (#1144), fix handling FITS-IDI `GAIN_CURVE` (#1151)
 - Fix precision in `T+dT` syntax (#1118)


## Measures
 - Allow offsets to JPL coordinates (#1160)

## Tables
 - Cache iteration boundaries while sorting tables (#1106)
 - Use RefTable for iteration (#1108)
 - Adios2 related changes: #1110, #1116, #1121, #1148
 - Make Dysco part of casacore (#1117, #1125, #1146, #1166)
 - Add TaQL commands `LIKE`, `COPYCOLUMN` and `DROPTABLE` (#1154, #1169)
 - Fix one bug in `rownr_t` migration for TiledDataStMan (#1156)
 - Improve documentation (#1192)

## Images
 - Support CARTA opening images with a custom handler (#1158)
 - Allow easier python handling of beams (#1184)


# 3.4.0

## General
 - Major modernization of Arrays, bringing it more in line with C++11 (#1012)
 - Use std::regex (#1072)
 - Remove many warnings (#1024, #1036, #1062)
 - Use FFTW as default FFT provider (#1029, #1047, #1049)
 - UVFITS improvements (#1033, #1040, #1064, #1066, #1067)
 - Support for 64-bit row numbers.


# 3.3.0

## General
 - Replace implementation of complex trig functions by std:: (#1010)
 - Fix bug SciMath / StatsFramework where parameters were passed incorrectly (#984)

## Tables
 - Pass sorting algorithm optio nthrough to table iterator (#992)
 - Support for ADIOS2 (#1006)
 - Add .casarc option to disable table locking (#1002)

## Measurement Set / UVFits
 - Fix antenna swapping in MSConcat (#977)
 - Merge PROCESSOR table in MSConcat (#1004)
 - Fix antenna positions in importing old VLA UVFITS (#1005)

## Images
 - More efficient WCS batch coordinate lookups (#932)


# 3.2.1

 - Increment the SO version, otherwise identical to 3.2 (#981)


# 3.2

## General
 - Remove some unused functionality (#854, #947)
 - Prevent setting OpenMP num_threads to zero (#962)

## Tables
 - Fix locking issue with IncrementalStorageManager (#970, #974)
 - Various improvements to TaQL
 - Various ADIOS2 improvements (#943, #966)

## Measures
 - Update hardcoded URLs for fetching measures data (#975)

## FITS
 - Correctly convert Nasmyth mount types (#946, #954)

# 3.1.2

 - Bump version in version.h (since in 3.1.1 it was still at '3.1.0')

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
 - Improve CMake FindPython (#922)

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
