                CTDS support of very large tables
                =====================


A major change has been made to the Casacore Table Data System (CTDS).
So far, tables larger than 2^31 rows could not be supported, but this
change makes it possible to have tables up to 2^63 rows.

## Data format change
---
The change had quite some impact on the Tables data format, but the
changes have been done in a compatible way.
 * The new software can still read and update existing tables.
   It means it is fully backward compatible.
 * A new table will be written in the old format if it contains less
   than 2^31 rows. It means that an older Casacore version can still
   access most tables created with the new version.


## API change
---
The API has changed considerably, both at the Tables and the
DataManager level.

The main changes at the Tables level are:
 * rownr_t (typedef-ed to uInt64) instead of uInt is now used to
   represent a row number.
 * class RowNumbers instead of Vector<uInt> is used to represent a
   vector of row numbers. It can convert automatically from and to a
   Vector of rownr_t and optionally to uInt (giving backward compatibility).
 * The functions getColumnXXX in class TableExprNode are available
   for RowNumbers and optionally for Vector<uInt>.

To assure that software is using 64-bit row numbers, conversion of
RowNumbers from and to Vector<uInt> is made explicit. However, if
IMPLICIT_CTDS_32BIT is defined, the conversion is implicit (thus automatic).
Similarly, the Vector<uInt> versions of TableExprNode::getColumnXXX
are only public if IMPLICIT_CTDS_32BIT is defined.
It means that with this #define existing software can be built against the
new and an old Casacore version without any change.

The DataManager interface has been changed more drastically, but still
in a backward compatible way. Specialized storage managers such as
LofarStMan, ASDMStMan and DyscoStMan should build fine against the new
and an old Casacore version without any change. 
However, in this way such storage managers can only support tables
with less than 2^31 rows. To extend it to 2^63 rows, they should use
the new interface as depicted in DataManagerColumn.h.
The main changes are:
 * Several virtual functions in class DataManager got the suffix 64
   (e.g. create64) to differentiate them from the old function names
   which still exist for backward compatibility.
 * DataManager column classes (such as ISMColumn) should be derived from
   class StManColumnBase which implements some common functionality.
 * The canAccess functions are obsolete and can be removed. Formerly the
   Tables classes ScalarColumn and ArrayColumn contained the default
   implementations for getColumn, getSlice, etc.. This has been moved to
   class DataManagerColumn making the canAccess functions obsolete
   in the new interface. Of course, these functions should be kept if
   backward compatibily is needed.
 * The interface of get/put functions handling arrays has been changed
   from void* to ArrayBase&, mainly to have it more clear. The native
   storage manager classes now often can use Array functionality to
   handle most array data types instead of having to handle these
   data types explicitly. 

Although the DataManager interface has changed considerably, it is
still fully backward compatible. The default implementations of the new
functions call the old functions in the StManColumn class and convert
rownr_t to uInt (while checking if its value is < 2^31).

The test programs tExternalStMan.cc and tExternalStManNew.cc show
how the old and the new interface are used in a data manager. They
reside in tables/DataMan/test.
Note that the build of tExternalStMan.cc and other unchanged data
manager classes give hidden virtual function warnings. The reason is
that in the new interface functions such as 'shape' are overloaded for
rownr_t and uInt, while only the uInt version is used in these classes
using the old interface.

Note that a nice side-effect of the DataManager change is that
libcasa_tables.so  shrunk about 10%. It will even be more once the
backward compatibility functionality can be removed.


## ABI change
---
As can be expected, the ABI has been changed considerably, so software
needs to be rebuilt between using the new and old Casacore libraries.
It results in the new major Casacore version 3.4.0.


## Changes in other software
---
All other software in the Casacore package, notably the MS package,
has been changed to use the new CTDS interface.
A few external packages (LOFAR, CASA) have been built against the new
interface. With one minor (backward compatible) change LOFAR built
fine. CASA built fine after defining -DIMPLICIT_CTDS_32BIT in its main
CMakeLists.txt. It means that both packages can be built against the
old and new interface without any problem.

Of course, over time packages such as CASA should change to using
rownr_t instead of uInt where applicable. 


## Backward compatibility functions
---
Several functions have been added to make the API change backward
compatible. They are intended to be temporary, so they should be
removed once all software using Casacore uses 64-bit row numbers.
Tables backward compatibility to be removed consists of the following
functions:
 * In class RowNumbers the Vector<uInt> and std::vector<uInt> constructor
   and the Vector<uInt> operator.
 * In class RefRows the functions using Vector<uInt>.
 * In class TableExprNode the functions getColumnXXX using Vector<uInt>.

Once all external data managers use 64-bit row numbers, the following
DataMan classes and functions can be removed.
 * In class DataManager the functions create, open, open1, resync, resync1,
   addRow and removeRow.
 * The entire class class StManColumn.
