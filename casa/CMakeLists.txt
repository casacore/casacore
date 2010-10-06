#
# CASA casa
#

add_library (casa_casa
Arrays/Array2Math.cc
Arrays/ArrayUtil2.cc
Arrays/ArrayError.cc
Arrays/Matrix2Math.cc
Arrays/AxesSpecifier.cc
Arrays/IPosition2.cc
Arrays/Slicer.cc
Arrays/ArrayOpsDiffShapes.cc
Arrays/AxesMapping.cc
Arrays/Array2.cc
Arrays/ArrayPosIter.cc
Arrays/ArrayBase.cc
Arrays/MaskArrMath2.cc
Arrays/Slice.cc
Arrays/ExtendSpecifier.cc
Arrays/IPosition.cc
BasicMath/Random.cc
BasicMath/Math.cc
BasicMath/Primes.cc
BasicSL/Constants.cc
BasicSL/RegexBase.cc
BasicSL/Complex.cc
BasicSL/String.cc
BasicSL/IComplex.cc
Containers/RecordInterface.cc
Containers/Record.cc
Containers/IterError.cc
Containers/HashMap2.cc
Containers/RecordFieldId.cc
Containers/RecordField2Writer.cc
Containers/List2.cc
Containers/Map2.cc
Containers/RecordDesc.cc
Containers/RecordDescRep.cc
Containers/ValueHolderRep.cc
Containers/Record2.cc
Containers/RecordRep.cc
Containers/StackError.cc
Containers/ValueHolder.cc
Containers/Record2Interface.cc
Containers/Stack2.cc
Exceptions/Error2.cc
HDF5/HDF5Record.cc
HDF5/HDF5Error.cc
HDF5/HDF5Group.cc
HDF5/HDF5File.cc
HDF5/HDF5Object.cc
HDF5/HDF5DataType.cc
HDF5/HDF5DataSet.cc
HDF5/HDF5HidMeta.cc
Inputs/Input.cc
Inputs/Param.cc
IO/BucketFile.cc
IO/ByteSinkSource.cc
IO/ByteSink.cc
IO/TapeIO.cc
IO/FilebufIO.cc
IO/ConversionIO.cc
IO/LargeFilebufIO.cc
IO/AipsIO.cc
IO/CanonicalIO.cc
IO/RawIO.cc
IO/TypeIO.cc
IO/LockFile.cc
IO/LargeFiledesIO.cc
IO/FileLocker.cc
IO/LECanonicalIO.cc
IO/ByteIO.cc
IO/FiledesIO.cc
IO/ByteSource.cc
IO/RegularFileIO.cc
IO/StreamIO.cc
IO/LargeRegularFileIO.cc
IO/MemoryIO.cc
IO/BucketCache.cc
IO/BaseSinkSource.cc
IO/MMapIO.cc
IO/MMapfdIO.cc
IO/BucketBase.cc
IO/BucketBuffered.cc
IO/BucketMapped.cc
Logging/LogSink.cc
Logging/LogOrigin.cc
Logging/LogIO.cc
Logging/LogFilterInterface.cc
Logging/StreamLogSink.cc
Logging/LogMessage.cc
Logging/LogFilter.cc
Logging/NullLogSink.cc
Logging/LogSinkInterface.cc
Logging/MemoryLogSink.cc
OS/PrecTimer.cc
OS/ModcompConversion.cc
OS/Time.cc
OS/CanonicalConversion.cc
OS/IBMDataConversion.cc
OS/ModcompDataConversion.cc
OS/DirectoryIterator.cc
OS/DynLib.cc
OS/CanonicalDataConversion.cc
OS/Path.cc
OS/Memory.cc
OS/LECanonicalConversion.cc
OS/File.cc
OS/Directory.cc
OS/LittleEndianConversion.cc
OS/Timer.cc
OS/DataConversion.cc
OS/VAXConversion.cc
OS/malloc.cc
OS/EnvVar.cc
OS/RegularFile.cc
OS/DOos.cc
OS/Conversion.cc
OS/VAXDataConversion.cc
OS/HostInfo.cc
OS/IBMConversion.cc
OS/RawDataConversion.cc
OS/LECanonicalDataConversion.cc
OS/SymLink.cc
Quanta/UnitMap3.cc
Quanta/UnitName.cc
Quanta/Unit.cc
Quanta/UnitMap6.cc
Quanta/MVDoppler.cc
Quanta/QC.cc
Quanta/MVBaseline.cc
Quanta/UnitDim.cc
Quanta/MVEarthMagnetic.cc
Quanta/MVPosition.cc
Quanta/MVDirection.cc
Quanta/MeasValue.cc
Quanta/UnitMap2.cc
Quanta/QMath2.cc
Quanta/UnitMap7.cc
Quanta/UnitVal.cc
Quanta/MVuvw.cc
Quanta/MVTime.cc
Quanta/MVDouble.cc
Quanta/UnitMap5.cc
Quanta/Euler.cc
Quanta/QBase.cc
Quanta/RotMatrix.cc
Quanta/MVRadialVelocity.cc
Quanta/UnitMap.cc
Quanta/UnitMap4.cc
Quanta/MVEpoch.cc
Quanta/MVAngle.cc
Quanta/QLogical2.cc
Quanta/Quantum2.cc
Quanta/MVFrequency.cc
Quanta/QuantumHolder.cc
System/PGPlotterInterface.cc
System/AipsrcVString.cc
System/PGPlotter.cc
System/ObjectID2.cc
System/AipsrcValue2.cc
System/AipsrcBool.cc
System/AppInfo.cc
System/Aipsrc.cc
System/AipsrcVBool.cc
System/PGPlotterNull.cc
System/ObjectID.cc
System/Choice.cc
System/ProgressMeter.cc
Utilities/cregex.cc
Utilities/RecordTransformable.cc
Utilities/DynBuffer.cc
Utilities/SortError.cc
Utilities/DataType.cc
Utilities/Sequence2.cc
Utilities/CountedPtr2.cc
Utilities/RegSequence.cc
Utilities/Sort.cc
Utilities/ValType.cc
Utilities/StringDistance.cc
Utilities/Notice.cc
Utilities/MUString.cc
Utilities/Regex.cc
Utilities/BitVector.cc
Utilities/Fallible2.cc
Utilities/CompositeNumber.cc
Utilities/Copy2.cc
aips.cc
version.cc
)

if (HDF5_FOUND)
    list (APPEND de_libraries ${HDF5_LIBRARIES})
endif (HDF5_FOUND)
if (READLINE_FOUND)
    list (APPEND de_libraries ${READLINE_LIBRARIES})
endif (READLINE_FOUND)

target_link_libraries (
casa_casa
${de_libraries}
${libm}
dl
)

install (
TARGETS casa_casa
LIBRARY DESTINATION lib
ARCHIVE DESTINATION lib
LIBRARY PERMISSIONS OWNER_READ OWNER_WRITE OWNER_EXECUTE GROUP_READ GROUP_EXECUTE WORLD_READ WORLD_EXECUTE
)

install (FILES
Arrays/ArrayAccessor.h
Arrays/ArrayBase.h
Arrays/ArrayError.h
Arrays/Array.h
Arrays/Array.tcc
Arrays/ArrayIO.h
Arrays/ArrayIO.tcc
Arrays/ArrayIter.h
Arrays/ArrayIter.tcc
Arrays/ArrayLogical.h
Arrays/ArrayLogical.tcc
Arrays/ArrayMath.h
Arrays/ArrayMath.tcc
Arrays/ArrayOpsDiffShapes.h
Arrays/ArrayOpsDiffShapes.tcc
Arrays/ArrayPartMath.h
Arrays/ArrayPartMath.tcc
Arrays/ArrayPosIter.h
Arrays/ArrayUtil.h
Arrays/ArrayUtil.tcc
Arrays/AxesMapping.h
Arrays/AxesSpecifier.h
Arrays/Cube.h
Arrays/Cube.tcc
Arrays/ExtendSpecifier.h
Arrays/IPosition.h
Arrays/LogiArrayFwd.h
Arrays/LogiArray.h
Arrays/LogiCube.h
Arrays/LogiMatrix.h
Arrays/LogiVector.h
Arrays/MaskArrIO.h
Arrays/MaskArrIO.tcc
Arrays/MaskArrLogi.h
Arrays/MaskArrLogi.tcc
Arrays/MaskArrMath.h
Arrays/MaskArrMath.tcc
Arrays/MaskedArray.h
Arrays/MaskedArray.tcc
Arrays/MaskLogiArrFwd.h
Arrays/MaskLogiArr.h
Arrays/Matrix.h
Arrays/Matrix.tcc
Arrays/MatrixIter.h
Arrays/MatrixIter.tcc
Arrays/MatrixMath.h
Arrays/MatrixMath.tcc
Arrays/Slice.h
Arrays/Slicer.h
Arrays/Vector.h
Arrays/Vector.tcc
Arrays/Vector2.tcc
Arrays/VectorIter.h
Arrays/VectorIter.tcc
Arrays/VectorSTLIterator.h
DESTINATION include/casacore/casa/Arrays
)

install (FILES
BasicMath/ConvertScalar.h
BasicMath/Functional.h
BasicMath/Functional.tcc
BasicMath/Functors.h
BasicMath/Math.h
BasicMath/Primes.h
BasicMath/Random.h
DESTINATION include/casacore/casa/BasicMath
)

install (FILES
BasicSL/Complexfwd.h
BasicSL/Complex.h
BasicSL/Constants.h
BasicSL/IComplex.h
BasicSL/RegexBase.h
BasicSL/String.h
DESTINATION include/casacore/casa/BasicSL
)

install (FILES
Containers/Block.h
Containers/BlockIO.h
Containers/BlockIO.tcc
Containers/HashMap.h
Containers/HashMap.tcc
Containers/HashMapIO.h
Containers/HashMapIO.tcc
Containers/HashMapIter.h
Containers/HashMapIter.tcc
Containers/IterError.h
Containers/Link.h
Containers/Link.tcc
Containers/List.h
Containers/List.tcc
Containers/ListIO.h
Containers/ListIO.tcc
Containers/Map.h
Containers/Map.tcc
Containers/MapIO.h
Containers/MapIO.tcc
Containers/ObjectPool.h
Containers/ObjectPool.tcc
Containers/ObjectStack.h
Containers/ObjectStack.tcc
Containers/OrderedMap.h
Containers/OrderedMap.tcc
Containers/OrderedPair.h
Containers/OrderedPair.tcc
Containers/OrdMapIO.h
Containers/OrdMapIO.tcc
Containers/OrdPairIO.h
Containers/OrdPairIO.tcc
Containers/PoolStack.h
Containers/PoolStack.tcc
Containers/Queue.h
Containers/Queue.tcc
Containers/RecordDesc.h
Containers/RecordDescRep.h
Containers/RecordField.h
Containers/RecordField.tcc
Containers/RecordFieldId.h
Containers/RecordFieldWriter.h
Containers/RecordFieldWriter.tcc
Containers/Record.h
Containers/RecordInterface.h
Containers/RecordRep.h
Containers/SimOrdMap.h
Containers/SimOrdMap.tcc
Containers/SimOrdMapIO.h
Containers/SimOrdMapIO.tcc
Containers/StackError.h
Containers/Stack.h
Containers/Stack.tcc
Containers/ValueHolder.h
Containers/ValueHolderRep.h
DESTINATION include/casacore/casa/Containers
)

install (FILES
Exceptions/Error.h
Exceptions/Error.tcc
DESTINATION include/casacore/casa/Exceptions
)

install (FILES
HDF5/HDF5DataSet.h
HDF5/HDF5DataType.h
HDF5/HDF5Error.h
HDF5/HDF5File.h
HDF5/HDF5Group.h
HDF5/HDF5HidMeta.h
HDF5/HDF5Object.h
HDF5/HDF5Record.h
DESTINATION include/casacore/casa/HDF5
)

install (FILES
Inputs/Input.h
Inputs/Param.h
DESTINATION include/casacore/casa/Inputs
)

install (FILES
IO/AipsIOCarray.h
IO/AipsIOCarray.tcc
IO/AipsIO.h
IO/BaseSinkSource.h
IO/BucketBase.h
IO/BucketBuffered.h
IO/BucketCache.h
IO/BucketFile.h
IO/BucketMapped.h
IO/ByteIO.h
IO/ByteSink.h
IO/ByteSinkSource.h
IO/ByteSource.h
IO/CanonicalIO.h
IO/ConversionIO.h
IO/FilebufIO.h
IO/FiledesIO.h
IO/FileLocker.h
IO/LargeFilebufIO.h
IO/LargeFiledesIO.h
IO/LargeIOFuncDef.h
IO/LargeRegularFileIO.h
IO/LECanonicalIO.h
IO/LockFile.h
IO/MemoryIO.h
IO/MMapfdIO.h
IO/MMapIO.h
IO/RawIO.h
IO/RegularFileIO.h
IO/StreamIO.h
IO/TapeIO.h
IO/TypeIO.h
DESTINATION include/casacore/casa/IO
)

install (FILES
Logging/LogFilter.h
Logging/LogFilterInterface.h
Logging/LogIO.h
Logging/LogMessage.h
Logging/LogOrigin.h
Logging/LogSink.h
Logging/LogSinkInterface.h
Logging/MemoryLogSink.h
Logging/NullLogSink.h
Logging/StreamLogSink.h
DESTINATION include/casacore/casa/Logging
)

install (FILES
OS/CanonicalConversion.h
OS/CanonicalDataConversion.h
OS/Conversion.h
OS/DataConversion.h
OS/Directory.h
OS/DirectoryIterator.h
OS/DOos.h
OS/DynLib.h
OS/EnvVar.h
OS/File.h
OS/HostInfoBsd.h
OS/HostInfoDarwin.h
OS/HostInfo.h
OS/HostInfoHpux.h
OS/HostInfoIrix.h
OS/HostInfoLinux.h
OS/HostInfoOsf1.h
OS/HostInfoSolaris.h
OS/IBMConversion.h
OS/IBMDataConversion.h
OS/LECanonicalConversion.h
OS/LECanonicalDataConversion.h
OS/LittleEndianConversion.h
OS/malloc.h
OS/Memory.h
OS/ModcompConversion.h
OS/ModcompDataConversion.h
OS/Path.h
OS/PrecTimer.h
OS/RawDataConversion.h
OS/RegularFile.h
OS/SymLink.h
OS/Time.h
OS/Timer.h
OS/VAXConversion.h
OS/VAXDataConversion.h
DESTINATION include/casacore/casa/OS
)

install (FILES
Quanta/Euler.h
Quanta/MeasValue.h
Quanta/MVAngle.h
Quanta/MVBaseline.h
Quanta/MVDirection.h
Quanta/MVDoppler.h
Quanta/MVDouble.h
Quanta/MVEarthMagnetic.h
Quanta/MVEpoch.h
Quanta/MVFrequency.h
Quanta/MVPosition.h
Quanta/MVRadialVelocity.h
Quanta/MVTime.h
Quanta/MVuvw.h
Quanta/QBase.h
Quanta/QC.h
Quanta/QLogical.h
Quanta/QLogical.tcc
Quanta/QMath.h
Quanta/QMath.tcc
Quanta/Quantum.h
Quanta/Quantum.tcc
Quanta/QuantumHolder.h
#Quanta/QuantumType.h
Quanta/RotMatrix.h
Quanta/UnitDim.h
Quanta/Unit.h
Quanta/UnitMap.h
Quanta/UnitName.h
Quanta/UnitVal.h
DESTINATION include/casacore/casa/Quanta
)

install (FILES
System/Aipsrc.h
System/AipsrcValue.h
System/AipsrcValue.tcc
System/AipsrcVector.h
System/AipsrcVector.tcc
System/AppInfo.h
System/Choice.h
System/ObjectID.h
System/PGPlotter.h
System/PGPlotterInterface.h
System/PGPlotterNull.h
System/ProgressMeter.h
DESTINATION include/casacore/casa/System
)

install (FILES
Utilities/Assert.h
Utilities/Assert.tcc
Utilities/BinarySearch.h
Utilities/BinarySearch.tcc
Utilities/BitVector.h
Utilities/CASATask.h
Utilities/Compare.h
Utilities/Compare.tcc
Utilities/CompositeNumber.h
Utilities/Copy.h
Utilities/Copy.tcc
Utilities/CountedPtr.h
Utilities/CountedPtr.tcc
Utilities/COWPtr.h
Utilities/COWPtr.tcc
Utilities/cregex.h
Utilities/DataType.h
Utilities/DefaultValue.h
Utilities/DynBuffer.h
Utilities/Fallible.h
Utilities/generic.h
Utilities/GenSort.h
Utilities/GenSort.tcc
Utilities/LinearSearch.h
Utilities/LinearSearch.tcc
Utilities/MUString.h
Utilities/Notice.h
Utilities/PtrHolder.h
Utilities/PtrHolder.tcc
Utilities/RecordTransformable.h
Utilities/Regex.h
Utilities/Register.h
Utilities/Register.tcc
Utilities/RegSequence.h
Utilities/Sequence.h
Utilities/Sequence.tcc
Utilities/SortError.h
Utilities/Sort.h
Utilities/StringDistance.h
Utilities/Template.h
Utilities/Template.tcc
Utilities/ValType.h
Utilities/ValTypeId.h
DESTINATION include/casacore/casa/Utilities
)

install (FILES
aipsdef.h
aipsenv.h
aips.h
aipstype.h
aipsxtype.h
Arrays.h
BasicMath.h
BasicSL.h
complex.h
Containers.h
Exceptions.h
fstream.h
HDF5.h
Inputs.h
IO.h
iomanip.h
iosfwd.h
iosstrfwd.h
iostream.h
istream.h
Logging.h
math.h
namespace.h
OS.h
ostream.h
Quanta.h
sstream.h
stdexcept.h
stdio.h
stdlib.h
stdmap.h
stdvector.h
string.h
System.h
typeinfo.h
Utilities.h
vector.h
version.h
DESTINATION include/casacore/casa
)

if (BUILD_TESTING)
	add_subdirectory (test)
	foreach (casa_module Arrays BasicMath BasicSL Containers
             HDF5 Inputs IO Logging OS Quanta System Utilities)
    	add_subdirectory (${casa_module}/test)
	endforeach (casa_module)
endif (BUILD_TESTING)

