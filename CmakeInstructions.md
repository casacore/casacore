# Introduction #

<br>The best way to use CMake is doing an out-of-source build by creating a directory like build/opt at the same level as the casacore directory. CMake mirrors the source directory structure in this build directory. In this way the source directory does not get polluted with object files, etc.<br>
<br>After the tests are built, they can be run using CTest and the results can be placed on a Dashboard.<br>
<br> Note that use of directories like build/opt and build/dbg is strongly recommended, because they automatically define the build type (see description of CMAKE_BUILD_TYPE below).<br>
<br>
In the remainder of this page it is assumed that one is more or less familiar with the CMake system, although some background information is given.<br>
<br><code>cmake --help</code> can be used to get help info.<br>
<br>
The BuildInstructions page contains general info about how to obtain casacore and which external packages are required to build casacore.<br>
<br>
<h2>Note</h2>

CMake 2.8.6, which ships for example with openSuSE 12.1, contains a broken <code>FindLAPACK.cmake</code>. The fix is relatively easy, but usually requires root access: replace every occurrence of the string <code>CMAKE_FIND_LIBRRAY_SUFFIXES</code> with <code>CMAKE_FIND_LIBRARY_SUFFIXES</code>.<br>
<br>
<h1>Details</h1>

CMake is basically a configure step. A subsequent <code>make</code> command uses the Makefiles created by CMake.<br>
<br>Using the -DMODULE option it is possible to configure for a specific module, so Makefiles for only that module and the modules it depends on are generated.<br>
Furthermore it will not look for external packages the modules to build do not depend on.<br>
<br>
Note that if a configure of the full package is done, it is still possible to build specific source files or modules by going to the correct directory in the build tree and run <code>make</code> in there.<br>
<br>
The modules that can be given are:<br>
<table><thead><th> <b>module</b> </th><th> <b>other modules built with it</b> </th><th> <b>external dependencies</b> </th></thead><tbody>
<tr><td> casa </td><td>  </td><td> readline, dlopen, HDF5 (all optional) </td></tr>
<tr><td> tables </td><td> casa </td><td> Bison/flex (required) </td></tr>
<tr><td> measures </td><td> tables scimath lattices meas </td><td> lapack/blas (required), FFTW3 (optional) </td></tr>
<tr><td> ms </td><td> measures fits msfits derivedmscal </td><td> cfitsio (required) </td></tr>
<tr><td> images </td><td> measures fits coordinates components mirlib </td><td> cfitsio, wcslib (all required) </td></tr></tbody></table>

<h3>Build options</h3>
The options to CMake are given using -D options. The following can be given:<br>
<br>
<table><thead><th> <b>option</b> </th><th> <b>default</b> </th><th> description</th></thead><tbody>
<tr><td> -DBUILD_TESTING </td><td> ON </td><td> ON or OFF telling if the Makefiles get commands to build test programs. If ON, test programs will be built but not executed by the <code>make</code> command. If OFF, the Makefiles do not contain rules to build test programs. </td></tr>
<tr><td> -DCMAKE_BUILD_TYPE </td><td> Debug or Release </td><td> Defines the build type. If not given Debug (-g) is used if the build directory is named dbg or debug. Otherwise Release (-O3) is used. </td></tr>
<tr><td> -DCMAKE_CXX_FLAGS </td><td> -W -Wall </td><td> The C++ compiler flags. </td></tr>
<tr><td> -DCMAKE_INSTALL_PREFIX </td><td> /usr/local </td><td> The install prefix. </td></tr>
<tr><td> -DMODULE </td><td> all </td><td> Defines the module (and its dependencies) to be built. Options are casa, tables, measures, ms, images, and all. </td></tr>
<tr><td> -DUSE_HDF5 </td><td> OFF </td><td> Defines if HDF5 needs to be found. If OFF, casacore is built without HDF5 support. Any version from HDF5-1.8.3 on can be used. </td></tr>
<tr><td> -DUSE_FFTW3 </td><td> OFF </td><td> Defines if FFTW3 needs to be used and found. If OFF, casacore uses FFTPack instead of FFTW3 to do Fast Fourier Transforms.</td></tr>
<tr><td> -DUSE_THREADS </td><td> OFF </td><td> Defines if thread-safety has to be compiled in. If ON, use of static variables in casacore is thread-safe (so far only in modules casa, tables, scimath, and measures). </td></tr>
<tr><td> -DUSE_OPENMP </td><td> OFF </td><td> Defines if OpenMP pragmas should be handled by the compiler. Casacore code contains a few such pragmas. In the future more casacore code will be parallelized using OpenMP. </td></tr>
<tr><td> -DBLAS_ROOT_DIR </td><td> /usr/local </td><td> The path where to find blas. </td></tr>
<tr><td> -DLAPACK_ROOT_DIR </td><td> /usr/local </td><td> The path where to find lapack. </td></tr>
<tr><td> -DCFITSIO_ROOT_DIR </td><td> /usr/local </td><td> The path where to find cfitsio. </td></tr>
<tr><td> -DCFITSIO_INCLUDE_DIR </td><td> /usr/local/include </td><td> The path where to find cfitsio header files </td></tr>
<tr><td> -DCFITSIO_LIBRARY </td><td> /usr/local/lib/libcfitsio.so </td><td> The name of the cfitsio library (can be used to link against static libs) </td></tr>
<tr><td> -DWCSLIB_ROOT_DIR </td><td> /usr/local </td><td> The path where to find wcslib. </td></tr>
<tr><td> -DHDF5_ROOT_DIR </td><td> /usr/local </td><td> The path where to find HDF5. </td></tr>
<tr><td> -DFFTW3_ROOT_DIR </td><td> /usr/local </td><td> The path where to find FFTW3. If not found, casacore uses FFTPack instead of FFTW3 to do Fast Fourier Transforms. </td></tr>
<tr><td> -DFFTW3_DISABLE_THREADS </td><td> ON </td><td> Defines if FFTW3 should use threads </td></tr>
<tr><td> -DDATA_DIR </td><td>  </td><td> The location of the measures data directory to compile in as the default search location. It should be the directory containing the <code>geodetic</code> and <code>ephemerides</code> directories to use. The value can contain an environment variable that is expanded at runtime (e.g. <code>-DDATA_DIR='$ENVVAR/measures/data'</code>). Note it is also possible to define measure tables locations in a $HOME/.casarc file. One can use the program <code>findmeastable</code> to show where casacore finds them. </td></tr></tbody></table>

It will be tested if readline and dlopen are available. If not, casacore is built without support for either of them.<br>
<br>
<h3>Generating preprocessor output or assembler code</h3>
Sometimes it is useful to look at the output of the preprocessor or the assembler code generated by the compiler. This output can be generated for a particular source file like:<br>
<br>
<pre><code>cd tables<br>
make Tables/Table.i                    # preprocess Table.cc<br>
less CMakeFiles/casa_tables.dir/Tables/Table.cc.i<br>
</code></pre>

Similarly, target <code>.s</code> can be used to create files containing assembler code.<br>
<br>
<h3>Executing and checking tests</h3>
Test programs can be executed using the <code>make test</code> or the <code>ctest</code> command. If done in the build directory, all tests will be executed. By doing it in, say, tables/Tables/test, only the test programs in there will be executed. Using ctest's -R or -E options, specific tests can be selected or excluded.<br>
<br>A casacore test not only tests if a test program succeeds, it can also check if its output matches the expected output.<br>
<br>ctest will report about any failing test.<br>
<br>
It is possible to run the tests through a tool like valgrind. It can be done by defining the environment variable CASACORE_CHECK. If defined and not blank, 0, no, or NO, its value will be used as a prefix to the command to execute.<br>
<br>A special value is 1, yes, or YES meaning that the script <code>casacore_memcheck</code> is used to execute valgrind's memcheck tool to check for errors, memory leaks and, open file descriptors. If problems are found, a summary is printed and the detailed valgrind output is written to the file <code>&lt;pgm&gt;.checktool.valgrind</code>. Thus the presence of such a file indicates that valgrind found some errors for test program <code>pgm</code>.<br>
<br>If a test uses a <code>.run</code> file, the <code>.checktool.valgrind</code> file can contain errors of multiple invocations of the test program.<br>
<br>
<h1>Examples</h1>

Install to custom location making use of multi-core processor builds<br>
<pre><code>mkdir build; cd build<br>
cmake -DCMAKE_INSTALL_PREFIX=/opt/casacore<br>
make -j4 &amp;&amp; make install<br>
</code></pre>
Statically link against wcslib<br>
<pre><code>cmake -DWCSLIB_LIBRARY=/usr/local/lib/libwcs.a ..<br>
</code></pre>