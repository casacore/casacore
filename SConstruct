import os
import sys

EnsureSConsVersion(1,0)

def quiet_print(msg):
    if not GetOption("silent"):
        print msg


env = Environment(ENV = { 'PATH' : os.environ[ 'PATH' ],
                          'HOME' : os.environ[ 'HOME' ]
                          },
                  tools = ["default", "casaoptions", "buildenv", "casa", 
		  	   "utils", "assaytest", "installer", "dependencies"],
                  toolpath = ["scons-tools"],
                  casashrdir="scons-tools",
                  DATA_DIR=".",
                  )
# keep a local sconsign database, rather than one in every directory
env.SConsignFile()


if not (env.Detect(["flex","lex"]) and env.Detect(["bison", "yacc"])):
    print "lex/yacc needs to be installed"
    env.Exit(1)

if not env.get('CXX', None):
    print 'A c++ compiler needs to be installed'
    env.Exit(1)

# create the installer which handles installing the final build
installer = env.Installer()

# Auto configure
if not env.GetOption('clean') and not env.GetOption("help"):
    conf = Configure(env)

    ## NOTE:
    # DON'T USE conf.env.Finish
    # Add all packages explicitly to 'env'
    # but use autoadd=0 for all CheckLibs


    # FreeBSD needs libkvm for casa/OS/HostInfo
    if sys.platform.startswith("freebsd"):
        if conf.CheckLib("kvm", autoadd=0):
            env.AppendUnique(LIBS=["kvm"])
        else:
            env.Exit(1)
    # DL
    if not conf.env.get("disable_dl"):
        pkgname = "dl"
        libname = env.get(pkgname+"_lib")
        conf.env.AddCustomPackage(pkgname)
        if conf.CheckLibWithHeader(libname, 'dlfcn.h', language='c',
                                   autoadd=0):
            env.AppendUnique(LIBS=[libname])
            env.Append(CPPFLAGS=['-DHAVE_DLOPEN'])
        else:
            env.Exit(1)
    else:
        print "Building without dlopen support"

    # fortran
    conf.env.CheckFortran(conf)
    f2cname = conf.env.get("f2c_lib", conf.env["F2CLIB"])
    conf.env.AddCustomPackage("f2c")
    if not conf.CheckLib(f2cname, autoadd=0):
        env.Exit(1)
    conf.env.PrependUnique(LIBS=f2cname)
    env.AddCustomPackage("f2c")
    env["F2CLIB"] = [f2cname]

    #blas or similar
    blasname = conf.env.get("blas_lib", "blas").split(",")
    conf.env.AddCustomPackage("blas")
    blasname.reverse()
    for b in blasname:
        if not conf.CheckLib(b, autoadd=0):
            env.Exit(1)
    conf.env.PrependUnique(LIBS=b)
    env["BLAS"] = blasname
    env.AddCustomPackage("blas")

    # lapack or similar
    lapackname = conf.env.get("lapack_lib", "lapack").split(",")
    conf.env.AddCustomPackage("lapack")
    lapackname.reverse()
    for l in lapackname:
        if not conf.CheckLib(l, autoadd=0):
            env.Exit(1)
    conf.env.PrependUnique(LIBS=l)
    env["LAPACK"] = lapackname
    env.AddCustomPackage("lapack")

    # HDF5
    if conf.env.get("enable_hdf5"):
        pkgname = "hdf5"
        libname = conf.env.get(pkgname+"_lib")
        conf.env.AddCustomPackage(pkgname)
        if conf.CheckLib(libname, autoadd=0):
            env.PrependUnique(LIBS=[libname])
            env.Append(CPPFLAGS=['-DHAVE_HDF5'])
        else:
            env.Exit(1)
    else:
        pass
        # quiet_print("Building without HDF5 support")

    # FFTW3
    if conf.env.get("enable_fftw3"):
        pkgname = "fftw3"
        deflib = "fftw3,fftw3f"
        if not conf.env.get("disable_fftw3_threads"):
            deflib += ",fftw3_threads,fftw3f_threads,pthread"
            env.Append(CPPFLAGS=['-DHAVE_FFTW3_THREADS'])
        # The fftw3_lib variable is defined as 'fftw3' if not explicitly given.
        # So set to default if the value is 'fftw3'.
        # Note that if explicitly given, it can never be done as 'fftw3'.
        libstr = conf.env.get(pkgname+"_lib")
        if libstr == 'fftw3':
            libstr= deflib
        libname = libstr.split(",")
        conf.env.AddCustomPackage(pkgname)
        for l in libname:
            if not conf.CheckLib(l, autoadd=0):
                env.Exit(1)
        env.PrependUnique(LIBS=[libname])
        env.Append(CPPFLAGS=['-DHAVE_FFTW3'])
    else:
        pass
        # quiet_print("Building without FFTW3 support")

    # cfitsio
    pkgname = "cfitsio"
    cfitsioname = conf.env.get(pkgname+"_lib")
    conf.env.AddCustomPackage(pkgname)
    if not conf.CheckLibWithHeader(cfitsioname, "fitsio.h", "c", autoadd=0):
        pkgincdir = env.get("%s_incdir" % pkgname)
            # default location
        if pkgincdir is None:
            # handle SuSE which puts header files in '/usr/include/libcfitsio0' ???
            suseincdir = '/usr/include/libcfitsio0'
            conf.env.PrependUnique(CPPPATH=[suseincdir])
            if not conf.CheckHeader(os.path.join(suseincdir,"fitsio.h")):
                env.Exit(1)
            if not conf.CheckLib(cfitsioname, autoadd=0):
                env.Exit(1)
            else:
                env["%s_incdir" % pkgname] = suseincdir
            
    env["CFITSIO"] = [cfitsioname]
    env.AddCustomPackage(pkgname)

    # wcslib
    pkgname = "wcs"
    libname = conf.env.get(pkgname+"_lib")
    conf.env.AddCustomPackage(pkgname)
    if not conf.CheckLibWithHeader(libname, "wcslib/wcs.h", "c", autoadd=0):
        env.Exit(1)
    env["WCS"] = [libname]
    env.AddCustomPackage(pkgname)

    # Measures data directory
    ddir = env.get("data_dir")
    if ddir:
        adir = os.path.abspath(os.path.expanduser(os.path.expandvars(ddir)))
        if os.path.exists(adir) \
               and os.path.exists(os.path.join(adir, 'ephemerides')) \
               and os.path.exists(os.path.join(adir, 'geodetic')):
            pass
        else:
            print """Warning: measures data directory given doesn't contain
            geodetic and ephemerides.
            Compiling in '%s' as default search location""" % ddir

    else:
        shrdir = env.get("sharedir") or \
            os.path.join(env.get("prefix"), "share")
        ddir = os.path.abspath(os.path.expanduser(os.path.expandvars(shrdir)))
        ddir = os.path.join(shrdir, "casacore", "data")
    # Note the escaped single quotes to handle the string in the define
    ddir = os.path.abspath(os.path.expanduser(os.path.expandvars(ddir)))
    env["DATA_DIR"] = '-DCASADATA=\'"%s"\'' % ddir
    conf.Finish()

# create an environment copy with the dbg/opt compiler flags
buildenv = env.BuildEnv(env.get("build_type"))
env.SConscript("SConscript" ,
               build_dir= buildenv["BUILDDIR"],
               duplicate=0, exports=["buildenv", "installer"])

# add the Tools to the casacore/share directory. This way they can be imported
# by the other casacore packages without having to duplicate them.
installer.AddShares("scons-tools", "*.py", "casacore/", True)
installer.AddProgram("scons-tools/casacore_assay")
installer.AddProgram("scons-tools/casacore_floatcheck")
