import os

def AppsBuilder(env, installer=None):
    myenv = env.Clone()
    pkgdir = os.path.split(os.path.abspath(".."))[-1]
    libname = "casa_"+pkgdir

    # get all the sourcefiles recursively
    cpps = myenv.SGlob("*.cc", recursive=True )

    myenv.PrependUnique(LIBS=myenv.GetDependencies(libname))
    myenv.PrependUnique(LIBS=[libname])
    myenv.PrependUnique(LIBPATH=[myenv["BUILDDIR"]])

    cpppath = myenv.get("CPPPATH", [])
    for i in cpps:
        # get the basename no suffix of the file
        tinfile = os.path.splitext(os.path.basename(i))[0]
        tdir = os.path.split(i)[0]
        tapp = myenv.Program(source=i, CPPPATH=cpppath+[tdir])
        myenv.Alias(pkgdir, tapp)
        myenv.Default(tapp)
        installer.AddProgram(tapp)
        # fix this to work with multiple files or if it doesn't exists
        prg = str(tapp[0])+".csh"
        if os.path.exists(prg):
            installer.AddProgram(prg)


def CasaBuilder(env, target=None, source="*.cc", installer=None,
                tests=True, test_libs=[]):
    if target is None:
        target = os.path.split(os.path.abspath("."))[-1]

    myenv = env.Clone()
    builddir = str(myenv["BUILDDIR"])

    # get all the sourcefiles recursively
    lls = myenv.SGlob("*.ll", recursive=True )
    for f in lls:
        myenv.CXXFile(f, CXXFILESUFFIX=".lcc")
        path = os.path.split(f)[0]
        myenv.AppendUnique(CPPPATH=[myenv.Dir(path)])

    yys = myenv.SGlob("*.yy", recursive=True )
    for f in yys:
        myenv.CXXFile(f, CXXFILESUFFIX=".ycc")

    cpps = myenv.SGlob(source, excludedirs=["test", "apps", "fortran"],
                       recursive=True )

    libname = "casa_%s" % target
    lib = os.path.join(builddir, libname)

    # dependencies ...
    myenv.PrependUnique(LIBS=myenv.GetDependencies(libname))
    if not myenv.get("disable_static"):
        slib =  myenv.StaticLibrary(target = lib, source = [cpps])
        myenv.Alias(target, slib)
        installer.AddLibrary(slib)

    if myenv.get("enable_shared"):
        dlib =  myenv.SharedLibrary(target = lib, source = [cpps])
        myenv.Alias(target, dlib)
        installer.AddLibrary(dlib)
    myenv.Default(target)

    rootdir = myenv.Dir("#").abspath
    # install headers, only works with absolute dir.
    installer.AddHeaders( rootdir+"/%s" % target, "*.h",
                          "casacore/%s" % target, True )
    installer.AddHeaders( rootdir+"/%s" % target , "*.tcc",
                          "casacore/%s" % target, True )

    if tests:
        testenv = myenv.Clone()
        testenv.PrependUnique(LIBPATH=[os.path.join(builddir, target)])
        # gather test files
        # SGlob works in build dir, so only for cc files
        tests = testenv.SGlob("test/*.cc")
        tests += testenv.SGlob("*/test/*.cc")
        # allow either linking against shared or static, static if both present
        testenv.PrependUnique(LIBS=[libname]+test_libs)
        for t in tests:
            testenv.addAssayTest(t, alias='test_'+target)


def generate(env):
    def BuildEnv(buildtype):
        lenv = env.Clone()
        # to find package based includes
        lenv.PrependUnique(CPPPATH=['#'])
        lenv.AppendUnique(CPPFLAGS=["-W", "-Wall", "-Woverloaded-virtual"])
        if buildtype == "dbg":
            lenv.AppendUnique(CPPFLAGS=["-g"])
        elif buildtype == "opt":
            lenv.AppendUnique(CPPFLAGS=["-O2"])
            lenv.AppendUnique(FORTRANFLAGS=["-O2"])
        # buildir name
        lenv["BUILDDIR"] = env.Dir("#/build_%s/%s" % (env.PlatformIdent(),
                                                      buildtype))
        lenv.PrependUnique(LIBPATH=[lenv["BUILDDIR"]])
        return lenv
    env.BuildEnv = BuildEnv

    env.AddMethod(CasaBuilder)
    env.AddMethod(AppsBuilder)

def exists(env):
    return 1
