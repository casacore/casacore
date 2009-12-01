import sys
import os
import glob
import re
import platform

sys.path.insert(0, ".")
import lib64linux

ARCHLIBDIR=lib64linux.get_libdir()

def _to_list(xf):
    if xf.count(","):
        return xf.split(",")
    return xf.split()

def generate(env):

    def SGlob(pattern, excludedirs=[], recursive=False):
	# always exclude .svn
	excludedirs.append(".svn")
        path = env.GetBuildPath('SConscript').replace('SConscript', '')	
	if recursive:
	    # remove '*' from pattern is accidentally specified
	    pattern=pattern.replace("*", "")
	    out = []
	    for d, ld, fls in os.walk(path):
		# remove directorys to be excluded 
		for exd in excludedirs:
		    if exd in ld:
			ld.remove(exd)
		for f in fls:		    
		    if f.endswith(pattern):
			drel=d.replace(path,"")
			out.append(os.path.join(drel,f))
	    return out
	else:
	    return [ i.replace(path, '') for i in  glob.glob(path + pattern) ]
    env.SGlob = SGlob

    def AddCustomPath(path=None):
        if path is None or not os.path.exists(path):
            env.Exit(1)
        env.PrependUnique(CPPPATH = [os.path.join(path, "include")])
        env.PrependUnique(LIBPATH = [os.path.join(path, ARCHLIBDIR)])
    env.AddCustomPath = AddCustomPath

    def AddCustomPackage(pkgname=None):
        if pkgname is None:
	    return
        #print env.Dump()
        pkgroot = env.get("%s_root" % pkgname)
        pkgincd = env.get("%s_incdir" % pkgname)
        pkglibd = env.get("%s_libdir" % pkgname)
	incd = None
	libd = None
        if pkgroot == "/usr":
            return
	if pkgroot is not None:
	    incd = os.path.join(pkgroot, "include")
	    libd = os.path.join(pkgroot, ARCHLIBDIR)
	else:	    
	    if pkgincd is not None:
		incd = pkgincd
	    if pkglibd is not None:
		libd = pkglibd
        # print "DEBUG",pkgname,pkgroot,pkgincd, pkglibd
	if incd is not None:
	    if not os.path.exists(incd):
		print "Custom %s include dir '%s' not found" % (pkgname, incd)
		env.Exit(1)
	    env.PrependUnique(CPPPATH = [incd])
	if libd is not None:
 	    if not os.path.exists(libd):
		print "Custom %s lib dir '%s' not found" % (pkgname, libd)
		env.Exit(1)
	    env.PrependUnique(LIBPATH = [libd])

    env.AddCustomPackage = AddCustomPackage

    def PlatformIdent():
	p = sys.platform
	# replace the trailing 2 in linux2
	p = re.sub(re.compile("2$"), "", p)
	return p + "_" + platform.machine()
    env.PlatformIdent = PlatformIdent

    def AddFlags():

        # add extra Hierachy
        hier = env.get("extra_root", None)
        if hier is not None and hier != "/usr":
            hier = os.path.expandvars(os.path.expanduser(hier))
            incdir = os.path.join(hier, 'include')
            env.MergeFlags("-I"+incdir)
            libdir = os.path.join(hier, ARCHLIBDIR)
            ldname = sys.platform == "darwin" and "DYLD_LIBRARY_PATH" or \
                "LD_LIBRARY_PATH"
            env.PrependENVPath(ldname, [libdir])

            # maybe also lib64
            env.MergeFlags("-L"+libdir)
            bindir = os.path.join(hier, 'bin')
            env.PrependENVPath("PATH", [bindir])

        env.MergeFlags(env.get("extra_cppflags", None))
        env.MergeFlags(env.get("extra_cxxflags", None))
        env.MergeFlags(env.get("extra_cflags", None))
        env.MergeFlags(env.get("extra_includedir", None))
        env.MergeFlags(env.get("extra_librarydir", None))

        xf=env.get("extra_libs", None)
        if xf:
            env.AppendUnique(LIBS=_to_list(xf))
        # need to add it to both
        linkf = env.ParseFlags(env.get("extra_linkflags", None))['LINKFLAGS']
        env.AppendUnique(LINKFLAGS=linkf)
        env.AppendUnique(SHLINKFLAGS=linkf)
        # custom not handled by ParseFlags
        xf=env.get("extra_fflags", None)
        if xf:
            env.AppendUnique(FORTRANFLAGS=_to_list(xf))
            env.AppendUnique(SHFORTRANFLAGS=_to_list(xf))
        xf=env.get("extra_ldlibrarypath", None)
        if xf:
            ldname = sys.platform == "darwin" and "DYLD_LIBRARY_PATH" or \
                "LD_LIBRARY_PATH"
            env.PrependENVPath(ldname, _to_list(xf))
        xf=env.get("extra_path", None)
        if xf:
            env.PrependENVPath("PATH", _to_list(xf))
    # set the extra flags where available
    AddFlags()

        
    def CheckFortran(conf):  
        def getf2clib(fc):
            fdict = {'gfortran': 'gfortran', 'g77': 'g2c', 'f77': 'f2c'}
            for k,v in fdict.items():
                if fc.startswith(k):
                    return v
        
	if not conf.env.has_key("FORTRAN"):
	    # auto-detect fortran
	    detect_fortran = conf.env.Detect(['gfortran', 'g77', 'f77'])
	    conf.env["FORTRAN"] = detect_fortran
        f2clib = conf.env.get("f2c_lib", getf2clib(conf.env["FORTRAN"]))
        conf.env["F2CLIB"] = [f2clib]
	if conf.env["FORTRAN"].startswith("g77"):
            fflags = ["-Wno-globals", "-fno-second-underscore"]
	    conf.env.AppendUnique(SHFORTRANFLAGS=fflags)
	    conf.env.AppendUnique(FORTRANFLAGS=fflags)
        conf.env.AppendUnique(SHFORTRANFLAGS=['-fPIC'])

    env.CheckFortran = CheckFortran

    def null_action(target, source, env): return 0

    def message(target, source, env):
        return "%s" % target[0]
    env.MessageAction = env.Action(null_action, message)

def exists(env):
    return 1
