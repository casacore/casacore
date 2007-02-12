import os
import re
import sys
import platform

def generate(env):
    def CheckCasaLib(context, lib):
        context.Message("Checking casa library '%s'..."%lib)
        context.Result(r)
        return r

    def AddCasaPlatform():
	pd = { "darwin": ["-DAIPS_DARWIN"],
	       "64bit": ["-D__x86_64__", "-DAIPS_64B"],
	       "linux": ["-DAIPS_LINUX"],
	       "cray": ["-DAIPS_LINUX", "-DAIPS_NOLARGEFILE", 
			"-DAIPS_NO_LEA_MALLOC" ,"-Minform=severe"]
	       }
	# -D_FILE_OFFSET_BITS=64 -D_LARGEFILE_SOURCE
	platfdefs = ["-DAIPS_STDLIB", "-DAIPS_AUTO_STL"]
	sysplf = sys.platform
	# do some dodgey cray detection...
	if os.environ.has_key("CATAMOUNT_DIR"):
	    sysplf = "cray"
	sysarch = platform.architecture()[0]
        if  sysplf == 'linux2':
	    platfdefs += pd["linux"]
	    if sysarch == '64bit':
		#env["CASAPLATFORM"] +=  sysarch
		#don't know why but lib*.a needs to have -fPIC here
		platfdefs += pd["64bit"]	    
		platfdefs += ["-fPIC"]
	elif sysplf == "cray":
	    # pgi compiler
	    # had a go at this, but it seem to need a whole lot
	    # of environment variables
	    # probably nedd a pgi builder
	    print "Currently not supported"
	    sys.exit()
	    env["CXX"] = "CC"
	    platfdefs += pd["xt3"]
	else:
	    platfdefs += pd[sysplf]
	env.Append(CPPFLAGS=platfdefs)


    def CasaLexYacc():
	env["LEXCOM"] = "$LEX $LEXFLAGS -t -P${BASENOSUFFIX} $SOURCES > $TARGET"
	env['YACCCOM']   = '$YACC $YACCFLAGS -p ${BASENOSUFFIX} -o $TARGET $SOURCES'


    def AddCasaTest(conf):
        conf.AddTests({'CheckCasa': CheckCasa})

    env.AddCasaTest = AddCasaTest
    env.AddCasaPlatform = AddCasaPlatform

def exists(env):
    return true
