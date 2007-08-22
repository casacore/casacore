import os
import re
import sys
import platform

def generate(env):
    def CustomCasaCom():
	"""
	Overwrite lex/yacc commands to include -p/-P options
	"""
	env["LEXCOM"] = "$LEX $LEXFLAGS -t -P${SOURCE.filebase} $SOURCES > $TARGET"
	env['YACCCOM']   = '$YACC $YACCFLAGS -p ${SOURCE.filebase} -o $TARGET $SOURCES'
    env.CustomCasaCom = CustomCasaCom

    def AddCasaPlatform():
	pd = { "darwin": ["-DAIPS_DARWIN", "-DAIPS_NO_LEA_MALLOC"],
	       "64bit": ["-D__x86_64__", "-DAIPS_64B"],
	       "linux": ["-DAIPS_LINUX"],
	       "cray": ["-DAIPS_CRAY_PGI", "-DAIPS_LINUX", "-DAIPS_NOLARGEFILE",
			"-DAIPS_NO_LEA_MALLOC" ,"-Minform=severe"]
	       }
	# -D_FILE_OFFSET_BITS=64 -D_LARGEFILE_SOURCE
	platfdefs = ["-DAIPS_STDLIB", "-DAIPS_AUTO_STL"]
	sysplf = sys.platform
	# do some dodgey cray detection...
	if os.environ.has_key("CATAMOUNT_DIR"):
	    sysplf = "cray"
	sysarch = platform.architecture()[0]
	if sysarch == '64bit':
	    platfdefs += pd["64bit"]
        if  sysplf == 'linux2':
	    platfdefs += pd["linux"]
	    if sysarch == '64bit':
		#env["CASAPLATFORM"] +=  sysarch
		#don't know why but lib*.a needs to have -fPIC here
		platfdefs += ["-fPIC"]
                env.AppendUnique(SHFORTRANFLAGS=["-fPIC"])
	elif sysplf == "cray":
	    # pgi compiler
	    # had a go at this, but it seem to need a whole lot
	    # of environment variables
	    # probably nedd a pgi builder
	    #print "Currently not supported"
	    #sys.exit()
	    #env["CXX"] = "CC"
	    platfdefs += pd["cray"]
            #platfdefs += ["-fPIC"]
	else:
	    platfdefs += pd[sysplf]
	if sys.byteorder == "little":
	    platfdefs += ["-DAIPS_LITTLE_ENDIAN"]
        env.AppendUnique(CPPFLAGS=platfdefs)
	if env["PLATFORM"] == 'darwin':
	    # otherwise darwin puts builddir into the name
	    env.Append(SHLINKFLAGS=["-install_name", "${TARGET.file}"])
	    env.Append(SHLINKFLAGS=["-single_module"])

    AddCasaPlatform()

    def CheckCasaLib(context, lib):
        context.Message("Checking casa library '%s'..."%lib)
        context.Result(r)
        return r

def exists(env):
    return true
