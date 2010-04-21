import os
import re
import sys
import platform
import SCons

def generate(env):
    env["LEXFLAGS"] = SCons.Util.CLVar("-P${SOURCE.filebase}")
    env["YACCFLAGS"] = SCons.Util.CLVar("-p ${SOURCE.filebase}")


    def DarwinDevSdk():
        import platform        
        devpath = { "4" : "/Developer/SDKs/MacOSX10.4u.sdk",
                    "5" : "/Developer/SDKs/MacOSX10.5.sdk",
                    "6" : "/Developer/SDKs/MacOSX10.6.sdk" }
        version = platform.mac_ver()[0].split(".")
        if version[0] != '10' or int(version[1]) < 4:
            print "Only Mac OS X >= 10.4 is supported"
            env.Exit(1)
        return devpath[version[1]]
    env.DarwinDevSdk = DarwinDevSdk

    def AddCasaPlatform():
	pd = { "darwin": [],
	       "64bit": [],
	       "linux2": [],
	       "cray": ["-Minform=severe"]
	       }
	# -D_FILE_OFFSET_BITS=64 -D_LARGEFILE_SOURCE
	platfdefs = []
	sysplf = sys.platform
	# do some dodgey cray detection...
	if os.environ.has_key("CATAMOUNT_DIR"):
	    sysplf = "cray"
	sysarch = platform.architecture()[0]
	if sysarch == '64bit':
	    platfdefs += pd["64bit"]
            platfdefs += ["-fPIC"]
            #don't know why but lib*.a needs to have -fPIC here
            env.AppendUnique(SHFORTRANFLAGS=["-fPIC"])
            env.AppendUnique(FORTRANFLAGS=["-fPIC"])
	elif sysplf == "cray":
	    # pgi compiler
	    # had a go at this, but it seem to need a whole lot
	    # of environment variables
	    # probably nedd a pgi builder
	    platfdefs += pd["cray"]
	else:
	    platfdefs += pd[sysplf]
        env.AppendUnique(CPPFLAGS=platfdefs)
	if env["PLATFORM"] == 'darwin':
            uniarch = env.get("universal", False)
            flags = []
            if uniarch:
                for i in uniarch.split(','):		
                    flags += ['-arch', i]
                ppflags =  flags + ['-isysroot' , env.DarwinDevSdk() ]
                linkflags = flags + ['-Wl,-syslibroot,%s' %  env.DarwinDevSdk()]
                env.Append(CPPFLAGS=ppflags)
                env.Append(FORTRANFLAGS=ppflags)
                env.Append(SHFORTRANFLAGS=ppflags)
                env.Append(SHLINKFLAGS=linkflags)
                env.Append(LINKFLAGS=linkflags)
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
