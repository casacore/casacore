import sys
import os

deps = {'casa' : None,
	'tables': ['casa'],
	'mirlib': ['casa'],
	'scimath': ['casa'],
	'measures': ['casa', 'tables', 'scimath']
	}

def run_scons(targets, extraarg=""):
    cwd = os.getcwd()
    for target in targets:
        os.chdir(target)
        command = "scons " #+ os.path.basename(target)
        # copy the command line args into the new command
        for arg in sys.argv[1:]:
            command += " " + arg + extraarg
        print "Building package: " + target
        sys.stdout.flush()
	print command
        os.system(command)
        sys.stdout.flush()
        os.chdir(cwd)
# always build casa with install target
xarg = ""
if "-h" not in sys.argv[1:] and "install" not in sys.argv[1:]:
    xarg = "install"
run_scons(['casa'], xarg)
run_scons(['tables/', 'mirlib/'])
