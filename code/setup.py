import sys
import os

def run_scons(targets):
    cwd = os.getcwd()
    for target in targets:
        os.chdir(os.path.dirname(target))
        command = "scons " #+ os.path.basename(target)
        # copy the command line args into the new command
        for arg in sys.argv[1:]:
            command += " " + arg
        print "Building package: " + target
        sys.stdout.flush()
	print command
        os.system(command)
        sys.stdout.flush()
        os.chdir(cwd)

run_scons(['casa/','tables/', 'mirlib/'])
