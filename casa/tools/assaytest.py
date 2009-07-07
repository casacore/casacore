import sys
import os
import glob
import shutil
import string
import subprocess

def copy_dir(src, dst):
    dirname = os.path.basename(os.path.normpath(src))
    dstdir = os.path.join(dst, dirname)

    for dp, dn, fn in os.walk(src):
        if ".svn" in dn: 
            dn.remove(".svn")
        dp1 = dp[dp.find(dirname):]
        if not os.path.exists(os.path.join(dst,dp1)):
            os.mkdir(os.path.join(dst,dp1))
        for f in fn:
            shutil.copy(os.path.join(dp, f), os.path.join(dst,dp1))

def add_ldpath(env):
    envvar = env.get("LIBPATH", [])
    cdir = os.path.abspath(os.curdir)
    outpth  = []
    for pth in envvar:
        spth = str(pth)
        if os.path.isabs(spth):
            outpth.append(spth)
        else:
            outpth.append(os.path.join(cdir, spth))

    outvar = string.join(outpth, os.path.pathsep)
    if len(outvar):
        ldvar = "LD_LIBRARY_PATH"
        if sys.platform == "darwin":
            ldvar = "DYLD_LIBRARY_PATH"
        ldpth = env["ENV"].get(ldvar, "")
        if len(ldpth) > 0:
            outvar += os.path.pathsep + ldpth
        env["ENV"][ldvar] = outvar 
    
def assayAux(target, source, env):
    infile = str(source[0].srcnode().path)
    outpath = os.path.split(str(target[0].path))[0]
    testaux = glob.glob("%s.*" % infile)
    testaux = [ i for i in testaux if not i.endswith('.cc') ]
    testaux = [ i for i in testaux if not i.endswith('.h') ]
    testaux = [ i for i in testaux if not i.endswith('~') ]
    extraaux = env.get("ASSAYAUX", [])
    testaux += [ os.path.join(os.path.split(infile)[0],
                              os.path.split(i)[1]) for i in extraaux ]
    for aux in testaux:
        dstaux = os.path.join(outpath, os.path.split(aux)[1])
        if os.path.isdir(aux):
            copy_dir(aux, outpath)
            env.Depends(target, env.Dir(aux))                
        else:
            shutil.copy(aux, outpath)
            env.Depends(target, env.File(aux))
            # in case they are read-only in svn
            os.chmod(dstaux, 0644)

def assayAction(target, source, env):
    """
    Action for a 'UnitTest' builder object.
    Runs the supplied executable, reporting failure to scons via the test exit
    status.
    When the test succeeds, the file target.passed is created to indicate that
    the test was successful and doesn't need running again unless dependencies
    change.
    """
    testpath =  os.path.split(str(source[0].abspath))
    assaycom = env.File(env["ASSAYCOM"]).abspath
    assaypath = os.path.split(assaycom)[0]
    env.PrependENVPath("PATH", assaypath)
    add_ldpath(env)    
    if not testpath[1]. startswith("t"):
        # don't execute demo programs
        return 0
    p = subprocess.Popen([assaycom, "./%s" % testpath[1]],
                         stdout=subprocess.PIPE, stderr=subprocess.PIPE,
                         env=env["ENV"], cwd=testpath[0])
    info,err = p.communicate()
    if len(info) > 0:
        if info.find("PASS") > -1 or info.find("OK") > -1:
            print info.strip()
            file(str(target[0]),'w').write(info)
            return 0
        else:
            print info.strip()
            if len(err) > 0:
                print err.strip()
            return 0
    print err.strip()
    return 0

def auxString(target, source, env):
    return ""

def assayActionString(target, source, env):
    """
    Return output string which will be seen when running unit tests.
    """
    if os.path.split(str(source[0]))[1].startswith("t"):
        return "Running assay ..."
    else:
        return ""

def addAssayTest(env, target=None, source=None, *args, **kwargs):
    """
    Add a unit test
    Parameters:
    target - If the target parameter is present, it is the name of the test
    executable
    source - list of source files to create the test executable.
    any additional parameters are passed along directly to env.Program().
    Returns:
    The scons node for the unit test.
    All tests added with addAssayTest can be run with the test alias:
    "scons test"
    Any test can be run in isolation from other tests, using the name of the
    test executable provided in the target parameter:
    "scons target"
"""
    if source is None:
        source = target
        target = None
    env.AppendUnique(CPPPATH=[os.path.split(source)[0]])
    if source.endswith(".py"):
        srcbase = source.replace(".py","")
        modelem = os.path.split(srcbase)
        mod = env.LoadableModule(os.path.join(modelem[0], "_"+modelem[1]+".so"),
                                 srcbase+".cc")
        env["ENV"]["PYTHONPATH"] = os.path.split(mod[0].abspath)[0]
        program = [env.File(srcbase+".py")]
        env.Depends(program, mod)
    else:
        program = env.Program(target, source, *args, **kwargs)
    utest = env.Assay(program)
    # add alias to run all unit tests.
    env.Alias(['test', 'check'], utest)
    # make an alias to run the test in isolation from the rest of the tests.
    env.Alias(str(program[0]), utest)
    # and now an alias just for the app name itself, path stripped
    env.Alias(os.path.basename(os.path.normpath(str(program[0]))), utest)
    return utest


def generate(env):
    env['BUILDERS']['Assay'] = env.Builder(
        action = [env.Action(assayAux, auxString),
                  env.Action(assayAction, assayActionString)],
        suffix='.passed')
    env["ASSAYCOM"] = os.path.join(env["casashrdir"][0],"casacore_assay")
    env.AddMethod(addAssayTest)

def exists(env):
    return 1
