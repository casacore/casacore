import sys
from  SCons.Variables import ListVariable, EnumVariable

def generate(env):
    def AddCompilerOptions( opts ):
        """ Adds the build environment options to the opts.  """
        opts.Add(("CC", "The c compiler", None))
        opts.Add(("CXX", "The c++ compiler", None))
        opts.Add(("extracppflags", "Extra preprocessor flags ", None))
        opts.Add(("extracxxflags", "Extra c++ compiler flags ", None))
        opts.Add(("extracflags", "Extra c compiler flags ", None))
        opts.Add(("extracpppath", "Extra include paths ", None))
        opts.Add(("extralibpath", "Extra library paths ", None))
        opts.Add(("extralinkflags", "Extra linker flags ", None))
        if sys.platform == 'darwin':
            opts.Add(ListVariable("universal", 
                                    "Build universal libraries under OS X?", 
                                    "", ["", "ppc", "i386", 
                                           "ppc64", "x86_64"]))
        opts.Add(("extraldlibrarypath", 
                 "The LD_LIBRARY_PATH does not get imported. Specify \
                  elements here which are needed for runtime purposes",
                 None))

    def AddFortranOptions( opts ):
        """ Adds the build environment options to the opts.  """
        opts.Add(("extrafflags", "Extra fortran flags ", None))
        opts.Add(("FORTRAN", "The fortran compiler", None))
        opts.Add(("f2clib", "The fortran to c library", None))

    def AddCasaOptions( opts ):
        """ Adds the build environment options to the opts.  """
        pass

    def AddBuildEnvOptions( opts ):
        """ Adds the build environment options to the opts.  """
        opts.Add(ListVariable("build", "The build type", 
                            "opt", ["opt", "dbg"]))

    def AddCommonOptions( opts ):
         AddBuildEnvOptions(opts)
         AddCompilerOptions(opts)
         AddCasaOptions(opts)

    env.AddCommonOptions = AddCommonOptions
    env.AddFortranOptions = AddFortranOptions

def exists(env):
    return 1
